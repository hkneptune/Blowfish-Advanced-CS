(*
 * Copyright 1997-2005 Markus Hahn 
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); 
 * you may not use this file except in compliance with the License. 
 * You may obtain a copy of the License at 
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)

{
  to reencrypt cryptfiles with a different algorithm and/or key
}

unit Reencryptor;

{$I config.inc}

interface
uses Classes,
     CrptFile,
     BFManager,
     PathSearch,
     WorkResults,
     FilesProgress,
     IntLists,
     RandomSource,
     SecureMem,
     StringRes,
     MessageCallBack;


// results keeper
type
  TReencryptResults = class(TWorkResults)
  public
    m_sr : TStrRes;
  public

    // constructor
    // -> string resources
    constructor Create(sr : TStrRes); reintroduce; overload;

    // gets the dimension of the entries in the rendered list
    // <- dimension of entries
    function GetDimension : Integer; override;

    // returns column informations
    // <- column infos
    function GetColInfos : TWorkResultsColumnInfos; override;

    // renders a result into a table row
    // -> number of result
    // -> array where to store the entries (correct sized!)
    // <- table row
    procedure Render(nIndex : Integer;
                     var rows : array of String); override;
  end;


// the reencryptor
type
  TReencryptor = class(TBFManager)
  private
    // members
    m_oldFile : TCryptFile;
    m_newFile : TCryptFile;

  public
    // constructor
    // -> name of the old cipher
    // -> name of the new cipher
    // -> random source
    // -> maximum number of errors allowed
    // -> progress callback
    // -> confirmation callback
    // -> string resources
    // exception: EBFError error occured
    // exception: EBFFatalError fatal error occured
    constructor Create(sOldCipher : String;
                       sNewCipher : String;
                       rndSrc : TRandomSource;
                       nMaxErrors : Integer;
                       workProgress : TBFWorkProgress;
                       confirmCB : TMessageCallBack;
                       sr : TStrRes); reintroduce; overload;

    // destructor
    // exception: EBFFatalError fatal error occured
    destructor Destroy; override;

    // the executor
    // -> the files to reencrypt
    // -> sizes of those files
    // -> old key
    // -> new key
    // <- results
    // exception: EBFFatalError fatal error occured
    // exception: EBFInterrupt user interrupt occured
    procedure Execute(files : TPathSearchContainer;
                      fileSizes : TWORD64List;
                      oldKey : TKeyMemory;
                      newKey : TKeyMemory;
                      var results : TReencryptResults);
  end;


implementation
uses SysUtils,
     CallBack,
     bfacslib,
     StringPlus,
     ProgressCallBack;


const
  STRRES_ID = 'REENCRYPTOR';


//////////////////////// TReencryptResults ////////////////////////


constructor TReencryptResults.Create(sr : TStrRes);
begin
  inherited Create;
  m_sr:=sr;
end;


function TReencryptResults.GetDimension : Integer;
begin
  // just two static columns (even when no errors occured)
  Result:=2;
end;


function TReencryptResults.GetColInfos : TWorkResultsColumnInfos;
begin
  Result:=TWorkResultsColumnInfos.Create;

  Result.AddInfo(WORKRESULTS_COLALIGN_LEFT,
                 70,
                 m_sr.Get(STRRES_ID, '000'));

  Result.AddInfo(WORKRESULTS_COLALIGN_LEFT,
                 30,
                 m_sr.Get(STRRES_ID, '001'));
end;



procedure TReencryptResults.Render(nIndex : Integer;
                                   var rows : array of String);
begin
  // get the file name
  rows[0]:=TStringKeeper(m_results.Items[nIndex]).Get;

  // what result? (remember that we won't get any warnings)
  case m_types.Get(nIndex) of
    WORKRESULTS_TYPE_SUCCESS :
      rows[1]:=m_sr.Get(STRRES_ID, '002');

    WORKRESULTS_TYPE_ERROR :
      rows[1]:=m_sr.Get(STRRES_ID, '003') + m_errorMessages.Strings[nIndex];

    WORKRESULTS_TYPE_SKIP :
      rows[1]:=m_sr.Get(STRRES_ID, '004') + m_errorMessages.Strings[nIndex];

    WORKRESULTS_TYPE_BREAK :
      rows[1]:=m_sr.Get(STRRES_ID, '005');
  end;
end;


//////////////////////// TReencryptor ////////////////////////


constructor TReencryptor.Create(sOldCipher : String;
                                sNewCipher : String;
                                rndSrc : TRandomSource;
                                nMaxErrors : Integer;
                                workProgress : TBFWorkProgress;
                                confirmCB : TMessageCallBack;
                                sr : TStrRes);
begin
  inherited Create(sOldCipher,
                   rndSrc,
                   nMaxErrors,
                   workProgress,
                   confirmCB,
                   sr);

  m_oldFile:=Nil;
  m_newFile:=Nil;
  try
    // init. the cryptfile reader
    m_oldFile:=TCryptFile.Create(sOldCipher, sr, rndSrc);
    // init. the cryptfile writer
    m_newFile:=TCryptFile.Create(sNewCipher, sr, rndSrc);
  except
    // fatal error occured?
    on ecff : ECryptFileFatal do
      // (the auto-called destructor will clean up all necessary stuff)
      raise EBFFatalError.Create(ecff.Message);

    // get that other error
    on ecfe : ECryptFileError do
      // (same as above)
      raise EBFError.Create(ecfe.Message);
  end;
end;


destructor TReencryptor.Destroy;
begin
  // clean up
  if (m_newFile <> Nil) then
    m_newFile.Destroy;
  if (m_oldFile <> Nil) then
    m_oldFile.Destroy;
end;


// our transfer buffer size (32kB)
const
  TRANSBUF_SIZE = 32768;


procedure TReencryptor.Execute(files : TPathSearchContainer;
                               fileSizes : TWORD64List;
                               oldKey : TKeyMemory;
                               newKey : TKeyMemory;
                               var results : TReencryptResults);
var
  qLastPos       : WORD64;
  qBytesLeft     : WORD64;
  qAssumedSize   : WORD64;
  qBytesTransf   : WORD64;
  lBytesToTrans  : WORD32;
  nAttr          : Integer;
  nI             : Integer;
  blWasError     : Boolean;
  blOldOpen      : Boolean;
  blNewOpen      : Boolean;
  sOldFile       : String;
  sNewFile       : String;
  wsFile         : WideString;
  pTransBuf      : Pointer;
  wprogress      : TBFWorkProgress;

// local subroutine to cleanup dynamic resources
// -> True: delete the temporary file / False: don't touch it
// <- True: success / False: fatal error (files couldn't get closed)
function CleanUp(blDelTempFile : Boolean) : Boolean;
begin
  Result:=True;
  FreeMem(pTransBuf);
  try
    if (blOldOpen) then
      m_oldFile.Close;
  except
    on ECryptFileError do
      Result:=False;
  end;
  try
    if (blNewOpen) then
      m_newFile.Close;
    if (blDelTempFile) then
      DeleteFile(sNewFile);
  except
    on ECryptFileError do
      Result:=False;
  end;
end;

begin

  // create our transfer buffer
  GetMem(pTransBuf, TRANSBUF_SIZE);

  // prepare the callback
  wprogress:=GetWorkProgress;
  with wprogress do begin
    SetMode(BFM_PROGRESS_REENCRYPT);
    SetFirstCall(True);
    ZeroPos;
    SetMaxPos(files.GetNumOfBytes);
    SetNumOfFiles(files.GetNumOfFiles);
  end;

  // make the result keeper
  results:=TReencryptResults.Create(m_sr);

  // now work through all files
  for nI:=1 to files.GetNumOfFiles do begin

    blWasError:=False;

    blOldOpen:=False;
    blNewOpen:=False;

    wprogress.SetCalled(False);
    qAssumedSize:=fileSizes.Get(nI - 1);

    try

      // determine the file name
      if (nI = 1) then
        sOldFile:=files.GetFirstFile
      else
        sOldFile:=files.GetNextFile;

      // get the attributes of the old cryptfile
      nAttr:=FileGetAttr(sOldFile);

      // enable access to the old cryptfile
      FileSetAttr(sOldFile, 0);

      // open the source file
      wsFile:=TStrPlus.StringToWideString(sOldFile);
      m_oldFile.OpenRead(PWideChar(wsFile),
                         oldKey.GetPtr,
                         oldKey.GetSize);
      blOldOpen:=True;

      // open the target file
      sNewFile:=TStrPlus.MakeTempFileName(ExtractFilePath(sOldFile));
      wsFile:=sNewFile;
      m_newFile.OpenCreate(PWideChar(wsFile),
                           newKey.GetPtr,
                           newKey.GetSize);
      blNewOpen:=True;

      // reset the progress callback
      with wprogress do begin
        SetChanged(True);
        SetFileNumber(nI);
        SetFileName(sOldFile);
        qLastPos:=GetActPos;
        SetFileSize(m_oldFile.GetFileSize(True));
        SetNumOfBytes(qAssumedSize);
        SetBytesDone(0);
      end;

      // transfer the data now
      qBytesLeft:=m_oldFile.GetFileSize;
      qBytesTransf:=0;
      while (qBytesLeft > 0) do begin

        // startup the callback
        with wprogress do begin
          SetCalled(True);
          CallBack;
          SetChanged(False);
          SetFirstCall(False);
        end;

        // now transfer some data
        if (qBytesLeft > TRANSBUF_SIZE) then
          lBytesToTrans:=TRANSBUF_SIZE
        else
          lBytesToTrans:=WORD32(qBytesLeft);
        m_oldFile.Read(pTransBuf,
                       lBytesToTrans);
        m_newFile.Write(pTransBuf,
                        lBytesToTrans);

        // next loop
        Dec(qBytesLeft, lBytesToTrans);

        // increase and check the progress level
        Inc(qBytesTransf, lBytesToTrans);
        if (qBytesTransf <= qAssumedSize) then begin
          with wprogress do begin
            IncPos(lBytesToTrans);
            SetBytesDone(qBytesTransf);
          end;
        end;
      end;

      // final callback (set to the correct next level because we didn't
      // count the header, init. data and padding bytes in the loop above)
      with wprogress do begin
        SetActPos(qLastPos + qAssumedSize);
        SetBytesDone(qAssumedSize);
        SetCalled(True);
        CallBack;
      end;

      // close the cryptfiles
      m_newFile.Close;
      blNewOpen:=False;
      m_oldFile.Close;
      blOldOpen:=False;

      // delete the old cryptfile
      if (not DeleteFile(sOldFile)) then begin
        // this is a strange thing, thus throw a fatal error
        results.Add(TStringKeeper.Create(sOldFile),
                    WORKRESULTS_TYPE_ERROR,
                    m_sr.Get(STRRES_ID, '006'));
        CleanUp(True);
        raise EBFFatalError.Create(m_sr.Get(STRRES_ID, '006'));
      end;

      // rename the new cryptfile
      if (not RenameFile(sNewFile, sOldFile)) then begin
        // an absolutly fatal error
        results.Add(TStringKeeper.Create(sOldFile),
                    WORKRESULTS_TYPE_ERROR,
                    m_sr.Get(STRRES_ID, '007'));
        CleanUp(False);
        // (describe the fatal situation in a detailed error message)
        raise EBFFatalError.Create(Format(m_sr.Get(STRRES_ID, '008'),
                                          [sNewFile,
                                           ExtractFileName(sOldFile)]));
      end;

      // restore the original attributes
      FileSetAttr(sNewFile, nAttr);

      // add the result, i.n.
      results.Add(TStringKeeper.Create(sOldFile),
                  WORKRESULTS_TYPE_SUCCESS,
                  '');
    except

      // interrupted?
      on ecbi : ECallBackInterrupt do begin
        results.Add(TStringKeeper.Create(sOldFile),
                    WORKRESULTS_TYPE_BREAK,
                    '');
        if (not CleanUp(True)) then
          raise EBFFatalError.Create(m_sr.Get(STRRES_ID, '009'))
        else
          raise EBFInterrupt.Create(ecbi.Message);
      end;

      // fatal error?
      on ecff : ECryptFileFatal do begin
        results.Add(TStringKeeper.Create(sOldFile),
                    WORKRESULTS_TYPE_ERROR,
                    ecff.Message);
        CleanUp(True);
        raise EBFFatalError.Create(ecff.Message);
      end;

      // other errors? (we will continue)
      on ecfe : ECryptFileError do begin
        if (ecfe is ECryptFileNoCryptFile) then
          // unencrypted files cause just a skip, not an error)
          results.Add(TStringKeeper.Create(sOldFile),
                      WORKRESULTS_TYPE_SKIP,
                      ecfe.Message)
        else begin
          results.Add(TStringKeeper.Create(sOldFile),
                      WORKRESULTS_TYPE_ERROR,
                      ecfe.Message);
          blWasError:=True;
        end;
      end;
    end;

    // if no callback was done we fire at least one to keep the interrupt
    // possibility alive)
    with m_workProgress do begin
      if (not GetCalled) then begin
        SetFileName(sOldFile);
        SetBytesDone(0);
        SetNumOfBytes(qAssumedSize);
        IncPos(qAssumedSize);
        SetFileSize(qAssumedSize);
        SetChanged(True);
        try
          CallBack;
        except
          on ecbi : ECallBackInterrupt do begin
            CleanUp(False);
            raise EBFInterrupt.Create(ecbi.Message);
          end;
        end;
        SetFirstCall(False);
      end;
    end;

    // too many errors?
    if (blWasError) then begin
      if (not IncAndCheckMaxErrors) then begin
        CleanUp(True);
        raise EBFInterrupt.Create(m_sr.Get(STRRES_ID, '005'))
      end;
    end;
  end;

  // final cleanup
  CleanUp(False);
end;



end.
