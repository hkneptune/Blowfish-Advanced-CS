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
  to deslack files
}

unit Deslacker;

{$I config.inc}

interface
uses
  Classes,
  BFManager,
  PathSearch,
  WorkResults,
  FilesProgress,
  IntLists,
  RandomSource,
  StringRes,
  StringPlus,
  MessageCallBack;


// results keeper
type
  TDeslackResults = class(TWorkResults)
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


// result codes for TDeslacker.ClearFileSlack()
const
  DESLACK_ERROR_NOERROR = 0;
  DESLACK_ERROR_ERROR   = 1;
  DESLACK_ERROR_FATAL   = 2;


// the deslacker
type
  TDeslacker = class(TBFBaseManager)
  public
    // the executor
    // -> the files to deslack
    // <- results
    // exception: EBFFatalError fatal error occured
    // exception: EBFInterrupt user interrupt occured
    procedure Execute(files : TPathSearchContainer;
                      var results : TDeslackResults);

    // clears a file's slack
    // -> name of the file to deslack
    // -> where to put an error message (subcontent contains the Win32 error)
    // <- result code, see DESLACK_ERROR_xxx
    function ClearFileSlack(const sFileName : String;
                            errorMess : TStringKeeper = Nil) : Integer;

  end;


implementation
uses
  Windows,
  SysUtils,
  CallBack,
  bfacslib,
  StringPlusI,
  ProgressCallBack;


const
  STRRES_ID = 'DESLACKER';


//////////////////////// TDeslackResults ////////////////////////


constructor TDeslackResults.Create(sr : TStrRes);
begin
  inherited Create;
  m_sr:=sr;
end;


function TDeslackResults.GetDimension : Integer;
begin
  // just two static columns (even when no errors occured)
  Result:=2;
end;


function TDeslackResults.GetColInfos : TWorkResultsColumnInfos;
begin
  Result:=TWorkResultsColumnInfos.Create;

  Result.AddInfo(WORKRESULTS_COLALIGN_LEFT,
                 70,
                 m_sr.Get(STRRES_ID, '000'));

  Result.AddInfo(WORKRESULTS_COLALIGN_LEFT,
                 30,
                 m_sr.Get(STRRES_ID, '001'));
end;



procedure TDeslackResults.Render(nIndex : Integer;
                                   var rows : array of String);
begin
  rows[0]:=TStringKeeper(m_results.Items[nIndex]).Get;

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


//////////////////////// TDeslacker ////////////////////////


procedure TDeslacker.Execute(files : TPathSearchContainer;
                             var results : TDeslackResults);
var
  nI         : Integer;
  blWasError : Boolean;
  sFileName  : String;
  sTemp      : String;
  dres       : TStringKeeper;
  wprogress  : TBFWorkProgress;

procedure CleanUp;
begin
  dres.Destroy;
end;

begin

  // prepare the callback
  wprogress:=GetWorkProgress;
  with wprogress do begin
    SetMode(BFM_PROGRESS_DESLACK);
    SetFirstCall(True);
    ZeroPos;
    SetMaxPos(files.GetNumOfFiles);
    SetNumOfFiles(files.GetNumOfFiles);
    SetBytesDone(0);  // (just to keep things right)
    SetNumOfBytes(0);
    SetFileSize(-1);
  end;

  // make the result keeper
  results:=TDeslackResults.Create(m_sr);

  // now work through all files
  dres:=TStringKeeper.Create;
  for nI:=1 to files.GetNumOfFiles do begin

    if (nI = 1) then
      sFileName:=files.GetFirstFile
    else
      sFileName:=files.GetNextFile;

    // first a callback to show what's going on
    // (FIXME: better to do this afterwards?)
    with m_workprogress do begin
      SetFileName(sFileName);
      IncPos;
      SetChanged(True);
      SetFileNumber(nI);
      try
        CallBack;
      except           
        on ecbi : ECallBackInterrupt do begin
          results.Add(TStringKeeper.Create(sFileName),
                      WORKRESULTS_TYPE_BREAK,
                      '');
          CleanUp;
          raise EBFInterrupt.Create(ecbi.Message);
        end;
      end;
      SetFirstCall(False);
    end;

    // deslack now
    blWasError:=False;
    case ClearFileSlack(sFileName, dres) of

      DESLACK_ERROR_ERROR : begin
        results.Add(TStringKeeper.Create(sFileName),
                    WORKRESULTS_TYPE_ERROR,
                    Format(m_sr.Get(STRRES_ID, 'FMT_ERR'),
                           [dres.Get, dres.GetSubCnt]));
        blWasError:=True;
      end;

      DESLACK_ERROR_FATAL : begin
        sTemp:=Format(m_sr.Get(STRRES_ID, 'FMT_ERR'),
                      [dres.Get, dres.GetSubCnt]);
        results.Add(TStringKeeper.Create(sFileName),
                    WORKRESULTS_TYPE_ERROR,
                    sTemp);
        CleanUp;
        raise EBFFatalError.Create(sTemp);
      end;

    else
      // must have been a success
      results.Add(TStringKeeper.Create(sFileName),
                  WORKRESULTS_TYPE_SUCCESS,
                  '');
    end;

    // too many errors?
    if (blWasError) then begin
      if (not IncAndCheckMaxErrors) then begin
        CleanUp;
        raise EBFInterrupt.Create(m_sr.Get(STRRES_ID, 'TOOMANYERRS'))
      end;
    end;
  end;

  // final cleanup
  CleanUp;
end;



function TDeslacker.ClearFileSlack(const sFileName : String;
                                   errorMess : TStringKeeper) : Integer;
const
  ASSUMED_CLUSTER_SIZE = 65536;
var
  qFileSize           : WORD64;
  dwFileSizeLo        : DWORD;
  dwFileSizeHi        : DWORD;
  dwSaveAttr          : DWORD;
  dwClusterSize       : DWORD;
  dwSectorsPerCluster : DWORD;
  dwDummy             : DWORD;
  dwWritten           : DWORD;
  dwBytesPerSector    : DWORD;
  lSlackSize          : WORD32;
  blWasError          : Boolean;
  clrFile             : THandle;
  creationTime        : TFileTime;
  lastAccessTime      : TFileTime;
  lastWriteTime       : TFileTime;
  pClearBuf           : Pointer;

procedure MakeErrMess(const sID : String);
begin
  if (errorMess = Nil) then
    Exit;
  with errorMess do begin
    SetCnt(m_sr.Get(STRRES_ID, sID));
    SetSubCnt(TStrPlusI.WinErrToStr(m_sr, GetLastError));
  end;
end;

begin
  // assume an error
  Result:=DESLACK_ERROR_ERROR;

  // store the file attribute
  dwSaveAttr:=GetFileAttributes(PChar(sFileName));
  if (dwSaveAttr = $ffffffff) then begin
    MakeErrMess('ERR_01');
    Exit;
  end;

  // reset the file attribute
  if (SetFileAttributes(PChar(sFileName), 0) = FALSE) then begin
    MakeErrMess('ERR_02');
    Exit;
  end;

  // try to get the cluster size
  if (GetDiskFreeSpace(PChar(Copy(sFileName, 1, 3)),
                       dwSectorsPerCluster,
                       dwBytesPerSector,
                       dwDummy,
                       dwDummy) = FALSE) then begin
    // we assume our own cluster size, to not take any chances a reltively big
    // size is selected (which equals clusters on large FAT16 systems)
    dwClusterSize:=ASSUMED_CLUSTER_SIZE
  end
  else begin
    dwClusterSize:=dwSectorsPerCluster * dwBytesPerSector;
  end;

  // open the file
  clrFile:=CreateFile(PChar(sFileName),
                      GENERIC_WRITE or GENERIC_READ,
                      0,
                      NULL,
                      OPEN_EXISTING,
                      FILE_FLAG_WRITE_THROUGH,
                      0);
  if (clrFile = INVALID_HANDLE_VALUE) then begin
    MakeErrMess('ERR_03');
    SetFileAttributes(PChar(sFileName),
                      dwSaveAttr);
    Exit;
  end;

  // store the filetime
  GetFileTime(clrFile,
              @creationTime,
              @lastAccessTime,
              @lastWriteTime);

  // get the file's size (no 64bit files supported)
  dwFileSizeLo:=GetFileSize(clrFile, @dwFileSizeHi);
  blWasError:=False;
  pClearBuf:=Nil;
  if ((dwFileSizeLo = $ffffffff) and (GetLastError <> NO_ERROR)) then begin
    MakeErrMess('ERR_04');
    blWasError:=True;
  end
  else begin
    qFileSize:=MakeWORD64(dwFileSizeLo, dwFileSizeHi);

    // slack exists?
    lSlackSize:=dwClusterSize - WORD32(qFileSize mod dwClusterSize);

    if ((lSlackSize <> 0) and (lSlackSize <> dwClusterSize)) then  begin

      // seek to the end of the file
      if (SetFilePointer(clrFile,
                         dwFileSizeLo,
                         @dwFileSizeHi,
                         FILE_BEGIN) = $ffffffff) then begin
        if (GetLastError <> NO_ERROR) then begin
          MakeErrMess('ERR_05');
          blWasError:=True;
        end;
      end;

      if (not blWasError) then begin

        // allocate the clear buffer
        GetMem(pClearBuf, lSlackSize);

        // fill the buffer with random data
        GetRandomSource.GetBytes(pClearBuf, Integer(lSlackSize));

        // write the whole buffer to deslack the cluster
        if (WriteFile(clrFile,
                      pClearBuf^,
                      lSlackSize,
                      dwWritten,
                      NULL) = FALSE) then begin
           MakeErrMess('ERR_06');
           blWasError:=True;
        end
        else begin
          // (take no chances)
          if (dwWritten <> lSlackSize) then begin
            MakeErrMess('ERR_06');
            blWasError:=True;
          end
          else begin
            // reset to the old file length, first step
            if (SetFilePointer(clrFile,
                               dwFileSizeLo,
                               @dwFileSizeHi,
                               FILE_BEGIN) = $ffffffff) then begin
              if (GetLastError <> NO_ERROR) then begin
                MakeErrMess('ERR_07');
                blWasError:=True;
                Result:=DESLACK_ERROR_FATAL;  // (this ain't no good!)
              end;
            end;

            if (not blWasError) then begin
              // second step
              if (SetEndOfFile(clrFile) = FALSE) then begin
                MakeErrMess('ERR_08');
                blWasError:=True;
                Result:=DESLACK_ERROR_FATAL;  // (bad one, too)
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  // flush all buffers
  FlushFileBuffers(clrFile);

  // free the clear buffer
  if (pClearBuf <> Nil) then
    FreeMem(pClearBuf);

  // restore the original filetime
  if (SetFileTime(clrFile,
                  @creationTime,
                  @lastAccessTime,
                  @lastWriteTime) = FALSE) then begin
    MakeErrMess('ERR_09');
    blWasError:=True;
  end;

  // close the file
  if (CloseHandle(clrFile) = FALSE) then begin

    Result:=DESLACK_ERROR_FATAL;  // (bad one, three)
    if (blWasError) then
      MakeErrMess('ERR_10')
    else
      MakeErrMess('ERR_11');
    Exit;
  end;

  // restore the original attribute (hands off if fatal error occured!)
  if (Result <> DESLACK_ERROR_FATAL) then begin

    if (SetFileAttributes(PChar(sFileName),
                          dwSaveAttr) = FALSE) then begin
      MakeErrMess('ERR_12');
      blWasError:=True;
    end;

    if (not blWasError) then
      Result:=DESLACK_ERROR_NOERROR;
  end;

end;


end.
