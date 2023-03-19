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

unit testBFAFile;

interface

function TestBFAFileAll : Boolean;

implementation
uses Windows, classes, Sysutils,
    IntLists,
    StringPlus,
    StringPlusI,
    CipherServer,
    StringRes,
    bfacslib,
    RandomSource,
    SecureMem,
    PathSearch,
    FileSupp,
    MessageCallBack,
    Reencryptor,
    BFManager,
    BFAFile;

///////////////////////////////////////////////////////////////////////////////

var
  _sr : TStrRes;
  _rs : TRandomSourceImpl;

///////////////////////////////////////////////////////////////////////////////

function CreateTempFileName(const sName : String) : String;
var
  tmp : array[0..MAX_PATH] of Char;
begin

  if (0 = Windows.GetTempPath(SizeOf(tmp), tmp)) then begin
    Result:=sName;
    Exit;
  end;

  Result:=TStrPlus.RTLPath(String(tmp)) + sName;
end;

///////////////////////////////////////////////////////////////////////////////

function CompareFiles(const sOne : String; const sTwo : String) : Boolean;
var
  flOne, flTwo : File;
  nToRead : Integer;
  nSizeOne, nSizeTwo : Integer;
  bufOne, bufTwo : array[0..1023] of Byte;

begin

  Result:=False;

{$I-}
  AssignFile(flOne, sOne);
  Reset(flOne, 1);
  if (0 <> IOResult) then begin
    WriteLn('cannot open "' + sOne + '" for comparsion');
    Exit;
  end;

  AssignFile(flTwo, sTwo);
  Reset(flTwo, 1);
  if (0 <> IOResult) then begin
    WriteLn('cannot open "' + sTwo + '" for comparsion');
    Exit;
  end;

  nSizeOne:=FileSize(flOne);
  nSizeTwo:=FileSize(flTwo);
  if (nSizeOne <> nSizeTwo) then begin
    WriteLn('file size mismatch ' + IntToStr(nSizeOne) +
                             '<>' + IntToStr(nSizeTwo) + ' before comparsion');
    CloseFile(flOne);
    CloseFile(flTwo);
    Exit;
  end;

  while (0 < nSizeOne) do begin

    if (SizeOf(bufOne) < nSizeOne) then nToRead:=SizeOf(bufOne)
                                   else nToRead:=nSizeOne;

    BlockRead(flOne, bufOne, nToRead);
    if (0 <> IOResult) then begin
      WriteLn('error reading file #1 for comparsion');
      CloseFile(flOne);
      CloseFile(flTwo);
      Exit;
    end;

    BlockRead(flTwo, bufTwo, nToRead);
    if (0 <> IOResult) then begin
      WriteLn('error reading file #2 for comparsion');
      CloseFile(flOne);
      CloseFile(flTwo);
      Exit;
    end;

    if (not CompareMem(@bufOne, @bufTwo, nToRead)) then begin
      WriteLn('data differences detected @' + IntToStr(nSizeTwo - nSizeOne));
      CloseFile(flOne);
      CloseFile(flTwo);
      Exit;
    end;

    Dec(nSizeOne, nToRead);
  end;

  CloseFile(flOne);
  if (0 <> IOResult) then begin
    WriteLn('error closing file #1 after comparsion');
    CloseFile(flTwo);
    Exit;
  end;
  CloseFile(flTwo);
  if (0 <> IOResult) then begin
    WriteLn('error closing file #2 after comparsion');
    Exit;
  end;

{$I+}

  Result:=True;
end;

///////////////////////////////////////////////////////////////////////////////

const
    FILLTYPE_123 = 0;

function CreateOriginalFile(
  sName : String;
  nFillType : Integer;
  nLen : Integer) : Boolean;
var
  nI, nBufLen, nToWrite : Integer;
  fl : File;
  buf : array[0..255] of Byte;
begin

  Result:=False;

  case nFillType of

    FILLTYPE_123: begin
      nBufLen:=256;
      for nI:=0 to (nBufLen - 1) do begin
        buf[nI]:=Byte(nI and $0ff);
      end;
    end;
    else begin
        Exit;
    end;
  end;

{$I-}
  AssignFile(fl, sName);
  Rewrite(fl, 1);

  if (0 <> IOResult) then begin
    WriteLn('cannot open file "' + sName + '" to create');
    Exit;
  end;

  while (0 < nLen) do begin

    if (nLen < nBufLen) then nToWrite:=nLen
                        else nToWrite:=nBufLen;

    BlockWrite(fl, buf, nToWrite);

    if (0 <> IOResult) then begin
      WriteLn('cannot write to file "' + sName + '" to create (disks full?)');
      CloseFile(fl);
      Exit;
    end;

    Dec(nLen, nToWrite);
  end;

  CloseFile(fl);

  if (0 <> IOResult) then begin
    WriteLn('cannot close file "' + sName + '" to create');
    Exit;
  end;

{$I+}

  Result:=True;
end;

///////////////////////////////////////////////////////////////////////////////

(* dummy progress callbacks *)

type
  TDummyProgress = class(TBFAFileProgress)
  private
    procedure CallBack; override;
  end;

procedure TDummyProgress.CallBack;
begin
end;

type
  TDummyWorkProgress = class(TBFWorkProgress)
  private
    procedure CallBack; override;
  end;

procedure TDummyWorkProgress.CallBack;
begin
end;

type
  TDummyMsgCB = class(TMessageCallBack)
  private
    procedure CallBack; override;
    function GetResult : Integer; override;    
  end;

procedure TDummyMsgCB.CallBack;
begin
end;

function TDummyMsgCB.GetResult : Integer;
begin
  Result:=MCB_RES_YES;  // (although it should never be invoked)
end;

///////////////////////////////////////////////////////////////////////////////

(* some halfway comprehensive test, covers proper encryption and decryption
   with different sizes and compression methods for now *)

const
  TEST_SIZES : array[0..11] of Integer =
    (0, 1, 7, 8, 15, 16, 17, 32, 256, 4096, 100117, 1234567);
  TEST_PASSWORD = 'all tests must pass';  

function TestCompatibility : Boolean;
var
  nCipher   : Integer;
  nCompress : Integer;
  nSizeIdx  : Integer;
  nSize     : Integer;
  sOrigName : String;
  sEncName  : String;
  sDecName  : String;
  setupEnc  : TBFAFileEncryptSetup;
  setupDec  : TBFAFileDecryptSetup;
  resultEnc : TBFAFileEncryptResult;
  resultDec : TBFAFileDecryptResult;
  ciphers   : TStringList;
  bfl       : TBFAFile;
  progress  : TDummyProgress;
  keyMem    : TKeyMemory;
begin

  Result:=False;

  // get all the ciphers to test
  ciphers:=TCipher.GetCipherNames(False);

  // make a dummy progress callback
  progress:=TDummyProgress.Create(Nil);

  // the test password needs to be converted to key memory
  keyMem:=TKeyMemory.Create(Length(TEST_PASSWORD));
  keyMem.SetData(PChar(TEST_PASSWORD), 0, Length(TEST_PASSWORD));

  for nCipher:=0 to (ciphers.Count - 1) do begin
  for nCompress:=0 to 3 do begin
  for nSizeIdx:=0 to 11 do begin
    nSize:=TEST_SIZES[nSizeIdx];

    // create the original file name in the temp directory
    sOrigName:=CreateTempFileName(
      'cph(' + ciphers[nCipher] + ')_' +
      'cmp(' + IntToStr(nCompress) + ')');

    Write(sOrigName);

    if (not CreateOriginalFile(sOrigName, 0, nSize)) then Exit;

    // create the setup, use constants where it makes sense
    // (*) probably good candidates for variation testing in the future
    setupEnc:=TBFAFileEncryptSetup.Create;

    with setupEnc do begin
      SetPassword(keyMem);
      SetTargetPath('');                // (*)
      SetRemoveSource(False);
      SetOverwriteExisting(True); 
      SetNoCache(False);
      SetKeepDirectories(True);
      SetBasePath('');
      SetHeaderFileName(sOrigName);     // (*)
      SetForceFileName('');             // (*)
      SetRename(False);                 // (*)        
      SetRandomRename(False);
      SetMaskName('maskerade');
      SetMaskNumber(100);
      SetMaskExt('msk');
      SetTryRename83(False);
      SetAddExtension(True);
      SetKeepDateTime(False);           // (*)
      SetKeepAttributes(False);         // (*)
      SetStorePath(False);              // (*)
      SetRelativePaths(False);
      SetCompress(nCompress);
      SetForceCompress(False);          
      SetSkipEncrypted(True);
      SetNoKeyHash(False);              // (*)
      SetWriteProtectAfter(False);
      SetNoCompressTypes(Nil);
    end;

    // create the encryptor
    bfl:=TBFAFile.Create(ciphers[nCipher], _sr, _rs);

    // start the encryption process
    resultEnc:=bfl.Encrypt(sOrigName, Nil, setupEnc, progress);
    setupEnc.Destroy;

    sEncName:=resultEnc.GetBFAFileName;
    resultEnc.Destroy;

    // before we decrypt we need to rename the original file, otherwise it'd be
    // overwritten...
    sDecName:=sOrigName + '.orig';

    DeleteFile(sDecName);
    if (not RenameFile(sOrigName, sDecName)) then begin
      WriteLn(', cannot rename "' + sOrigName + '" to "' + sDecName + '"');
      Exit;
    end;

    // decrypt the file, so we know that recovery is always possible...

    // create the decryptor
    setupDec:=TBFAFileDecryptSetup.Create;

    with setupDec do begin
      SetPassword(keyMem);
      SetTargetPath('');
      SetRemoveSource(False);
      SetOverwriteExisting(False);
      SetNoCache(False);
      SetKeepDirectories(True);
      SetBasePath('');
      SetIgnoreCRC32(False);
      SetRestorePath(False);
      SetNoKeyCheck(False);
      SetAcceptTruncated(False);
      SetFileInfoOnly(False);
      SetDirectHeaderInfo(False);
    end;

    // start the decryption process
    Write(', decrypting...');

    resultDec:=bfl.Decrypt(sEncName, setupDec, progress);
    setupDec.Destroy;

    // name reconstructed?
    if (resultDec.GetOriginalFileName <> sOrigName) then begin
      WriteLn(', ERROR: original name does not match');
      Exit;
    end;

    // same size?
    if (Integer(resultDec.GetBytesWritten) <> nSize) then begin
      WriteLn(', ERROR: original size does not match');
      Exit;
    end;

    // TODO: maybe request header info to see if it matches, too
    resultDec.Destroy;

    // (we don't need this anymore)
    bfl.Destroy;

    // that's good, but we also need to compare if the files do match
    if (not CompareFiles(sOrigName, sDecName)) then Exit;

    // decryption was successfull
    WriteLn(', done.');
  end;
  end;
  end;

  // cleanup
  keyMem.Destroy;
  ciphers.Destroy;
  progress.Destroy;

  // this test has passed successfully
  Result:=True;
end;

///////////////////////////////////////////////////////////////////////////////

(* test to check for compatibility with older versions, based on a set of files
   manually created with bfaCS 2.13 - since cipher names have changed
   adjustments we have to do some translation *)

// TODO: maybe a second set for BFA97, too?   

const
  REF213_PASSWORD       = '123';
  REF123_DIR_RTL        = '.\testbfacs\ref213\';
  REF213_SEARCH_PATTERN = '*.bfa';
  REF213_ORIGFILE       = 'test.txt';
  REF213_ORIGFILE_PATH  = REF123_DIR_RTL + REF213_ORIGFILE;
  TOKEN_SEPA            = '.';
  CIPHER_TRANSLATE : array[0..6, 0..1] of String = (
    ('blowfish'  , 'Blowfish'  ),
    ('cast'      , 'CAST'      ),
    ('pc1'       , 'ARCFOUR'   ),
    ('serpent'   , 'Serpent'   ),
    ('triple-des', 'Triple-DES'),
    ('rjindael'  , 'AES'       ),
    ('twofish'   , 'Twofish'   ));

function TestBackwardsCompatibility : Boolean;
var
  nI, nPos  : Integer;
  nTests    : Integer;
  sCipher   : String;
  sDecName  : String;
  search    : TSearchRec;
  setupDec  : TBFAFileDecryptSetup;
  resultDec : TBFAFileDecryptResult;
  bfl       : TBFAFile;
  progress  : TDummyProgress;
  keyMem    : TKeyMemory;

begin

  Result:=False;

  // make the environment
  keyMem:=TKeyMemory.Create(Length(REF213_PASSWORD));
  keyMem.SetData(PChar(REF213_PASSWORD), 0, Length(REF213_PASSWORD));

  progress:=TDummyProgress.Create(Nil);

  // search for all files out there
  nTests:=0;

  if (0 = FindFirst(
    REF123_DIR_RTL + REF213_SEARCH_PATTERN, faReadonly, search)) then begin
    repeat

      Write('decrypting ' + search.Name + '...');

      // we need to look up the right cipher name
      nPos:=Pos(TOKEN_SEPA, search.Name);
      if (0 = nPos) then begin
        WriteLn(', ERROR: missing ''.'' in filename');
        Break;
      end;

      sCipher:=Copy(search.Name, 1, nPos - 1);

      // translate the cipher
      for nI:=0 to 6 do begin
        if (0 = CompareText(CIPHER_TRANSLATE[nI, 0], sCipher)) then begin
          sCipher:=CIPHER_TRANSLATE[nI, 1]; // (translation happens here)
          Break;
        end;
      end;
      if (6 < nI) then begin
        WriteLn(', ERROR: unkown cipher "' + sCipher + '"');
        Exit;
      end;

      // assemble the name of the file we assume the decryptor will produce
      sDecName:=CreateTempFileName(REF213_ORIGFILE);

      // make sure the file's not there already
      DeleteFile(sDecName);

      // now we try to decrypt the file to the temp folder
      setupDec:=TBFAFileDecryptSetup.Create;

      with setupDec do begin
        SetPassword(keyMem);
        SetTargetPath(CreateTempFileName(''));
        SetRemoveSource(False);
        SetOverwriteExisting(False);
        SetNoCache(False);
        SetKeepDirectories(False);
        SetBasePath('');
        SetIgnoreCRC32(False);
        SetRestorePath(False);
        SetNoKeyCheck(False);
        SetAcceptTruncated(False);
        SetFileInfoOnly(False);
        SetDirectHeaderInfo(False);
      end;

      // create the encryptor
      bfl:=TBFAFile.Create(sCipher, _sr, _rs);

      resultDec:=bfl.Decrypt(REF123_DIR_RTL + search.Name, setupDec, progress);
      setupDec.Destroy;

      // name reconstructed?
      if (ExtractFileName(resultDec.GetOriginalFileName) <> REF213_ORIGFILE)
        then begin
        WriteLn(', ERROR: no match with reference name');
        Exit;
      end;

      // make sure the file's content matches the original one
      if (not CompareFiles(
        resultDec.GetOriginalFileName,
        REF213_ORIGFILE_PATH)) then begin
        Exit;
      end;

      // cleanup what was created during this round
      resultDec.Destroy;
      bfl.Destroy;
      WriteLn(', done.');

      Inc(nTests);
    until (0 <> FindNext(search));

    FindClose(search);
  end;

  progress.Destroy;
  keyMem.Destroy;

  Result:=(14 = nTests);
end;

///////////////////////////////////////////////////////////////////////////////

(* tests for covering almost all possible reencryption scenarios, shares some
   constants of the TestCompatibility() function *)

const
  TEST_PASSWORD2 = 'changed my mind';

function TestReencryption : Boolean;
var
  nCipher     : Integer;
  nCipher2    : Integer;
  nCompress   : Integer;
  nSizeIdx    : Integer;
  nSize       : Integer;
  sOrigName   : String;
  sEncName    : String;
  sDecName    : String;
  sReencName  : String;
  flsizes     : TWORD64List;
  setupEnc    : TBFAFileEncryptSetup;
  setupDec    : TBFAFileDecryptSetup;
  resultEnc   : TBFAFileEncryptResult;
  resultDec   : TBFAFileDecryptResult;
  resultReenc : TReencryptResults;
  ciphers     : TStringList;
  bfl         : TBFAFile;
  reenc       : TReencryptor;
  progress    : TDummyProgress;
  workProgress: TDummyWorkProgress;
  msgCB       : TDummyMsgCB;
  keyMem      : TKeyMemory;
  keyMem2     : TKeyMemory;
  psc         : TPathSearchContainer;
begin

  Result:=False;

  // setip and basic loop similar to the compatibility test
  ciphers:=TCipher.GetCipherNames(False);
  progress:=TDummyProgress.Create(Nil);
  workProgress:=TDummyWorkProgress.Create(Nil);
  msgCB:=TDummyMsgCB.Create(Nil);

  keyMem:=TKeyMemory.Create(Length(TEST_PASSWORD));
  keyMem.SetData(PChar(TEST_PASSWORD), 0, Length(TEST_PASSWORD));
  keyMem2:=TKeyMemory.Create(Length(TEST_PASSWORD2));
  keyMem2.SetData(PChar(TEST_PASSWORD2), 0, Length(TEST_PASSWORD2));

  for nCipher:=0 to (ciphers.Count - 1) do begin
  for nCompress:=0 to 3 do begin
  for nSizeIdx:=0 to 11 do begin
    nSize:=TEST_SIZES[nSizeIdx];

    // create the original file name and encrypt it

    sOrigName:=CreateTempFileName(
      'cph(' + ciphers[nCipher] + ')_' +
      'sz(' + IntToStr(nSize) +  ')_' +
      'cmp(' + IntToStr(nCompress) + ')');

    WriteLn(sOrigName);

    if (not CreateOriginalFile(sOrigName, 0, nSize)) then Exit;

    setupEnc:=TBFAFileEncryptSetup.Create;
    with setupEnc do begin
      SetPassword(keyMem);
      SetTargetPath('');
      SetRemoveSource(False);
      SetOverwriteExisting(True);
      SetNoCache(False);
      SetKeepDirectories(True);
      SetBasePath('');
      SetHeaderFileName(sOrigName);
      SetForceFileName('');
      SetRename(False);
      SetRandomRename(False);
      SetMaskName('maskerade');
      SetMaskNumber(100);
      SetMaskExt('msk');
      SetTryRename83(False);
      SetAddExtension(True);
      SetKeepDateTime(False);
      SetKeepAttributes(False);
      SetStorePath(False);
      SetRelativePaths(False);
      SetCompress(nCompress);
      SetForceCompress(False);
      SetSkipEncrypted(True);
      SetNoKeyHash(False);
      SetWriteProtectAfter(False);
      SetNoCompressTypes(Nil);
    end;

    bfl:=TBFAFile.Create(ciphers[nCipher], _sr, _rs);

    resultEnc:=bfl.Encrypt(sOrigName, Nil, setupEnc, progress);
    setupEnc.Destroy;

    sEncName:=resultEnc.GetBFAFileName;
    resultEnc.Destroy;

    bfl.Destroy;    // we can't keep it

    // rename the original file for comparison purposes
    sDecName:=sOrigName + '.orig';
    DeleteFile(sDecName);
    if (not RenameFile(sOrigName, sDecName)) then begin
      WriteLn(', cannot rename "' + sOrigName + '" to "' + sDecName + '"');
      Exit;
    end;

    // we need to make a copy of the encrypted file, since the name will not
    // be changed during reencryption and thus we'd have deal with a change
    sReencName:=sEncName + '.reenc';

    // now we will reencrypt to every possible cipher (including the current
    // one) with a new password...

    for nCipher2:=0 to (ciphers.Count - 1) do begin

      Write('  reencrypting with ' + ciphers[nCipher2] + ' ...');

      // remove a previously (reencrypted) copy first
      DeleteFile(sReencName);
      if (FALSE = Windows.CopyFile(
        PChar(sEncName), PChar(sReencName), TRUE)) then begin
        WriteLn(', ERROR: cannot copy "' + sEncName + '" to "' +
          sReencName + '", Win32 error (' +
          TStrPlusI.WinErrToStr(_sr, Windows.GetLastError) + ')');
        Exit;
      end;

      // we need to pack the name and the file of its size into a container
      psc:=TPathSearchContainer.Create(_sr);
      psc.AddSingleFile(sReencName);
      flsizes:=TWORD64List.Create;
      flsizes.Add(TFileSupport.GetFile64Len(sEncName, _sr));

      // set up the reencryption instance
      reenc:=TReencryptor.Create(
        ciphers[nCipher],
        ciphers[nCipher2],
        _rs,
        0,      // (no errors are allowed)
        workProgress,
        msgCB,
        _sr);

      // do the reencryption
      reenc.Execute(
        psc,
        flsizes,
        keyMem,
        keyMem2,
        resultReenc);

      // (for this the high level results don't matter to us)
      resultReenc.Destroy;

      // don't need the local reencryptor things anymore
      reenc.Destroy;
      psc.Destroy;
      flsizes.Destroy;

      // now we try to decrypt the reencypted file, which then should prove
      // that its transformation was successful
      Write(', decrypting...');

      setupDec:=TBFAFileDecryptSetup.Create;

      // delete previous copies
      DeleteFile(sOrigName);

      with setupDec do begin
        SetPassword(keyMem2);   // (use the second key, of course)
        SetTargetPath(CreateTempFileName(''));
        SetRemoveSource(False);
        SetOverwriteExisting(True);     // (that's fine here)
        SetNoCache(False);
        SetKeepDirectories(True);
        SetBasePath('');
        SetIgnoreCRC32(False);
        SetRestorePath(False);
        SetNoKeyCheck(False);
        SetAcceptTruncated(False);
        SetFileInfoOnly(False);
        SetDirectHeaderInfo(False);
      end;

      bfl:=TBFAFile.Create(ciphers[nCipher2], _sr, _rs); // (and the 2nd cipher)

      resultDec:=bfl.Decrypt(sReencName, setupDec, progress);
      setupDec.Destroy;

      bfl.Destroy;

      // now check that the decrypted content (from the reencrypted file) is ok
      if (ExtractFileName(resultDec.GetOriginalFileName) <>
          ExtractFileName(sOrigName)) then begin
        WriteLn(', ERROR: original name does not match the reencrypted one');
        Exit;
      end;
      if (Integer(resultDec.GetBytesWritten) <> nSize) then begin
        WriteLn(', ERROR: original size does not match the reencrypted one');
        Exit;
      end;
      resultDec.Destroy;
      if (not CompareFiles(sOrigName, sDecName)) then Exit;

      WriteLn(', done.');
      
    end; { of "for nCipher2:=..." }
  end;
  end;
  end;

  keyMem2.Destroy;
  keyMem.Destroy;
  ciphers.Destroy;
  msgCB.Destroy;
  workProgress.Destroy;
  progress.Destroy;

  // this test has passed successfully
  Result:=True;
end;

///////////////////////////////////////////////////////////////////////////////

function TestBFAFileAll : Boolean;
begin

  Result:=True;

  if TestCompatibility then
  if TestBackwardsCompatibility then
  if TestReencryption then Exit;

  Result:=False;
end;

///////////////////////////////////////////////////////////////////////////////

(* we probably have to move this or use globals, as soon as other tests in
   different units rely on string resources *)

initialization
  _sr:=TStrRes.Create('bfaCS_US.sr');
  _rs:=TRandomSourceImpl.Create;

finalization
  _rs.Destroy;
  _sr.Destroy;

end.
