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

program testbfacs;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  testCompress in 'testCompress.pas',
  Compress in '..\Compress.pas',
  ZLibEx in '..\ZLibEx.pas',
  bfacslib in '..\bfacslib.pas',
  LZSS in '..\LZSS.pas',
  StringPlus in '..\StringPlus.pas',
  testStringPlus in 'testStringPlus.pas',
  CipherServer in '..\CipherServer.pas',
  RandomSource in '..\RandomSource.pas',
  RandomPool in '..\RandomPool.pas',
  ErrorCode in '..\ErrorCode.pas',
  General in '..\General.pas',
  CrptFile in '..\CrptFile.pas',
  KeyCreator in '..\KeyCreator.pas',
  SecureMem in '..\SecureMem.pas',
  KeyHash in '..\KeyHash.pas',
  StringRes in '..\StringRes.pas',
  Configuration in '..\Configuration.pas',
  Globals in '..\Globals.pas',
  CallBack in '..\CallBack.pas',
  FileBrowser in '..\FileBrowser.pas',
  MessageCallBack in '..\MessageCallBack.pas',
  CipherManager in '..\CipherManager.pas',
  CipherServerTools in '..\CipherServerTools.pas',
  BFAFile in '..\BFAFile.pas',
  ProgressCallBack in '..\ProgressCallBack.pas',
  Wipe in '..\Wipe.pas',
  FileSupp in '..\FileSupp.pas',
  StringPlusI in '..\StringPlusI.pas',
  CRC32 in '..\CRC32.pas',
  KeyCache in '..\KeyCache.pas',
  RandomManager in '..\RandomManager.pas',
  SeedBuffer in '..\SeedBuffer.pas',
  Yarrow in '..\Yarrow.pas',
  History in '..\History.pas',
  PasswordWin in '..\PasswordWin.pas' {PasswordForm},
  Options in '..\Options.pas',
  BFManager in '..\BFManager.pas',
  PathSearch in '..\PathSearch.pas',
  IntLists in '..\IntLists.pas',
  WorkResults in '..\WorkResults.pas',
  FilesProgress in '..\FilesProgress.pas',
  Win32Diagnosis in '..\Win32Diagnosis.pas',
  BFJob in '..\BFJob.pas',
  BFAViewer in '..\BFAViewer.pas',
  KeyChecker in '..\KeyChecker.pas',
  MD5 in '..\MD5.pas',
  Digest in '..\Digest.pas',
  BFAWorkWith in '..\BFAWorkWith.pas',
  Favorites in '..\Favorites.pas',
  WipeManager in '..\WipeManager.pas',
  BFAManager in '..\BFAManager.pas',
  Reencryptor in '..\Reencryptor.pas',
  Deslacker in '..\Deslacker.pas',
  TimeUtils in '..\TimeUtils.pas',
  ShortcutChecker in '..\ShortcutChecker.pas',
  KeyDisk in '..\KeyDisk.pas',
  PasswConfirm in '..\PasswConfirm.pas' {PasswConfirmWin},
  DestSelect in '..\DestSelect.pas' {DestSelectWin},
  BrowseForFolder in '..\BrowseForFolder.pas',
  HtmlHelpAPI in '..\HtmlHelpAPI.pas',
  testBFAFile in 'testBFAFile.pas',
  MemCheck in '..\MemCheck.pas',
  TipWin in '..\TipWin.pas' {TipForm};

begin

{$IFDEF USE_MEMCHECK}
    MemChk;
{$ENDIF}

    if TestCompression then
    if TestStrPlus then
    if TestBFAFileAll then
    begin
        WriteLn('++++all tests passed++++');
        ReadLn;
        Exit;
    end;

    WriteLn('----TESTS FAILED----');
    ReadLn;

end.
