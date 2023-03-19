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

program Tester;

uses
  Forms,
  TesterForm in 'TesterForm.pas' {TestMain},
  LZSS in 'LZSS.pas',
  CipherServer in 'CipherServer.pas',
  FilesProgress in 'FilesProgress.pas',
  RandomSource in 'RandomSource.pas',
  SeedBuffer in 'SeedBuffer.pas',
  Configuration in 'Configuration.pas',
  StringRes in 'StringRes.pas',
  WorkResults in 'WorkResults.pas',
  IntLists in 'IntLists.pas',
  BFManager in 'BFManager.pas',
  MessageCallBack in 'MessageCallBack.pas',
  RandomProvider in 'RandomProvider.pas',
  BFJob in 'BFJob.pas',
  Compress in 'Compress.pas',
  Reencryptor in 'Reencryptor.pas',
  Globals in 'Globals.pas',
  RandomManager in 'RandomManager.pas',
  Yarrow in 'Yarrow.pas',
  bfacslib in 'bfacslib.pas',
  SHA1 in 'SHA1.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TTestMain, TestMain);
  Application.Run;
end.
