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

program bfacs;

uses
  Forms,
  main in 'main.pas' {MainForm},
  Globals in 'Globals.pas',
  LoaderWin in 'LoaderWin.pas' {LoaderForm},
  About in 'About.pas' {AboutForm},
  DestSelect in 'DestSelect.pas' {DestSelectWin},
  Favorites in 'Favorites.pas',
  MessBoxYNAC in 'MessBoxYNAC.pas' {YNACBox},
  FavWin in 'FavWin.pas' {FavoritesWin},
  StringSearchWin in 'StringSearchWin.pas' {StringSearchForm},
  RenameWin in 'RenameWin.pas' {RenameForm},
  PasswordWin in 'PasswordWin.pas' {PasswordForm},
  KeyCache in 'KeyCache.pas',
  Options in 'Options.pas',
  KeyDisk in 'KeyDisk.pas',
  PathInput in 'PathInput.pas' {PathInputWin},
  KeyChecker in 'KeyChecker.pas',
  PasswConfirm in 'PasswConfirm.pas' {PasswConfirmWin},
  CheckBoxList in 'CheckBoxList.pas',
  ShortcutChecker in 'ShortcutChecker.pas',
  ShellListener in 'ShellListener.pas',
  BFManager in 'BFManager.pas',
  BFAManager in 'BFAManager.pas',
  BFJob in 'BFJob.pas',
  LogWin in 'LogWin.pas' {LogForm},
  TimeUtils in 'TimeUtils.pas',
  ProgressWin in 'ProgressWin.pas' {ProgressForm},
  Executor in 'Executor.pas',
  GUIMessageCBImpl in 'GUIMessageCBImpl.pas',
  PathSearchWin in 'PathSearchWin.pas' {PathSearchForm},
  MakeDirWin in 'MakeDirWin.pas' {MakeDirForm},
  WipeManager in 'WipeManager.pas',
  Reencryptor in 'Reencryptor.pas',
  deslacker in 'deslacker.pas',
  BFAWorkWith in 'BFAWorkWith.pas',
  BFAViewer in 'BFAViewer.pas',
  MakeKeyDiskWin in 'MakeKeyDiskWin.pas' {MakeKeyDiskForm},
  RegistryKeys in 'RegistryKeys.pas',
  Startup in 'Startup.pas',
  JobChooserWin in 'JobChooserWin.pas' {JobChooserForm},
  DiskClear in 'DiskClear.pas',
  DiskClearWin in 'DiskClearWin.pas' {DiskClearForm},
  SettingsWin in 'SettingsWin.pas' {SettingsForm},
  FileBrowser in 'FileBrowser.pas',
  TipWin in 'TipWin.pas' {TipForm},
  SDFCWin in 'SDFCWin.pas' {SDFCForm},
  bfacslib in 'bfacslib.pas',
  MemCheck in 'MemCheck.pas',
  RandomPool in 'RandomPool.pas',
  HtmlHelpAPI in 'HtmlHelpAPI.pas',
  ComCtrls in 'VCLfix\comctrls.pas',
  GlobalsGUI in 'GlobalsGUI.pas';

{$R *.RES}
{$R winxp.res}
{$R images\prgicons.res}

begin

{$IFDEF USE_MEMCHECK}
  MemChk;
{$ENDIF}

  _ListenForShellJob;
  Application.Initialize;
  Application.Title := 'Blowfish Advanced CS';
  Application.HelpFile := 'bfacs_US.chm';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TYNACBox, YNACBox);
  Application.CreateForm(TLoaderForm, LoaderForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TDestSelectWin, DestSelectWin);
  Application.CreateForm(TFavoritesWin, FavoritesWin);
  Application.CreateForm(TStringSearchForm, StringSearchForm);
  Application.CreateForm(TRenameForm, RenameForm);
  Application.CreateForm(TPasswordForm, PasswordForm);
  Application.CreateForm(TPathInputWin, PathInputWin);
  Application.CreateForm(TPasswConfirmWin, PasswConfirmWin);
  Application.CreateForm(TLogForm, LogForm);
  Application.CreateForm(TProgressForm, ProgressForm);
  Application.CreateForm(TPathSearchForm, PathSearchForm);
  Application.CreateForm(TMakeDirForm, MakeDirForm);
  Application.CreateForm(TMakeKeyDiskForm, MakeKeyDiskForm);
  Application.CreateForm(TJobChooserForm, JobChooserForm);
  Application.CreateForm(TDiskClearForm, DiskClearForm);
  Application.CreateForm(TSettingsForm, SettingsForm);
  Application.CreateForm(TTipForm, TipForm);
  Application.CreateForm(TSDFCForm, SDFCForm);
  Application.Run;
end.
