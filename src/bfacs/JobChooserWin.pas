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
  dialog (plus associated callback) to select a job mode
}

unit JobChooserWin;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,
  Configuration,
  Startup;


type
  TJobChooserForm = class(TForm)
    ActionGroup: TRadioGroup;
    CancelBtn: TButton;
    OKBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure OKBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    function FormHelp(Command: Word; Data: Integer;
      var CallHelp: Boolean): Boolean;
  private
    m_lookup    : array[0..6] of Integer;
    m_config    : TConfigurationSection;
    m_nSelected : Integer;
  public
    // brings the form in the right mode
    // -> True: folders out there / False: no folders
    // -> True: single file / False: multiple files
    // -> True: .BFA files only / False: mixed file types
    procedure SetMode(blFolders : Boolean;
                      blSingleFile : Boolean = False;
                      blBFAOnly : Boolean = False);

    // gets the selected mode
    // <- mode, see BFJOB_MODE_xx
    function GetSelectedMode : Integer;
  end;


type
  TJobChooserCB = class(TJobRequestCallBack)
  public
    // callback routine
    // exception: ECallbackInterrupt - if the user pressed [Cancel]
    procedure CallBack; override;

  end;



var
  JobChooserForm: TJobChooserForm;

implementation
uses
  HtmlHelpAPI,
  StringRes,
  ShortcutChecker,
  Globals,
  callBack,
  Options,
  BFJob;

{$R *.DFM}




//////////////////////////// TJobChooserForm ////////////////////////////


// the configuration ID
const
  CONFIG_ID = 'JOBCHOOSERFORM';


// resource listener
type
  TJobChooserFormStrResListener = class(TStrResListener)
  private
    m_theWin : TJobChooserForm;
  public
    constructor Create(theWin : TJobChooserForm);
    procedure ChangeStrings(sr : TStrRes); override;
  end;

constructor TJobChooserFormStrResListener.Create(theWin : TJobChooserForm);
begin
  m_theWin:=theWin;
end;


procedure TJobChooserFormStrResListener.ChangeStrings(sr : TStrRes);
var
  scc : TShortcutChecker;
begin
  scc:=TShortcutChecker.Create;
  with m_theWin, sr, scc do begin
    Caption:=Get(CONFIG_ID, 'CAPTION');
    OKBtn.Caption:=AddShortcut(Get(CONFIG_ID, 'OKBTN'));
    CancelBtn.Caption:=AddShortcut(Get(CONFIG_ID, 'CANCELBTN'));
    ActionGroup.Caption:=Get(CONFIG_ID, 'ACTION');  // (no shortcut here?)
  end;
  scc.Destroy;
end;


// configuration checker
type
  TJobChooserFormCC = class(TConfigurationChecker)
  private
    m_parent : TJobChooserForm;
  public
    constructor Create(parent : TJobChooserForm);
    procedure RunCheck(section : TConfigurationSection); override;
  end;

constructor TJobChooserFormCC.Create(parent : TJobChooserForm);
begin
  m_parent:=parent;
end;

procedure TJobChooserFormCC.RunCheck(section : TConfigurationSection);
begin
  CheckInt(section, 'LEFT', (Screen.Width - m_parent.Width) shr 1, 0);
  CheckInt(section, 'TOP', (Screen.Height - m_parent.Height) shr 1, 0);
  CheckInt(section, 'LASTSELECT', 0, 0);
end;



procedure TJobChooserForm.FormCreate(Sender: TObject);
var
  cc       : TJobChooserFormCC;
  listener : TJobChooserFormStrResListener;
begin
  // get the configuration section
  cc:=TJobChooserFormCC.Create(Self);
  m_config:=_globals.GetCfg.GetSection(CONFIG_ID, cc);
  cc.Destroy;

  // set up the window's position
  Left:=m_config.GetIntegerOption('LEFT');
  Top:=m_config.GetIntegerOption('TOP');

  // init. listener stuff
  listener:=TJobChooserFormStrResListener.Create(Self);
  _globals.GetSr.AddListener(listener);
  listener.ChangeStrings(_globals.GetSr);
end;


procedure TJobChooserForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  m_config.FixIntegerOption('LEFT', Left);
  m_config.FixIntegerOption('TOP', Top);
end;

procedure TJobChooserForm.SetMode(blFolders : Boolean;
                                  blSingleFile: Boolean = False;
                                  blBFAOnly : Boolean = False);
var
  nPos : Integer;

procedure AddEntry(const sID : String; nMode : Integer);
begin
  ActionGroup.Items.Add(_globals.GetSr.Get(CONFIG_ID, sID));
  m_lookup[nPos]:=nMode;
  Inc(nPos);
end;

begin
  ActionGroup.Items.Clear;
  nPos:=0;

  if ((not blBFAOnly) or blFolders) then
    AddEntry('SEL_ENCRYPT', BFJOB_MODE_ENCRYPT);

  if (blBFAOnly or blFolders) then
    AddEntry('SEL_DECRYPT', BFJOB_MODE_DECRYPT);

  AddEntry('SEL_WIPE', BFJOB_MODE_WIPE);

  if (blBFAOnly or blFolders) then
    AddEntry('SEL_REENCRYPT', BFJOB_MODE_REENCRYPT);

  AddEntry('SEL_DESLACK', BFJOB_MODE_DESLACK);

  if ((blBFAOnly and blSingleFile) and (not blFolders)) then begin
    AddEntry('SEL_VIEW', BFJOB_MODE_VIEW);
    AddEntry('SEL_WORKWITH', BFJOB_MODE_WORKWITH);
  end;
end;



function TJobChooserForm.GetSelectedMode : Integer;
begin
  Result:=m_nSelected;
end;



procedure TJobChooserForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) then begin
    ModalResult:=mrCancel;
    Close;
    Key:=#0;
  end;
end;

procedure TJobChooserForm.OKBtnClick(Sender: TObject);
begin
  m_nSelected:=m_lookup[ActionGroup.ItemIndex];
  m_config.FixIntegerOption('LASTSELECT', m_nSelected);
end;

procedure TJobChooserForm.FormShow(Sender: TObject);
var
  nI   : Integer;
  nSel : Integer;
begin
  m_nSelected:=BFJOB_MODE_NONE;

  // set the last selected index, if possible
  nSel:=m_config.GetIntegerOption('LASTSELECT');
  for nI:=0 to (ActionGroup.Items.Count - 1) do begin
    if (m_lookup[nI] = nSel) then begin
      ActionGroup.ItemIndex:=nI;
      Exit;
    end;
  end;
  ActionGroup.ItemIndex:=0;
end;


//////////////////////////// TJobChooserCB ////////////////////////////


procedure TJobChooserCB.CallBack;
begin
  with GetCallBackObj as TJobChooserForm do begin
    case m_nRequestMode of
      JRCB_REQMODE_ONEFILE    : SetMode(False, True, False);
      JRCB_REQMODE_ONEBFAFILE : SetMode(False, True, True);
      JRCB_REQMODE_FILES      : SetMode(False, False, False);
      JRCB_REQMODE_BFAFILES   : SetMode(False, False, True);
      JRCB_REQMODE_FOLDERS    : SetMode(True);
    end;

    if (ShowModal = mrCancel) then
      raise ECallBackInterrupt.Create('cancel pressed');

    m_nJobMode:=GetSelectedMode;
  end;
end;



function TJobChooserForm.FormHelp(Command: Word; Data: Integer;
  var CallHelp: Boolean): Boolean;
begin
  CallHelp := False;
  Result:=HtmlHelp_ShowContext(Data);
end;

end.
