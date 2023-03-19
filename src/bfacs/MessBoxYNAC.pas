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
  implements a message box for yes/no/all/cancel requests
}


unit MessBoxYNAC;

interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls,
  Configuration;

// codes for default buttons
const
  YNACBOX_DEFBTN_DONTCARE = 0;
  YNACBOX_DEFBTN_YES      = 1;
  YNACBOX_DEFBTN_ALL      = 2;
  YNACBOX_DEFBTN_NO       = 3;
  YNACBOX_DEFBTN_CANCEL   = 4;


type
  TYNACBox = class(TForm)
    MsgView: TLabel;
    YesBtn: TButton;
    AllBtn: TButton;
    NoBtn: TButton;
    CancelBtn: TButton;
    QuestionMark: TImage;
    procedure FormCreate(Sender: TObject);
    procedure Setup(sMessage, sCaption : String;
                    nDefBtn : Integer = YNACBOX_DEFBTN_DONTCARE);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    function FormHelp(Command: Word; Data: Integer;
      var CallHelp: Boolean): Boolean;
  private
    m_config : TConfigurationSection;
  public
    { Public declarations }
  end;

var
  YNACBox : TYNACBox;

implementation
uses
  GlobalsGUI,
  HtmlHelpAPI,
  ShortcutChecker,
  StringRes,
  Options;

{$R *.DFM}


//////////////////////////// TYNACBox ////////////////////////////

// the configuration ID
const
  CONFIG_ID = 'MESSBOXYNAC';


// resource listener
type
  TYNACBoxStrResListener = class(TStrResListener)
  private
    m_theWin : TYNACBox;
  public
    constructor Create(theWin : TYNACBox);
    procedure ChangeStrings(sr : TStrRes); override;
  end;

constructor TYNACBoxStrResListener.Create(theWin : TYNACBox);
begin
  m_theWin:=theWin;
end;


procedure TYNACBoxStrResListener.ChangeStrings(sr : TStrRes);
var
  scc : TShortcutChecker;
begin
  scc:=TShortcutChecker.Create;
  with m_theWin, sr, scc do begin
    YesBtn.Caption   :=AddShortcut(Get(CONFIG_ID, 'YESBTN'));
    CancelBtn.Caption:=AddShortcut(Get(CONFIG_ID, 'CANCELBTN'));
    AllBtn.Caption   :=AddShortcut(Get(CONFIG_ID, 'ALLBTN'));
    NoBtn.Caption    :=AddShortcut(Get(CONFIG_ID, 'NOBTN'));
  end;
  scc.Destroy;
end;


// configuration checker
type
  TYNACBoxCC = class(TConfigurationChecker)
  private
    m_parent : TYNACBox;
  public
    constructor Create(parent : TYNACBox);
    procedure RunCheck(section : TConfigurationSection); override;
  end;

constructor TYNACBoxCC.Create(parent : TYNACBox);
begin
  m_parent:=parent;
end;

procedure TYNACBoxCC.RunCheck(section : TConfigurationSection);
begin
  CheckInt(section, 'LEFT', (Screen.Width - m_parent.Width) shr 1);
  CheckInt(section, 'TOP', (Screen.Height - m_parent.Height) shr 1);
end;



procedure TYNACBox.FormCreate(Sender: TObject);
var
  cc       : TYNACBoxCC;
  listener : TYNACBoxStrResListener;
begin
  // get the configuration section
  cc:=TYNACBoxCC.Create(Self);
  m_config:=_globals.GetCfg.GetSection(CONFIG_ID, cc);
  cc.Destroy;

  // set up the window's position
  with m_config do begin
    // main form borders
    Left:=GetIntegerOption('LEFT');
    Top:=GetIntegerOption('TOP');
  end;

  // init. listener stuff
  listener:=TYNACBoxStrResListener.Create(Self);
  _globals.GetSr.AddListener(listener);
  listener.ChangeStrings(_globals.GetSr);

  // clear the caption
  Caption:='';
end;


procedure TYNACBox.Setup(sMessage, sCaption : String;
                         nDefBtn : Integer = YNACBOX_DEFBTN_DONTCARE);
var
  nBtnTop : Integer;
begin
  // get the new caption and message
  Caption:=sCaption;
  MsgView.Caption:=sMessage;

  // adjust the window size and the buttons to the message
  nBtnTop:=MsgView.Top + MsgView.Height + 16;
  YesBtn.Top:=nBtnTop;
  AllBtn.Top:=nBtnTop;
  NoBtn.Top:=nBtnTop;
  CancelBtn.Top:=nBtnTop;
  ClientHeight:=CancelBtn.Top + CancelBtn.Height + 8;

  // set the focus to the default button
  case (nDefBtn) of
    YNACBOX_DEFBTN_YES    : YesBtn.SetFocus;
    YNACBOX_DEFBTN_ALL    : AllBtn.SetFocus;
    YNACBOX_DEFBTN_NO     : NoBtn.SetFocus;
    YNACBOX_DEFBTN_CANCEL : CancelBtn.SetFocus;
  end;
end;

procedure TYNACBox.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  m_config.FixIntegerOption('LEFT', Left);
  m_config.FixIntegerOption('TOP', Top);
end;

function TYNACBox.FormHelp(Command: Word; Data: Integer;
  var CallHelp: Boolean): Boolean;
begin
  CallHelp := False;
  Result:=HtmlHelp_ShowContext(Data);
end;

end.
