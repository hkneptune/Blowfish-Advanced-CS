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
  loader info form
}

unit LoaderWin;

interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,
  CallBack, ExtCtrls;

type
  TLoaderForm = class(TForm)
    LoaderInfo: TStaticText;
    LoaderTitle: TStaticText;
    BorderLine: TBevel;
    ProgIcon: TImage;
    LoaderVersion: TStaticText;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


// our loader callback
type
  TLoaderCallBack = class(TCallBack)
  public
    procedure CallBack; override;
  end;


var
  LoaderForm: TLoaderForm;

implementation
uses General,
     Win32Diagnosis,
     StringPlus;


{$R *.DFM}

procedure TLoaderForm.FormCreate(Sender: TObject);
begin
  // show the application little
  LoaderTitle.Caption:=PROGRAM_NAME;
  LoaderVersion.Caption :=
    TWin32Diagnosis.GetModuleVersion(Application.ExeName);

  // place us in the middle of the screen
  Left:=(Screen.Width - Width) shr 1;
  Top:=(Screen.Height - Height) shr 1;

  // show the program icon
  ProgIcon.Picture.Icon:=Application.Icon;
end;



//////////////////////// TLoaderCallBack ////////////////////////

procedure TLoaderCallBack.CallBack;
begin
  // just show the simple message
  with m_callbackObj as TLoaderForm do begin
    LoaderInfo.Caption:=GetMessage;
    Refresh;
  end;
end;



end.
