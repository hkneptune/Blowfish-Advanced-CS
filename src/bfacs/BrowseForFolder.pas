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
  encapsulates the Windows "browse for folder" dialog
}

unit BrowseForFolder;


interface
uses Windows,
     ShlObj;


type
  TBrowseForFolder = class
  private
    // members
    m_sFolder    : String;
    m_browseInfo : TBrowseInfo;

  public
    // basic constructor, to avoid execution with illegal values
    constructor Create;

    // sets up the initial folder
    // -> sSetFolder initial folder
    procedure Setup(const sFolder : String);

    // returns the actual folder
    // <- actual selected folder
    function GetFolder : String;

    // executes the dialog
    // -> prompt
    // -> handle of the parent window
    // <- True: folder was selected / False: user canceled the dialog
    function Execute(const sPrompt : String;
                     whandle : HWND) : Boolean;
  end;


implementation
uses
  ActiveX,
  Options, 
  Globals;

// (new Windows constants which weren't around at the time Delphi 5 shipped)
const
  BIF_STATUSTEXT       = $0004;
  BIF_NEWDIALOGSTYLE   = $0040;
  BIF_RETURNONLYFSDIRS = $0080;
  BIF_SHAREABLE        = $0100;
  BIF_USENEWUI         = BIF_EDITBOX or BIF_NEWDIALOGSTYLE;

//////////////////////////// TBrowseForFolder ////////////////////////////

  
// internal callback function
function BrowseCallback(Wnd: HWND;
                        unMsg: UINT;
                        lParam, lpData: LPARAM) : Integer; stdcall;
var
  bff : TBrowseForFolder;
begin

  bff := TBrowseForFolder(lpData);

  Result := 0;
  if (unMsg = BFFM_INITIALIZED) then
    with bff do begin
      if (m_sFolder <> '') then
        SendMessage(Wnd , BFFM_SetSelection , 1 , LongInt(PChar(m_sFolder)));
    end;
end;


constructor TBrowseForFolder.Create;
begin
  m_sFolder:='';
end;


procedure TBrowseForFolder.Setup(const sFolder : String);
begin
  m_sFolder:=sFolder;
end;


function TBrowseForFolder.GetFolder : String;
begin
  GetFolder:=m_sFolder;
end;


function TBrowseForFolder.Execute(const sPrompt : String;
                                  whandle : HWND) : Boolean;
var
  buffer     : array[0..MAX_PATH] of Char;
  ItemIdList : PItemIDList;
begin
  with m_browseInfo do
    begin
      hwndOwner:=whandle;
      pidlRoot:= Nil;
      pszDisplayName:=buffer;
      lpszTitle:=PChar(sPrompt);
      ulFlags:=BIF_RETURNONLYFSDIRS or BIF_NEWDIALOGSTYLE or BIF_EDITBOX;
      lpfn:=@BrowseCallback;
      lParam:=Longint(Self);
    end;
  ItemIdList:=SHBrowseForFolder(m_browseInfo);
  if (ItemIDList = Nil) then begin
    Execute:=False;
    Exit;
  end;
  Execute:=SHGetPathFromIDList(ItemIDList, buffer);
  m_sFolder:=buffer;
end;

initialization
  ActiveX.CoInitialize(Nil);

finalization
  ActiveX.CoUninitialize;

end.
