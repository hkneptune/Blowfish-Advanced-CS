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
  message callback implemented with dialog boxes
}

unit GUIMessageCBImpl;

interface
uses
  MessagecallBack;


type
  TGUIMessageCBImpl = class(TMessageCallBack)
  public

    // callback method
    // exception: ECallBackInterrupt if the user pressed [Cancel] (remember
    //                               that the cancel button is not always
    //                               visible, so it won't be necessary to
    //                               check for an exception)
    procedure CallBack; override;

  end;


implementation
uses
  Forms,
  Windows,
  MessBoxYNAC,
  Controls,
  CallBack,
  General;



//////////////////////////// TGUIMessageCBImpl ////////////////////////////


procedure TGUIMessageCBImpl.CallBack;
var
  nType : Integer;
begin

  // standard or ynac message box?

  if (GetStyle = MCB_STYLE_YESNOALLCANCEL) then begin

    with YNACBox do begin
      Setup(GetMessage, PROGRAM_NAME);

      case ShowModal of
        mrYes : SetResult(MCB_RES_YES);
        mrNo  : SetResult(MCB_RES_NO);
        mrAll : SetResult(MCB_RES_ALL);
      else
        raise ECallBackInterrupt.Create('cancel pressed');
      end;
    end;

  end
  else begin

    case GetStyle of
      MCB_STYLE_OK          : nType:=MB_OK;
      MCB_STYLE_OKCANCEL    : nType:=MB_OKCANCEL;
      MCB_STYLE_YESNO       : nType:=MB_YESNO;
      MCB_STYLE_YESNOCANCEL : nType:=MB_YESNOCANCEL;
    else
      nType:=0;
    end;

    case GetKindOf of
      MCB_KINDOF_INFORMATION  : nType:=nType or MB_ICONINFORMATION;
      MCB_KINDOF_EXCLAMATION  : nType:=nType or MB_ICONEXCLAMATION;
   //   MCB_KINDOF_WARNING      : nType:=nType or MB_ICONQUESTION;
      MCB_KINDOF_QUESTION     : nType:=nType or MB_ICONQUESTION;
      MCB_KINDOF_ERROR        : nType:=nType or MB_ICONSTOP;
   //   MCB_KINDOF_STOP         : nType:=nType or MB_ICONSTOP;
      MCB_KINDOF_CONFIRMATION : nType:=nType or MB_ICONINFORMATION;
    end;

    case Application.MessageBox(PChar(GetMessage),
                                PROGRAM_NAME,
                                nType) of
      IDOK  : SetResult(MCB_RES_OK);
      IDYES : SetResult(MCB_RES_YES);
      IDNO  : SetResult(MCB_RES_NO);
    else
      raise ECallBackInterrupt.Create('cancel');
    end;

  end;

end;


end.
