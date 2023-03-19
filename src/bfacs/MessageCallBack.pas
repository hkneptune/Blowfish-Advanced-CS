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
  callback class for confirmations and hints
}

unit MessageCallBack;

{$I config.inc}

interface
uses CallBack;


// style codes
const
  MCB_STYLE_OK             = 0;
  MCB_STYLE_OKCANCEL       = 1;
  MCB_STYLE_YESNO          = 2;
  MCB_STYLE_YESNOCANCEL    = 3;
  MCB_STYLE_YESNOALLCANCEL = 4;


// kind codes
const
  MCB_KINDOF_PURE         = 0;
  MCB_KINDOF_INFORMATION  = 1;
  MCB_KINDOF_EXCLAMATION  = 2;
  MCB_KINDOF_WARNING      = 2;
  MCB_KINDOF_QUESTION     = 3;
  MCB_KINDOF_ERROR        = 4;
  MCB_KINDOF_STOP         = 4;
  MCB_KINDOF_CONFIRMATION = 5;


// result codes
const
  MCB_RES_OK  = 0;
  MCB_RES_YES = MCB_RES_OK;
  MCB_RES_NO  = 2;
  MCB_RES_ALL = 3;



// callback to get a confirmation for the start of the working, remember that
// the CallBack() method might raise the ECallBackInterrupt exception if the
// user doesn't want to go on (and [Cancel] is visible)
type
  TMessageCallBack = class(TCallBack)
  private
    // members
    m_nStyle  : Integer;
    m_nResult : Integer;
    m_nKindOf : Integer;

  protected
    // sets the result code
    // -> result code
    procedure SetResult(nResult : Integer); virtual;

    // gets the style
    // <- style
    function GetStyle : Integer; virtual;

    // gets the kind of code
    // <- kind of code
    function GetKindOf : Integer; virtual;

  public
    // sets the kind of code (if needed)
    // -> new kind of code, see MCB_KINDOF_xxx constants
    procedure SetKindOf(nKindOf : Integer); virtual;

    // sets the style
    // -> new style
    procedure SetStyle(nStyle : Integer); virtual;

    // gets the result code
    // <- result code, see MCB_RES_xxx constants
    function GetResult : Integer; virtual;
  end;


implementation


procedure TMessageCallBack.SetStyle(nStyle : Integer);
begin
  m_nStyle:=nStyle;
end;


function TMessageCallBack.GetStyle : Integer;
begin
  Result:=m_nStyle;
end;


procedure TMessageCallBack.SetResult(nResult : Integer);
begin
  m_nResult:=nResult;
end;


function TMessageCallBack.GetResult : Integer;
begin
  Result:=m_nResult;
end;


procedure TMessageCallBack.SetKindOf(nKindOf : Integer);
begin
  m_nKindOf:=nKindOf;
end;


function TMessageCallBack.GetKindOf : Integer;
begin
  Result:=m_nKindOf;
end;



end.
