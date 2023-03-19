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
  classes for progress messaging
}

unit ProgressCallBack;

interface
uses SysUtils,
     bfacslib,
     CallBack;


// the progress callback class
type
  TProgressCallBack = class(TCallBack)
  protected
    m_qActPos : WORD64;
    m_qMaxPos : WORD64;

    // helper routine to get the right shift value to convert a 64bit
    // level to a 32bit one
    // -> the value to examine
    // <- the necessary shift level to be in range
    function GetShrInt31Level(qVal : WORD64) : Integer;

  public
    // creates the progress callback object
    // -> object to modify during the callback
    constructor Create(setcallbackobj : TObject); override;

    // sets the progress level to zero
    procedure ZeroPos; virtual;

    // sets the progress level to the max. position
    procedure FullPos; virtual;

    // increases the progress level
    // -> value to which to increase the level
    procedure IncPos(nToAdd : Integer = 1); virtual;

    // gets the progress level
    // <- the progress level
    function GetActPos : WORD64; virtual;

    // sets the progress level
    // -> new progress level (content will be copied)
    procedure SetActPos(qSetActPos :  WORD64); virtual;

    // gets the maximal level
    // <- the maximal level
    function GetMaxPos : WORD64; virtual;

    // sets the progress level
    // -> new maximal level (content will be copied)
    procedure SetMaxPos(qSetMaxPos :  WORD64); virtual;

    // return the 31bit integer level
    // -> where to return the progress level
    // -> where to return the maximal level
    procedure GetIntLevels(var vnActPos : Integer;
                           var vnMaxPos : Integer); virtual;
  end;





implementation


constructor TProgressCallBack.Create(setcallbackobj : TObject);
begin
  inherited Create(setcallbackobj);
  // set up the progress indicators
  m_qActPos:=0;
  m_qMaxPos:=0;
end;


procedure TProgressCallBack.ZeroPos;
begin
  m_qActPos:=0;
end;


procedure TProgressCallBack.FullPos;
begin
  m_qActPos:=m_qMaxPos;
end;


procedure TProgressCallBack.IncPos(nToAdd : Integer = 1);
begin
  Inc(m_qActPos, nToAdd);
end;


function TProgressCallBack.GetActPos : WORD64;
begin
  Result:=m_qActPos;
end;


procedure TProgressCallBack.SetActPos(qSetActPos :  WORD64);
begin
  m_qActPos:=qSetActPos;
end;


function TProgressCallBack.GetMaxPos : WORD64;
begin
  Result:=m_qMaxPos
end;


procedure TProgressCallBack.SetMaxPos(qSetMaxPos :  WORD64);
begin
  m_qMaxPos:=qSetMaxPos;
end;


function TProgressCallBack.GetShrInt31Level(qVal : WORD64) : Integer;
begin
  // search the highest bit in the higher 32bit value
  Result:=33;
  while (((qVal and $8000000000000000) = 0) and (Result > 0)) do begin
    qVal:=qVal shl 1;
    Dec(Result);
  end;
end;


procedure TProgressCallBack.GetIntLevels(var vnActPos : Integer;
                                         var vnMaxPos : Integer);
var
  nToShift : Integer;
begin
  // return the 31bit levels
  nToShift:=GetShrInt31Level(m_qMaxPos);
  vnActPos:=(m_qActPos shr nToShift) and $7fffffff;
  vnMaxPos:=(m_qMaxPos shr nToShift) and $7fffffff;
end;


end.
