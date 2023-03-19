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
  classes and defs for callbacks
}

unit CallBack;

interface
uses SysUtils;


// signal flags, their meaning can be individualy interpreted,
// e.g. useful to bring up and init. a dialog or to shut it
// down when the work's done
const
  CALLBACK_SIGNAL_NULL   = 0;      // nothing special state
  CALLBACK_SIGNAL_START  = 1;      // called the first time
  CALLBACK_SIGNAL_STOP   = 2;      // called the last time
  CALLBACK_SIGNAL_CUSTOM = 1000;   // inherited classes can define their own
                                   // signals by starting at this number


// the very base class
type
  TCallBack = class
  protected
    // members
    m_nSignal           : Integer;
    m_blCalled          : Boolean;
    m_blSimpleMessageCB : Boolean;
    m_blChanged         : Boolean;
    m_sMessage          : String;
    m_callBackObj       : TObject;

  public
    // init. with the callback object
    // -> object to modify during the callback
    constructor Create(callBackObj : TObject); virtual;

    // to call back the docked object
    // exception: ECallBackInterrupt if interrupted
    procedure CallBack; virtual; abstract;

    // gets the callback object
    // <- object (instance) to modify during the callback
    function GetCallBackObj : TObject; virtual;

    // sets the callback object
    // -> object to modify during the callback
    procedure SetCallBackObj(callBackObj :  TObject); virtual;

    // gets the message (the simple case isn't a must, this can also be used
    // to provide additional information to a callback)
    // <- message
    function GetMessage : String;

    // sets the message (see above)
    // -> new (simple) message
    procedure SetMessage(sMessage : String);

    // shows up if the callback is a simple message carrier (by default this
    // method will return False, it should be declared for every individual
    // case if a callback thrower delivers simple messages)
    // <- True: simple message / False: something else
    function IsSimpleMessage : Boolean;

    // sets if the callback just contains a simple message
    // -> True: simple message / False: anything else
    procedure SetSimpleMessageState(blState : Boolean);

    // gets the changed flag (indicates that something new has begun)
    // <- changed flag
    function GetChanged : Boolean;

    // sets the changed flag
    // -> new changed flag state
    procedure SetChanged(blChanged : Boolean);

    // gets a signal flag
    // -> resets the signal to CALLBACK_SIGNAL_NULL after the call
    function GetSignal(blReset : Boolean = True) : Integer;

    // sets a signal flag
    // -> the signal to set
    procedure SetSignal(nSignal : Integer);

    // sets the flag for callback action detection
    // -> True: callback occured / False: nothing happened (yet)
    procedure SetCalled(blCalled : Boolean);

    // to detect if a callback action
    // <- True: callback occured / False: nothing happened
    function GetCalled : Boolean;

  end;


// exception class to detect an interruption
type
  ECallBackInterrupt = class(Exception);

implementation

//////////////////////////// TCallBack ////////////////////////////

constructor TCallBack.Create(callBackObj : TObject);
begin
  m_callBackObj:=callBackObj;

  // (it's not a simple message callback by def.)
  m_blSimpleMessageCB:=False;

  m_nSignal:=CALLBACK_SIGNAL_NULL;
end;


procedure TCallBack.SetCallBackObj(callBackObj : TObject);
begin
  m_callBackObj:=callBackObj;
end;


function TCallBack.GetCallBackObj : TObject;
begin
  Result:=m_callBackObj;
end;


function TCallBack.GetMessage : String;
begin
  Result:=m_sMessage;
end;


procedure TCallBack.SetMessage(sMessage : String);
begin
  m_sMessage:=sMessage;
end;


function TCallBack.IsSimpleMessage : Boolean;
begin
  Result:=m_blSimpleMessageCB;
end;


procedure TCallBack.SetSimpleMessageState(blState : Boolean);
begin
  m_blSimpleMessageCB:=blState;
end;


function TCallBack.GetChanged : Boolean;
begin
  Result:=m_blChanged;
end;


procedure TCallBack.SetChanged(blChanged : Boolean);
begin
  m_blChanged:=blChanged;
end;

function TCallBack.GetSignal(blReset : Boolean = True) : Integer;
begin
  Result:=m_nSignal;
  if (blReset) then
    m_nSignal:=CALLBACK_SIGNAL_NULL;
end;

procedure TCallBack.SetSignal(nSignal : Integer);
begin
  m_nSignal:=nSignal;
end;

procedure TCallBack.SetCalled(blCalled : Boolean);
begin
  m_blCalled:=blCalled;
end;

function TCallBack.GetCalled : Boolean;
begin
  Result:=m_blCalled;
end;


end.
