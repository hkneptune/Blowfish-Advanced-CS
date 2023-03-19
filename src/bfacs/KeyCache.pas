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
  to cache a previously entered key
}

unit KeyCache;

interface
uses
  ExtCtrls,
  CallBack,
  SecureMem,
  RandomManager;



// supported password input modes, dependant on the job to do
const
  PASSWINPUT_MODE_KEYINPUT   = 0;
  PASSWINPUT_MODE_VIEW       = 1;
  PASSWINPUT_MODE_WORKWITH   = 2;
  PASSWINPUT_MODE_SCANNER    = 10;
  PASSWINPUT_MODE_REENCRYPT1 = 20;
  PASSWINPUT_MODE_REENCRYPT2 = 21;
  PASSWINPUT_MODE_ENCRYPT    = 30;
  PASSWINPUT_MODE_DECRYPT    = 31;


// basic interface for password input
// (FIXME: own unit for this?)
type
  TPasswordInput = class
  public
     // executor
     // -> mode to show the password dialog, see PASSWINPUT_MODE_xxx
     // <- the key got (Nil equals error / user interrupt)
    function Execute(nMode : Integer) : TKeyMemory; virtual; abstract;
  end;

  
// infinite expiration time
const
  KEYCACHE_EXIPRETIME_NEVER = -1;


// callback to visualize the remaining time (an interrupt will stop the timer)
type
  TKeyCacheCallBack = class(TCallBack)
  private
    // members
    m_nRemainingSeconds : Integer;

  public
    // sets the remaining seconds
    // -> remaining seconds
    procedure SetRemainingSeconds(nRest : Integer);

    // gets the number of remaining seconds
    // <- remaining seconds
    function GetRemainingSeconds : Integer;
  end;


type
  TKeyCache = class
  private
    // members
    m_nRemainSecs   : Integer;
    m_blValid       : Boolean;
    m_blExpireRun   : Boolean;
    m_blLastSuccess : Boolean;
    m_timer         : TTimer;
    m_key           : TEncryptedKeyMemory;
    m_kcCB          : TKeyCacheCallBack;
    m_rm            : TRandomManager;

    // timer handler
    // -> the object that receives the notification
    procedure OnTimer(sender : TObject);

  public

    // constructor
    // -> random manager
    // -> key cache callback (maybe Nil, if unwanted)
    constructor Create(rm : TRandomManager;
                       kcCB : TKeyCacheCallBack = Nil);

    // destructor
    destructor Destroy; override;

    // sets the key
    // -> the new key
    // -> expiration time of that key (measured in seconds)
    procedure SetKey(key : TKeyMemory;
                     nExpireTime : Integer = KEYCACHE_EXIPRETIME_NEVER);

    // gets the _reference_ to the key
    // <- ref. to the key (may be Nil if no key was set)
    function GetKey : TEncryptedKeyMemory;

    // checks if the content is still valid
    // <- True: key still accessable / False: no key or key expired
    function IsValid : Boolean;

    // expire the key manually
    procedure Expire;

    // stop the timer
    procedure StopTimer;

    // resume the timer
    procedure ResumeTimer;

    // get the timer state
    // <- True: expiration in progress / False: not
    function ExpirationInProgress : Boolean;

    // modify the expiration time directly
    // -> new expiration time (in seconds)
    procedure SetExpirationTime(nNewExpireTime : Integer);

    // sets the last (success) flag
    // -> True: last operation with this key was a success / False: was not
    procedure SetLastSuccess(blLastSuccess : Boolean);

    // gets the last (succ.) flag
    // <- True: last op was successfull / False: was not
    function GetLastSuccess : Boolean;

  end;


implementation
uses
  General,
  SysUtils;


//////////////////////////// TKeyCacheCallBack ////////////////////////////


procedure TKeyCacheCallBack.SetRemainingSeconds(nRest : Integer);
begin
  m_nRemainingSeconds:=nRest;
end;


function TKeyCacheCallBack.GetRemainingSeconds : Integer;
begin
  Result:=m_nRemainingSeconds;
end;



//////////////////////////// TKeyCache ////////////////////////////


constructor TKeyCache.Create(rm : TRandomManager;
                             kcCB : TKeyCacheCallBack = Nil);
begin
  // init. the members
  m_rm:=rm;
  m_blValid:=False;
  m_timer:=TTimer.Create(Nil);
  m_timer.Interval:=1000;  // (timer steps in seconds)
  m_timer.OnTimer:=OnTimer;
  m_timer.Enabled:=False;
  m_key:=Nil;
  m_nRemainSecs:=0;
  m_kcCB:=kcCB;
  m_blLastSuccess:=False;
end;

destructor TKeyCache.Destroy;
begin
  // shutdown timer and destroy a possible existing key
  m_timer.Enabled:=False;
  m_timer.Destroy;
  if (m_key <> Nil) then begin
    m_key.Clear; // (just to be sure twice)
    m_key.Destroy;
  end;
end;

procedure TKeyCache.SetKey(key : TKeyMemory;
                           nExpireTime : Integer = KEYCACHE_EXIPRETIME_NEVER);
begin
  // stop the perhaps running expiration timer
  m_timer.Enabled:=False;

  // kill the old key
  if (m_key <> Nil) then
    m_key.Destroy;

  // copy the new key
  m_key:=TEncryptedKeyMemory.Create(key.GetSize, m_rm.GetRandomSource);
  m_key.SetData(key.GetPtr, 0, key.GetSize);
  m_key.SetConfirmed(key.GetConfirmed);

  // start the expiration timer, i. n.
  if (m_key <> Nil) then begin
    m_blValid:=True;
    if (KEYCACHE_EXIPRETIME_NEVER = nExpireTime) then begin

      // at least report that a key is now in the cache
      if (Nil <> m_kcCB) then begin
        m_kcCB.SetRemainingSeconds(KEYCACHE_EXIPRETIME_NEVER);
        m_kcCB.CallBack;
      end;
      m_blExpireRun:=False;
    end
    else begin
      m_nRemainSecs:=nExpireTime;
      m_timer.Enabled:=True;
      m_blExpireRun:=True;
    end;
  end;
end;

function TKeyCache.GetKey : TEncryptedKeyMemory;
begin
  // this should never happen, but if so its better to shutdown that hard
  // than taking the risk of using an invalid key
  if ((not m_blValid) or (m_key = Nil)) then
    RunError(RUNERROR_KEYCACHE_INVALIDKEY);
  Result:=m_key;
end;

function TKeyCache.IsValid : Boolean;
begin
  Result:=m_blValid and (m_key <> Nil);
end;

procedure TKeyCache.Expire;
begin
  // turn off the timer
  StopTimer;

  // expire the key (clear it)
  m_blValid:=False;
  m_key:=Nil;
  if (m_key <> Nil) then
    m_key.Clear;
end;

procedure TKeyCache.OnTimer(sender : TObject);
begin
  // dead event?
  if (not m_blExpireRun) then
    Exit;

  // expire?
  if (m_nRemainSecs <= 0) then begin
    Expire;
    Exit;
  end;

  // decrease the number of remaining seconds
  Dec(m_nRemainSecs);

  // inform, i. n.
  if (m_kcCB <> Nil) then begin
    try
      m_kcCB.SetRemainingSeconds(m_nRemainSecs);
      m_kcCB.CallBack;
    except
      // must we stop the timer?
      on ECallBackInterrupt do begin
        // (FIXME: this reaction is not very smart, perhaps we should expire
        //         the key additionally?)
        StopTimer;
        Exit;
      end;
    end;
  end;
end;


procedure TKeyCache.StopTimer;
begin
  // pause/stop the timer
  if (not m_blExpireRun) then
    Exit;
  m_timer.Enabled:=False;
  m_blExpireRun:=False;
end;


procedure TKeyCache.ResumeTimer;
begin
  // restart the timer
  if (m_blExpireRun) then
    Exit;
  m_timer.Enabled:=True;
  m_blExpireRun:=True;
end;


procedure TKeyCache.SetExpirationTime(nNewExpireTime : Integer);
begin
  // stop the timer if we change to an infinite time
  // (remember that the expiration time is only valid if the timer is active!)
  if (nNewExpireTime = KEYCACHE_EXIPRETIME_NEVER) then begin
    if (m_blExpireRun) then
      StopTimer;
    Exit;
  end;

  // set the new expiration time
  m_nRemainSecs:=nNewExpireTime;

  // resume the countdown, i. n.
  if ((not m_blExpireRun) and m_blValid) then
    ResumeTimer;
end;


function TKeyCache.ExpirationInProgress : Boolean;
begin
  Result:=m_blExpireRun;
end;

procedure TKeyCache.SetLastSuccess(blLastSuccess : Boolean);
begin
  m_blLastSuccess:=blLastSuccess;
end;

function TKeyCache.GetLastSuccess : Boolean;
begin
  Result:=m_blLastSuccess;
end;


end.
