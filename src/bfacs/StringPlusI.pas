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
  string support routines, internationalized
}

unit StringPlusI;

{$Q-}

interface
uses Windows,
     Classes,
     bfacslib,
     StringRes,
     StringPlus;



// the class for holding the functions (we inherit from TStrPlus,
// so internationalized application only have to use one class),
// all static methods must be called with a string resource (this
// is better than providing a global instance)
type
  TStrPlusI = class(TStrPlus)
  public

    // converts an integer into a formatted string with separation
    // -> string resources
    // -> 64bit integer value
    // <- string representation
    class function Sepa1000(sr : TStrRes;
                            const lValue : WORD64) : String;

    // converts a date+time stamp delivered by the Win32
    // file search function in a formatted string
    // -> string resources
    // -> date+time stamp
    // <- formatted string (country dependent)
    class function Win32FileTimeToStr(sr : TStrRes;
                                      const rawTime : TFILETIME) : String;


    // calculates a percentage
    // -> string resources
    // -> the level
    // -> the maximum
    // -> numbers behind the comma
    // <- string representation of the percentage
    class function CalcPercent(sr : TStrRes;
                               qLevel : WORD64;
                               qMax : WORD64;
                               bPrecision : Byte = 0) : String;

    // gets a string representation of a Win32(/OLE) error
    // -> string resources
    // -> error code
    // <- the string repr.
    class function WinErrToStr(sr : TStrRes;
                               dwError : DWORD) : String;
  end;



implementation
uses SysUtils;



//////////////////////////// TStrPlusI ////////////////////////////


// the string resource ID
const
  STRRES_ID = 'StrPlusI';


class function TStrPlusI.Sepa1000(sr : TStrRes;
                                  const lValue : WORD64) : String;
var
  nI, nJ : Integer;
  nLen   : Integer;
  sTemp  : String;
begin
  Result:=IntToStr(lValue);
  nLen:=Length(Result);
  if (nLen > 1) then begin
    sTemp:='';
    nJ:=0;
    for nI:=nLen downto 1 do begin
      if (nJ = 3) then begin
        sTemp:=sr.Get(STRRES_ID, '000') + sTemp;
        nJ:=0;
      end;
      Inc(nJ);
      sTemp:=Result[nI] + sTemp;
    end;
    Result:=sTemp;
  end;
end;



class function TStrPlusI.Win32FileTimeToStr(sr : TStrRes;
                                            const rawtime : TFileTime) : String;
var
  sFrmt : String;
  ftime : TFileTime;
  stime : TSystemTime;
  ttime : TDateTime;
begin
  FileTimeToLocalFiletime(rawtime, ftime);
  FileTimeToSystemtime(ftime, stime);
  sFrmt:=sr.Get(STRRES_ID, '001');
  with stime do begin
    try
      Result:=FormatDateTime(sFrmt, SystemTimeToDateTime(stime));
    except
      on EConvertError do begin
        // (this should never happen)
        ttime:=EncodeDate(1980, 1, 1) +
               EncodeTime(0, 0, 0, 0);
        Result:=FormatDateTime(sFrmt, ttime);
      end;
    end;
  end;
end;


// max. precision allowed for the CalcPercent() method
const
  MAX_PRECISION = 8;


class function TStrPlusI.CalcPercent(sr : TStrRes;
                                     qLevel : WORD64;
                                     qMax : WORD64;
                                     bPrecision : Byte = 0) : String;
var
  dLevel   : Double;
  dMax     : Double;
  dPercent : Double;
begin
  if ((qLevel = 0) or (qMax = 0)) then
    dPercent:=0
  else begin
    dLevel:=qLevel;
    dMax:=qMax;
    dPercent:=(dLevel * 100) / dMax;
  end;
  Result:=Replace(Format('%0.' + IntToStr(bPrecision) + 'f', [dPercent]),
                  sr.Get(STRRES_ID, '002'),
                  sr.Get(STRRES_ID, '003'));
end;


function MAKELANGID(dwP, dwS : DWORD) : DWORD;
begin
  Result:=(dwS shl 10) or dwP;
end;


class function TStrPlusI.WinErrToStr(sr : TStrRes;
                                     dwError : DWORD) : String;
var
  pMessBuf : PChar;
begin
  if (FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER or
                      FORMAT_MESSAGE_FROM_SYSTEM,
                    Nil,
                    dwError,
                    MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                    @pMessBuf,
                    0,
                    Nil) = 0) then
    Result:=sr.Get(STRRES_ID, '004')
  else begin
    Result:=FilterIllegalChars(String(pMessBuf), False);
    LocalFree(HLOCAL(pMessBuf));
  end;
end;



end.

