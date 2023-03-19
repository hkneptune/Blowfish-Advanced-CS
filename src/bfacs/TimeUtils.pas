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
  some stuff for time measurements
}

unit TimeUtils;

interface
uses bfacslib;


type
  TTimeUtils = class

    // gets the current time in 100 nano secs
    // <- current time
    class function GetNanoTime : WORD64;

    // converts a 64bit nano secs time into days, hours and minutes
    // -> number of 100 nano secs
    // -> where to put the days
    // -> where to put the minutes
    // -> where to put the minutes
    // -> where to put the seconds
    // -> where to put the milliseconds
    class procedure SeparateNanoTime(qNano100Secs : WORD64;
                                     var vqDays : WORD64;
                                     var vqHours : WORD64;
                                     var vqMinutes : WORD64;
                                     var vlSeconds : WORD32;
                                     var vlMilliSecs : WORD32);

    // converts a 64bit nano secs time into a "...hh:mm:ss" string repr.
    // -> number of 100 nano secs
    // <- string representation
    class function NanoTimeToStr(qNano100Secs : WORD64) : String;

  end;


implementation
uses Windows,
     SysUtils;


class function TTimeUtils.GetNanoTime : WORD64;
var
  sysTime  : TSystemTime;
  fileTime : TFileTime;
begin
  GetSystemTime(sysTime);
  SystemTimeToFileTime(sysTime, fileTime);
  Result:=MakeWORD64(fileTime.dwLowDateTime,
                     fileTime.dwHighDateTime);
end;


class procedure TTimeUtils.SeparateNanoTime(qNano100Secs : WORD64;
                                            var vqDays : WORD64;
                                            var vqHours : WORD64;
                                            var vqMinutes : WORD64;
                                            var vlSeconds : WORD32;
                                            var vlMilliSecs : WORD32);
const
  // (Delphi cannot calculate constants larger  than signed 32 bit integers,
  //  so we have to precalculate them by ourselves)
  DIV_DAY  : WORD64 = 864000000000;
  DIV_HOUR : WORD64 = 36000000000;
  DIV_MIN  : WORD64 = 600000000;
  DIV_SEC  : WORD32 = 10000000;
  DIV_MSEC : WORD32 = 10000;

var
  lNano100Secs : WORD32;

begin
  vqDays:=qNano100Secs div DIV_DAY;
  qNano100Secs:=qNano100Secs mod DIV_DAY;

  vqHours:=qNano100Secs div DIV_HOUR;
  qNano100Secs:=qNano100Secs mod DIV_HOUR;

  vqMinutes:=qNano100Secs div DIV_MIN;
  lNano100Secs:=WORD32(qNano100Secs mod DIV_MIN); // 32 bits are sufficent now

  vlSeconds:=lNano100Secs div DIV_SEC;
  lNano100Secs:=lNano100Secs mod DIV_SEC;

  vlMilliSecs:=lNano100Secs div DIV_MSEC;
end;


class function TTimeUtils.NanoTimeToStr(qNano100Secs : WORD64) : String;
var
  qDays      : WORD64;
  qHours     : WORD64;
  qMinutes   : WORD64;
  lSeconds   : WORD32;
  lMilliSecs : WORD32;
  sTemp      : String;

begin
  SeparateNanoTime(qNano100Secs,
                   qDays,
                   qHours,
                   qMinutes,
                   lSeconds,
                   lMilliSecs);

  // (we must risk an overflow here)
  Inc(qHours, (qDays * 24));
  sTemp:=IntToStr(qHours);
  if (Length(sTemp) = 1) then
    sTemp:='0' + sTemp;
  Result:=sTemp + ':';

  sTemp:=IntToStr(qMinutes);
  if (Length(sTemp) = 1) then
    sTemp:='0' + sTemp;
  Result:=Result + sTemp + ':';

  sTemp:=IntToStr(lSeconds);
  if (Length(sTemp) = 1) then
    sTemp:='0' + sTemp;
  Result:=Result + sTemp;

end;


end.
