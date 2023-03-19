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
  defines version number of the application and offers some debug routines
  used in almost all modules
}

unit General;

{$I config.inc}

interface

// constant stuff
const
   // the program name
  PROGRAM_NAME      = 'Blowfish Advanced CS';
  PROGRAM_SHORTNAME = 'bfaCS';

// runtime errors
const
  RUNERROR_KEYCACHE_INVALIDKEY    = 10;
  RUNERROR_ABOUT_INVALIDSIGNATURE = 20;
  RUNERROR_ABOUT_INVALIDREGKEY    = 21;
  RUNERROR_MAIN_STARTUPERROR      = 30;
  RUNERROR_DISKCLEARWIN_MODEERROR = 40;
  RUNERROR_DISKCLEARWIN_HOWTOHUH  = 41;
  RUNERROR_BFJOBBATCH_UNKNOWNMODE = 50;
  RUNERROR_BFJOBBATCH_NOSIMPLEJOB = 51;


{$ifdef __DEBUG}


// shows a single debug message
// -> the text to show
procedure DebM(sText : String);

// shows a debug message plus a decimal integer value
// -> the text to show
// -> the integer to show in decimal
procedure DebD(sText : String;
               nValue : Integer);

// shows a debug message plus a (32bit) hexadecimal value
// -> the text to show
// -> the integer to show in hex
procedure DebH(sText : String;
               lValue : Longword);

// shows a debug message plus a string
// -> the text to show
// -> the string
procedure DebS(sText : String;
               sValue : String);

{$endif}

implementation
uses Forms, SysUtils;

{$ifdef __DEBUG}

procedure DebM(sText : String);
begin
  Application.MessageBox(PChar(sText), 'DEBUG:', 0);
end;

procedure DebD(sText : String;
               nValue : Integer);
begin
  Application.MessageBox(PChar(sText + ' --> ' + IntToStr(nValue)),
                         'DEBUG:',
                         0);
end;

procedure DebH(sText : String;
               lValue : Longword);
begin
  Application.MessageBox(PChar(sText + ' --> ' + IntToHex(lValue, 8)),
                         'DEBUG:',
                         0);
end;

procedure DebS(sText : String;
               sValue : String);
begin
  Application.MessageBox(PChar(sText + ' >>>' + sValue + '<<<'),
                         'DEBUG:',
                         0);
end;

{$endif}


end.


