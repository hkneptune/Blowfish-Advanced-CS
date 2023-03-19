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
  some helping stuff to handle with the ciphers 
}

unit CipherServerTools;

interface
uses
  StringRes;


// our helper class
type
  TCipherServerTools = class

  public
    // converts an error cipher server code in a readable message
    // -> the error code
    // -> string resources
    // <- string representation of the error code
    class function ErrToStr(nErrCode : Integer;
                            sr : TStrRes) : String;
  end;


implementation
uses SysUtils,
     CipherServer;

//////////////////////////// TCipherServerTools ////////////////////////////


// ID for the string resources
const
 STRRES_ID = 'CIPHERSERVERTOOLS';


class function TCipherServerTools.ErrToStr(nErrCode : Integer;
                                           sr : TStrRes) : String;
begin
  with sr do
    case nErrCode of
      ECDE_NOERROR            : Result:=Get(STRRES_ID, 'ETS01');
      ECDE_ERROR	      : Result:=Get(STRRES_ID, 'ETS02');
      ECDE_CIPHERNOTFOUND     : Result:=Get(STRRES_ID, 'ETS03');
      ECDE_INVALIDCIPHER      : Result:=Get(STRRES_ID, 'ETS04');
      ECDE_OUTOFMEMORY        : Result:=Get(STRRES_ID, 'ETS05');
      ECDE_WEAKKEY            : Result:=Get(STRRES_ID, 'ETS06');
      ECDE_SELFTESTFAILED     : Result:=Get(STRRES_ID, 'ETS07');
      ECDE_SESSIONACTIVE      : Result:=Get(STRRES_ID, 'ETS08');
      ECDE_NOSESSION          : Result:=Get(STRRES_ID, 'ETS09');
      ECDE_WRONGSESSIONTYPE   : Result:=Get(STRRES_ID, 'ETS10');
    else
      Result:=sr.Get(STRRES_ID, 'ETSUNKNOWN');
    end;
end;



end.
