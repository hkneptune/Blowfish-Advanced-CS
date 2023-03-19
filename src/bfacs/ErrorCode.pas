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
  implements a simple exception type for error code handling
}

unit ErrorCode;

interface
uses SysUtils;

// the exception type
type
  EErrorCode = class(Exception)
  protected
    // member(s)
    m_nErrorCode : Integer;
  public

    // create a new exception with a defined error code
    // -> error code
    constructor Create(nSetErrorCode : Integer); virtual;

    // returns the error code of the exception
    // <- error code
    function GetErrorCode : Integer; virtual;
  end;


implementation


//////////////////////////// EErrorCode ////////////////////////////

constructor EErrorCode.Create(nSetErrorCode : Integer);
begin
  // just store the error code
  m_nErrorCode:=nSetErrorCode;
end;


function EErrorCode.GetErrorCode : Integer;
begin
  Result:=m_nErrorCode;
end;


end.
