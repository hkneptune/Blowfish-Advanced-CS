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
  helper class to automate the shortcut settings of controls (&_)
}

unit ShortcutChecker;

interface

type
  TShortcutChecker = class
  private
    // the shortcut character state arrays
    m_charUsed : array[0..25] of Byte;

  public
    // constructor
    constructor Create;

    // resets the flags
    // -> True: reset scanned chars, too / False: do not
    procedure Reset(blResetScans : Boolean = True);

    // scans a string for shortcuts, marks them for used
    // -> string to scan
    procedure Scan(const sSrc : String);

    // tries to convert a pure string into one with a shortcut
    // -> source string
    // <- the string with a shortcut (may contain no shortcut, if no
    //    characters where found)
    function AddShortcut(sSrc : String) : String;

  end;

implementation
uses SysUtils;


//////////////////////////// TShortcutChecker ////////////////////////////


// the possible states
const
  STATE_UNUSED = 0;
  STATE_BYSCAN = 1;
  STATE_INUSE  = 2;

// the shortcut char.
const
  SHORTCUT_CHAR = '&';


constructor TShortcutChecker.Create;
begin
  // init. our array
  Reset;
end;

procedure TShortcutChecker.Reset(blResetScans : Boolean = True);
var
  nI : Integer;
begin
  // reset all necessary entries
  for nI:=0 to 25 do
    if ((not ((not blResetScans) and (m_charUsed[nI] = STATE_BYSCAN))) or
        blResetScans) then
    m_charUsed[nI]:=STATE_UNUSED;
end;

procedure TShortcutChecker.Scan(const sSrc : String);
var
  nPos : Integer;
  cSC  : Char;
begin

  // search for the shortcut
  nPos:=Pos(SHORTCUT_CHAR, sSrc);
  if ((nPos > 0) and (nPos < Length(sSrc))) then begin

    // accept A-Z only
    cSC:=UpCase(sSrc[nPos + 1]);
    if ((cSC >= 'A') and (cSC <= 'Z')) then
      m_charUsed[Ord(cSC) - Ord('A')]:=STATE_BYSCAN;
  end;
end;

function TShortcutChecker.AddShortCut(sSrc : String) : String;
var
  nPos  : Integer;
  nLen  : Integer;
  nIdx  : Integer;
  nChar : Integer;
  sBak  : String;
begin
  sBak:=sSrc;
  sSrc:=UpperCase(sSrc);
  nLen:=Length(sSrc);

  // (FIXME: some kind of dumb search, doesn't remember already scanned chars)
  for nPos:=1 to nLen do begin

    nChar:=Ord(sSrc[nPos]);
    if ((nChar >= Ord('A')) and (nChar <= Ord('Z'))) then begin

      nIdx:=nChar - Ord('A');
      if (m_charUsed[nIdx] = STATE_UNUSED) then begin

        // found a shortcut, reserve it and return the modified string
        m_charUsed[nIdx]:=STATE_INUSE;
        Result:=Copy(sBak, 1, nPos - 1) +
                SHORTCUT_CHAR +
                Copy(sBak, nPos, nLen - nPos + 1);
        Exit;
      end;
    end;
  end;

  // nothing to shortcut
  Result:=sBak;
end;


end.
