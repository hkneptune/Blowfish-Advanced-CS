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
  (wide) string support routines
}

unit StringPlus;

{$Q-}

interface
uses Windows,
     Classes,
     bfacslib;


// simple string keeper (readonly)
type
  TStringKeeper = class
  private
    // members
    m_sCnt    : String;
    m_sSubCnt : String;

  public
    // constructor
    // -> the string to keep
    // -> subcontent string (optional)
    constructor Create(const sCnt : String = '';
                       const sSubCnt : String = '');

    // gets the content
    // <- the string
    function Get : String;

    // sets the content
    // -> the string
    procedure SetCnt(const sCnt : String);

    // gets the sub content
    // <- the subcontent string
    function GetSubCnt : String;

    // sets the sub content
    // -> the subcontent string
    procedure SetSubCnt(const sSubCnt : String);
  end;




// the class for holding the functions
type
  TStrPlus = class
  public

    // returns the last character of a string, does not check for empty strings
    // -> string
    // <- last character
    class function GetLastChar(const sTheStr : String) : Char;

    // deletes a string before setting its length to zero
    // -> the string to clear
    // -> length of the string can be provided for faster exexution
    class procedure ClearString(var sTheStr : String;
                                nStrLen : Integer = -1);

    // converts an (unsigned) integer value into a decimal string
    // -> integer value
    // <- decimal string
    class function DecToStr(nValue : Integer) : String;

    // converts a string into an (unsigned) integer
    // -> string to convert
    // -> where to but the integer value
    // <- True: conversion succeeded / False: illegal characters
    class function StrToDec(const sInput : String;
                            var nOutput : Integer) : Boolean;

    // creates a string of a user defined length containing random characters,
    // suitable for file renaming
    // -> desired length
    // <- random string
    class function RandomStr(nLen : Integer) : String;

    // creates a string with a defined number of a single character
    // -> the character
    // -> number of times to repeat
    // <- string
    class function MulChar(const cValue : Char;
                           nLen : Integer) : String;

    // converts a string with binary definitions ('\xx' with xx as an 8bit hex
    // and '\' for a single '\') to a real string
    // -> string to parse
    // -> where to return the real string
    // <- length of the password (-1 equals an error)
    class function GetBinStr(const sSrc : String;
                             var sDest : String) : Integer;

    // creates a relative path from an absolute path
    // -> absolute path
    // <- relative path
    class function RelativePath(const sAbsPath : String) : String;

    // converts a path to a string without an ending '\'
    // -> raw path
    // <- filtered path
    class function PurePath(const sPath : String) : String;

    // converts a path to an RTL (Ready To Link) path, if necessary
    // -> raw path
    // <- RTL path
    class function RTLPath(const sPath : String) : String;

    // tries its best to convert a short filename (plus path) into a long one
    // -> short file name
    // <- long file name
    class function LongFileName(const sShortName : String) : String;

    // converts a memory block of bytes in a hex string representation
    // -> pointer to the memory block
    // -> number of bytes to represent
    // -> separating character (ignored if zero)
    // <- hex string representation
    class function BytesToHexStr(pData : Pointer;
                                 nNumOfBytes : Integer;
                                 cSeparator : Char = #0) : String;

    // converts a string with hex values back to bytes, the string must _not_
    // contain any seperators
    // -> source string
    // -> where to copy the bytes
    // <- number of converted bytes (-1 equals an error)
    class function HexStrToBytes(const sSource : String;
                                 pData : Pointer) : Integer;

    // check if a string is a valid binhex representation
    // -> the string to check
    // -> demanded binary size (-1 equals "don't care")
    // <- True: valid / False: invalid
    class function IsBinHexStr(const sCheckThis : String;
                               nMustBinLen : Integer = -1) : Boolean;

    // returns a complete formatted heap status information string,
    // mainly usable for debugging purposes
    // <- heap status information string
    class function GetHeapStatusInfo : String;

    // creates a version string in the "x.xx.xxx ADDON" format
    // -> major version number
    // -> minor version number (ignored if -1)
    // -> build number (ignored if -1 or nMinor equals -1)
    // -> additional version information
    class function VersionFormat(nMajor : Integer;
                                 nMinor : Integer = -1;
                                 nBuild : Integer = -1;
                                 const sAddOn : String = '') : String;

    // creates a version string in the "x.xx.xx.xxx ADDON" format
    // -> major version number
    // -> minor version number (ignored if -1)
    // -> subminor version number (ignored if -1)
    // -> build number (ignored if -1 or nMinor equals -1)
    // -> additional version information
    class function VersionFormatEx(nMajor : Integer;
                                   nMinor : Integer = -1;
                                   nSubMinor : Integer = -1;
                                   nBuild : Integer = -1;
                                   const sAddOn : String = '') : String;

    // converts a string into a wide string
    // -> 8bit string
    // <- wide string
    class function StringToWideString(const sASCIIStr : String) : WideString;

    // converts file attributes to a uppercase string
    // -> the attributes
    // <- the string representation
    class function FileAttrToStr(lAttributes : WORD32) : String;

    // extracts the extension from a file
    // -> the filename
    // <- the extension (may be empty)
    class function ExtractFileExtension(const sFileName : String) : String;

    // splits a string which is seperated by a special character
    // -> the string to split
    // -> splitting character
    // <- the seperated strings (caller must destroy the list)
    class function StringSplit(const sSource : String;
                               cSplitter : Char) : TStringList;

    // converts an IP address to a string, doesn't check for invalid adresses
    // -> the IP address
    // -> True: big endian / False: little endian format
    // <- the strin representation in the xxx.xxx.xxx.xxx format (decimal)
    class function IPAddrToStr(lIPAddr : WORD32;
                               blBigEndian : Boolean) : String;


    // filters illegal characters (< 32) out of a string
    // -> the string to clean
    // -> True: don't filter #13, #10 / False: filter all
    // <- the filtered string
    class function FilterIllegalChars(const sDirtyStr : String;
                                      blAllowLineBreaks : Boolean) : String;

    // replaces substrings
    // -> the source string
    // -> the string to replace
    // -> the new string
    // <- replaced string
    class function Replace(const sSource : String;
                           const sThis : String;
                           const sThat : String) : String;

    // gets the root of an absolute path
    // -> absolute path
    // <- root
    class function RootPath(const sPath : String) : String;

    // checks if the system supports unicode
    // <- True: Unicode supported / False: ASCII only
    class function IsUnicodeOS : Boolean;

    // compares if a substring is contained from the beginning (case sens.)
    // -> the substring
    // -> the string to search in
    // -> number of characters to compare
    // <- True: substring is in the string at the beginning
    class function CompareFromBegin(const sSub : String;
                                    const sMain : String;
                                    nToCmp : Integer) : Boolean;

    // compares if a substring is contained at the end
    // -> the substring
    // -> the string to search in
    // -> True: case sensitive / False: case insensitive
    // <- True: substring is in the string at the end
    class function CompareFromEnd(const sSub : String;
                                  const sMain : String;
                                  blCaseSensitive : Boolean) : Boolean;

    // tries to get the parent path of a given one (doesn't check if this path
    // exists or the original path is valid, of course)
    // -> the path to go up
    // <- the parent path (may be empty, if it doesn't exist), pure
    class function ParentPath(const sPath : String) : String;

    // stores a stringlist into a single string
    // -> the list to store
    // -> the separating character
    // <- the string
    class function ListToStr(theList : TStringList;
                             cSeparator : Char = ',') : String;

    // expands a single string to a stringlist
    // -> the string to expand
    // -> the separating character
    // -> True: trim the extracted entries / False: do not touch
    // <- string list
    class function StrToList(const sTheStr : String;
                             cSeparator : Char = ',';
                             blTrim : Boolean = True) : TStringList;


    // creates a temporary filename
    // -> path where the temporary file should be created
    // -> some prefix
    // <- temporary file name (given path included!)
    class function MakeTempFileName(const sPath : String;
                                    const sPrefix : String = '') : String;

    // extract concatenated zero terminated strings, terminated with another #0
    // -> pointer to the first C string
    // <- extracted strings
    class function ExtractCStrings(pRawList : Pointer) : TStringList;

    // dumps binary content in a debugger-like memory view
    // -> data to show
    // -> number of bytes
    // -> bytes per line (-1 for default)
    // <- multi-line dump
    class function HexDump(
        data : Pointer;
        nNumOfBytes : Integer;
        nBytesPerLine : Integer = -1) : String;

    // BASE64 encodes binary data
    // -> data to encode
    // -> number of bytes
    // <- encoded string
    class function Base64Encode(
        pRaw : Pointer;
        nLen : Integer) : String;

    // BASE64 decodes a string
    // -> string to decode
    // -> where to put the number of decoded bytes (on success)
    // <- decoded bytes, must be released calling FreeMem(), always zero
    //    terminated (without extra the #0 being counted); Nil on error
    class function Base64Decode(
        const sEnc : String;
        var vnLen : Integer) : PChar;

  end;


implementation
uses SysUtils, FileCtrl;


// internal stuff
var
  versionInfo         : TOSVersionInfo;
  _blUnicodeSupported : Boolean;



//////////////////////////// TStringKeeper ////////////////////////////


constructor TStringKeeper.Create(const sCnt : String = '';
                                 const sSubCnt : String = '');
begin
  SetCnt(sCnt);
  SetSubCnt(sSubCnt);
end;

function TStringKeeper.Get : String;
begin
  Result:=m_sCnt;
end;

procedure TStringKeeper.SetCnt(const sCnt : String);
begin
  m_sCnt:=sCnt;
end;

function TStringKeeper.GetSubCnt : String;
begin
  Result:=m_sSubCnt;
end;

procedure TStringKeeper.SetSubCnt(const sSubCnt : String);
begin
  m_sSubCnt:=sSubCnt;
end;



//////////////////////////// TStrPlus ////////////////////////////


class function TStrPlus.GetLastChar(const sTheStr : String) : Char;
var
  nLen : Integer;
begin
  nLen:=Length(sTheStr);
  if (nLen = 0) then
    Result:=#0
  else
    Result:=sTheStr[Length(sTheStr)];
end;


class procedure TStrPlus.ClearString(var sTheStr : String;
                                     nStrLen : Integer = -1);
begin
  if (nStrLen = -1) then
    nStrLen:=Length(sTheStr);
  FillChar(sTheStr[1], nStrLen, 0);
  (PInteger(@nStrLen))^:=0; // trust no one, not even the compiler :)
  sTheStr:='';
end;



class function TStrPlus.DecToStr(nValue : Integer) : String;
var
  nRest : Integer;
begin
  Result:='';
  repeat
    nRest:=nValue mod 10;
    nValue:=nValue div 10;
    Result:=Concat(Char(Ord('0') + nRest), Result);
  until (nValue = 0);
end;



class function TStrPlus.StrToDec(const sInput : String;
                                 var nOutput : Integer) : Boolean;
var
  blDone : Boolean;
  nPos   : Integer;
  nMulti : Integer;
begin
  // assume error
  Result:=False;

  // avoid special cases
  nPos:=Length(sInput);
  if (nPos = 0) then
    Exit;
  if (nPos > 1) and (sInput[1] = '0') then
    Exit;
  nMulti:=1;
  nOutput:=0;
  blDone:=False;
  while (not blDone) do begin

    // illegal character?
    if (sInput[nPos] < '0') or (sInput[nPos] > '9') then begin
      blDone:=True;
    end
    else begin
      nOutput:=nOutput + (Ord(sInput[nPos]) - Ord('0')) * nMulti;
      nMulti:=(nMulti shl 3) + (nMulti shl 1);  // (faster than "nMulti * 10")
      Dec(nPos);
      if (nPos = 0) then begin
        blDone:=True;
        Result:=True;
      end;
    end;
  end;
end;



class function TStrPlus.RandomStr(nLen : Integer) : String;
const
  RND_SET = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
begin
  SetLength(Result, nLen);

  // use all characters from the set above
  while (nLen > 0) do begin
    Result[nLen]:=RND_SET[Random(Length(RND_SET)) + 1];
    Dec(nLen);
  end;
end;


class function TStrPlus.MulChar(const cValue : Char;
                                nLen : Integer) : String;
begin
  SetLength(Result, nLen);
  while (nLen > 0) do begin
    Result[nLen]:=cValue;
    Dec(nLen);
  end;
end;



class function TStrPlus.GetBinStr(const sSrc : String;
                                  var sDest : String) : Integer;
const
  BIN_DEF_PREFIX = '\';
var
  blWasError : Boolean;
  bBuild     : Byte;
  cAct       : Char;
  nPos       : Integer;
  nConvPos   : Integer;
  nLen       : Integer;
begin

  // search for binary sequences
  sDest:='';
  nLen:=Length(sSrc);
  nPos:=1;
  blWasError:=False;
  Result:=0;
  while ((nPos <= nLen) and (not blWasError)) do begin

    // sequence detected?
    if (sSrc[nPos] = BIN_DEF_PREFIX) then begin
      if (nPos < nLen) then begin

        // non-binary?
        if (sSrc[nPos + 1] = BIN_DEF_PREFIX) then begin
          sDest:=sDest + BIN_DEF_PREFIX;
          Inc(Result);
          Inc(nPos, 2);
        end
        else begin

          // valid binary?
          if (nPos < nLen - 1) then begin
            bBuild:=0;
            for nConvPos:=(nPos + 1) to (nPos + 2) do begin
              cAct:=UpCase(sSrc[nConvPos]);
              if (cAct >= '0') and (cAct <= '9') then begin
                bBuild:=bBuild shl 4;
                bBuild:=bBuild or (Ord(cAct) - Ord('0'));
              end
              else begin
                if (cAct >= 'A') and (cAct <= 'F') then begin
                  bBuild:=bBuild shl 4;
                  bBuild:=bBuild or (Ord(cAct) - Ord('A') + 10);
                end
                else blWasError:=True;
              end;
            end;
            sDest:=sDest + Chr(bBuild);
            Inc(Result);
            Inc(nPos, 3);
          end
          else
            blWasError:=True;
        end;
      end
      else
        blWasError:=True;
    end
    else begin
      sDest:=sDest + sSrc[nPos];
      Inc(nPos);
      Inc(Result);
    end;
  end;

  // return the real string if succeded
  if (blWasError) then begin
    FillChar(sDest[1], Length(sDest), #0);
    sDest:='';
    Result:=-1;
  end;
end;



class function TStrPlus.RelativePath(const sAbsPath : String) : String;
var
  nI   : Integer;
  nPos : Integer;
  nLen : Integer;
begin
  // (short) non-absolute path?
  nLen:=Length(sAbsPath);
  if (nLen < 3) then begin
    if (nLen = 0) then
      Result:=''
    else
      if (sAbsPath[1] = '\') then
        Result:=Copy(sAbsPath, 2, nLen - 1)
      else
        Result:=sAbsPath;
    Exit;
  end;

  // drive path?
  if (sAbsPath[2] = ':') then begin
    if (sAbsPath[3] = '\') then
      Result:=Copy(sAbsPath, 4, nLen - 3)
    else
      Result:=Copy(sAbsPath, 3, nLen - 2);
    Exit;
  end;

  // NETBIOS path?
  if ((sAbsPath[1] = '\') and (sAbsPath[2] = '\')) then begin
    nI:=0;
    for nPos:=3 to nLen do begin
      if (sAbsPath[nPos] = '\') then begin
        Inc(nI);
        if (nI = 2) then begin
          Result:=Copy(sAbsPath, nPos + 1, nLen - nPos);
          Exit;
        end;
      end;
    end;
    Result:='';
    Exit;
  end;

  // handle the non-absolute path
  if (sAbsPath[1] = '\') then
    Result:=Copy(sAbsPath, 2, nLen - 1)
  else
    Result:=sAbsPath;
end;


class function TStrPlus.PurePath(const sPath : String) : String;
var
  nLen : Integer;
begin
  // no ending '\'?
  if (GetLastChar(sPath) <> '\') then begin
    Result:=sPath;
    Exit;
  end;

  // drive path?
  nLen:=Length(sPath);
  if (nLen = 3) then
    if (sPath[2] = ':') then begin
      Result:=sPath;
      Exit;
    end;

  // cut now
  Result:=Copy(sPath, 1, nLen - 1);
end;


class function TStrPlus.RTLPath(const sPath : String) : String;
begin
  // this construction is simple
  if (Length(sPath) = 0) then
    Result:=''
  else
    if ((GetLastChar(sPath) = '\') or (GetLastChar(sPath) = ':')) then
      Result:=sPath
    else
      Result:=sPath + '\';
end;


class function TStrPlus.LongFileName(const sShortName : String) : String;
var
  sTemp : String;
  dta   : TSearchRec;
begin
  // no NETBIOS, joker or phantom file names
  if (Pos('\\', sShortName) > 0)  or (Pos('*', sShortName) > 0) or
      (Pos('?', sShortName) > 0) or ((not FileExists(sShortName) and
      (not DirectoryExists(sShortName)))) then begin
    Result:=sShortName;
    Exit;
  end;
  sTemp:=sShortName;
  Result:='';
  while (FindFirst(sTemp, $3f, dta) = 0) do begin
    Result:='\' + dta.Name + Result;
    SysUtils.FindClose(dta);
    SetLength(sTemp, Length(ExtractFilePath(sTemp)) - 1);
    if (Length(sTemp) <= 2) then
      Break;
  end;
  Result:=sTemp + Result;
end;


// hextab used in the following method (must be upcased!)
const
  HEXTAB : String = '0123456789ABCDEF';


class function TStrPlus.BytesToHexStr(pData : Pointer;
                                      nNumOfBytes : Integer;
                                      cSeparator : Char = #0) : String;
var
  nI       : Integer;
  nPos     : Integer;
  pBytePtr : PChar;
  bValue   : Byte;
  blSepa   : Boolean;
begin
  pBytePtr:=pData;
  blSepa:=(cSeparator <> Chr(0));
  if (blSepa) then
    SetLength(Result, nNumOfBytes * 3 - 1)
  else
    SetLength(Result, nNumOfBytes shl 1);
  nPos:=1;
  for nI:=1 to nNumOfBytes do begin
    bValue:=Ord(pBytePtr^);
    Inc(pBytePtr);
    Result[nPos]:=HEXTAB[(bValue shr 4) + 1];
    Inc(nPos);
    Result[nPos]:=HEXTAB[(bValue and $0f) + 1];
    Inc(nPos);
    if ((nI < nNumOfBytes) and blSepa) then begin
      Result[nPos]:=cSeparator;
      Inc(nPos);
    end;
  end;
end;


class function TStrPlus.HexStrToBytes(const sSource : String;
                                      pData : Pointer) : Integer;
var
  nI       : Integer;
  nLen     : Integer;
  nPos     : Integer;
  pBytePtr : PChar;
  bValue   : Byte;
  cTemp    : Char;
begin
  // assume an error
  Result:=-1;

  // illegal length?
  nLen:=Length(sSource);
  if (nLen and 1 <> 0) then
    Exit;

  // try to convert all the bytes
  pBytePtr:=pData;
  nPos:=1;
  while (nPos < nLen) do begin
    bValue:=0;
    for nI:=0 to 1 do begin
      cTemp:=UpCase(sSource[nPos]);
      if ((cTemp >= '0') and (cTemp <= '9')) then
        bValue:=(bValue shl 4) or (Ord(cTemp) - Ord('0'))
      else
        if ((cTemp >= 'A') and (cTemp <= 'F')) then
          bValue:=(bValue shl 4) or (Ord(cTemp) - Ord('A') + 10)
        else
          Exit; // illegal character detected
      Inc(nPos);
    end;
    pBytePtr^:=Chr(bValue);
    Inc(pBytePtr);
  end;

  // successfully converted
  Result:=nLen shr 1;
end;


class function TStrPlus.IsBinHexStr(const sCheckThis : String;
                                    nMustBinLen : Integer = -1) : Boolean;
var
  nI, nJ   : Integer;
  nLen     : Integer;
  cActChar : Char;
begin
  // assume an error
  Result:=False;

  // correct length?
  nLen:=Length(sCheckThis);
  if ((nLen and 1) <> 0) then
    Exit;
  if (nMustBinLen <> -1) then
    if ((nLen shr 1) <> nMustBinLen) then
      Exit;

  // now check the single chars (used the HEXTAB string from above)
  for nI:=1 to nLen do begin

    cActChar:=Upcase(sCheckThis[nI]);
    nJ:=0;
    while (nJ < 16) do begin
      if (HEXTAB[nJ + 1] = cActChar) then
        Break;
      Inc(nJ);
    end;

    // no found in tab?
    if (nJ = 16) then
      Exit;
  end;

  // success
  Result:=True;

end;




class function TStrPlus.GetHeapStatusInfo : String;
var
  hstat : THeapStatus;
begin
  hstat:=GetHeapStatus;
  with hstat do begin
    Result:='TotalAddrSpace = ' + IntToStr(TotalAddrSpace) + #13#10 +
            'TotalUncommitted = ' + IntToStr(TotalUncommitted) + #13#10 +
            'TotalCommitted = ' + IntToStr(TotalCommitted) + #13#10 +
            'TotalAllocated = ' + IntToStr(TotalAllocated) + #13#10 +
            'TotalFree = ' + IntToStr(TotalFree) + #13#10 +
            'FreeSmall = ' + IntToStr(FreeSmall) + #13#10 +
            'FreeBig = ' + IntToStr(FreeBig) + #13#10 +
            'Unused = ' + IntToStr(Unused) + #13#10 +
            'Overhead = ' + IntToStr(Overhead) + #13#10 +
            'HeapErrorCode = ' + IntToStr(HeapErrorCode);
  end;
end;



class function TStrPlus.VersionFormat(nMajor : Integer;
                                      nMinor : Integer = -1;
                                      nBuild : Integer = -1;
                                      const sAddOn : String = '') : String;
var
  sTemp : String;
begin
  Result:=IntToStr(nMajor);
  if ((nMinor <> -1) and (nMinor < 100)) then begin
    sTemp:=IntToStr(nMinor);
    Result:=Result + '.' + Copy('00', 1, 2 - Length(sTemp)) + sTemp;
    if ((nBuild <> -1)  and (nBuild < 1000)) then begin
      sTemp:=IntToStr(nBuild);
      Result:=Result + '.' + Copy('000', 1, 3 - Length(sTemp)) + sTemp;
    end;
  end;
  if (Length(sAddOn) > 0) then
    Result:=Result + ' ' + sAddOn;
end;


class function TStrPlus.VersionFormatEx
  (nMajor : Integer;
   nMinor : Integer = -1;
   nSubMinor : Integer = -1;
   nBuild : Integer = -1;
   const sAddOn : String = '') : String;
var
  sTemp : String;
begin
  Result:=IntToStr(nMajor);
  if ((nMinor <> -1) and (nMinor < 100)) then begin
    sTemp:=IntToStr(nMinor);
    Result:=Result + '.' + Copy('00', 1, 2 - Length(sTemp)) + sTemp;
    if ((nSubMinor <> -1) and (nSubMinor < 100)) then begin
      sTemp:=IntToStr(nSubMinor);
      Result:=Result + '.' + Copy('00', 1, 2 - Length(sTemp)) + sTemp;
      if ((nBuild <> -1)  and (nBuild < 1000)) then begin
        sTemp:=IntToStr(nBuild);
        Result:=Result + '.' + Copy('000', 1, 3 - Length(sTemp)) + sTemp;
      end;
    end;  
  end;
  if (Length(sAddOn) > 0) then
    Result:=Result + ' ' + sAddOn;
end;



class function TStrPlus.StringToWideString(const sASCIIStr : String)
                                             : WideString;
var
  nLen : Integer;
begin
  nLen:=Length(sASCIIStr);
  SetLength(Result, nLen);
  StringToWideChar(sASCIIStr, PWideChar(Result), nLen + 1);
end;

class function TStrPlus.FileAttrToStr(lAttributes : WORD32) : String;
begin
  if ((lAttributes and FILE_ATTRIBUTE_ARCHIVE) = FILE_ATTRIBUTE_ARCHIVE) then
    Result:='A'
  else
    Result:='';
  if ((lAttributes and FILE_ATTRIBUTE_READONLY) = FILE_ATTRIBUTE_READONLY) then
    Result:=Result + 'R';
  if ((lAttributes and FILE_ATTRIBUTE_HIDDEN) = FILE_ATTRIBUTE_HIDDEN) then
    Result:=Result + 'H';
  if ((lAttributes and FILE_ATTRIBUTE_SYSTEM) = FILE_ATTRIBUTE_SYSTEM) then
    Result:=Result + 'S';
  if ((lAttributes and FILE_ATTRIBUTE_COMPRESSED)
        = FILE_ATTRIBUTE_COMPRESSED) then
    Result:=Result + 'C';
end;


class function TStrPlus.ExtractFileExtension(const sFileName : String) : String;
var
  nPos : Integer;
  nLen : Integer;
begin
  nLen:=Length(sFileName);
  nPos:=nLen;
  while (nPos > 0) do begin
    if (sFileName[nPos] = '.') then begin
      Result:=Copy(sFileName, nPos + 1, nLen - nPos);
      Exit;
    end;
    Dec(nPos);
  end;

  // no extension found at all
  Result:='';
end;


class function TStrPlus.StringSplit(const sSource : String;
                                    cSplitter : Char) : TStringList;
var
  nStartPos : Integer;
  nPos      : Integer;
  nLen      : Integer;

begin
  // create the list
  Result:=TStringList.Create;
  Result.BeginUpdate;

  // now scan the string
  nStartPos:=1;
  nLen:=Length(sSource);
  for nPos:=1 to nLen do
    if (sSource[nPos] = cSplitter) then begin
      Result.Add(Copy(sSource, nStartPos, nPos - nStartPos));
      nStartPos:=nPos + 1;
    end;

  // add the final string
  if (nStartPos <= nLen) then
    Result.Add(Copy(sSource, nStartPos, nLen - nStartPos + 1));
  Result.EndUpdate;
end;


class function TStrPlus.IPAddrToStr(lIPAddr : WORD32;
                                    blBigEndian : Boolean) : String;
var
  nI : Integer;
begin
  if (blBigEndian) then
    for nI:=3 downto 0 do begin
      if (nI < 3) then
        Result:=IntToStr((lIPAddr shr (nI shl 3)) and $0ff) + '.' + Result
      else
        Result:=IntToStr((lIPAddr shr (nI shl 3)) and $0ff);
    end
  else
    for nI:=0 to 3 do begin
      if (nI > 0) then
        Result:=IntToStr((lIPAddr shr (nI shl 3)) and $0ff) + '.' + Result
      else
        Result:=IntToStr((lIPAddr shr (nI shl 3)) and $0ff);
    end;
end;


class function TStrPlus.FilterIllegalChars(const sDirtyStr : String;
                                           blAllowLineBreaks : Boolean)
                                             : String;
var
  nPos     : Integer;
  nLen     : Integer;
  nNewLen  : Integer;
  cActChar : Char;
  blAddIt  : Boolean;
begin
  // fast filtering
  nLen:=Length(sDirtyStr);
  SetLength(Result, nLen);
  nNewLen:=0;
  for nPos:=1 to nLen do begin
    cActChar:=sDirtyStr[nPos];
    blAddIt:=False;
    case cActChar of
      #0..#31 : begin
        if (blAllowLineBreaks) then
          case cActChar of
            #13 : blAddIt:=True;
            #10 : blAddIt:=True;
          end;
      end;
    else
      blAddIt:=True;
    end;
    if (blAddIt) then begin
      Inc(nNewLen);
      Result[nNewLen]:=cActChar;
    end;
  end;
  SetLength(Result, nNewLen);
end;


class function TStrPlus.Replace(const sSource : String;
                                const sThis : String;
                                const sThat : String) : String;
var
  nPos       : Integer;
  nThisLen   : Integer;
  sNewSource : String;
begin
  // avoid silly cases
  nThisLen:=Length(sThis);
  if ((Length(sSource) = 0) or (nThisLen = 0)) then begin
    Result:=sSource;
    Exit;
  end;

  // FIXME: maybe faster with linear scanning and buffer build
  sNewSource:=sSource;
  Result:='';
  repeat
    nPos:=Pos(sThis, sNewSource);
    if (nPos <> 0) then begin
      Result:=Result + Copy(sNewSource, 1, nPos - 1) + sThat;
      sNewSource:=Copy(sNewSource,
                    nPos + nThisLen,
                    Length(sNewSource) - (nPos + nThisLen) + 1);
    end
    else
      Result:=Result + sNewSource;
  until (nPos = 0);
end;


class function TStrPlus.RootPath(const sPath : String) : String;
var
  nI   : Integer;
  nPos : Integer;
  nLen : Integer;
begin
  // short (not an absolute) path?
  nLen:=Length(sPath);
  if (nLen < 3) then begin
    Result:=sPath;
    Exit;
  end;

  // drive path?
  if (sPath[2] = ':') then begin
    if (sPath[3] = '\') then
      Result:=Copy(sPath, 1, 3)
    else
      Result:=Copy(sPath, 1, 2) + '\';
    Exit;
  end;

  // NETBIOS path?
  if (Copy(sPath, 1, 2) = '\\') then begin
    nI:=0;
    for nPos:=3 to nLen do begin
      if (sPath[nPos] = '\') then begin
        Inc(nI);
        if (nI = 2) then begin
          Result:=Copy(sPath, 1, nPos);
          Exit;
        end;
      end;
    end;
  end;

  // not an absolute path
  Result:=sPath;
end;


class function TStrPlus.IsUnicodeOS : Boolean;
begin
  Result:=_blUnicodeSupported;
end;

class function TStrPlus.CompareFromBegin(const sSub : String;
                                         const sMain : String;
                                         nToCmp : Integer) : Boolean;
var
  nI    : Integer;
  pSub  : PChar;
  pMain : PChar;
begin
  // we use the zero string property of long strings for a fast comparison
  pSub:=PChar(sSub);
  pMain:=PChar(sMain);
  nI:=0;
  while ((pSub^ <> #0) and (nI < nToCmp)) do begin

    if ((pMain^ <> #0) and (pMain^ = pSub^)) then begin
      Inc(pSub);
      Inc(pMain);
      Inc(nI);
    end
    else begin
      Result:=False;
      Exit;
    end;
  end;
  Result:=True;
end;


class function TStrPlus.CompareFromEnd(const sSub : String;
                                       const sMain : String;
                                       blCaseSensitive : Boolean) : Boolean;
var
  nMainPos : Integer;
  nSubPos  : Integer;
  nBottom  : Integer;
  nSubLen  : Integer;
  nMainLen : Integer;
  sSubNew  : String;
  sMainNew : String;
begin
  // assume non nonequality
  Result:=False;

  // treat simple cases
  if ((sSub = '') or (sMain = '')) then
    Exit;
  nSubLen:=Length(sSub);
  nMainLen:=Length(sMain);
  if (nSubLen > nMainLen) then
    Exit;

  // prepare the string for comparison, i.n.
  if (blCaseSensitive) then begin
    sSubNew:=sSub;
    sMainNew:=sMain;
  end
  else begin
    sSubNew:=AnsiUpperCase(sSub);
    sMainNew:=AnsiUpperCase(sMain);
  end;

  // compare now
  nSubPos:=nSubLen;
  nBottom:=nMainLen - nSubLen + 1;
  for nMainPos:=nMainLen downto nBottom do
    if (sSubNew[nSubPos] <> sMainNew[nMainPos]) then
      Exit
    else
      Dec(nSubPos);

  // equal
  Result:=True;
end;


class function TStrPlus.ParentPath(const sPath : String) : String;
var
  nI       : Integer;
  nPos     : Integer;
  nLen     : Integer;
  sPathNew : String;
begin
  // be pessimistic
  Result:='';

  // first get a non-RTL path
  sPathNew:=PurePath(sPath);

  // does it make sense to parse it? (minimum is "a:\b")
  nLen:=Length(sPathNew);
  if (nLen < 4) then
    Exit;

  // now search the upper level (minimum criteria see above)
  nPos:=nLen;
  while (nPos > 1) do
    if (sPathNew[nPos] = '\') then
      Break
    else
      Dec(nPos);

  // network paths need another check
  if (Copy(sPathNew, 1, 2) = '\\') then begin

    // skip hopeless cases (minimum is "\\a\b\c")
    if (nPos < 4) then
      Exit;

    // search for a '\' in between
    for nI:=(nPos - 1) downto 3 do
      if (sPathNew[nI] = '\') then
        Break;
    if (nI > 3) then
      Result:=Copy(sPathNew, 1, nPos - 1);
  end
  else
    // check special drive path case
    if ((sPathNew[2] = ':') and (nPos = 3)) then
      Result:=Copy(sPathNew, 1, 3)
    else
      Result:=Copy(sPathNew, 1, nPos - 1);
end;


class function TStrPlus.ListToStr(theList : TStringList;
                                  cSeparator : Char = ',') : String;
var
  nI     : Integer;
  nUpIdx : Integer;
begin
  // (FIXME: slow build routine, buffer will be much)
  nUpIdx:=theList.Count - 1;
  Result:='';
  for nI:=0 to nUpIdx do begin
    Result:=Result + theList.Strings[nI];
    if (nI < nUpIdx) then
      Result:=Result + cSeparator;
  end;
end;



class function TStrPlus.StrToList(const sTheStr : String;
                                  cSeparator : Char = ',';
                                  blTrim : Boolean = True) : TStringList;
var
  nLen      : Integer;
  nPos      : Integer;
  nLastSepa : Integer;
  sEntry    : String;
begin
  nLen:=Length(sTheStr);
  nLastSepa:=0;
  Result:=TStringList.Create;
  Result.BeginUpdate;
  for nPos:=1 to nLen do begin
    if (sTheStr[nPos] = cSeparator) then begin
      sEntry:=Copy(sTheStr,
                   nLastSepa + 1,
                   nPos - nLastSepa - 1);
      if (blTrim) then
        Result.Add(Trim(sEntry))
      else
        Result.Add(sEntry);
      nLastSepa:=nPos;
    end;
  end;
  if (nLastSepa < nLen) then begin
    sEntry:=Copy(sTheStr,
                 nLastSepa + 1,
                 nLen - nLastSepa);
    if (blTrim) then
      Result.Add(Trim(sEntry))
    else
      Result.Add(sEntry);
  end;
  Result.EndUpdate;
end;


class function TStrPlus.MakeTempFileName(
        const sPath : String;
        const sPrefix : String = '') : String;
var
  buf : array[0..MAX_PATH] of Char;
begin

  if (0 = Windows.GetTempFileName(PChar(sPath),
        PChar(sPrefix),
        0,
        @buf)) then begin
    // FIXME: good workaround/replacement (at least it's 8.3 compatible)?
    Result:=RTLPath(sPath) +
            IntToHex(Random(65536), 4) +
            IntToHex(Random(65536), 4) +
            '.TMP';
  end
  else begin
    Result:=String(PChar(@buf));
  end;
end;


class function TStrPlus.ExtractCStrings(pRawList : Pointer) : TStringList;
var
  pRun : PChar;
begin
  Result:=TStringList.Create;
  Result.BeginUpdate;
  pRun:=pRawList;
  while (pRun^ <> #0) do begin
    Result.Add(String(pRun));
    pRun:=pRun + Length(String(pRun)) + 1;
  end;
  Result.EndUpdate;
end;


class function TStrPlus.HexDump(
  data : Pointer;
  nNumOfBytes : Integer;
  nBytesPerLine : Integer) : String;
var
  nPos, nC, nAddrW : Integer;
  pc : PChar;
  sLeft, sRight, sPad, sAddr : String;
  cVal : Char;
  lst : TStringList;
begin

  if (Nil = data) then begin
    Result := '(nil)';
    Exit;
  end;

  if (1 > nNumOfBytes) then begin
    Result := '';
    Exit;
  end;

  if (1 > nBytesPerLine) then nBytesPerLine := 16;

  lst := TStringList.Create;
  lst.Capacity := nNumOfBytes div nBytesPerLine + 1;

  if (0 = (nNumOfBytes and $ffff0000)) then nAddrW := 4 else nAddrW := 8;

  pc := PChar(data);

  nPos := 0;
  nC := 0;

  sAddr := IntToHex(0, nAddrW);

  while (nNumOfBytes > nPos) do begin

    cVal := pc[nPos];

    sLeft := sLeft + IntToHex(Byte(cVal), 2) + ' ';

    if (' ' > cVal) then sRight := sRight + '.'
    else                 sRight := sRight + cVal;

    Inc(nPos);
    Inc(nC);

    if (nBytesPerLine <= nC) then begin
      lst.Add(sAddr + '  ' + sLeft + '   ' + sRight);
      nC := 0;
      sLeft := '';
      sRight := '';
      sAddr := IntToHex(nPos, nAddrW);
    end;
  end;

  if (0 < nC) then begin

    nC := (nBytesPerLine - nC) * 3;
    SetLength(sPad, nC);
    for nPos := 1 to nC do sPad[nPos] := ' ';

    lst.Add(sAddr + '  ' + sLeft + sPad + '   ' + sRight);
  end;

  pc := lst.GetText;

  Result := pc;

  StrDispose(pc);
end;

class function TStrPlus.Base64Encode(
  pRaw : Pointer;
  nLen : Integer) : String;
var
  hnd  : WORD32;
  nEnc : Integer;
  pbuf : PChar;
begin

  GetMem(pbuf, BASE64_CALCOUTP_ENC(nLen) + 1);

  hnd := BASE64_HANDLE_INIT;

  nEnc := BASE64_Encode(hnd, pRaw, nLen, pbuf, 1);

  pbuf[nEnc] := #0;

  Result := pbuf;

  FreeMem(pbuf);
end;


class function TStrPlus.Base64Decode(
  const sEnc : String;
  var vnLen : Integer) : PChar;
var
  hnd : WORD32;
begin
  GetMem(Result, BASE64_CALCOUTP_DEC(Length(sEnc)) + 1);

  hnd := BASE64_HANDLE_INIT;
  vnLen := BASE64_Decode(hnd, PChar(sEnc), Length(sEnc), Result);

  if (-1 = vnLen) then begin
    FreeMem(Result);
    Result := Nil;
  end
  else begin
    Result[vnLen] := #0;
  end;
end;


// startup to do...
initialization
  // this unit uses the internal random generator,
  // so it's better to init. it right here
  Randomize;

  // Windows NT4/2K/XP/2003?
  versionInfo.dwOSVersionInfoSize:=SizeOf(versionInfo);
  GetVersionEx(versionInfo);
  _blUnicodeSupported:=(VER_PLATFORM_WIN32_NT = versioninfo.dwPlatformId);

end.

