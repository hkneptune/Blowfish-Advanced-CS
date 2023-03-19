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
  everything to store a program's settings and configuration
}

unit Configuration;

{$I config.inc}

interface
uses Classes, SysUtils;


// the comment prefix
const
  CONFIGURATION_COMMENTPREFIX = '//';

// integer range
const
  CONFIGURATION_MININT = Low(Integer);
  CONFIGURATION_MAXINT = High(Integer);


// configuration error type (we cannot send messages because TConfiguration is
// used by TStrRes and thus internationalisation might not be available)
type
  EConfigurationError = class(Exception)

    // constructor
    constructor Create; reintroduce;
  end;


// checker class (to check a section and set default values, if necessary)
type
  TConfigurationSection = class; // (forward)
  TConfigurationChecker = class

  protected
    // checks if an integer option exists, creates a new one (with range
    // checking) if not
    // -> the cfg. section to check
    // -> the option's ID
    // -> default value to set if the option doesn't exist
    // -> min. valid value
    // -> max. valid value
    procedure CheckInt(section : TConfigurationSection;
                       sID : String;
                       nDefault : Integer = 0;
                       nMin : Integer = CONFIGURATION_MININT;
                       nMax : Integer = CONFIGURATION_MAXINT);

    // checks if a boolean option exists, creates a new one if not
    // -> the cfg. section to check
    // -> the option's ID
    // -> default state to set if the option doesn't exist
    procedure CheckBool(section : TConfigurationSection;
                        sID : String;
                        blDefault : Boolean = False);

    // checks if a boolean option exists, creates a new one if not
    // -> the cfg. section to check
    // -> the option's ID
    // -> default string to set if the option doesn't exist
    procedure CheckString(section : TConfigurationSection;
                          sID : String;
                          sDefault : String = '');


    // checks if a character (stored as an integer) option exists,
    //  creates a new one if not
    // -> the cfg. section to check
    // -> the option's ID
    // -> default character to set if the option doesn't exist
    procedure CheckChar(section : TConfigurationSection;
                        sID : String;
                        cDefault : Char); // (FIXME: what's a good def. char?)
  public

    // called back to
    // - check an individual section for its completeness (all options exist?)
    // - create missing options with default values
    // - verify existing options for the correctness
    // - remove unknown options (only complementary)
    // -> the cfg. section to check
    procedure RunCheck(section : TConfigurationSection); virtual; abstract;
  end;


  // section class (no "type" prefixed because of the forward above)
  TConfigurationSection = class
  private
    // the ID to identify the section, must be init.
    // in the constructor (case insensitive)
    m_sID : String;

  protected
    // the idents (all store in uppercase format)
    m_idents : TStringList;

    // the contents (all stored without leading and ending whitespaces)
    m_contents : TStringList;

    // the contents to be stored (as above)
    m_fixedContents : TStringList;

    // search for a given ident
    // -> ident to look for (will be automatically uppercased)
    // <- index of the option in the lists (-1 = option doesn't exist)
    function SearchIdent(var vsIdent : String) : Integer;

  public
    // the constructor
    // -> the indentifier
    constructor Create(sID : String); virtual;

    // the destructor
    destructor Destroy; override;

    // to check and set default values, must be called before using such a
    // class to guarantee correct contents or existing options repectively
    // -> the checker, implemented by the caller
    procedure Verify(checker : TConfigurationChecker); virtual;

    // to get a boolean option
    // -> boolean option identifier
    // <- return option value
    // exception:
    // EConfigurationError - option doesn't exist
    function GetBooleanOption(sIdent : String) : Boolean; virtual;

    // to set a boolean option
    // -> option identifier
    // -> new boolean value
    // exception:
    // EConfigurationError - option doesn't exist
    procedure SetBooleanOption(sIdent : String;
                               blContent : Boolean); virtual;

    // to fix a boolean option (will then be stored, too)
    // -> option identifier
    // -> new boolean value to fix
    // -> True: create a new entry, i.n. / False: throw error
    // -> True: set the complete option / False: fix only
    // exception:
    // EConfigurationError - option doesn't exist
    procedure FixBooleanOption(sIdent : String;
                               blContent : Boolean;
                               blForceNew : Boolean = False;
                               blSetAll : Boolean = True); virtual;

    // to get an integer option
    // -> integer option identifier
    // <- return option value
    // exception:
    // EConfigurationError - option doesn't exist
    function GetIntegerOption(sIdent : String) : Integer; virtual;

    // to set an integer option
    // -> option identifier
    // -> new integer value
    // exception:
    // EConfigurationError - option doesn't exist
    procedure SetIntegerOption(sIdent : String;
                               nContent : Integer); virtual;

    // to fix an integer option (will then be stored, too)
    // -> option identifier
    // -> new integer value to fix
    // -> True: create a new entry, i.n. / False: throw error
    // -> True: set the complete option / False: fix only
    // exception:
    // EConfigurationError - option doesn't exist
    procedure FixIntegerOption(sIdent : String;
                               nContent : Integer;
                               blForceNew : Boolean = False;
                               blSetAll : Boolean = True); virtual;

    // to get a string option
    // -> string option identifier
    // <- return option content
    // exception:
    // EConfigurationError - option doesn't exist
    function GetStringOption(sIdent : String) : String; virtual;

    // to set a string option
    // -> option identifier
    // -> new string content
    // exception:
    // EConfigurationError - option doesn't exist
    procedure SetStringOption(sIdent : String;
                              sContent : String); virtual;

    // to fix a string option (will then be stored, too)
    // -> option identifier
    // -> new string content to fix
    // -> True: create a new entry, i.n. / False: throw error
    // -> True: set the complete option / False: fix only
    // exception:
    // EConfigurationError - option doesn't exist
    procedure FixStringOption(sIdent : String;
                              sContent : String;
                              blForceNew : Boolean = False;
                              blSetAll : Boolean = True); virtual;

    // checks if an option is already present
    // -> option identifier
    // <- True: option exists / False: option unknown
    function OptionExists(sIdent : String) : Boolean; virtual;

    // removes an option
    // -> option identifier
    // exception:
    // EConfigurationError - option doesn't exist
    procedure RemoveOption(sIdent : String); virtual;

    // fixes a general option which its current (temporary) content
    // -> option identifier
    // exception:
    // EConfigurationError - option doesn't exist
    procedure FixOption(sIdent : String); virtual;

    // normal getters and setters
    function GetID : String; virtual;
    procedure SetID(sID : String); virtual;

    // to modify the internals, should only be used by checkers and to
    // load or save the ident and content strings
    function GetIdents : TStringList; virtual;
    function GetContents : TStringList; virtual;
    function GetFixedContents : TStringList; virtual;

    // to make a copy of the section
    // <- the clone
    function Clone : TConfigurationSection; virtual;

    // to restore the fixed settings
    procedure Restore; virtual;

  end;


// section container class with file load and save capabilities
type
  TConfigurationCensor = class;

  TConfiguration = class
  private
    // list of TConfigurationSection
    m_sectionList : TList;

    // the censor
    m_censor : TConfigurationCensor;
    
    // to search for a section
    // -> section ID (will be uppercased afterwards)
    // <- index in the list (-1 equals "not in the list")
    function SearchSection(var vsID : String) : Integer;

  public
    // constructor
    constructor Create; virtual;

    // destructor
    destructor Destroy; override;

    // clear the whole container
    procedure Clear;

    // load the configuration from a file
    // -> name of the configuration file
    // exception:
    // EConfigurationError - if the loading failed
    procedure LoadFromFile(sConfigFile : String);

    // save the configuration to a file
    // -> name of the configuration file
    // -> true: throw error if saving failed / false: do not
    // exception:
    // EConfigurationError - failed to save
    procedure SaveToFile(sConfigFile : String; blWarnings : Boolean);

    // get the reference of an existing section (even if it doesn't exists
    // a new and empty section will be delivered)
    // -> ID section identifier (always case insensitive)
    // -> to check the section, implemented by the caller
    // <- the section reference
    function GetSection(sSectionID : String;
                        checker : TConfigurationChecker = Nil)
                          : TConfigurationSection; virtual;

    // get the number of all sections
    // <- number of sections
    function GetNumOfSections : Integer; virtual;

    // get a section by its index
    // -> index of the section
    // <- the section reference
    function GetSectionByIndex(nIdx : Integer) : TConfigurationSection; virtual;

    // checks for a section's existance
    // <- True: section exists / False: no section with this ID
    function SectionExists(sSectionID : String) : Boolean;

    // sets a censor
    // -> new censor (may be Nil, destroyed later)
    procedure SetCensor(censor : TConfigurationCensor);

  end;

// a censor filters out information not wanted to be stored an is called
// every time before a configuration is stored physically
  TConfigurationCensor = class
  public
    // apply censorship
    // -> the configuration to censor
    procedure Apply(cfg : TConfiguration); virtual; abstract;
  end;



implementation
uses Windows, StringRes, General;


/////////////////////// EConfigurationError ///////////////////////

constructor EConfigurationError.Create;
begin
   // provide a properly set message anyway
  inherited Create('EConfigurationError');
end;


/////////////////////// TConfigurationChecker ///////////////////////

procedure TConfigurationChecker.CheckInt(section : TConfigurationSection;
                                         sID : String;
                                         nDefault : Integer = 0;
                                         nMin : Integer = CONFIGURATION_MININT;
                                         nMax : Integer = CONFIGURATION_MAXINT);
var
  nCheckThis : Integer;
begin
  if (not section.OptionExists(sID)) then
    section.FixIntegerOption(sID, nDefault, True)
  else begin
    nCheckThis:=section.GetIntegerOption(sID);
    if ((nCheckThis > nMax) or
        (nCheckThis < nMin)) then
      section.FixIntegerOption(sID, nDefault, True);
  end;
end;

procedure TConfigurationChecker.CheckBool(section : TConfigurationSection;
                                          sID : String;
                                          blDefault : Boolean = False);
begin
  if (not section.OptionExists(sID)) then
    section.FixBooleanOption(sID, blDefault, True);
end;

procedure TConfigurationChecker.CheckString(section : TConfigurationSection;
                                            sID : String;
                                            sDefault : String = '');
begin
  if (not section.OptionExists(sID)) then
    section.FixStringOption(sID, sDefault, True);
end;

procedure TConfigurationChecker.CheckChar(section : TConfigurationSection;
                                          sID : String;
                                          cDefault : Char);
var
  nCheckThis : Integer;
begin
  if (not section.OptionExists(sID)) then
    section.FixIntegerOption(sID, Ord(cDefault), True)
  else begin
    nCheckThis:=section.GetIntegerOption(sID); // (only printables, please)
    if ((nCheckThis < 32) or (nCheckThis > 255)) then
      section.FixIntegerOption(sID, Ord(cDefault), True);
  end;
end;




/////////////////////// TConfigurationSection ///////////////////////



constructor TConfigurationSection.Create(sID : String);
begin
  // copy the ID
  m_sID:=sID;

  // create the lists
  m_idents:=TStringList.Create;
  m_contents:=TStringList.Create;
  m_fixedContents:=TStringList.Create;
end;


destructor TConfigurationSection.Destroy;
begin
  // free the lists
  m_fixedContents.Destroy;
  m_contents.Destroy;
  m_idents.Destroy;
end;


procedure TConfigurationSection.Verify(checker : TConfigurationChecker);
begin
  // just call the checker method
  checker.RunCheck(Self);
end;


function TConfigurationSection.SearchIdent(var vsIdent : String) : Integer;
begin
  // search for the ident
  Result:=0;
  vsIdent:=UpperCase(vsIdent);
  while (Result < m_idents.Count) do begin
    if (m_idents.Strings[Result] = vsIdent) then
      Exit;
    Inc(Result);
  end;

  // nothing found
  Result:=-1;
end;


function TConfigurationSection.GetBooleanOption(sIdent : String) : Boolean;
var
  nIdx : Integer;
begin
  // try to get a boolean
  try
    nIdx:=SearchIdent(sIdent);
    if (nIdx = -1) then
      raise EConfigurationError.Create;
    if (StrToInt(m_contents.Strings[nIdx]) = 0) then
      Result:=False
    else
      Result:=True;
  except
    on EConvertError do
      raise EConfigurationError.Create;
  end;
end;


procedure TConfigurationSection.SetBooleanOption(sIdent : String;
                                                 blContent : Boolean);
var
  nIdx : Integer;
begin
  // set the new option (boolean is stored as an integer like)
  nIdx:=SearchIdent(sIdent);
  if (nIdx = -1) then
    raise EConfigurationError.Create;
  if (blContent) then
    m_contents.Strings[nIdx]:='1'
  else
    m_contents.Strings[nIdx]:='0';
end;


procedure TConfigurationSection.FixBooleanOption(sIdent : String;
                                                 blContent : Boolean;
                                                 blForceNew : Boolean = False;
                                                 blSetAll : Boolean = True);
var
  nIdx : Integer;
begin
  // set the new option (integer like) in both lists (if allowed)
  nIdx:=SearchIdent(sIdent);
  if (nIdx = -1) then
    if (not blForceNew) then
      raise EConfigurationError.Create
    else begin
      nIdx:=m_contents.Count; // (go on)
      m_idents.Add(sIdent);
      m_contents.Add('');
      m_fixedContents.Add('');
      blSetAll:=True;  // (if it's new we must set both!)
    end;
  if (blContent) then begin
    if (blSetAll) then
      m_contents.Strings[nIdx]:='1';
    m_fixedContents.Strings[nIdx]:='1';
  end
  else begin
    if (blSetAll) then
      m_contents.Strings[nIdx]:='0';
    m_fixedContents.Strings[nIdx]:='0';
  end;
end;


function TConfigurationSection.GetIntegerOption(sIdent : String) : Integer;
var
  nIdx : Integer;
begin
  // try to get an integer
  nIdx:=SearchIdent(sIdent);
  if (nIdx = -1) then
    raise EConfigurationError.Create;
  try
    Result:=StrToInt(m_contents.Strings[nIdx]);
  except
    on EConvertError do
      raise EConfigurationError.Create;
  end;
end;


procedure TConfigurationSection.SetIntegerOption(sIdent : String;
                                                 nContent : Integer);
var
  nIdx : Integer;
begin
  // set the new integer option
  nIdx:=SearchIdent(sIdent);
  if (nIdx = -1) then
    raise EConfigurationError.Create;
  m_contents.Strings[nIdx]:=IntToStr(nContent);
end;


procedure TConfigurationSection.FixIntegerOption(sIdent : String;
                                                 nContent : Integer;
                                                 blForceNew : Boolean = False;
                                                 blSetAll : Boolean = True);
var
  nIdx : Integer;
begin
  // set the new integer option in both lists (if allowed)
  nIdx:=SearchIdent(sIdent);
  if (nIdx = -1) then
    if (not blForceNew) then
      raise EConfigurationError.Create
    else begin
      m_idents.Add(sIdent);
      m_contents.Add(IntToStr(nContent)); // (set it always here!)
      m_fixedContents.Add(IntToStr(nContent));
      Exit;
    end;
  if (blSetAll) then
    m_contents.Strings[nIdx]:=IntToStr(nContent);
  m_fixedContents.Strings[nIdx]:=IntToStr(nContent);
end;


function TConfigurationSection.GetStringOption(sIdent : String) : String;
var
  nIdx : Integer;
begin
  // try to get a string
  nIdx:=SearchIdent(sIdent);
  if (nIdx = -1) then
    raise EConfigurationError.Create;
  Result:=m_contents.Strings[nIdx];
end;


procedure TConfigurationSection.SetStringOption(sIdent : String;
                                                sContent : String);
var
  nIdx : Integer;
begin
  // set the new string option
  nIdx:=SearchIdent(sIdent);
  if (nIdx = -1) then
    raise EConfigurationError.Create;
  m_contents[nIdx]:=sContent;
end;


procedure TConfigurationSection.FixStringOption(sIdent : String;
                                                sContent : String;
                                                blForceNew : Boolean = False;
                                                blSetAll : Boolean = True);
var
  nIdx : Integer;
begin
  // set the new integer option in both lists (if allowed)
  nIdx:=SearchIdent(sIdent);
  if (nIdx = -1) then
    if (not blForceNew) then
      raise EConfigurationError.Create
    else begin
      m_idents.Add(sIdent);
      m_contents.Add(sContent); // (set it always here, because it's new)
      m_fixedContents.Add(sContent);
      Exit;
    end;
  if (blSetAll) then
    m_contents.Strings[nIdx]:=sContent;
  m_fixedContents.Strings[nIdx]:=sContent;
end;


procedure TConfigurationSection.RemoveOption(sIdent : String);
var
  nIdx : Integer;
begin
  nIdx:=SearchIdent(sIdent);
  if (nIdx = -1) then
    raise EConfigurationError.Create;
  m_idents.Delete(nIdx);
  m_contents.Delete(nIdx);
  m_fixedContents.Delete(nIdx);
end;

procedure TConfigurationSection.FixOption(sIdent : String);
var
  nIdx : Integer;
begin
  nIdx:=SearchIdent(sIdent);
  if (nIdx = -1) then
    raise EConfigurationError.Create;

  // (just copy the content)
  m_fixedContents.Strings[nIdx]:=m_contents.Strings[nIdx];
end;



function TConfigurationSection.GetID : String;
begin
  Result:=m_sID;
end;


function TConfigurationSection.OptionExists(sIdent : String) : Boolean;
begin
  Result:=not (SearchIdent(sIdent) = -1)
end;


procedure TConfigurationSection.SetID(sID : String);
begin
  m_sID:=sID;
end;


function TConfigurationSection.GetIdents : TStringList;
begin
  Result:=m_idents;
end;


function TConfigurationSection.GetContents : TStringList;
begin
  Result:=m_contents;
end;


function TConfigurationSection.GetFixedContents : TStringList;
begin
  Result:=m_fixedContents;
end;


function TConfigurationSection.Clone : TConfigurationSection;
var
  nI : Integer;
begin
  // create the clone and its private members
  Result:=TConfigurationSection.Create(m_sID);
  Result.m_idents:=TStringList.Create;
  Result.m_contents:=TStringList.Create;
  Result.m_fixedContents:=TStringList.Create;

  m_idents.BeginUpdate;
  m_contents.BeginUpdate;
  m_fixedContents.BeginUpdate;

  // copy the options
  for nI:=0 to (m_idents.Count - 1) do begin
    Result.m_idents.Add(m_idents.Strings[nI]);
    Result.m_contents.Add(m_contents.Strings[nI]);
    Result.m_fixedContents.Add(m_fixedContents.Strings[nI]);
  end;

  m_fixedContents.EndUpdate;
  m_contents.EndUpdate;
  m_idents.EndUpdate;
end;


procedure TConfigurationSection.Restore;
var
  nI : Integer;
begin
  m_contents.BeginUpdate;
  m_contents.Clear;
  for nI:=0 to (m_fixedContents.Count - 1) do
    m_contents.Add(m_fixedContents.Strings[nI]);
  m_contents.EndUpdate;
end;


/////////////////////// TConfiguration ///////////////////////


// to identify a section header title in the configuration file
const
  SHT_INTRO = '[';
  SHT_OUTRO = ']';

// the equals symbol (what else)
const
  EQUALS_SYMBOL = '=';



function TConfiguration.SearchSection(var vsID : String) : Integer;
begin
  // search for the section index
  Result:=0;
  vsID:=UpperCase(vsID);
  while (Result < m_sectionList.Count) do begin
    if (vsID = TConfigurationSection(m_sectionList.Items[Result]).GetID) then
      Exit;
    Inc(Result);
  end;
  Result:=-1;
end;


constructor TConfiguration.Create;
begin
  // create the list for the single sections
  m_sectionList:=TList.Create;
end;


destructor TConfiguration.Destroy;
begin
  // free all sections
  Clear;

  // no more censorship
  if (m_censor <> Nil) then
    m_censor.Destroy;

  // free the section list
  m_sectionList.Destroy;
end;


procedure TConfiguration.Clear;
var
  nI : Integer;
begin
  // destroy all sections
  for nI:=0 to (m_sectionList.Count - 1) do
    TConfigurationSection(m_sectionList.Items[nI]).Destroy;

  // clear the list
  m_sectionList.Clear;
end;


procedure TConfiguration.LoadFromFile(sConfigFile : String);
var
  nListLen      : Integer;
  nEquPos       : Integer;
  nIdx          : Integer;
  nLen          : Integer;
  sActLine      : String;
  sSectionTitle : String;
  sIdent        : String;
  sContent      : String;
  cfgFile       : TextFile;
  identList     : TStringList;
  newSection    : TConfigurationSection;
begin
  // clear the list before doing any action
  Clear;

{$I-}
  // open the file
  AssignFile(cfgFile, sConfigFile);
  Reset(cfgFile);
  if (IOResult <> 0) then
    raise EConfigurationError.Create;

  // start reading
  newSection:=Nil;
  while (not Eof(cfgFile)) do begin

    // read a line
    ReadLn(cfgFile, sActLine);
    if (IOResult <> 0) then begin
      CloseFile(cfgFile);
      raise EConfigurationError.Create;
    end;

    // skip empty (or senseless lines) and comments
    if (Copy(sActLine, 1, Length(CONFIGURATION_COMMENTPREFIX)) <>
        CONFIGURATION_COMMENTPREFIX) then begin
      nLen:=Length(sActLine);
      if (nLen > 2) then begin

        // section header?
        if ((sActLine[1] = SHT_INTRO) and
            (sActLine[nLen] = SHT_OUTRO)) then begin

          // yes, get the section title
          sSectionTitle:=UpperCase(Copy(sActLine, 2, nLen - 2));

          // section already in the list?
          nIdx:=SearchSection(sSectionTitle);
          if (nIdx <> -1) then
            // yes, reload the reference
            newSection:=TConfigurationSection(m_sectionList.Items[nIdx])
          else begin
            // no, create and add a new sectiom
            newSection:=TConfigurationSection.Create(sSectionTitle);
            m_sectionList.Add(newSection);
        end;
        end
        else begin
          // ignore any lines if no section header was detected until now
          if (newSection <> Nil) then begin

            // line contains a (valid) equation?
            nEquPos:=Pos(EQUALS_SYMBOL, sActLine);
            if (nEquPos > 1) then begin

              // extract ident and content
              sIdent:=UpperCase(Trim(Copy(sActLine, 1, nEquPos - 1)));
              sContent:=Trim(Copy(sActLine, nEquPos + 1, nLen - nEquPos));

              // ident already in the section?
              identList:=newSection.getIdents;
              // (search directly to increase the speed)
              nIdx:=0;
              nListLen:=identList.Count;
              while (nIdx < nListLen) do begin
                if (identList.Strings[nIdx] = sIdent) then
                  break;
                Inc(nIdx);
              end;

              // modify/add the entry
              if (nIdx = nListLen) then begin
                identList.Add(sIdent);
                newSection.GetContents.Add(sContent);
                newSection.GetFixedContents.Add(sContent);
              end
              else begin
                identList.Strings[nIdx]:=sIdent;
                newSection.GetContents.Strings[nIdx]:=sContent;
                newSection.GetFixedContents.Strings[nIdx]:=sContent;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  // close the configuration file
  CloseFile(cfgFile);
  if (IOResult <> 0) then
    raise EConfigurationError.Create;

end;


procedure TConfiguration.SaveToFile(
        sConfigFile : String;
        blWarnings : Boolean);
var
  nI, nJ     : Integer;
  nNumOfOpts : Integer;
  blWasError : Boolean;
  sActLine   : String;
  hCfgFile   : THandle;
  actSection : TConfigurationSection;

procedure WriteLine(const sText : String = '');
var
  dwWritten : DWORD;
begin
  blWasError:=(FALSE = Windows.WriteFile(
        hCfgFile, PChar(sText)^, Length(sText), dwWritten, Nil));
  if (blWasError) then Exit;
  blWasError:=(FALSE = Windows.WriteFile(
        hCfgFile, #13#10, 2, dwWritten, Nil));
end;

begin

  // create the file
  hCfgFile:=Windows.CreateFile(
        PChar(sConfigFile),
        GENERIC_WRITE,
        FILE_SHARE_WRITE,
        Nil,
        CREATE_ALWAYS,
        FILE_ATTRIBUTE_NORMAL,
        0);

  if (INVALID_HANDLE_VALUE = hCfgFile) then begin
    // if the file is readonly we won't warn
    if (blWarnings) then begin
      raise EConfigurationError.Create
    end
    else
      Exit;
  end;

  // let the censor check the configuration
  if (m_censor <> Nil) then
    m_censor.Apply(Self);

  // put out all sections
  blWasError:=False;
  nI:=0;
  while ((nI < m_sectionList.Count) and (not blWasError)) do begin

    // get a section
    actSection:=TConfigurationSection(m_sectionList.Items[nI]);

    // write section header
    sActLine:=SHT_INTRO + actSection.GetID + SHT_OUTRO;
    WriteLine(sActLine);

    // write section options (idents and fixed contents)
    if (not blWasError) then begin
      nNumOfOpts:=actSection.GetIdents.Count;
      nJ:=0;
      while ((nJ < nNumOfOpts) and (not blWasError)) do begin
        // assemble the option expression
        sActLine:=actSection.GetIdents.Strings[nJ] +
                  EQUALS_SYMBOL +
                  actSection.GetFixedContents.Strings[nJ];

        // put out the option
        WriteLine(sActLine);

        // next option
        Inc(nJ);
      end;
    end;

    // put out empty line to seperate the sections
    if (not blWasError) then begin
      WriteLine;
    end;

    // next section
    Inc(nI);
  end;

  // close the configuration file
  if (FALSE = Windows.CloseHandle(hCfgFile)) then blWasError:=True;

  // i/o error occured?
  if (blWasError) then begin
    // yes, try to remove the invalid configuration file
    Windows.DeleteFile(PChar(sConfigFile));
    if (blWarnings) then
      raise EConfigurationError.Create;
  end;

end;


function TConfiguration.GetSection(sSectionID : String;
                                   checker : TConfigurationChecker = Nil)
                                     : TConfigurationSection;
var
  nIdx : Integer;
begin
  // section exists?
  nIdx:=SearchSection(sSectionID);
  if (nIdx = -1) then begin
    // no, create and add a new (empty) section
    Result:=TConfigurationSection.Create(sSectionID);
    m_sectionList.Add(Result);
  end
  else
    // yes, return the reference
    Result:=m_sectionList.Items[nIdx];

  // check before deliver, i.n.
  if (checker <> Nil) then
    Result.Verify(checker);
end;


function TConfiguration.GetNumOfSections : Integer;
begin
  // return the number of sections
  Result:=m_sectionList.Count;
end;


function TConfiguration.GetSectionByIndex(nIdx : Integer)
                                           : TConfigurationSection;
begin
   // get the section (reference)
  Result:=TConfigurationSection(m_sectionList.Items[nIdx]);
end;

function TConfiguration.SectionExists(sSectionID : String) : Boolean;
begin
  Result:=(SearchSection(sSectionID) <> -1);
end;


procedure TConfiguration.SetCensor(censor : TConfigurationCensor);
begin
  if (m_censor <> Nil) then
    m_censor.Destroy;
  m_censor:=censor;
end;


end.

