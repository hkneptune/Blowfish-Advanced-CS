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
  to manage the ciphers (test, set default, exclude debug ciphers, ...)
}

unit CipherManager;

interface
uses SysUtils, Classes,
     CipherServer,
     Configuration;

{$I config.inc}

// our own error classes
type
  ECipherManagerError  = class(Exception);


// we use our own configuration ID
const
  CIPHERMANAGER_CONFIGID = 'CIPHERMANAGER';


type
  TCipherManager = class
  private
    // members
    m_config : TConfigurationSection;
    m_cnames : TStringList;

  protected
    // gets the default cipher
    // <- default (second) cipher name
    function GetDefaultCipher : String;

  public
    // constructor
    // -> the configuration where to load & store
    // -> test ciphers before accepting them
    // exception: ECipherManagerError invalid cipher detected
    constructor Create(cfg : TConfiguration;
                       blTestCiphers : Boolean = False); reintroduce; overload;

    // destructor
    destructor Destroy; override;

    // sets the current selected cipher
    // -> cipher name
    // -> True: fix this selection / False: only temporary
    // exception: ECipherManagerError cipher with that name doesn't exist
    procedure SetCurrentCipher(const sCipherName : String;
                               blFix : Boolean = True);

    // gets the current cipher
    // <- cipher name
    function GetCurrentCipher : String;

    // sets the reencryption ciphers
    // -> 1st cipher
    // -> 2nd cipher
    // -> True: fix this selection / False: only temporary
    // exception: ECipherManagerError cipher with that name doesn't exist
    procedure SetReencryptionCiphers(const sFirstCipher : String;
                                     const sSecondCipher : String;
                                     blFix : Boolean = True);

    // gets the reencryption ciphers
    // -> where to put the first cipher
    // -> where to put the second cipher
    procedure GetReencryptionCiphers(var vsFirstCipher : String;
                                     var vsSecondCipher : String);

    // gets a list of all available ciphers
    // <- list of cipher names (reference only!)
    function GetCiphers : TStringList;

    // sets default ciphers
    procedure SetDefaultCiphers;

    // restores the fixed settings
    procedure Restore;
  end;


implementation
uses
  Forms,
  StringPlus,
  CipherServerTools;



//////////////////////////// TCipherManager ////////////////////////////


const
  CFGID_CURRENTCIPHER = 'CurrentCipher';
  CFGID_REENCCIPHER1  = 'ReencCipher1';
  CFGID_REENCCIPHER2  = 'ReencCipher2';


// configuration checker
type
  TCipherManagerCC = class(TConfigurationChecker)
  private
    m_parent : TCipherManager;
  public
    constructor Create(parent : TCipherManager);
    procedure RunCheck(section : TConfigurationSection); override;
  end;

constructor TCipherManagerCC.Create(parent : TCipherManager);
begin
  m_parent:=parent;
end;

procedure TCipherManagerCC.RunCheck(section : TConfigurationSection);
begin
  CheckString(section, CFGID_CURRENTCIPHER, m_parent.GetDefaultCipher);
  CheckString(section, CFGID_REENCCIPHER1, m_parent.GetDefaultCipher);
  CheckString(section, CFGID_REENCCIPHER2, m_parent.GetDefaultCipher);
end;


constructor TCipherManager.Create(cfg : TConfiguration;
                                  blTestCiphers : Boolean = False);
var
  nI  : Integer;
  cc  : TCipherManagerCC;
  cip : TCipher;
begin

  // get the cipher name list
  m_cnames:=TCipher.GetCipherNames;

  // test all ciphers, if necessary
  if (blTestCiphers) then begin

    for nI:=0 to (m_cnames.Count - 1) do begin
      cip:=Nil;
      try
        cip:=TCipher.Create(m_cnames.Strings[nI]);
        cip.ExecuteSelfTest(True);
        cip.Destroy;
      except
        on ECipherError do begin
          if (cip <> Nil) then
            cip.Destroy;
          raise ECipherManagerError.Create('invalid cipher "' +
            m_cnames.Strings[nI] + '" detected');
        end;
      end;
    end;
  end;

  // check the configuration
  cc:=TCipherManagerCC.Create(Self);
  m_config:=cfg.GetSection(CIPHERMANAGER_CONFIGID, cc);
  cc.Destroy;
end;


destructor TCipherManager.Destroy;
begin
  m_cnames.Destroy;
end;



procedure TCipherManager.SetCurrentCipher(const sCipherName : String;
                                          blFix : Boolean = True);
begin
  // cipher in the list?
  if (m_cnames.IndexOf(sCipherName) = -1) then
    raise ECipherManagerError.Create('invalid cipher name passed');

  if (blFix) then
    m_config.FixStringOption(CFGID_CURRENTCIPHER, sCipherName)
  else
    m_config.SetStringOption(CFGID_CURRENTCIPHER, sCipherName);
end;


procedure TCipherManager.GetReencryptionCiphers(var vsFirstCipher : String;
                                                var vsSecondCipher : String);
begin
  vsFirstCipher:=m_config.GetStringOption(CFGID_REENCCIPHER1);
  vsSecondCipher:=m_config.GetStringOption(CFGID_REENCCIPHER2);
end;


procedure TCipherManager.SetReencryptionCiphers(const sFirstCipher : String;
                                                const sSecondCipher : String;
                                                blFix : Boolean = True);
begin
  // cipher in the list?
  if ((m_cnames.IndexOf(sFirstCipher) = -1) or
      (m_cnames.IndexOf(sSecondCipher) = -1)) then
    raise ECipherManagerError.Create('invalid cipher name passed');

  if (blFix) then begin
    m_config.FixStringOption(CFGID_REENCCIPHER1, sFirstCipher);
    m_config.FixStringOption(CFGID_REENCCIPHER2, sSecondCipher);
  end
  else begin
    m_config.SetStringOption(CFGID_REENCCIPHER1, sFirstCipher);
    m_config.SetStringOption(CFGID_REENCCIPHER2, sSecondCipher);
  end;
end;


function TCipherManager.GetCurrentCipher : String;
begin
  Result:=m_config.GetStringOption(CFGID_CURRENTCIPHER);
end;


function TCipherManager.GetCiphers : TStringList;
begin
  Result:=m_cnames;
end;


procedure TCipherManager.SetDefaultCiphers;
begin
  SetCurrentCipher(GetDefaultCipher);
  SetReencryptionCiphers(GetDefaultCipher, GetDefaultCipher);
end;


function TCipherManager.GetDefaultCipher : String;
begin
  Result:=m_cnames.Strings[0];
end;


procedure TCipherManager.Restore;
begin
  m_config.Restore;
end;


end.

