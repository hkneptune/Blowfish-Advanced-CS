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

unit TesterForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, Buttons,
  RandomSource, SeedBuffer, ExtCtrls, Menus, ActnList, KeyCache;

type
  TTestMain = class(TForm)
    SheetHolder: TPageControl;
    StrPlusSheet: TTabSheet;
    StrPlus_GetLastChar_Btn: TButton;
    StrPlus_MulChar_Btn: TButton;
    StrPlus_ClearString_Btn: TButton;
    StrPlus_StrToDec_Btn: TButton;
    StrPlus_Sepa1000_Btn: TButton;
    StrPlus_RelativePath_Btn: TButton;
    StrPlus_GetBinStr_Btn: TButton;
    StrPlus_RandomStr_Btn: TButton;
    StrPlus_Win32FileTimeToStr_Btn: TButton;
    StrPlus_PurePath_Btn: TButton;
    StrPlus_BytesToHexStr_Btn: TButton;
    StrPlus_RTLPath_Btn: TButton;
    StrPlus_HexStrToBytes_Btn: TButton;
    StrPlus_LongFileName_Btn: TButton;
    CryptPakSheet: TTabSheet;
    CryptPak_MD5_Btn: TButton;
    CryptPak_SHA1_Btn: TButton;
    CryptPak_CRC32_Btn: TButton;
    HeapCheckSheet: TTabSheet;
    HeapCheckBtn: TButton;
    CryptPak_RandomPool_Btn: TButton;
    OpenDialog: TOpenDialog;
    CryptPak_CipherServer_Btn: TButton;
    MiscSheet: TTabSheet;
    SecureMemBtn: TButton;
    KeyCreatorBtn: TButton;
    StrPlus_VersionFormat_Btn: TButton;
    BFAFileSheet: TTabSheet;
    BFAFileEncryptBtn: TButton;
    WipeSheet: TTabSheet;
    WipeSimpleBtn: TButton;
    WipeProgressBar: TProgressBar;
    ProgressCallBackBtn: TButton;
    WipeDODBtn: TButton;
    WipeSFSBtn: TButton;
    StopWipeBtn: TBitBtn;
    PassInfo: TLabel;
    PathSearchSheet: TTabSheet;
    PathSearchBox: TEdit;
    PathSearchAbortBtn: TBitBtn;
    PathSearchList: TListBox;
    KillPathBtn: TButton;
    PathSearchContainerBtn: TButton;
    PathSearchProgressInfo: TLabel;
    StrPlus_StringToWideString_Btn: TButton;
    GetFile64LenBtn: TButton;
    EncryptedMemoryBtn: TButton;
    BytesInSeedBufferInfo: TLabel;
    AutoFlushSwitch: TCheckBox;
    FlushBtn: TButton;
    BISBBox: TLabel;
    RandomSourceBtn: TButton;
    SeedStuffBorder: TBevel;
    CrptFileBtn: TButton;
    KeyHashBtn: TButton;
    NoCacheSwitch: TCheckBox;
    CryptProgressBar: TProgressBar;
    NoKeyHashCheckSwitch: TCheckBox;
    BFAFileProgressBar: TProgressBar;
    BFAFileInputFileNameInfo: TLabel;
    BFAFileOutputFileNameInfo: TLabel;
    BFAFileSizeInfo: TLabel;
    BFAFileDecryptBtn: TButton;
    BFAFileStateInfo: TLabel;
    BFAFileCancelBtn: TBitBtn;
    BFAFileWipePassInfo: TLabel;
    GetDiskFreeSpaceBtn: TButton;
    BFArchiveListPopupMenu: TPopupMenu;
    BFArchivePUM_SortByName: TMenuItem;
    BFArchivePUM_SortByPath: TMenuItem;
    BFArchivePUM_SortBySize: TMenuItem;
    BFArchivePUM_SortByCompressedSize: TMenuItem;
    BFArchivePUM_SortByRatio: TMenuItem;
    BFArchivePUM_SortByCompressType: TMenuItem;
    BFArchivePUM_SortByTime: TMenuItem;
    BFArchivePUM_SortByAttributes: TMenuItem;
    BFArchivePUM_N1: TMenuItem;
    BFArchivePUM_UpwardsSwitch: TMenuItem;
    BFArchivePUM_SortByExtension: TMenuItem;
    BFArchivePUM_SortByPosInArc: TMenuItem;
    StrPlus_Replace_Btn: TButton;
    ActionList1: TActionList;
    Action1: TAction;
    PathList: TListBox;
    StrPlus_CalcPercentBtn: TButton;
    Reporter: TRichEdit;
    StrPlus_RootPathBtn: TButton;
    StrPlus_IsUnicodeOSBtn: TButton;
    StrPlus_CompareFromBeginBtn: TButton;
    StrPlus_CompareFromEndBtn: TButton;
    StrPlus_ParentPath_Btn: TButton;
    DummyListView: TListView;
    KeyCacheValidBtn: TButton;
    KeyCacheSetBtn: TButton;
    ShowRandomBtn: TButton;
    randomShow: TStaticText;
    KeyCacheInfo: TStaticText;
    KeyCacheStopResumeBtn: TButton;
    StrPlus_StrToListTest: TButton;
    CryptPak_ChrunchKeyBtn: TButton;
    CryptPak_YarrowBtn: TButton;
    ClusterAdjustBtn: TButton;
    procedure StrPlus_GetLastChar_BtnClick(Sender: TObject);
    procedure StrPlus_ClearString_BtnClick(Sender: TObject);
    procedure StrPlus_Sepa1000_BtnClick(Sender: TObject);
    procedure StrPlus_StrToDec_BtnClick(Sender: TObject);
    procedure StrPlus_RandomStr_BtnClick(Sender: TObject);
    procedure StrPlus_MulChar_BtnClick(Sender: TObject);
    procedure StrPlus_Win32FileTimeToStr_BtnClick(Sender: TObject);
    procedure StrPlus_BytesToHexStr_BtnClick(Sender: TObject);
    procedure StrPlus_HexStrToBytes_BtnClick(Sender: TObject);
    procedure StrPlus_GetBinStr_BtnClick(Sender: TObject);
    procedure StrPlus_RelativePath_BtnClick(Sender: TObject);
    procedure StrPlus_PurePath_BtnClick(Sender: TObject);
    procedure StrPlus_RTLPath_BtnClick(Sender: TObject);
    procedure StrPlus_LongFileName_BtnClick(Sender: TObject);
    procedure CryptPak_MD5_BtnClick(Sender: TObject);
    procedure CryptPak_SHA1_BtnClick(Sender: TObject);
    procedure CryptPak_CRC32_BtnClick(Sender: TObject);
    procedure HeapCheckBtnClick(Sender: TObject);
    procedure CryptPak_RandomPool_BtnClick(Sender: TObject);
    procedure CryptPak_CipherServer_BtnClick(Sender: TObject);
    procedure SecureMemBtnClick(Sender: TObject);
    procedure KeyCreatorBtnClick(Sender: TObject);
    procedure StrPlus_VersionFormat_BtnClick(Sender: TObject);
    procedure WipeSimpleBtnClick(Sender: TObject);
    procedure ProgressCallBackBtnClick(Sender: TObject);
    procedure WipeDODBtnClick(Sender: TObject);
    procedure WipeSFSBtnClick(Sender: TObject);
    procedure StopWipeBtnClick(Sender: TObject);
    procedure PathSearchBoxKeyPress(Sender: TObject; var Key: Char);
    procedure PathSearchAbortBtnClick(Sender: TObject);
    procedure PathSearchContainerBtnClick(Sender: TObject);
    procedure KillPathBtnClick(Sender: TObject);
    procedure StrPlus_StringToWideString_BtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GetFile64LenBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure EncryptedMemoryBtnClick(Sender: TObject);
    procedure MiscSheetMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure AutoFlushSwitchClick(Sender: TObject);
    procedure FlushBtnClick(Sender: TObject);
    procedure RandomSourceBtnClick(Sender: TObject);
    procedure CrptFileBtnClick(Sender: TObject);
    procedure KeyHashBtnClick(Sender: TObject);
    procedure BFAFileEncryptBtnClick(Sender: TObject);
    procedure BFAFileCancelBtnClick(Sender: TObject);
    procedure GetDiskFreeSpaceBtnClick(Sender: TObject);
    procedure BFAFileDecryptBtnClick(Sender: TObject);
    procedure StrPlus_RootPathBtnClick(Sender: TObject);
    procedure StrPlus_IsUnicodeOSBtnClick(Sender: TObject);
    procedure StrPlus_CompareFromBeginBtnClick(Sender: TObject);
    procedure StrPlus_Replace_BtnClick(Sender: TObject);
    procedure StrPlus_CalcPercentBtnClick(Sender: TObject);
    procedure StrPlus_CompareFromEndBtnClick(Sender: TObject);
    procedure StrPlus_ParentPath_BtnClick(Sender: TObject);
    procedure KeyCacheValidBtnClick(Sender: TObject);
    procedure KeyCacheSetBtnClick(Sender: TObject);
    procedure ShowRandomBtnClick(Sender: TObject);
    procedure KeyCacheStopResumeBtnClick(Sender: TObject);
    procedure StrPlus_StrToListTestClick(Sender: TObject);
    procedure CryptPak_ChrunchKeyBtnClick(Sender: TObject);
    procedure CryptPak_YarrowBtnClick(Sender: TObject);
    procedure ClusterAdjustBtnClick(Sender: TObject);
  public
    m_blWasBreak : Boolean;
    m_blAbortPathSearch : Boolean;
    m_blAbortMakeBFZ : Boolean;
    m_sBrowsePath : String;
    m_kci : TKeyCacheCallBack;
  end;

var
  TestMain: TTestMain;

implementation
uses bfacslib, StringPlus, MD5, SHA1, CRC32, RandomPool, LZSS, CipherServer,
     SecureMem, KeyCreator, Wipe, ProgressCallBack, PathSearch, General,
     BrowseForFolder, FileSupp, CrptFile,KeyHash, BFAFile, FileBrowser, Yarrow,
     CallBack, Globals, StringPlusI, MessageCallBack;

{$R *.DFM}


type
  TKCI = class(TKeyCacheCallBack)
  public
    procedure CallBack; override;
  end;

procedure TKCI.CallBack;
begin
  with GetCallBackObj as TTestMain do begin
    KeyCacheInfo.Caption:=IntToStr(GetRemainingSeconds);
  end;
end;




const
  SEEDBUFSIZE = 777;

procedure TTestMain.StrPlus_GetLastChar_BtnClick(Sender: TObject);
var
  sTest : String;
begin
  sTest:=InputBox('TStrPlus.GetLastChar', 'Enter a test message:', 'ABC');
  if (sTest = '') then
    ShowMessage('Nothing entered.')
  else
    ShowMessage(TStrPlus.GetLastChar(sTest));
end;

procedure TTestMain.StrPlus_ClearString_BtnClick(Sender: TObject);
var
  sTest : String;
begin
  sTest:=InputBox('TStrPlus.ClearString', 'Enter a message to clear:', 'ABC');
  TStrPlus.ClearString(sTest);
  ShowMessage('>>>' + sTest + '<<<');
end;

procedure TTestMain.StrPlus_Sepa1000_BtnClick(Sender: TObject);
begin
  ShowMessage(TStrPlusI.Sepa1000(_globals.GetSr, 1));
  ShowMessage(TStrPlusI.Sepa1000(_globals.GetSr, 1234));
  ShowMessage(TStrPlusI.Sepa1000(_globals.GetSr, 2001002003));
  ShowMessage(TStrPlusI.Sepa1000(_globals.GetSr, 123456789123456789));
end;

procedure TTestMain.StrPlus_StrToDec_BtnClick(Sender: TObject);
var
  nResult : Integer;
  sTest   : String;
begin
  sTest:=InputBox('TStrPlus.StrToDec', 'Enter an integer value:', '123456');
  if (not TStrPlus.StrToDec(sTest, nResult)) then
    ShowMessage('ERROR')
  else
    ShowMessage(TStrPlus.DecToStr(nResult));
end;

procedure TTestMain.StrPlus_RandomStr_BtnClick(Sender: TObject);
begin
  ShowMessage(TStrPlus.RandomStr(17));
end;

procedure TTestMain.StrPlus_MulChar_BtnClick(Sender: TObject);
begin
  ShowMessage(TStrPlus.MulChar('#', 5));
end;

procedure TTestMain.StrPlus_Win32FileTimeToStr_BtnClick(Sender: TObject);
var
  fhandle        : THandle;
  sFileName      : String;
  creationTime   : TFileTime;
  lastAccessTime : TFileTime;
  lastWriteTime  : TFileTime;
begin
  sFileName:='C:\AUTOEXEC.BAT';
  if (not InputQuery('TStrPlus.Win32FileTimeToStr',
                     'File to examine:', sFileName)) then
    Exit;
  fhandle:=CreateFile(PChar(sFileName),
                      GENERIC_READ,
                      0, nil, OPEN_EXISTING, 0, 0);
  if (fhandle = INVALID_HANDLE_VALUE) then begin
    MessageBeep(UINT(-1));
    Exit;
  end;
  GetFileTime(fhandle, @creationTime, @lastAccessTime, @lastWriteTime);
  CloseHandle(fhandle);
  ShowMessage('creationTime : ' +
              TStrPlusI.Win32FileTimeToStr(_globals.GetSr, creationTime) +
              #13#10 + 'lastAccessTime : ' +
              TStrPlusI.Win32FileTimeToStr(_globals.GetSr, lastAccessTime) +
              #13#10 + 'lastWriteTime : ' +
              TStrPlusI.Win32FileTimeToStr(_globals.GetSr, lastWriteTime));
end;

procedure TTestMain.StrPlus_BytesToHexStr_BtnClick(Sender: TObject);
const
  BUFSIZE = 20;
var
  nI      : Integer;
  testbuf : array[0..BUFSIZE - 1] of Byte;
begin
  for nI:=0 to (BUFSIZE - 1) do
    testbuf[nI]:=nI and $0ff;
  ShowMessage(TStrPlus.BytesToHexStr(@testbuf, BUFSIZE, ':'));
  ShowMessage(TStrPlus.BytesToHexStr(@testbuf, BUFSIZE, Chr(0)));
end;

procedure TTestMain.StrPlus_HexStrToBytes_BtnClick(Sender: TObject);
var
  nRes  : Integer;
  sTest : String;
  buf   : array[0..1023] of Byte;
begin
  sTest:=InputBox('TStrPlus.HexStrToBytes', 'Enter a hex string:',
                  '000102030405060708ff1020');
  nRes:=TStrPlus.HexStrToBytes(sTest, @buf);
  if (nRes = -1) then
    ShowMessage('ERROR')
  else
    ShowMessage(TStrPlus.BytesToHexStr(@buf, nRes, ':'));
end;

procedure TTestMain.StrPlus_GetBinStr_BtnClick(Sender: TObject);
var
  nLen  : Integer;
  sTest : String;
  sOut  : String;
begin
  sTest:=InputBox('TStrPlus.GetBinStr', 'Enter binary string:',
                  'test\aaABC');
  nLen:=TStrPlus.GetBinStr(sTest, sOut);
  if (nLen = -1) then
    ShowMessage('ERROR')
  else
    ShowMessage(TStrPlus.BytesToHexStr(PChar(sOut), nLen, ':'));
end;

procedure TTestMain.StrPlus_RelativePath_BtnClick(Sender: TObject);
var
  sTest : String;
begin
  sTest:=InputBox('TStrPlus.RelativePath', 'Enter a path:', 'C:\WINDOWS');
  ShowMessage('>>>' + TStrPlus.RelativePath(sTest)+ '<<<');
end;


procedure TTestMain.StrPlus_PurePath_BtnClick(Sender: TObject);
var
  sTest : String;
begin
  sTest:=InputBox('TStrPlus.PurePath', 'Enter a path:', 'C:\WINDOWS\');
  ShowMessage('>>>' + TStrPlus.PurePath(sTest)+ '<<<');
end;

procedure TTestMain.StrPlus_RTLPath_BtnClick(Sender: TObject);
var
  sTest : String;
begin
  sTest:=InputBox('TStrPlus.RTLPath', 'Enter a path:', 'C:\WINDOWS');
  ShowMessage('>>>' + TStrPlus.RTLPath(sTest)+ '<<<');
end;

procedure TTestMain.StrPlus_LongFileName_BtnClick(Sender: TObject);
var
  sTest : String;
begin
  sTest:=InputBox('TStrPlus.LongFileName', 'Enter a path/file:', 'C:\PROGRA~1\BFA97\BFA97.EXE');
  ShowMessage('>>>' + TStrPlus.LongFileName(sTest)+ '<<<');
end;

procedure TTestMain.CryptPak_MD5_BtnClick(Sender: TObject);
const
  REFSTR : String = 'ABCDEFG - Marshmellows for you and me!';
var
  sTest  : String;
  digest : TMD5Digest;
  tmpdig : TMD5Digest;
  hasher : TMD5;
  buf    : array[0..MD5_DIGESTSIZE - 1] of Byte;
begin
  hasher:=TMD5.Create;
  if (not hasher.SelfTest) then begin
    ShowMessage('selftest failed');
    hasher.Destroy;
    Exit;
  end;
  hasher.Update(PChar(REFSTR), Length(REFSTR));
  digest:=TMD5Digest.Create;
  hasher.Finalize(digest);
  ShowMessage(IntToStr(digest.GetDigestSize) + #13#10 +
              digest.GetHexStr + #13#10 +
              IntToHex(digest.GetFolded32, 8));
  hasher.Reset;
  hasher.Update(PChar(REFSTR), Length(REFSTR));
  hasher.Finalize(digest);
  ShowMessage(digest.GetHexStr);
  hasher.Destroy;
  digest.GetData(@buf);
  digest.SetData(@buf);
  ShowMessage(digest.GetHexStr + #13#10 +
              TStrPlus.BytesToHexStr(@buf, MD5_DIGESTSIZE, Chr(0)));
  tmpdig:=TMD5Digest.Create;
  if (digest.Equals(tmpdig)) then
    ShowMessage('True')
  else
    ShowMessage('False');
  tmpdig.CopyFrom(digest);
  if (digest.Equals(tmpdig)) then
    ShowMessage('True')
  else
    ShowMessage('False');
  tmpdig.Destroy;
  sTest:=InputBox('MD5', 'Enter something to hash:', REFSTR);
  hasher:=TMD5.Create;
  hasher.Update(PChar(sTest), Length(sTest));
  hasher.Finalize(digest);
  hasher.Destroy;
  ShowMessage(digest.GetHexStr);
  digest.Destroy;
end;



procedure TTestMain.CryptPak_SHA1_BtnClick(Sender: TObject);
const
  REFSTR : String = 'ABCDEFG - Marshmellows for you and me!';
var
  sTest  : String;
  digest : TSHA1Digest;
  tmpdig : TSHA1Digest;
  hasher : TSHA1;
  buf    : array[0..SHA1_DIGESTSIZE - 1] of Byte;
begin
  hasher:=TSHA1.Create;
  if (not hasher.SelfTest) then begin
    ShowMessage('selftest failed');
    hasher.Destroy;
    Exit;
  end;
  hasher.Update(PChar(REFSTR), Length(REFSTR));
  digest:=TSHA1Digest.Create;
  hasher.Finalize(digest);
  ShowMessage(IntToStr(digest.GetDigestSize) + #13#10 +
              digest.GetHexStr);
  hasher.Reset;
  hasher.Update(PChar(REFSTR), Length(REFSTR));
  hasher.Finalize(digest);
  ShowMessage(digest.GetHexStr);
  hasher.Destroy;
  digest.GetData(@buf);
  digest.SetData(@buf);
  ShowMessage(digest.GetHexStr + #13#10 +
              TStrPlus.BytesToHexStr(@buf, SHA1_DIGESTSIZE, Chr(0)));
  tmpdig:=TSHA1Digest.Create;
  if (digest.Equals(tmpdig)) then
    ShowMessage('True')
  else
    ShowMessage('False');
  tmpdig.CopyFrom(digest);
  if (digest.Equals(tmpdig)) then
    ShowMessage('True')
  else
    ShowMessage('False');
  tmpdig.Destroy;
  sTest:=InputBox('SHA-1', 'Enter something to hash:', REFSTR);
  hasher:=TSHA1.Create;
  hasher.Update(PChar(sTest), Length(sTest));
  hasher.Finalize(digest);
  hasher.Destroy;
  ShowMessage(digest.GetHexStr);
  digest.Destroy;
end;


procedure TTestMain.CryptPak_CRC32_BtnClick(Sender: TObject);
const
  REFSTR : String = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
var
  sTest : String;
  crc1  : TCRC32;
  crc2  : TCRC32;
begin
  sTest:=InputBox('CRC32', 'Enter something to checksum:', REFSTR);
  crc1:=TCRC32.Create;
  crc1.Update(PChar(sTest), Length(sTest));
  ShowMessage(IntToHex(crc1.Finalize, 8));
  crc1.Reset;
  crc1.Update(PChar(sTest), Length(sTest));
  crc2:=TCRC32.Create;
  crc2.SetValue(crc1.GetValue);
  ShowMessage(IntToHex(crc2.Finalize, 8));
  crc2.Destroy;
  crc1.Destroy;
end;

procedure TTestMain.HeapCheckBtnClick(Sender: TObject);
begin
  ShowMessage(TStrPlus.GetHeapStatusInfo);
end;

procedure TTestMain.CryptPak_RandomPool_BtnClick(Sender: TObject);
const
  BUFSIZE   = 1000;
  TESTLOOPS = 2345;
var
  nI, nJ   : Integer;
  qSummary : WORD64;
  sAddSeed : String;
  rndpool  : TRandomPool;
  buf      : array[0..BUFSIZE - 1] of Byte;
begin
  sAddSeed:=InputBox('RandomPool', 'Enter additional seed:', sAddSeed);
  rndpool:=TRandomPool.Create(PChar(sAddSeed), Length(sAddSeed));
  nI:=0;
  qSummary:=0;
  while (nI < TESTLOOPS) do begin
    rndpool.GetRandomBytes(@buf, BUFSIZE);
    for nJ:=0 to (BUFSIZE - 1) do
      Inc(qSummary, buf[nJ]);
    Inc(nI);
  end;
  qSummary:=qSummary div (BUFSIZE *  TESTLOOPS);
  ShowMessage('Average = ' + IntToStr(qSummary) + ' (= 127?)');
  rndpool.Destroy;
end;

procedure TTestMain.CryptPak_CipherServer_BtnClick(Sender: TObject);
var
  nI            : Integer;
  nNumOfBlocks  : Integer;
  lKeySize      : WORD32;
  blLoaded      : Boolean;
  blGotInfo     : Boolean;
  blIsSession   : Boolean;
  sCipherName   : String;
  sMessage      : String;
  sInBuf        : String;
  sOutBuf       : String;
  sLastBuf      : String;
  pInitData     : Pointer;
  pInitDataShow : PWORD8Buf;
  cinfo         : TCipherInfo;
  cipher        : TCipher;
  key           : array[0..63355] of Byte;
  rndbuf        : array[0..9] of Byte;
begin
  // select a cipher
  sCipherName:='Blowfish';
  if (not InputQuery('Cipher Server',
                     'Which cipher (name, please)?',
                     sCipherName)) then
    Exit;

  // get cipher information directly
  try
    cinfo:=TCipherInfo.Create(sCipherName);
  except
    on ece: ECipherError do begin
      ShowMessage('Error #' + IntToStr(ece.GetErrorCode));
      Exit;
    end;
  end;
  with cinfo do begin
    sMessage:='Block Size : ' + IntToStr(GetBlockSize) + #13#10 +
	      'Key Size : ' + IntToStr(GetKeySize) + #13#10;
    if (IsOwnHasher) then
      sMessage:=sMessage + 'Hashes Key : True' + #13#10
    else
      sMessage:=sMessage + 'Hashes Key : False' + #13#10;
    sMessage:=sMessage +
	      'Init. Data Size : ' + IntToStr(GetInitDataSize) + #13#10 +
	      'Context Size : ' + IntToStr(cinfo.GetContextSize) + #13#10 +
	      'SizeOf : ' + IntToStr(GetSizeOf) + #13#10;
    sMessage:=sMessage + 'Cipher is : ';
    // (we don't show debug ciphers, do we?)
    case GetCipherIs of
      CIPHER_IS_XORSTREAM : sMessage:=sMessage + 'XORSTREAM';
      CIPHER_IS_BLOCK     : sMessage:=sMessage + 'BLOCK';
      CIPHER_IS_BLOCKLINK : sMessage:=sMessage + 'BLOCKLINK';
      CIPHER_IS_NOBLOCK   : sMessage:=sMessage + 'NOBLOCK';
    end;
  end;
  ShowMessage(sMessage);
  cinfo.Destroy;
  // start test
  blLoaded:=False;
  blIsSession:=False;
  blGotInfo:=False;
  cipher:=Nil;
  pInitData:=NULL;
  try
    // load the cipher
    cipher:=TCipher.Create(sCipherName,
                           _globals.GetRndMng.GetRandomSource,
                           Nil,
                           0);
    blLoaded:=True;
    sMessage:='Cipher successfully loaded.' + #13#10;
    // get the cipher information
    cinfo:=cipher.GetInfoBlock;
    blGotInfo:=True;
    with cinfo do begin
      sMessage:=sMessage +
                'Cipher Information Block:' + #13#10 +
 	        '  Block Size : ' + IntToStr(GetBlockSize) + #13#10 +
	        '  Key Size : ' + IntToStr(GetKeySize) + #13#10;
      if (IsOwnHasher) then
        sMessage:=sMessage + '  Hashes Key : True' + #13#10
      else
        sMessage:=sMessage + '  Hashes Key : False' + #13#10;
      sMessage:=sMessage +
    	        '  Init. Data Size : ' + IntToStr(GetInitDataSize) + #13#10 +
  	        '  Context Size : ' + IntToStr(cinfo.GetContextSize) + #13#10 +
	        '  SizeOf : ' + IntToStr(GetSizeOf) + #13#10;
      sMessage:=sMessage + 'Cipher is : ';
      case GetCipherIs of
        CIPHER_IS_XORSTREAM : sMessage:=sMessage + 'XORSTREAM';
        CIPHER_IS_BLOCK     : sMessage:=sMessage + 'BLOCK';
        CIPHER_IS_BLOCKLINK : sMessage:=sMessage + 'BLOCKLINK';
        CIPHER_IS_NOBLOCK   : sMessage:=sMessage + 'NOBLOCK';
      end;
      sMessage:=sMessage + #13#10;
    end;
    // execute the selftest(s)
    cipher.ExecuteSelfTest(False);
    sMessage:=sMessage + 'Standard selftest passed.' + #13#10;
    cipher.ExecuteSelfTest(True);
    sMessage:=sMessage + 'Extended selftest passed.' + #13#10;
    // open a new session for encryption, using a simply created key
    if (cinfo.IsOwnHasher) then
      lKeySize:=1024
    else
      lKeySize:=cinfo.GetKeySize;
    for nI:=0 to (lKeySize - 1) do
      key[nI]:=nI and $0ff;
    GetMem(pInitData, cinfo.GetInitDataSize);
    cipher.OpenEncryptionSession(@key, lKeySize, pInitData);
    blIsSession:=True;
    // show the created init. data, i.n.
    if (cinfo.GetInitDataSize > 0) then begin
      sMessage:=sMessage + 'initialisation data returned by the cipher: ';
      pInitDataShow:=pInitData;
      for nI:=0 to (cinfo.GetInitDataSize - 1) do
        sMessage:=sMessage + IntToHex(Ord(pInitDataShow[nI]), 2) + ' ';
      sMessage:=sMessage + #13#10;
    end;
    // get some test data from the user
    sInBuf:=InputBox('Testing cipher ' + sCipherName,
                     'Something to encrypt please:',
                     'Enter your message to scramble right here.');
    // (don't forget the ending zero -> + 1)
    nNumOfBlocks:=Length(sInBuf) + 1;
    if (nNumOfBlocks = 1) then
      nNumOfBlocks:=0  // allows zero data
    else begin
      if (nNumOfBlocks mod cinfo.GetBlockSize <> 0) then
        nNumOfBlocks:=nNumOfBlocks div cinfo.GetBlockSize + 1
      else
        nNumOfBlocks:=nNumOfBlocks div cinfo.GetBlockSize;
    end;
    if (cinfo.GetBlockSize > 1) then
      sMessage:=sMessage + 'number of ' + IntToStr(cinfo.GetBlockSize) +
                ' byte blocks = ' + IntToStr(nNumOfBlocks) + #13#10;
    // encrypt this data (we use strings here for an easier buffer handling)
    SetLength(sInBuf, 65536);
    SetLength(sOutBuf, 65536);
    cipher.EncryptBlocks(PChar(sInBuf), PChar(sOutBuf), nNumOfBlocks);
    sMessage:=sMessage + 'encrypted message: ';
    for nI:=1 to (nNumOfBlocks * cinfo.GetBlockSize) do begin
      sMessage:=sMessage + IntToHex(Ord(sOutBuf[nI]), 2);
      if (nI mod cinfo.GetBlockSize = 0) then
        sMessage:=sMessage + ' ';
    end;
    sMessage:=sMessage + #13#10;
    // close the encryption session
    cipher.CloseSession;
    blIsSession:=False;
    // open a decryption session (using the same key, of course)
    cipher.OpenDecryptionSession(@key, lKeySize, pInitData);
    blIsSession:=True;
    // reset before decrypting (just for test purposes)
    cipher.ResetSession(pInitData);
    // decrypt the data, interrupt after the first block, if possible
    SetLength(sLastBuf, 65536);
    if (nNumOfBlocks < 2) then
      cipher.DecryptBlocks(PChar(sOutBuf), PChar(sLastBuf), nNumOfBlocks)
    else begin
      cipher.DecryptBlocks(PChar(sOutBuf), PChar(sLastBuf), 1);
      cipher.DecryptBlocksInsert(@sOutBuf[cinfo.GetBlockSize + 1],
                               @sLastBuf[cinfo.GetBlockSize + 1],
                               nNumOfBlocks - 1, PChar(sOutBuf));
    end;
    sMessage:=sMessage + 'decrypted message: ' + PChar(sLastBuf) + #13#10;
    // close the decryption session
    cipher.CloseSession;
    blIsSession:=False;
    // free the init. data
    FreeMem(pInitData);
    // destroy the information block keeper
    cinfo.Destroy;
    blGotInfo:=False;
    // get ten random bytes
    cipher.GetRandomData(@rndbuf, 10);
    sMessage:=sMessage + TStrPlus.BytesToHexStr(@rndbuf, 10, ' ') + #13#10;
    // release the cipher
    cipher.Destroy;
    blLoaded:=False;
    sMessage:=sMessage + 'Cipher successfully released.';
  except
    on ece: ECipherError do begin
      ShowMessage('Error #' + IntToStr(ece.GetErrorCode));
      try
        if (blGotInfo) then
          cinfo.Destroy;
        if (blIsSession) then begin
          cipher.CloseSession;
          FreeMem(pInitData);
        end;
        if (blLoaded) then
          cipher.Destroy;
        Exit;
      except
        on ece: ECipherError do
          ShowMessage('cleanup after error failed, error #"' +
                      IntToStr(ece.GetErrorCode));
      end;
      Exit;
    end;
  end;
  // show what we got
  ShowMessage(sMessage);
end;

procedure TTestMain.SecureMemBtnClick(Sender: TObject);
const
  TESTMEMSIZE = 300000;
var
  nI    : Integer;
  pBuf1 : PWORD8Buf;
  pBuf2 : PWORD8Buf;
  smem1 : TKeyMemory;
  smem2 : TKeyMemory;
begin

  // test secure memory cells
  try
    smem1:=TKeyMemory.Create(TESTMEMSIZE shr 1);
    smem2:=TKeyMemory.Create(TESTMEMSIZE);
  except
    on E: EOutOfMemory do begin
      ShowMessage('OH NO! Out of memory!');
      Exit;
    end;
  end;
  pBuf1:=smem1.GetPtr;
  pBuf2:=smem2.GetPtr;

  for nI:=0 to (TESTMEMSIZE shr 1) - 1 do
    pBuf1^[nI]:=nI and $0ff;

  smem2.SetData(pBuf1, TESTMEMSIZE shr 2, smem1.GetSize);
  smem2.GetData(pBuf1, TESTMEMSIZE shr 2, TESTMEMSIZE shr 1);

  for nI:=0 to (TESTMEMSIZE shr 1) - 1 do
    if (pBuf1^[nI] <> (nI and $0ff)) then begin
      ShowMessage('error #1');
      smem2.Destroy;
      smem1.Destroy;
      Exit;
    end;

  for nI:=0 to (TESTMEMSIZE shr 1) - 1 do
    if (pBuf2^[nI + (TESTMEMSIZE shr 2)] <> (nI and $0ff)) then begin
      ShowMessage('error #2');
      smem2.Destroy;
      smem1.Destroy;
      Exit;
    end;

  ShowMessage('okidoki');
  smem2.Destroy;
  smem1.Destroy;
  ShowMessage('done');
end;

procedure TTestMain.KeyCreatorBtnClick(Sender: TObject);
const
  SALTSIZE = 17;
  KEYSIZE  = 20;
var
  sPassword, sSalt : String;
  kc : TKeyCreator;
  key : TKeyMemory;
begin
  sPassword:=InputBox('Testing TKeyCreator',
                      'Enter the password:',
                      'ABCDEFG');
  sSalt:=TStrPlus.RandomStr(SALTSIZE);
  sSalt:=InputBox('Testing TKeyCreator',
                  'Enter the salt:',
                  sSalt);
  kc:=TKeyCreator.Create(PChar(sSalt), Length(sSalt),
                         PChar(sPassword), Length(sPassword));
  key:=kc.MakeKey(KEYSIZE);
  kc.Destroy;
  ShowMessage('hashed "' + sSalt + sPassword + '" down to this key:' + #13#10 +
              TStrPlus.BytesToHexStr(key.GetPtr, KEYSIZE, ' '));
  key.Destroy;
end;

procedure TTestMain.StrPlus_VersionFormat_BtnClick(Sender: TObject);
begin
  ShowMessage(TStrPlus.VersionFormat(3,
                                    15,
                                    439,
                                    'Beta 2'));
  ShowMessage(TStrPlus.VersionFormat(2,
                                    15,
                                    -1,
                                    ''));
  ShowMessage(TStrPlus.VersionFormat(1,
                                    -1,
                                    77777,
                                    'Final Release'));
end;



// our own progress callback object
type
  TMyProgress = class(TWipeProgress)
  public
    procedure CallBack; override;
  end;

// the progress handler
procedure TMyProgress.CallBack;
var
  nActPos : Integer;
  nMaxPos : Integer;
  mainwin : TTestMain;
begin
  GetIntLevels(nActPos, nMaxPos);
  mainwin:=TTestMain(m_callbackobj);
  mainwin.WipeProgressBar.Min:=0;
  mainwin.WipeProgressBar.Max:=nMaxPos;
  mainwin.WipeProgressBar.Position:=nActPos;
  if (GetPass <> WIPE_PASS_NOLOOPS) then
    mainwin.PassInfo.Caption:='Pass #' + IntToStr(GetPass);
  Application.ProcessMessages;
  if (mainwin.m_blWasBreak) then
    raise EWipeWasBreak.Create('user break detected');
end;



procedure TTestMain.ProgressCallBackBtnClick(Sender: TObject);
var
  nI     : Integer;
  proggy : TMyProgress;
begin
  // test progress callback
  Randomize;
  proggy:=TMyProgress.Create(Self);
  proggy.SetMaxPos(MakeWORD64(7777, 0));
  m_blWasBreak:=False;
  try
    for nI:=1 to proggy.GetMaxPos do begin
      proggy.CallBack;
      proggy.IncPos(1);
      if ((nI mod 1000) = 0) then
        proggy.IncPass;
    end;
  except
    on ewwb : EWipeWasBreak do begin
      ShowMessage(ewwb.Message);
    end;
  end;
  proggy.Destroy;
  MessageBeep(UINT(-1));
end;



procedure TTestMain.WipeSimpleBtnClick(Sender: TObject);
var
  sFileName : String;
  wiper     : TWipeSimple;
  pcb       : TMyProgress;
begin
  // test simple wiping
  with OpenDialog do begin
    InitialDir:='d:\test';
    Filter:='All files (*.*)|*.*';
    Title:='Select file for wiping';
    if (not Execute) then
      Exit
    else
      sFileName:=FileName;
  end;
  pcb:=TMyProgress.Create(Self);
  PassInfo.Caption:='';
  m_blWasBreak:=False;
  wiper:=Nil;
  try
    wiper:=TWipeSimple.Create(_globals.GetRndMng.GetRandomSource);
    if (not wiper.Execute(sFileName, pcb, _globals.GetSr)) then
      ShowMessage('weak wiping');
    wiper.Destroy;
  except
    on e : Exception do begin
      ShowMessage(e.Message);
      if (wiper <> Nil) then begin
        try
          wiper.Destroy;
        except
          ShowMessage('not good');
        end;
      end;
    end;
  end;
  pcb.Destroy;
end;



procedure TTestMain.WipeDODBtnClick(Sender: TObject);
var
  sFileName : String;
  wiper     : TWipeDOD;
  pcb       : TMyProgress;
begin
  // test simple wiping
  with OpenDialog do begin
    InitialDir:='d:\test';
    Filter:='All files (*.*)|*.*';
    Title:='Select file for wiping';
    if (not Execute) then
      Exit
    else
      sFileName:=FileName;
  end;
  pcb:=TMyProgress.Create(Self);
  m_blWasBreak:=False;
  wiper:=Nil;
  try
    wiper:=TWipeDOD.Create(_globals.GetRndMng.GetRandomSource);
    if (not wiper.Execute(sFileName, pcb, _globals.GetSr)) then
      ShowMessage('weak wiping');
    wiper.Destroy;
  except
    on e : Exception do begin
      ShowMessage(e.Message);
      if (wiper <> Nil) then begin
        try
          wiper.Destroy;
        except
          ShowMessage('not good');
        end;
      end;
    end;
  end;
  pcb.Destroy;
end;

procedure TTestMain.WipeSFSBtnClick(Sender: TObject);
var
  sFileName : String;
  wiper     : TWipeSFS;
  pcb       : TMyProgress;
begin
  // test simple wiping
  with OpenDialog do begin
    InitialDir:='d:\test';
    Filter:='All files (*.*)|*.*';
    Title:='Select file for wiping';
    if (not Execute) then
      Exit
    else
      sFileName:=FileName;
  end;
  pcb:=TMyProgress.Create(Self);
  m_blWasBreak:=False;
  wiper:=Nil;
  try
    wiper:=TWipeSFS.Create;
    if (not wiper.Execute(sFileName, pcb, _globals.GetSr)) then
      ShowMessage('weak wiping');
    wiper.Destroy;
  except
    on e : Exception do begin
      ShowMessage(e.Message);
      if (wiper <> Nil) then begin
        try
          wiper.Destroy;
        except
          ShowMessage('not good');
        end;
      end;
    end;
  end;
  pcb.Destroy;
end;

procedure TTestMain.StopWipeBtnClick(Sender: TObject);
begin
  // set the break flag
  m_blWasBreak:=True;
end;

type
  TPathSearchProgress = class(TPathSearchCallBack)
  public
    procedure CallBack; override;
  end;


procedure TPathSearchProgress.CallBack;
begin
  TTestMain(m_callbackobj).PathSearchProgressInfo.Caption:=GetActFile;
  Application.ProcessMessages;
  if (TTestMain(m_callbackobj).m_blAbortPathSearch) then
    raise ECallBackInterrupt.Create('interrupted');
end;


procedure TTestMain.PathSearchBoxKeyPress(Sender: TObject; var Key: Char);
var
  nI  : Integer;
  ps  : TPathSearch;
  pcb : TPathSearchProgress;
begin
  // start if the return key is pressed
  if (Key = #13) then begin
    PathSearchList.Items.Clear;
    PathList.Items.Clear;
    PathSearchBox.Enabled:=False;
    m_blAbortPathSearch:=False;
    ps:=TPathSearch.Create(_globals.GetSr);
    pcb:=TPathSearchProgress.Create(Self);
    try
      ps.RegisterFiles(PathSearchBox.Text, 0, True, pcb, True);
    except
      on e : Exception do
        ShowMessage(e.Message);
    end;
    pcb.Destroy;
    PathSearchProgressInfo.Caption:='files: ' + IntToStr(ps.GetNumOfFiles) +
                                    ', dirs: ' + IntToStr(ps.GetNumOfDirs) +
                                    ', bytes together: ' +
                                    IntToStr(ps.GetNumOfBytes);
    if (ps.GetNumOfFiles > 0) then
      PathSearchList.Items.Add(ps.GetFirstFile);
    if (ps.GetNumOfDirs > 0) then
      PathList.Items.Add(ps.GetFirstDir);
    for nI:=2 to ps.GetNumOfFiles do
      PathList.Items.Add(ps.GetNextFile);
    for nI:=2 to ps.GetNumOfDirs do
      PathList.Items.Add(ps.GetNextDir);
    ps.Destroy;
    PathSearchBox.Enabled:=True;
  end;
end;

procedure TTestMain.PathSearchAbortBtnClick(Sender: TObject);
begin
  m_blAbortPathSearch:=True;
end;

procedure TTestMain.PathSearchContainerBtnClick(Sender: TObject);
var
  nI  : Integer;
  psc : TPathSearchContainer;
  pcb : TPathSearchProgress;
begin
  PathSearchList.Items.Clear;
  PathList.Items.Clear;
  PathSearchBox.Enabled:=False;
  m_blAbortPathSearch:=False;
  psc:=TPathSearchContainer.Create(_globals.GetSr);
  pcb:=TPathSearchProgress.Create(Self);
  try
    psc.AddTree(PathSearchBox.Text, 0, True, pcb);
    psc.AddTree(PathSearchBox.Text, 0, True, pcb);
  except
    on e : Exception do
      ShowMessage(e.Message);
  end;
  pcb.Destroy;
  PathSearchProgressInfo.Caption:='files: ' + IntToStr(psc.GetNumOfFiles) +
                                  ', dirs: ' + IntToStr(psc.GetNumOfDirs) +
                                  ', bytes together: ' +
                                  TStrPlusI.Sepa1000(_globals.GetSr,
                                                     psc.GetNumOfBytes);
  if (psc.GetNumOfFiles > 0) then
    PathSearchList.Items.Add(psc.GetFirstFile);
  for nI:=2 to psc.GetNumOfFiles do
    PathSearchList.Items.Add(psc.GetNextFile);
  if (psc.GetNumOfDirs > 0) then
    PathList.Items.Add(psc.GetFirstDir);
  for nI:=2 to psc.GetNumOfDirs do
    PathList.Items.Add(psc.GetNextDir);
  psc.Destroy;
  PathSearchBox.Enabled:=True;
end;

procedure TTestMain.KillPathBtnClick(Sender: TObject);
var
  ptk : TPathToKill;
  pcb : TPathSearchProgress;
begin
  if (Application.MessageBox('Are you sure?', 'Confirmation',
                             MB_ICONQUESTION or MB_YESNO) = IDNO) then
    Exit;
  ptk:=TPathToKill.Create(PathSearchBox.Text, _globals.GetSr);
  pcb:=TPathSearchProgress.Create(Self);
  m_blAbortPathSearch:=False;
  try
    ptk.Erase(pcb);
  except
    on e : Exception do begin
      pcb.Destroy;
      ptk.Destroy;
      ShowMessage(e.Message);
      Exit;
    end;
  end;
  pcb.Destroy;
  ptk.Destroy;
  ShowMessage('done');
end;

procedure TTestMain.StrPlus_StringToWideString_BtnClick(Sender: TObject);
var
  sAStr : String;
  wsWStr : WideString;
begin
  sAStr:=InputBox('TTStrPlus.StringToWideString', 'Enter something:', 'ABCDEF');
  wsWStr:=TStrPlus.StringToWideString(sAStr);
  ShowMessage('>>>' + wsWStr + '<<<' + #13#10 +
              IntToStr(SizeOf(wsWStr[2])));
end;

type
  TDummyCB = class(TCallBack)
  public
    procedure CallBack; override;
  end;
  TDummyKCCB = class(TKeyCacheCallBack)
  public
    procedure CallBack; override;
  end;
  TDummyMCB = class(TMessageCallBack)
  public
    procedure CallBack; override;
  end;

procedure TDummyCB.CallBack;
begin
  // NOP
end;

procedure TDummyKCCB.CallBack;
begin
  // NOP
end;

procedure TDummyMCB.CallBack;
begin
  raise ECallbackInterrupt.Create('message callback?!?');
end;



var
  dcb   : TDummyCB;
  dkccb : TDummyKCCB;
  dmcb  : TDummyMCB;

procedure TTestMain.FormCreate(Sender: TObject);
begin
  // init. different stuff
  m_sBrowsePath:=GetCurrentDir;
  m_kci:=TKCI.Create(Self);

  // place us in the mid
  Left:=(Screen.Width - Width) shr 1;
  Top:=(Screen.Height - Height) shr 1;

  // init.
  dcb:=TDummyCB.Create(Nil);
  dkccb:=TDummyKCCB.Create(Nil);
  dmcb:=TDummyMCB.Create(Nil);

  
  _globals:=TGlobals.Create(DummyListView,
                            Nil,
                            dcb,
                            dkccb,
                            dmcb,
                            dcb);

end;


procedure TTestMain.GetFile64LenBtnClick(Sender: TObject);
var
  qFileLen : WORD64;
begin

  // getting 64bit file length
  with OpenDialog do begin
    Filter:='All files (*.*)|*.*';
    Title:='Select file for getting its length';
    if (not Execute) then
      Exit;
    try
      qFileLen:=TFileSupport.GetFile64Len(FileName, _globals.GetSr);
      ShowMessage('"' + FileName + '" is ' +
                  TStrPlusI.Sepa1000(_globals.GetSr, qFileLen) +
                  ' bytes large.');
    except
      on efse : EFileSupportError do
        ShowMessage('ERROR: ' + efse.Message);
    end;
  end;
end;

procedure TTestMain.FormDestroy(Sender: TObject);
begin
  _globals.GetKeyCache.StopTimer;
  m_kci.Destroy;
  _globals.Destroy;
end;

procedure TTestMain.EncryptedMemoryBtnClick(Sender: TObject);
const
  TESTMEMSIZE = 67890; // (must be even)
var
  nI    : Integer;
  emem1 : TEncryptedKeyMemory;
  emem2 : TEncryptedKeyMemory;
  buf   : array[0..(TESTMEMSIZE - 1)] of WORD8;
begin

  // test encrypted memory
  try
    emem1:=TEncryptedKeyMemory.Create(TESTMEMSIZE,
                                      _globals.GetRndMng.GetRandomSource);
    emem2:=TEncryptedKeyMemory.Create(TESTMEMSIZE,
                                      _globals.GetRndMng.GetRandomSource);
  except
    on E: EOutOfMemory do begin
      ShowMessage('OH NO! Out of memory!');
      Exit;
    end;
    on Exception do begin
      ShowMessage('check your code, dude!');
      Exit;
    end;
  end;

  for nI:=0 to (TESTMEMSIZE - 1) do
    buf[nI]:=nI and $0ff;

  emem2.SetData(@buf, 0, emem2.GetSize);
  FillChar(buf, TESTMEMSIZE, 0);
  emem2.GetData(@buf, 0, TESTMEMSIZE shr 1);
  emem2.GetData(@buf[TESTMEMSIZE shr 1], TESTMEMSIZE shr 1, TESTMEMSIZE shr 1);

  for nI:=0 to (TESTMEMSIZE - 1) do
    if (buf[nI] <> (nI and $0ff)) then begin
      ShowMessage('error #1');
      emem2.Destroy;
      emem1.Destroy;
      Exit;
    end;

  emem1.SetData(@buf, 0, emem1.GetSize);
  emem1.Clear;
  emem1.GetData(@buf, 0, TESTMEMSIZE);
  for nI:=0 to (TESTMEMSIZE - 1) do
    if (buf[nI] <> 0) then begin
      ShowMessage('error #2');
      emem2.Destroy;
      emem1.Destroy;
      Exit;
    end;

  ShowMessage('okidoki');
  emem2.Destroy;
  emem1.Destroy;
  ShowMessage('done');
end;

procedure TTestMain.MiscSheetMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
   // add data to the seed buffer
   _globals.GetRndMng.AddSeed(X);
   _globals.GetRndMng.AddSeed(Y);
   // show what's going on
   BISBBox.Caption:=IntToStr(_globals.GetRndMng.GetSeedBuffer.GetBytesInBuf);
end;

procedure TTestMain.AutoFlushSwitchClick(Sender: TObject);
begin
   // set the auto-flush flag
   _globals.GetRndMng.GetSeedBuffer.SetAutoFlush(AutoFlushSwitch.Checked);
end;

procedure TTestMain.FlushBtnClick(Sender: TObject);
begin
   // flush the seed buffer
   _globals.GetRndMng.FlushSeed;
   BISBBox.Caption:=IntToStr(_globals.GetRndMng.GetSeedBuffer.GetBytesInBuf);
end;

procedure TTestMain.RandomSourceBtnClick(Sender: TObject);
const
  NUMOFRNDBYTES = 1000;
var
  rndbuf : array[1..NUMOFRNDBYTES] of WORD8;
begin
  // get some random bytes
  _globals.GetRndMng.GetRandomSource.GetBytes(@rndbuf, NUMOFRNDBYTES);
  ShowMessage(TStrPlus.BytesToHexStr(@rndbuf, NUMOFRNDBYTES, ' '));
end;

procedure TTestMain.CrptFileBtnClick(Sender: TObject);
const
  TESTLOOPS       = 22;
  BUFSIZE         = 123456;
  CIPHERNAME      = 'Twofish';
  TESTFILENAME    = 'e:\test\test.cf';
var
  blOpened  : Boolean;
  nI, nJ    : Integer;
  sKey      : String;
  outbuf    : array[1..BUFSIZE] of Byte;
  inbuf     : array[1..BUFSIZE] of Byte;
  qFileSize : WORD64;
  cf        : TCryptFile;
begin
  cf:=Nil;
  blOpened:=False;
  try
    // create the cryptfile
    cf:=TCryptFile.Create(CIPHERNAME, _globals.GetSr,
                          _globals.GetRndMng.GetRandomSource , Nil, 0);
    cf.SetNoCache(NoCacheSwitch.Checked);
    cf.SetNoKeyHashCheck(NoKeyHashCheckSwitch.Checked);
    sKey:=InputBox('Password', 'Enter the password:', 'superburschi');
    cf.OpenCreate(TESTFILENAME, PChar(sKey), Length(sKey));
    blOpened:=True;
    Randomize;
    for nI:=1 to BUFSIZE do
      outbuf[nI]:=Random(256);
    for nI:=1 to TESTLOOPS do
      cf.Write(@outbuf, BUFSIZE);
    cf.Close;
    blOpened:=False;

    // re-read the data from the cryptfile
    sKey:=InputBox('Password', 'Enter the password again:', 'superburschi');
    cf.OpenRead(TESTFILENAME, PChar(sKey), Length(sKey));
    blOpened:=True;
    qFileSize:=cf.GetFileSize;
    ShowMessage('filesize: ' + TStrPlusI.Sepa1000(_globals.GetSr, qFilesize));
    for nJ:=1 to TESTLOOPS  do begin
      cf.Read(@inbuf, BUFSIZE);
      for nI:=1 to BUFSIZE do begin
        if (outbuf[nI] <> inbuf[nI]) then begin
          ShowMessage('invalid data in loop #' + IntToStr(nJ) + ' @ pos ' +
                      IntToStr(nI));
          break;
        end;
        inbuf[nI]:=0;
      end;
    end;
    cf.Close;
    blOpened:=False;
    cf.Destroy;
    ShowMessage('All tests passed.');
  except
    on ecfe : ECryptFileError do begin
      ShowMessage(ecfe.Message);
      try
        if (blOpened) then
          cf.Close;
        if (cf <> Nil) then
          cf.Destroy;
      except
        ShowMessage('Not good...');
      end;
      Exit;
    end;
  end;
end;

procedure TTestMain.KeyHashBtnClick(Sender: TObject);
var
  sSalt : String;
  sKey  : String;
  khs   : TKeyHashSimple;
  khe   : TKeyHashExtended;
begin
  // testing the key hasher

  sSalt:=InputBox('TKeyHashSimple', 'Enter the salt:', 'secret');
  sKey:=InputBox('TKeyHashSimple', 'Enter the key:', 'salt');
  khs:=TKeyHashSimple.Create(PChar(sSalt), Length(sSalt),
                             PChar(sKey), Length(sKey));
  ShowMessage(IntToHex(khs.Make32, 8));
  khs.Destroy;

  sSalt:=InputBox('TKeyHashExtended', 'Enter the salt:', 'secret');
  sKey:=InputBox('TKeyHashExtended', 'Enter the key:', 'salt');
  khe:=TKeyHashExtended.CreateEx(PChar(sSalt), Length(sSalt),
                                 PChar(sKey), Length(sKey),
                                 65536);
  ShowMessage(IntToHex(khe.Make32, 8));
  khe.Destroy;
end;


procedure TTestMain.BFAFileCancelBtnClick(Sender: TObject);
begin
  // set the break signal
  m_blWasBreak:=True;
end;

procedure TTestMain.GetDiskFreeSpaceBtnClick(Sender: TObject);
var
  sDrivePath : String;
  qFreeSpace : WORD64;
begin
  try
    sDrivePath:=InputBox('GetDiskFreeSpace(Ex)', 'Enter drive path:', 'C:\');
    qFreeSpace:=TFileSupport.GetDiskFreeBytes(sDrivePath, _globals.GetSr);
    ShowMessage('free disk space on "' + sDrivePath + '" : ' +
                TStrPlusI.Sepa1000(_globals.GetSr, qFreeSpace));
  except
    on efse : EFileSupportError do
      ShowMessage('ERROR: ' + efse.Message);
  end;
end;


// our own progress callback object for .BFA files
type
  TMyBFAFileProgress = class(TBFAFileProgress)
  public
    procedure CallBack; override;
  end;

// the progress handler
procedure TMyBFAFileProgress.CallBack;
var
  nActPos : Integer;
  nMaxPos : Integer;
  mainwin : TTestMain;
begin
  // status changed?
  mainwin:=TTestMain(m_callbackobj);
  if (GetChanged) then begin

    // yes, show the state
    if ((GetProgressState and BFAFILE_PROGRESS_ENCRYPT) =
        BFAFILE_PROGRESS_ENCRYPT) then
      mainwin.BFAFileStateInfo.Caption:='encrypting...'
    else
      if ((GetProgressState and BFAFILE_PROGRESS_DECRYPT) =
          BFAFILE_PROGRESS_DECRYPT) then
        mainwin.BFAFileStateInfo.Caption:='decrypting...'
      else
        mainwin.BFAFileStateInfo.Caption:='wiping...';

    // show the new filenames and the size
    mainwin.BFAFileOutputFileNameInfo.Caption:=GetInputFileName;
    mainwin.BFAFileOutputFileNameInfo.Caption:=GetOutputFileName;
    mainwin.BFAFileSizeInfo.Caption:=TStrPlusI.Sepa1000(_globals.GetSr,
                                                        GetFileSize);
  end;

  // set the current level
  if ((GetProgressState and BFAFILE_PROGRESS_WIPE) = 0) then begin
    // normal working level
    GetIntLevels(nActPos, nMaxPos);
    mainwin:=TTestMain(m_callbackobj);
    mainwin.BFAFileProgressBar.Min:=0;
    mainwin.BFAFileProgressBar.Max:=nMaxPos;
    mainwin.BFAFileProgressBar.Position:=nActPos;
  end
  else begin
    // show wipe progress
    with GetWipeProgressObj do begin
      if (GetPass <> WIPE_PASS_NOLOOPS) then
        mainwin.BFAFileWipePassInfo.Caption:=IntToStr(GetPass);
      GetIntLevels(nActPos, nMaxPos);
      mainwin:=TTestMain(m_callbackobj);
      mainwin.BFAFileProgressBar.Min:=0;
      mainwin.BFAFileProgressBar.Max:=nMaxPos;
      mainwin.BFAFileProgressBar.Position:=nActPos;
    end;
  end;

  // refresh
  Application.ProcessMessages;

  // interrupted?
  if (mainwin.m_blWasBreak) then
    raise ECallBackInterrupt.Create('encryption interrupted');
end;



const
  CIPHERNAME = 'PC1';


procedure TTestMain.BFAFileEncryptBtnClick(Sender: TObject);
const
  TESTFILEBASE   : String = 'e:\test';
  TESTFILENAME   : String = 'e:\test\test.dat';
var
  sPassword : String;
  wiper     : TWipeSimple;
  bprogress : TMyBFAFileProgress;
  bfile     : TBFAFile;
  bsetup    : TBFAFileEncryptSetup;
  bresult   : TBFAFileEncryptResult;
  smem      : TKeyMemory;
  nct       : TStringList;
begin
  // get a password
  sPassword:=InputBox('Encryption Test', 'Enter a password:', '123');

  // put it into secure memory
  smem:=TKeyMemory.Create(Length(sPassword));
  smem.SetData(PChar(sPassword), 0, Length(sPassword));

  // create our wipe object
  wiper:=TWipeSimple.Create(_globals.GetRndMng.GetRandomSource);

  // create our setup object
  bsetup:=TBFAFileEncryptSetup.Create;

  nct:=TStringList.Create;
  nct.Add('zip');
  nct.Add('jpg');
  nct.Add('gif');
//  nct.Add('dat');

  with bsetup do begin
    SetPassword(smem);
    SetTargetPath('');
    SetRemoveSource(True);
    SetOverwriteExisting(True);
    SetNoCache(False);
    SetBasePath(TESTFILEBASE);
    SetRename(False);
    SetRandomRename(False);
    SetRelativePaths(True);
    SetMaskName('TEST');
    SetMaskNumber(12);
    SetMaskExt('GOO');
    SetTryRename83(True);
    SetAddExtension(True);
    SetKeepDateTime(True);
    SetKeepAttributes(False);
    SetStorePath(True);
    SetCompress(BFAFILE_COMPRESS_LZSS);
    SetSkipEncrypted(True);
    SetNoKeyHash(False);
    SetWriteProtectAfter(True);
    SetNoCompressTypes(nct);
  end;

  // create the progress callback
  bprogress:=TMyBFAFileProgress.Create(Self);

  // do the job
  bfile:=Nil;
  m_blWasBreak:=False;
  try
    bfile:=TBFAFile.Create(CIPHERNAME,
                           _globals.GetSr,
                           _globals.GetRndMng.GetRandomSource,
                           Nil,
                           0);
    bresult:=bfile.Encrypt(TESTFILENAME,
                           wiper,
                           bsetup,
                           bprogress);
    //show what we got
    with bresult do
      ShowMessage('warnings: ' + IntToHex(GetWarnings, 8) + #13#10 +
                  'original files size: ' +
                  TStrPlusI.Sepa1000(_globals.GetSr, GetOriginalFileSize) +
                  #13#10 +
                  'bytes compressed: ' +
                  TStrPlusI.Sepa1000(_globals.GetSr, GetBytesCompressed) +
                  #13#10 +
                  'name of cryptfile: ' + GetBFAFileName);

    bresult.Destroy;
    bfile.Destroy;
  except
    on bfe : EBFAFileError do begin
      if (bfile <> Nil) then
        bfile.Destroy;
      ShowMessage('ERROR OCCURED: <<<' + bfe.Message + '>>>');
    end;
  end;

  nct.Destroy;

  // clean up the rest
  bprogress.Destroy;
  bsetup.Destroy;
  wiper.Destroy;
  smem.Destroy;
end;


procedure TTestMain.BFAFileDecryptBtnClick(Sender: TObject);
var
  sPassword : String;
  sFileName : String;
  bprogress : TMyBFAFileProgress;
  bfile     : TBFAFile;
  bsetup    : TBFAFileDecryptSetup;
  bresult   : TBFAFileDecryptResult;
  smem      : TKeyMemory;
begin
  // get file to decrypt
  with OpenDialog do begin
    InitialDir:='D:\test';
    Filter:='All files (*.*)|*.*';
    Title:='Select Cryptfile';
    if (not Execute) then
      Exit;
    sFileName:=FileName;
  end;

  // get a password
  sPassword:=InputBox('Decryption Test', 'Enter the password:', 'sosolala');

  // put it into secure memory
  smem:=TKeyMemory.Create(Length(sPassword));
  smem.SetData(PChar(sPassword), 0, Length(sPassword));

  // create our setup object
  bsetup:=TBFAFileDecryptSetup.Create;
  with bsetup do begin
    SetPassword(smem);
    SetTargetPath('');
    SetRemoveSource(False);
    SetOverwriteExisting(True);
    SetNoCache(False);
    SetIgnoreCRC32(False);
    SetRestorePath(True);
    SetNoKeyCheck(False);
    SetAcceptTruncated(False);
    SetFileInfoOnly(False);
  end;

  // create the progress callback
  bprogress:=TMyBFAFileProgress.Create(Self);

  // do the job
  bfile:=Nil;
  try
    bfile:=TBFAFile.Create(CIPHERNAME,
                           _globals.GetSr,
                           _globals.GetRndMng.GetRandomSource,
                           Nil,
                           0);
    bresult:=bfile.Decrypt(sFileName,
                           bsetup,
                           bprogress);
    //show what we got
    with bresult do
      ShowMessage('warnings: ' + IntToHex(GetWarnings, 8) + #13#10 +
                  'bytes written: ' +
                  TStrPlusI.Sepa1000(_globals.GetSr, GetBytesWritten) + #13#10 +
                  'original file name: ' + GetOriginalFileName);

    bresult.Destroy;
    bfile.Destroy;
  except
    on bfe : EBFAFileError do begin
      if (bfile <> Nil) then
        bfile.Destroy;
      ShowMessage('ERROR OCCURED: <<<' + bfe.Message + '>>>');
    end;
  end;

  // clean up the rest
  bprogress.Destroy;
  bsetup.Destroy;
  smem.Destroy;
end;




procedure TTestMain.StrPlus_RootPathBtnClick(Sender: TObject);
begin
  ShowMessage(TStrPlus.RootPath('C:\'));
  ShowMessage(TStrPlus.RootPath('C:'));
  ShowMessage(TStrPlus.RootPath('\\hahn\sosola\xxxx\oops.txt'));
  ShowMessage(TStrPlus.RootPath('solo'));
  ShowMessage(TStrPlus.RootPath(''));
  ShowMessage(TStrPlus.RootPath('super\hallo'));
  ShowMessage(TStrPlus.RootPath('C:\WINNT'));
  ShowMessage(TStrPlus.RootPath('D:\HALLO!!!'));
end;

procedure TTestMain.StrPlus_IsUnicodeOSBtnClick(Sender: TObject);
begin
  if (TStrPlus.IsUnicodeOS) then
    ShowMessage('Unicode')
  else
    ShowMessage('no Unicode');
end;

procedure TTestMain.StrPlus_CompareFromBeginBtnClick(Sender: TObject);
begin
  if (TStrPlus.CompareFromBegin('oki', 'okidoki', 3)) then
    ShowMessage('equal')
  else
    ShowMessage('different');
  if (TStrPlus.CompareFromBegin('baboxx', 'baboooo', 5)) then
    ShowMessage('equal')
  else
    ShowMessage('different');
  if (TStrPlus.CompareFromBegin('baboooo', 'babo', 5)) then
    ShowMessage('equal')
  else
    ShowMessage('different');
end;




procedure TTestMain.StrPlus_Replace_BtnClick(Sender: TObject);
var
  sSource  : String;
  sReplace : String;
  sWith    : String;
begin

  sSource:=InputBox('TStrPlus.Replace', 'Enter the source string:', '');
  sReplace:=InputBox('TStrPlus.Replace', 'Replace...', '');
  sWith:=InputBox('TStrPlus.Replace', '...with', '');
  ShowMessage(TStrPlus.Replace(sSource, sReplace, sWith));
end;

procedure TTestMain.StrPlus_CalcPercentBtnClick(Sender: TObject);
begin
  ShowMessage(TStrPlusI.CalcPercent(_globals.GetSr, 50, 100, 0));
  ShowMessage(TStrPlusI.CalcPercent(_globals.GetSr, 0, 100, 1));
  ShowMessage(TStrPlusI.CalcPercent(_globals.GetSr, 100, 100, 2));
  ShowMessage(TStrPlusI.CalcPercent(_globals.GetSr, 231321, 86774657676, 4));
  ShowMessage(TStrPlusI.CalcPercent(_globals.GetSr, 33, 34, 6));
end;

procedure TTestMain.StrPlus_CompareFromEndBtnClick(Sender: TObject);
begin
  if (TStrPlus.CompareFromEnd('oki', 'okidoki', False)) then
    ShowMessage('equal')
  else
    ShowMessage('different');
  if (TStrPlus.CompareFromEnd('baboxx', 'baboooo', False)) then
    ShowMessage('equal')
  else
    ShowMessage('different');
  if (TStrPlus.CompareFromEnd('bo', 'babo', True)) then
    ShowMessage('equal')
  else
    ShowMessage('different');
  if (TStrPlus.CompareFromEnd('bO', 'babo', True)) then
    ShowMessage('equal')
  else
    ShowMessage('different');
end;

const
  PP_TESTS : array[1..8] of String = (
               'c:\windows',
               'c:\',
               '\\mallory\d\',
               '\\mallory\d\pub',
               'aaa\bbb',
               'aloa\sosolala\sss',
               'd:\x\y',
               '\\sharon\doof\kopp\munich\now');

procedure TTestMain.StrPlus_ParentPath_BtnClick(Sender: TObject);
var
  nI : Integer;
begin
  for nI:=1 to 8 do
    ShowMessage(PP_TESTS[nI] + ' --> >>>' +
                TStrPlus.ParentPath(PP_TESTS[nI]) +
                '<<<');
end;

procedure TTestMain.KeyCacheValidBtnClick(Sender: TObject);
begin
  if (_globals.GetKeyCache.IsValid) then
    ShowMessage('valid')
  else
    ShowMessage('invalid');
end;

procedure TTestMain.KeyCacheSetBtnClick(Sender: TObject);
var
  newKey : TKeyMemory;
begin
  newKey:=TKeyMemory.Create(1000);
  _globals.GetKeyCache.SetKey(newKey, 10);
end;

procedure TTestMain.ShowRandomBtnClick(Sender: TObject);
var
  buf : array[0..15] of Byte;
begin
  _globals.GetRndMng.GetRandomSource.GetBytes(@buf, 16);
  randomShow.Caption:=TStrPlus.BytesToHexStr(@buf, 16, ' ');
end;

procedure TTestMain.KeyCacheStopResumeBtnClick(Sender: TObject);
begin
  // start/stop the key cache expiration
  if (_globals.GetKeyCache.ExpirationInProgress) then
    _globals.GetKeyCache.StopTimer
  else
    _globals.GetKeyCache.ResumeTimer;
end;

procedure TTestMain.StrPlus_StrToListTestClick(Sender: TObject);
const
  NUM_OF_ENTRIES = 10;
var
  nI     : Integer;
  sBuf   : String;
  inLst  : TStringList;
  outLst : TStringList;

procedure ShowList(lst : TStringList);
var
  nIdx     : Integer;
  nUpIdx : Integer;
  sOut   : String;
begin
  sOut:='';
  nUpIdx:=lst.Count - 1;
  for nIdx:=0 to nUpIdx do begin
    sOut:=sOut + lst.Strings[nIdx];
    if (nIdx < nUpIdx) then
      sOut:=sOut + #13#10;
  end;
  ShowMessage(sOut);
end;

begin

  inLst:=TStringList.Create;
  for nI:=1 to NUM_OF_ENTRIES do
    inLst.Add('   ' + IntToStr(Random(1000000000)));
  ShowList(inLst);

  sBuf:=TStrPlus.ListToStr(inLst);
  ShowMessage(sBuf);
  inLst.Destroy;

  outLst:=TStrPLus.StrToList(sBuf);
  ShowList(outLst);
  outLst.Destroy;
end;

procedure TTestMain.CryptPak_ChrunchKeyBtnClick(Sender: TObject);
const
  KEYSIZE = 18;
var
  sPassw : String;
  sSalt : String;
  key : array[1..KEYSIZE] of WORD8;
  lBuildBufSize : WORD32;
  pBuildBuf : Pointer;
  kk : TKeyCreator;
  sm : TKeyMemory;
begin

  sPassw:=InputBox('CrunchKey', 'Password:', '123');
  sSalt:=InputBox('CrunchKey', 'Salt:', '123');

  bfacslib.Support_CrunchKey(PChar(sPassw),
                             Length(sPassw),
                             PChar(sSalt),
                             Length(sSalt),
                             @key,
                             KEYSIZE,
                             CRUNCHKEY_METHOD_SHAEXTXORLOOP);

  ShowMessage(TStrPlus.BytesToHexStr(@key, KEYSIZE, ' '));

  lBuildBufSize:=Support_GetCrunchKeyBuildBufSize(
                   Length(sPassw), Length(sSalt), KEYSIZE,
 	           CRUNCHKEY_METHOD_SHAEXTXORLOOP);

  GetMem(pBuildBuf, lBuildBufSize);

  bfacslib.Support_CrunchKey(PChar(sPassw),
                             Length(sPassw),
                             PChar(sSalt),
                             Length(sSalt),
                             @key,
                             KEYSIZE,
                             CRUNCHKEY_METHOD_SHAEXTXORLOOP,
                             pBuildBuf);

  ShowMessage(TStrPlus.BytesToHexStr(@key, KEYSIZE, ' '));

  FreeMem(pBuildBuf);

  kk:=TKeyCreator.Create(PChar(sSalt), Length(sSalt),
                         PChar(sPassw), Length(sPassw));

  sm:=kk.MakeKey(KEYSIZE);

  ShowMessage(TStrPlus.BytesToHexStr(sm.GetPtr, KEYSIZE, ' '));

  sm.Destroy;

  kk.Destroy;
end;

procedure TTestMain.CryptPak_YarrowBtnClick(Sender: TObject);
const
  BUFSIZE   = 1000;
  TESTLOOPS = 2345;
var
  nI, nJ   : Integer;
  qSummary : WORD64;
  yrw      : TYarrow;
  buf      : array[0..BUFSIZE - 1] of Byte;
begin
  yrw:=TYarrow.Create;
  nI:=0;
  qSummary:=0;
  while (nI < TESTLOOPS) do begin
    yrw.GetBytes(@buf, BUFSIZE);
    for nJ:=0 to (BUFSIZE - 1) do
      Inc(qSummary, buf[nJ]);
    Inc(nI);
  end;
  qSummary:=qSummary div (BUFSIZE *  TESTLOOPS);
  ShowMessage('Average = ' + IntToStr(qSummary) + ' (= 127?)');
  yrw.Destroy;
end;

procedure TTestMain.ClusterAdjustBtnClick(Sender: TObject);
var
  ca : TFileClusterAdjuster;
  qFileSize : WORD64;
  qAdjustedFileSize : WORD64;
  sPath : String;
begin

  sPath:=InputBox('Cluster Adjust', 'Enter file name:', '');

  if (0 = Length(sPath)) then Exit;
  ca:=Nil;
  try

    qFileSize:=TFileSupport.GetFile64Len(sPath, _globals.GetSr);

    ca:=TFileClusterAdjuster.Create;

    if (ca.ClusterAdjust(sPath, qFileSize, qAdjustedFileSize, True)) then begin
      ShowMessage('success' + #13#10 + IntToStr(qFileSize) + ' ->' + #13#10 +
        IntToStr(qAdjustedFileSize));
    end
    else begin
      ShowMessage('FAILED, ' + IntToStr(qFileSize) + '=' + IntToStr(qAdjustedFileSize));
    end;
  except
    on e : Exception do begin
      ShowMessage('ERROR: ' + e.Message);
    end;
  end;

  if (ca <> Nil) then ca.Destroy;
end;

end.






