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
  a really complex file browser, chunky style and dixy fried
}


unit FileBrowser;

interface
uses ComCtrls,
     Classes,
     Dialogs,
     Windows,
     ActiveX,
     Controls,
     ShellApi,
     ShlObj,
     CallBack,
     bfacslib,
     SysUtils,
     StringRes,
     Configuration,
     MessageCallBack,
     CipherManager,
     BFAFile,
     SecureMem,
     KeyCache,
     History,
     PasswordWin;


// icon index look up
type
  TIconIndexLookup = class
  public
    // members
    m_nNumOfLookups : Integer;
    m_lookupTable   : array of String;
    m_indexes       : array of Integer;
    m_typeInfos     : array of String;

  public

    // constructor
    constructor Create;

    // destructor
    destructor Destroy; override;

    // gets the icon index of a file
    // -> path+name of the file
    // -> where to return the type information
    // -> True: don't use lookup table / False: vice versa
    // <- index of the icon in the system image list (-1 if not found)
    function GetIndex(const sFile : String;
                      var vsTypeInfo : String;
                      blNoCacheLookup : Boolean = False) : Integer;

    // gets the icon index of a special file type, this won't work for
    // individual iconized files like .EXE, .ICO and so on
    // -> the extension of the file
    // -> where to return the type information
    // <- index of the icon in the system image list
    function GetIndexByExtension(sExt : String;
                                 var vsTypeInfo : String) : Integer;

    // clears the lookup table
    procedure ClearTable;

    // checks if a file has an individual icon
    // -> the file name to check
    // <- True: individual / False: not (icon index is cachable)
    class function HasIndividualIcon(const sFile : String) : Boolean;

  end;


// the configuration section ID
const
   FB_CFG_ID = 'FILEBROWSER';


// configuration IDs (some are also used for string resource loading)
const
   FB_CFGID_COLNAME          = 'ColName';
   FB_CFGID_COLREALNAME      = 'ColRealName';
   FB_CFGID_COLSIZE          = 'ColSize';
   FB_CFGID_COLREALSIZE      = 'ColRealSize';
   FB_CFGID_COLTIME          = 'ColTime';
   FB_CFGID_COLATTR          = 'ColAttr';
   FB_CFGID_COLTYPE          = 'ColType';
   FB_CFGID_VIEWSTYLE        = 'ViewStyle';
   FB_CFGID_SORT             = 'Sort';
   FB_CFGID_HOTTRACKING      = 'HotTracking';
   FB_CFGID_HTMLSTYLE        = 'HTMLStyle';
   FB_CFGID_GRIDLINES        = 'GridLines';
   FB_CFGID_AUTOARRANGE      = 'AutoArrange';
   FB_CFGID_FLATVIEW         = 'FlatView';
   FB_CFGID_DRIVEPATH_PREFIX = 'DrivePath_';
   FB_CFGID_FONTCOLOR        = 'FontColor';
   FB_CFGID_FONTNAME         = 'FontName';
   FB_CFGID_FONTSIZE         = 'FontSize';
   FB_CFGID_FONTSTYLE        = 'FontStyle';
   FB_CFGID_SCANBFAFILESONLY = 'ScanBFAFilesOnly';
   FB_CFGID_REPLACESCANICONS = 'ReplaceScanIcons';
   FB_CFGID_AUTOREFRESH      = 'AutoRefresh';
   FB_CFGID_PLACEDRIVESFIRST = 'PlaceDrivesFirst';


// the item identifiers (bit flags)
const
  FILEBROWSER_ITEMIS_FILE        = 1;
  FILEBROWSER_ITEMIS_FOLDER      = 2;
  FILEBROWSER_ITEMIS_DRIVE       = 4;
  FILEBROWSER_ITEMIS_NETWORKROOT = 8;
  FILEBROWSER_ITEMIS_NETWORKUNIT = 16;
  FILEBROWSER_ITEMIS_UPONELEVEL  = 32;
  FILEBROWSER_ITEMIS_SCANNED     = $01000; // (set by the scanner only...)
  FILEBROWSER_ITEMIS_ENCRYPTED   = $02000;

// sort codes (bit mask)
const
  FILEBROWSER_SORT_UNSORTED    = 1;
  FILEBROWSER_SORT_FILE        = 2;
  FILEBROWSER_SORT_REALNAME    = 4;
  FILEBROWSER_SORT_SIZE        = 8;
  FILEBROWSER_SORT_REALSIZE    = 16;
  FILEBROWSER_SORT_TIME        = 32;
  FILEBROWSER_SORT_ATTR        = 64;
  FILEBROWSER_SORT_TYPE        = 128;
  FILEBROWSER_SORT_UPWARDS     = $010000; // the sort direction bit
  // (just for runtime purposes)
  FILEBROWSER_SORTEX_DRIVESFIRST = $10000000; // place drives first


// check action codes (bit mask)
const
  FILEBROWSER_SELECTED_SINGLEFILE   = 1;
  FILEBROWSER_SELECTED_FILES        = 2;
  FILEBROWSER_SELECTED_SINGLEFOLDER = 4;
  FILEBROWSER_SELECTED_FOLDERS      = 8;
  FILEBROWSER_SELECTED_SINGLEDRIVE  = 16;
  FILEBROWSER_SELECTED_DRIVES       = 32;
  FILEBROWSER_SELECTED_NOTREADONLY  = 64;
  FILEBROWSER_SELECTED_FORMATTABLE  = 128;


// double click results
const
  FILEBROWSER_DBLCLICK_EXECUTED    = 0;
  FILEBROWSER_DBLCLICK_SKIPPED     = 1;
  FILEBROWSER_DBLCLICK_CHANGEDPATH = 2;
  FILEBROWSER_DBLCLICK_BFAREQUEST  = 3;
  FILEBROWSER_DBLCLICK_BFJREQUEST  = 4;
  FILEBROWSER_DBLCLICK_NOP         = 5;

// drag + drop work modes
const
  FILEBROWSER_DRAG_INVALID = 0;
  FILEBROWSER_DRAG_COPY    = 1;
  FILEBROWSER_DRAG_MOVE    = 2;




// we need information for every list entry which is stored in a structure,
// to improve the speed there's no class type used right here
type
  PFileBrowserItemInfo = ^TFileBrowserItemInfo;
  TFileBrowserItemInfo = packed record
    qFileSize     : WORD64;  // size of the file (if it's one)
    qRealFileSize : WORD64;  // real size of the file (if encrypted)
    qFileTime     : WORD64;  // last write time of the file/folder
    lItemIs       : WORD32;  // kind of item, see FILEBROWSER_ITEMIS_xxx
    lAttrTypePIDL : WORD32;  // attributes of the file/folder or drive type
                             // or PIDL of the network unit/root
    lReserved0    : WORD32;  // reserved (used for refreshing checks)
  end;



// request callback for getting a new file name
type
  TFileBrowserRenameRequest = class
  public
    // callback routine
    // -> the old filename
    // -> where to put the new file name
    // <- True: rename action confirmed / False: cancel request
    function Request(sOld : String;
                     var vsNew : String) : Boolean; virtual; abstract;
  end;


// special setup to enable BFA handling
type
  TFileBrowserBFASupport = class
  private
    m_cipherManager : TCipherManager;
    m_passwInput    : TPasswordInput;

  public
    // constructor
    // -> cipher manager
    // -> BFA viewer
    // -> password input
    constructor Create(cipherManager  : TCipherManager;
                       passwInput : TPasswordInput);

    // gets the cipher manager
    // <- cipher manager
    function GetCipherManager : TCipherManager;

    // gets the password input
    // <- password input
    function GetPasswordInput : TPasswordInput;
  end;


// file browser result errors
type
  EFileBrowserError     = class(Exception);
  EFileBrowserWarning   = class(EFileBrowserError);
  EFileBrowserInterrupt = class(EFileBrowserError);


// our own special callback codes
const
  FILEBROWSER_CALLBACK_NOBREAK = CALLBACK_SIGNAL_CUSTOM;

// we cannot go deeper
const
  MAX_PATH_DEPTH = 32000;


// the file browser
type
  TBFAScanner = class;
  TFileChangesNotifier = class;
  TFileBrowser = class
  protected
    // members
    m_listView          : TListView;
    m_sr                : TStrRes;
    m_msgCB             : TMessageCallBack;
    m_callBack          : TCallBack;
    m_config            : TConfigurationSection;

    m_nNumOfFiles       : Integer;
    m_qNumOfBytes       : WORD64;
    m_nNumOfFolders     : Integer;
    m_nNumOfSelDrives   : Integer;
    m_nNumOfSelDirs     : Integer;
    m_nNumOfSelFiles    : Integer;
    m_nNumOfSelOther    : Integer;
    m_qNumOfSelBytes    : WORD64;
    m_sContentInfo      : String;

    m_iconLookup        : TIconIndexLookup;

    m_smallImages       : TImageList;
    m_largeImages       : TImageList;

    m_sPath             : String;
    m_sSelection        : String;
    m_sLastDrivePath    : String;
    m_blReadOnlyDrive   : Boolean;

    m_blPureNetWork     : Boolean;
    m_shellMalloc       : ActiveX.IMalloc;
    m_blShellNetworkOK  : Boolean;
    m_networkPIDLs      : array[0..MAX_PATH_DEPTH] of PItemIDList;
    m_nNetworkDepth     : Integer;  // zero equals "no network view"

    m_nDragDropMode     : Integer;

    m_nExclAttrMask     : Integer;

    m_scanner           : TBFAScanner;
    m_saveScanner       : TBFAScanner;
    m_scanKey           : TKeyMemory;
    m_blScannerReady    : Boolean;

    m_bfaSupport        : TFileBrowserBFASupport;

    m_changesNotifier   : TFileChangesNotifier;
    m_blHandleSelection : Boolean;

    m_hist              : THistory;

  protected

    // (re)labels the cokuns, ading the right sort sign
    procedure ColumnsRelabel;

    // update the content info string
    procedure UpdateContentInfo;

    // clears all selection keepers
    procedure NoSelect;

    // adds an item info
    // <- pointer to the new item info
    function AddItemInfo : PFileBrowserItemInfo;

    // clears all items infos
    procedure ClearItemInfos;

    // to remove an item (plus its info)
    // -> index of the item to remove
    procedure RemoveItem(nIdx : Integer);

    // create an absolute PIDL out of the existing network stack (+ pidl)
    // -> the given PDIL (ignored if Nil)
    // <- the absolute PIDL
    function MakeAbsolutePIDL(pidl : PItemIDList = Nil) : PItemIDList;

    // to check a drop target
    // -> coordinates of the drop target (if any)
    // <- the target item (Nil equals "no valid target")
    function GetDropTarget(nX, nY : Integer) : TListItem;

    // internal file selection
    // -> select. code
    // -> item for selection by string
    // -> True: case sensitive comparison / False: cs. insens.
    procedure Selector(nSelCode : Integer;
                       sSubStr : String = '';
                       blCaseSens : Boolean = False);

    // controls the auto refresh feature
    // -> True: auto refresh on (also called to set a new path) / False: off
    procedure AutoRefresh(blActive : Boolean = True);

  public
    // constructor
    // -> the host list view
    // -> string resources
    // -> where to save and load the settings
    // -> message callback for user requests
    // -> callback for sending messages during different processes
    // -> BFA support (will be taken over and destroyed later)
    constructor Create(listView : TListView;
                       sr : TStrRes;
                       cfg : TConfiguration;
                       msgCB : TMessageCallBack;
                       callBack : TCallBack = Nil;
                       bfaSupport : TFileBrowserBFASupport = Nil);

    // destructor
    destructor Destroy; override;

    // fixes the settings (of the associated list view)
    // -> True: store the column sttings only / False: store all
    procedure FixSettings(blColSetsOnly : Boolean = False);

    // browses to the default path
    // exception: EFileBrowserError error, detailed message was stored
    // exception: EFileBrowserWarning if a minor conflict occured
    // exception: EFileBrowserInterrupt user break detected
    procedure JoinDefaultPath;

    // sets the exclude attribute mask
    // -> the exclude mask
    procedure SetExclAttrMask(nExclAttrMask : Integer);

    // sets a new path
    // -> the new path
    // -> True : update the associated history / False: don't touch anything
    // exception: EFileBrowserError error, detailed message was stored
    // exception: EFileBrowserWarning if a minor conflict occured
    // exception: EFileBrowserInterrupt user break detected
    procedure ChangePath(sPath : String;
                         blUpdateHistory : Boolean = True);

    // refreshes the current path
    // -> True : update the associated history / False: don't touch anything
    // exception: EFileBrowserError error, detailed message was stored
    // exception: EFileBrowserWarning if a minor conflict occured
    // exception: EFileBrowserInterrupt user break detected
    procedure Refresh(blUpdateHistory : Boolean = True);

    // resets the layout of the associated listview
    procedure Layout;

    // (re)sets the current style with all necessary changes
    procedure ChangeViewStyle;

    // gets the current path of the browser
    // -> True: deliver with the current file selection / False: no select.
    // <- current path (empty if we're in a pure network environment)
    function GetCurrentPath(blAll : Boolean = True) : String;

    // handles a double click
    // <- result code (see FILEBROWSER_DBLCLICK_xxx)
    // exception: EFileBrowserError error, detailed message was stored
    // exception: EFileBrowserWarning if a minor conflict occured
    // exception: EFileBrowserInterrupt user break detected
    function HandleDoubleClick : Integer;

    // checks if we can go up one level
    // <- True: we can go up / False: guess...
    function CanGoUp : Boolean;

    // goes up one level (does nothing if we cannot go up)
    // exception: EFileBrowserError error, detailed message was stored
    // exception: EFileBrowserWarning if a minor conflict occured
    // exception: EFileBrowserInterrupt user break detected
    procedure UpOneLevel;

    // gets the number of files in the current path
    // <- number of files
    function GetNumOfFiles : Integer;

    // gets the number of folders in the current path
    // <- number of folders
    function GetNumOfFolders : Integer;

    // gets the number of selected objects in the current path
    // <- number of selected objects (files, directories, drives)
    function GetNumOfSelObjs : Integer;

    // gets the number of selected files
    // <- number of selected files
    function GetNumOfSelFiles : Integer;

    // gets the number of selected directories
    // <- number of selected directories
    function GetNumOfSelDirs : Integer;

    // gets the number of selected drives
    // <- number of selected drives
    function GetNumOfSelDrives : Integer;

    // gets the number of selected other items
    // <- number of selected
    function GetNumOfSelOther : Integer;

    // gets the number of bytes of all selected files
    // <- number of bytes of selected files
    function GetNumOfSelBytes : WORD64;

    // make an info message which shows the current path state
    // <- path content info
    function GetContentInfo : String;

    // make an info message which shows the current selection state
    // <- selection state
    function GetSelectedInfo : String;

    // make an info message which shows the numbers of files and folders
    // <- numbers string representation
    function GetCountInfo : String;

    // handles a selection event
    // -> the item that was (de)selected
    // -> the (selected) state of that item
    procedure HandleSelection(item: TListItem;
                              blState : Boolean);

    // handles a click on a column
    procedure HandleColumnClick(col : TListColumn);

    // selects all files and folders
    procedure SelectAll;

    // selects files
    procedure SelectFiles;

    // selects folders
    procedure SelectFolders;

    // selects encrypted files
    procedure SelectEncrypted;

    // selects decrypted files
    procedure SelectDecrypted;

    // selects files containing a given substring
    // -> the substring
    // -> True: case sensitive comparison / False: cs. insens.
    procedure SelectByString(sCont : String;
                             blCaseSens : Boolean = False);

    // copies current select files to a given destination
    // -> destination path
    // -> message callback for confirmations
    // exception: EFileBrowserError error occured
    // exception: EFileBrowserWarning if not all objects could be copied
    // exception: EFileBrowserInterrupt user break detected
    procedure CopyObjects(sDestPath : String;
                          msgCB : TMessageCallBack);

    // moves current select files and folders to a given destination  (stops a
    // running scanner)
    // -> destination path
    // -> message callback for confirmations
    // exception: EFileBrowserError error occured
    // exception: EFileBrowserWarning if not all objects could be moved
    // exception: EFileBrowserInterrupt user break detected
    procedure MoveObjects(sDestPath : String;
                          msgCB : TMessageCallBack);

    // deletes current select files and folders
    // exception: EFileBrowserError error occured
    // exception: EFileBrowserWarning if not all objects could be deleted
    // exception: EFileBrowserInterrupt user break detected
    procedure DeleteObjects;

    // handles a drag start
    // -> work mode, see FILEBROWSER_DRAG_xxx
    procedure HandleDragStart(nMode : Integer);

    // handles drag over events
    // -> current cursor position
    // <- True: drop ok / False: drop not ok
    function HandleDragOver(nX, nY : Integer) : Boolean;

    // handles a drag+drop event
    // -> final cursor position
    // -> message callback for confirmations
    procedure HandleDragDrop(nX, nY : Integer;
                             msgCB : TMessageCallBack);

    // gets the currently selected object (file or folder)
    // -> True: get the object and its path / False: no path
    // <- the selected file
    // exception: EFileBrowserError not only one or no object was selected
    function GetSelectedObject(blWithPath : Boolean = False) : String;

    // check if the current path is on a read-only drive
    // <- True: read-only drive (CD-ROM) / False: write access enabled(?)
    function IsReadOnlyDrive : Boolean;

    // renames the currently selected files and/or folders (stops a running
    // scanner)
    // -> rename request
    // -> message callback for error reporting
    procedure RenameObjects(renReq : TFileBrowserRenameRequest;
                            errCB : TMessageCallBack);

    // creates new folders (may contain multiple subfolders, stops a running
    // scanner)
    // -> names of the new folders, will be destroyed later
    // exception: EFileBrowserError folder(s) couldn't be created
    // exception: EFileBrowserWarning attributes couldn't be set
    procedure NewFolders(names : TStringList);

    // check a selection
    // -> demand mask, see FILEBROWSER_SELECTED_xxx
    // <- True: check passed / False: something is wrong
    function CheckSelection(nType : Integer) : Boolean;

    // starts the BFA scanner
    // exception: EFileBrowserError scanner couldn't be started
    // exception: EFileBrowserInterrupted user canceled the key input
    procedure StartBFAScanner;

    // stops the BFA scanner
    procedure StopBFAScanner;

    // resumes the BFA scanner
    // exception: EFileBrowserError scanner couldn't be started
    procedure ResumeBFAScanner;

    // suspends the BFA scanner
    procedure SuspendBFAScanner;

    // checks if a valid scanner is available
    // <- True: scanner is there / False: no scanner
    function IsBFAScannerReady : Boolean;

    // gets selected files and folders
    // -> where to add the selected files
    // -> where to add the selected folders
    procedure GetSelectedFilesAndFolders(files : TStringList;
                                         folders : TStringList);

    // gets a single selected file
    // <- single selected file (empty if not a single file is selected)
    function GetSingleSelectedFile : String;

    // to turn on/off the auto refresh from outside
    // -> True: on /False: off
    procedure DoAutoRefresh(blState : Boolean);

    // to detect a runnign auto refresh
    // <- True: active / False: not active
    function IsAutoRefreshActive : Boolean;

    // updates the current path view, which means removing files and folders
    // no longer existing and to add new items (mainly called by a
    // TFileChangesNotifier thread when changes were detected)
    // -> True: (re)sort afterwards / False: don't even think about it
    procedure Update(blSort : Boolean = False);

    // gets the best looking insert positions for new files and folders
    // <- True: we want to insert a new folder / False: just the default
    function GetInsertPos(blBestForDir : Boolean = False) : Integer;

    // to check if the current path's readonly
    // <- True: readonly / False: not
    function IsReadOnlyPath : Boolean;

    // to choose a new font
    // -> font dialog
    procedure ChooseFont(fntDlg : TFontDialog);

    // gets the last valid path changd to
    // <- last valid path (may be empty)
    function GetLastValidPath : String;

    // (re)sets the columns
    procedure MakeColumns;

    // gets the history
    // <- the history (ref. only)
    function GetHistory : THistory;

    // gets the associated list view
    function GetListView : TListView;

    // shows the format dialog (if a single drive was selected)
    function FormatDrive : Boolean;

  public
    // <- True: formatting is supported / False: not
    class function FormatSupported : Boolean;

  end;



// exception for the BFA scanner
  EBFAScannerError = class(Exception);

// BFA scanner thread (only used internally)
  TBFAScanner = class(TThread)
  private
    // members
    m_fbrowser  : TFileBrowser;
    m_blRunning : Boolean;
    m_blBFAOnly : Boolean;
    m_blReplIco : Boolean;
    m_critSect  : TRTLCriticalSection;
    m_bfaFile   : TBFAFile;
    m_scanKey   : TKeyMemory;
    m_sr        : TStrRes;

    // synchronized stuff (FIXME: do we really need this?)
    m_qRealFileSize : WORD64;
    m_lItemIs       : WORD32;
    m_nItemsCount   : Integer;
    m_nActIndex     : Integer;
    m_nImageIndex   : Integer;
    m_blResult      : Boolean;
    m_sRealFileName : String;
    m_sRealType     : String;
    m_sCaption      : String;
    procedure GetCountOfItems; // <- m_nItemsCount
    procedure SetFileItem; // -> m_nActIndex,
                           // -> m_sRealFileName,
                           // -> m_sRealFileSize
                           // -> m_lItemIs
    procedure GetFileItem; // -> m_nActIndex,
                           // -> m_sCaption
                           // -> m_lItemIs
                           // <- m_blResult

  public
    // constructor
    // -> the associated file browser
    // -> _reference_ to the decryption key
    // -> the cipher's name to use for scanning
    // -> True: scan only .BFA files / False: scan all
    // -> True: replace icons of scanned files / False: not
    // -> string resources
    // exception: EBFAScannerError if any init. error occured
    constructor Create(fbrowser : TFileBrowser;
                       scanKey : TKeyMemory;
                       sCipherName : String;
                       blScanBFAFilesOnly : Boolean;
                       blReplaceIcons : Boolean;
                       sr : TStrRes);  reintroduce; overload;

    // destructor
    destructor Destroy; override;

    // the scanner method
    procedure Execute; override;

    // acccess the running flag thread safe
    // -> True: read action / False: write action
    // -> new value (if blRead equals False, the default value
    //    is only used to allow easy calls to read out the flag)
    // <- the current value of the flag
    function AccessRunningFlag(blRead : Boolean = True;
                               blNewValue : Boolean = False) : Boolean;
  end;


// special thread to detect changes in the current path for auto refreshing,
// this guy is only used internally
  TFileChangesNotifier = class(TThread)
  private
    // members
    m_fbrowser : TFileBrowser;
    m_notifies : TWOHandleArray;

    // allows synchronized access to the file browser (this is absolutely
    // necessary right here!)
    procedure RefreshBrowser;

  public
    // constructor
    constructor Create(fbrowser : TFileBrowser); reintroduce; overload;

    // destructor
    destructor Destroy; override;

    // the thread proc
    procedure Execute; override;

    // shuts down the thread, waits for its termination
    procedure Shutdown;

  end;



implementation
uses FileCtrl,
     Graphics,
     Forms,
     StringPlus,
     StringPlusI,
     General,
     FileSupp,
     BFJob,
     PathSearch;


//////////////////////// TIconIndexLookup ////////////////////////


// increment of the lookup table
const
  LOOKUP_INC = 128;


constructor TIconIndexLookup.Create;
begin
  m_nNumOfLookups:=0;
  SetLength(m_lookupTable, LOOKUP_INC);
  SetLength(m_indexes, LOOKUP_INC);
  SetLength(m_typeInfos, LOOKUP_INC);
end;


destructor TIconIndexLookup.Destroy;
begin
  Finalize(m_typeInfos);
  Finalize(m_indexes);
  Finalize(m_lookupTable);
end;


function TIconIndexLookup.GetIndex(const sFile : String;
                                   var vsTypeInfo : String;
                                   blNoCacheLookup : Boolean = False) : Integer;
var
  fileInfo : TSHFileInfo;
begin
  // allowed to use the cache?
  if (not blNoCacheLookup) then begin
    if (HasIndividualIcon(sFile)) then begin
      Result:=GetIndexByExtension(TStrPlus.ExtractFileExtension(sFile),
                                  vsTypeInfo);
      Exit;
    end;
  end;

  // get the infos "manually"
  SHGetFileInfo(PChar(sFile),
                0,
                fileInfo,
                SizeOf(fileInfo),
                SHGFI_ICON or SHGFI_TYPENAME);
  vsTypeInfo:=fileInfo.szTypeName;
  Result:=fileInfo.iIcon;
end;


function TIconIndexLookup.GetIndexByExtension(sExt : String;
                                              var vsTypeInfo : String)
                                                : Integer;
var
  nI       : Integer;
  nLen     : Integer;
  fileInfo : TSHFileInfo;
begin
  // do we have the type already in the lookup table?
  sExt:=AnsiUpperCase(sExt);
  nI:=0;
  while (nI < m_nNumOfLookups) do begin
    if (m_lookupTable[nI] = sExt) then
      Break;
    Inc(nI);
  end;

  // found something?
  if (nI < m_nNumOfLookups) then begin
    vsTypeInfo:=m_typeInfos[nI];
    Result:=m_indexes[nI];
    Exit;
  end;

  // must we enlarge the lookup table?
  nLen:=Length(m_indexes);
  if (m_nNumOfLookUps = nLen) then begin
    SetLength(m_lookupTable, nLen + LOOKUP_INC);
    SetLength(m_indexes, nLen + LOOKUP_INC);
    SetLength(m_typeInfos, nLen + LOOKUP_INC);
  end;

  // get type and information from the shell and create a new table entry
  SHGetFileInfo(PChar('*.' + sExt),
                0,
                fileInfo,
                SizeOf(fileInfo),
                SHGFI_ICON or SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES or
                SHGFI_TYPENAME);
  m_lookupTable[m_nNumOfLookups]:=sExt;
  m_indexes[m_nNumOfLookups]:=fileInfo.iIcon;
  m_typeInfos[m_nNumOfLookups]:=fileInfo.szTypeName;
  Inc(m_nNumOfLookups);

  // return icon index and type information
  vsTypeInfo:=fileInfo.szTypeName;
  Result:=fileInfo.iIcon;
end;


procedure TIconIndexLookup.ClearTable;
begin
  // reset
  SetLength(m_lookupTable, 0);
  SetLength(m_indexes, 0);
  SetLength(m_typeInfos, 0);
  m_nNumOfLookups:=0;
end;



class function TIconIndexLookup.HasIndividualIcon(const sFile : String)
                                                    : Boolean;
var
  sExt : String;
begin
  sExt:=AnsiUpperCase(TStrPlus.ExtractFileExtension(sFile));
  Result:=((sExt <> 'EXE') and
           (sExt <> 'ICO') and
           (sExt <> 'CUR') and
           (sExt <> 'LNK'))
end;


//////////////////////// TFileBrowserBFASupport ////////////////////////


constructor TFileBrowserBFASupport.Create(cipherManager  : TCipherManager;
                                          passwInput : TPasswordInput);
begin
  m_cipherManager:=cipherManager;
  m_passwInput:=passwInput;
end;

function TFileBrowserBFASupport.GetCipherManager : TCipherManager;
begin
  Result:=m_cipherManager;
end;

function TFileBrowserBFASupport.GetPasswordInput : TPasswordInput;
begin
  Result:=m_passwInput;
end;



//////////////////////// TFileBrowser ////////////////////////




// column IDs for setting subitems (subtract 1 each to get the subitem index)
const
  COL_FILENAME = 0;
  COL_REALNAME = 1;
  COL_SIZE     = 2;
  COL_REALSIZE = 3;
  COL_TIME     = 4;
  COL_ATTR     = 5;
  COL_TYPE     = 6;



// the configuration checker
type
  TFileBrowserCC = class(TConfigurationChecker)
  public
    procedure RunCheck(section : TConfigurationSection); override;
  end;

procedure TFileBrowserCC.RunCheck(section : TConfigurationSection);
var
  cDrive : Char;
begin
  // work through all settings
  CheckInt(section, FB_CFGID_COLNAME, 150);
  CheckInt(section, FB_CFGID_COLREALNAME, 150);
  CheckInt(section, FB_CFGID_COLSIZE, 80);
  CheckInt(section, FB_CFGID_COLREALSIZE, 80);
  CheckInt(section, FB_CFGID_COLTIME, 110);
  CheckInt(section, FB_CFGID_COLATTR, 60);
  CheckInt(section, FB_CFGID_COLTYPE, 120);
  CheckInt(section, FB_CFGID_VIEWSTYLE, Integer(vsList));
  CheckInt(section, FB_CFGID_SORT, FILEBROWSER_SORT_FILE);
  CheckBool(section, FB_CFGID_HOTTRACKING, False);
  CheckBool(section, FB_CFGID_HTMLSTYLE, False);
  CheckBool(section, FB_CFGID_GRIDLINES, True);
  CheckBool(section, FB_CFGID_AUTOARRANGE, True);
  CheckBool(section, FB_CFGID_FLATVIEW, False);
  CheckInt(section, FB_CFGID_FONTCOLOR , Integer(clWindowText));
  CheckString(section, FB_CFGID_FONTNAME , 'MS Sans Serif');
  CheckInt(section, FB_CFGID_FONTSIZE , 8);
  CheckInt(section, FB_CFGID_FONTSTYLE , 0);
  CheckBool(section, FB_CFGID_REPLACESCANICONS, False);
  CheckBool(section, FB_CFGID_AUTOREFRESH, True);
  CheckBool(section, FB_CFGID_SCANBFAFILESONLY, True);
  CheckBool(section, FB_CFGID_PLACEDRIVESFIRST, False);

  for cDrive:='A' to 'Z' do
    CheckString(section, FB_CFGID_DRIVEPATH_PREFIX + cDrive, '');
end;


procedure TFileBrowser.NoSelect;
begin
  m_listView.Selected:=Nil;
  m_nNumOfSelFiles:=0;
  m_nNumOfSelDirs:=0;
  m_nNumOfSelDrives:=0;
  m_nNumOfSelOther:=0;
  m_qNumOfSelBytes:=0;
end;


procedure TFileBrowser.MakeColumns;

function AddColumn(const sID : String) : TListColumn;
begin
  Result:=m_listView.Columns.Add;
  Result.Caption:=m_sr.Get(FB_CFG_ID, sID);
  Result.Width:=m_config.GetIntegerOption(sID);
end;


begin
  // store current columns widths, if possible
  if (m_listView.Columns.Count > 0) then
    FixSettings(True);

  // create all columns
//  m_listView.Columns.BeginUpdate;
  m_listView.Columns.Clear;
  AddColumn(FB_CFGID_COLNAME);
  AddColumn(FB_CFGID_COLREALNAME);
  AddColumn(FB_CFGID_COLSIZE).Alignment:=taRightJustify;
  AddColumn(FB_CFGID_COLREALSIZE).Alignment:=taRightJustify;
  AddColumn(FB_CFGID_COLTIME);
  AddColumn(FB_CFGID_COLATTR);
  AddColumn(FB_CFGID_COLTYPE);
  ColumnsRelabel;
//  m_listView.Columns.EndUpdate;
//  Update;
end;



function TFileBrowser.AddItemInfo : PFileBrowserItemInfo;
begin
  // allocate an item info
  GetMem(Result, SizeOf(TFileBrowserItemInfo));
end;


procedure TFileBrowser.ClearItemInfos;
var
  nI     : Integer;
  nUpIdx : Integer;
begin
  nUpIdx:=m_listView.Items.Count - 1;
  for nI:=0 to nUpIdx do
    FreeMem(Pointer(m_listView.Items[nI].Data));
end;

procedure TFileBrowser.RemoveItem(nIdx : Integer);
var
  blSaveHS  : Boolean;
  delItem   : TListItem;
  pItemInfo : PFileBrowserItemInfo;
begin
  blSaveHS:=m_blHandleSelection;
  m_blHandleSelection:=False;
  delItem:=m_listView.Items[nIdx];
  pItemInfo:=PFileBrowserItemInfo(delItem.Data);

  if ((pItemInfo^.lItemIs and FILEBROWSER_ITEMIS_FILE) <> 0) then begin
    Dec(m_nNumOfFiles);
    Dec(m_qNumOfBytes, pItemInfo^.qFileSize);
    if (delItem.Selected) then begin
      Dec(m_nNumOfSelFiles);
      Dec(m_qNumOfSelBytes, pItemInfo^.qFileSize);
    end;
  end
  else begin
    Dec(m_nNumOfFolders);
     if (delItem.Selected) then
       Dec(m_nNumOfSelDirs);
  end;

  FreeMem(Pointer(pItemInfo));
  m_listView.Items.Delete(nIdx);
  m_blHandleSelection:=blSaveHS;
end;


constructor TFileBrowser.Create(listView : TListView;
                                sr : TStrRes;
                                cfg : TConfiguration;
                                msgCB : TMessageCallBack;
                                callBack : TCallBack = Nil;
                                bfaSupport : TFileBrowserBFASupport = Nil);
var
  cc    : TFileBrowserCC;
  dummy : TSHFileInfo;
begin
  // copy the necessary external references
  m_listView:=listView;
  m_sr:=sr;
  m_msgCB:=msgCB;
  m_callBack:=callBack;
  m_bfaSupport:=bfaSupport;

  // reset path and selection
  m_sPath:='';
  m_sSelection:='*.*';
  m_sLastDrivePath:='';

  // init. all content descriptors
  m_nNumOfFiles:=0;
  m_nNumOfFolders:=0;
  NoSelect;
  m_sContentInfo:='';
  m_nExclAttrMask:=TFileSupport.MakeExcludeAttrMask(False, False, False, False);

  // create the history
  m_hist:=THistory.Create;

  // load and check the configuration
  cc:=TFileBrowserCC.Create;
  m_config:= cfg.GetSection(FB_CFG_ID, cc);
  cc.Destroy;

  // make our icon index (and file type description) lookup
  m_iconLookup:=TIconIndexLookup.Create;

  // init. system image lists
  m_smallImages:=TImageList.Create(m_listView);
  m_smallImages.ShareImages:=True;
  m_smallImages.Handle:=SHGetFileInfo('',
                                      0,
                                      dummy,
                                      SizeOf(dummy),
                                      SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  m_listView.SmallImages:=m_smallImages;
  m_largeImages:=TImageList.Create(m_listView);
  m_largeImages.ShareImages:=True;
  m_largeImages.Handle:=SHGetFileInfo('',
                                      0,
                                      dummy,
                                      SizeOf(dummy),
                                      SHGFI_SYSICONINDEX or SHGFI_ICON);
  m_listView.LargeImages:=m_largeImages;

  // get all necessary stuff for accessing network objects via the shell
  m_blShellNetworkOK:=(SHGetMalloc(m_shellMalloc) <> NOERROR);

  // no active scanner thread (currently) and scanner not ready yet
  m_scanner:=Nil;
  m_saveScanner:=Nil;
  m_blScannerReady:=False;
  m_scanKey:=Nil;

  // no auto refresh enabled yet
  m_changesNotifier:=Nil;

  // selection handling enabled
  m_blHandleSelection:=True;

  // make the right columns
  MakeColumns;

end;



destructor TFileBrowser.Destroy;
begin
  // clean up
  StopBFAScanner;
  AutoRefresh(False);
  if (m_saveScanner <> Nil) then
    m_saveScanner.Destroy;
  if (m_scanKey <> Nil) then
    m_scanKey.Destroy;
  if (m_bfaSupport <> Nil) then
    m_bfaSupport.Destroy;
(*
  if (m_blShellNetworkOK) then
    m_shellMalloc.Release;
*)
  m_listView.LargeImages:=Nil;
  m_listView.SmallImages:=Nil;
  m_largeImages.Destroy;
  m_smallImages.Destroy;
  m_hist.Destroy;
  ClearItemInfos;
  m_iconLookup.Destroy;
end;


procedure TFileBrowser.FixSettings(blColSetsOnly : Boolean = False);
var
  cDrive : Char;
begin
  // store columns widths (if any out there)
  if (m_listView.Columns.Count > 5) then begin
    // store the first column's width, if possible
    if (m_listView.ViewStyle = vsReport) then
      m_config.FixIntegerOption(FB_CFGID_COLNAME, m_listview.Columns[0].Width);

    // store standard listview columns widths
    m_config.FixIntegerOption(FB_CFGID_COLREALNAME,
                              m_listview.Columns[COL_REALNAME].Width);
    m_config.FixIntegerOption(FB_CFGID_COLSIZE,
                              m_listview.Columns[COL_SIZE].Width);
    m_config.FixIntegerOption(FB_CFGID_COLREALSIZE,
                              m_listview.Columns[COL_REALSIZE].Width);
    m_config.FixIntegerOption(FB_CFGID_COLTIME,
                              m_listview.Columns[COL_TIME].Width);
    m_config.FixIntegerOption(FB_CFGID_COLATTR,
                              m_listview.Columns[COL_ATTR].Width);
    m_config.FixIntegerOption(FB_CFGID_COLTYPE,
                              m_listview.Columns[COL_TYPE].Width);
  end;

  // done yet?
  if (blColSetsOnly) then
    Exit;

  // fix drive paths
  for cDrive:='A' to 'Z' do
    m_config.FixStringOption(FB_CFGID_DRIVEPATH_PREFIX + cDrive,
                             m_config.GetStringOption(FB_CFGID_DRIVEPATH_PREFIX
                             + cDrive))
end;


procedure TFileBrowser.SetExclAttrMask(nExclAttrMask : Integer);
begin
  m_nExclAttrMask:=nExclAttrMask;
end;


procedure TFileBrowser.ChangePath(sPath : String;
                                  blUpdateHistory : Boolean = True);
begin
  // given path exists?
  if (DirectoryExists(sPath)) then begin
    m_sPath:=TStrPlus.PurePath(sPath);
    m_sSelection:='*.*';
  end
  else begin
    // perhaps we have a selection?
    if (DirectoryExists(ExtractFilePath(sPath))) then begin
      m_sPath:=TStrPlus.PurePath(ExtractFilePath(sPath));
      m_sSelection:=ExtractFileName(sPath);
    end
    else
      raise EFileBrowserError.Create(Format(m_sr.Get(FB_CFG_ID, 'ERR00'),
                                            [sPath]));
  end;

  // we have a path, a network view will be left now
  m_blPureNetWork:=False;
  m_nNetworkDepth:=0;
  // (store the drive path, if possible)
  if (m_sPath[2] = ':') then
    m_sLastDrivePath:=m_sPath;
  Refresh(blUpdateHistory);
end;


function TFileBrowser.MakeAbsolutePIDL(pidl : PItemIDList = Nil) : PItemIDList;
var
  nI     : Integer;
  nSize  : Integer;
  pBuild : PItemIDList;
  pSave  : PItemIDList;

// local sub to concat two PIDLs
function ConcatPIDLs(pidl1, pidl2 : PItemIDList): PItemIDList;
var
  nSize1, nSize2 : Integer;
// (sublocal sub to get the size of a PIDL)
function GetPIDLSize(pidl: PItemIDList) : Integer;
begin
  Result:=0;
  Inc(Result, SizeOf(pidl^.mkID.cb));
  while (pidl^.mkID.cb <> 0) do begin
    Inc(Result, pidl^.mkID.cb);
    Inc(WORD32(pidl), pidl^.mkID.cb);
  end;
end;
begin
  nSize1:=GetPIDLSize(pidl1) - SizeOf(pidl1^.mkID.cb);
  nSize2:=GetPIDLSize(pidl2);
  nSize:=nSize1 + nSize2;
  GetMem(Result, nSize);
  FillChar(Result^, nSize, 0);
  Move(pidl1^, Result^, nSize1);
  Move(pidl2^, PChar(Result)[nSize1], nSize2);
end;

begin
  // concat all PIDLs from the stack
  pBuild:=m_networkPIDLs[0];
  nSize:=0;
  nI:=0;
  if (m_nNetworkDepth > 1) then begin
    nI:=1;
    while (nI < m_nNetworkDepth) do begin
      pSave:=pBuild;
      pBuild:=ConcatPIDLs(pBuild, m_networkPIDLs[nI]);
      if (nI > 1) then
        FreeMem(pSave);
      Inc(nI);
    end;
  end;

  // add the given one, i. n.
  if (pidl <> Nil) then begin
    pBuild:=ConcatPIDLs(pBuild, pidl);
    nI:=1; // (need this below)
  end;

  // move the PIDL into shell memory
  if (nI > 0) then begin
    Result:=m_shellMalloc.Alloc(nSize);
    Move(pBuild^, Result^, nSize);
    FreeMem(pBuild);
  end
  else
    Result:=pBuild;

end;



// callback function for sorting the items
// -> the items to compare
// -> how to sort, see FILEBROWSER_SORT_xxx constants
function CompareItems(item1, item2: TListItem;
                      nSortMode : Integer) : Integer; stdcall;
const
  ITEMIS_FLAGS       = FILEBROWSER_ITEMIS_ENCRYPTED;
  DRIVES_AND_NETWORK = FILEBROWSER_ITEMIS_DRIVE or
                       FILEBROWSER_ITEMIS_NETWORKUNIT or
                       FILEBROWSER_ITEMIS_NETWORKROOT;
var
  qDiff      : WORD64;
  lItemIs1   : WORD32;
  lItemIs2   : WORD32;
  nType1     : Integer;
  nType2     : Integer;
  nNegator   : Integer;
  pItemInfo1 : PFileBrowserItemInfo;
  pItemInfo2 : PFileBrowserItemInfo;
begin
  // get the item info pointers and extract identifiers (to improve the speed)
  pItemInfo1:=PFileBrowserItemInfo(item1.Data);
  pItemInfo2:=PFileBrowserItemInfo(item2.Data);
  lItemIs1:=pItemInfo1^.lItemIs;
  lItemIs2:=pItemInfo2^.lItemIs;

  // the up-one-level item is _always_ up
  if ((lItemIs1 and FILEBROWSER_ITEMIS_UPONELEVEL) <> 0) then begin
    Result:=-1;
    Exit;
  end;
  if ((lItemIs2 and FILEBROWSER_ITEMIS_UPONELEVEL) <> 0) then begin
    Result:=1;
    Exit;
  end;

  // drives and network items are always up or down
  if (((lItemIs1 and DRIVES_AND_NETWORK) <> 0) or
      ((lItemIs2 and DRIVES_AND_NETWORK) <> 0)) then begin

    // create linear sort types
    case (lItemIs1 and (not ITEMIS_FLAGS)) of
      FILEBROWSER_ITEMIS_NETWORKUNIT : nType1:=1;
      FILEBROWSER_ITEMIS_DRIVE       : nType1:=2;
      FILEBROWSER_ITEMIS_NETWORKROOT : nType1:=3;
    else
      nType1:=0;
    end;
    case (lItemIs2 and (not ITEMIS_FLAGS)) of
      FILEBROWSER_ITEMIS_NETWORKUNIT : nType2:=1;
      FILEBROWSER_ITEMIS_DRIVE       : nType2:=2;
      FILEBROWSER_ITEMIS_NETWORKROOT : nType2:=3;
    else
      nType2:=0;
    end;

    // prepare the negator for the case that one of the items is a folder/file
    if ((nSortMode and FILEBROWSER_SORTEX_DRIVESFIRST) <> 0) then
      nNegator:=1
    else
      nNegator:=-1;

    // comparsion is now easy
    if (nType1 = 0) then
      Result:=1 * nNegator
    else
      if (nType2 = 0) then
        Result:=-1 * nNegator
      else begin
        if (nType1 = nType2) then begin
          // (drives need a special sorting)
          if (nType1 = 2) then
            Result:=CompareText(
                     Copy(item1.Caption, Pos(':', item1.Caption) - 1, 1),
                     Copy(item2.Caption, Pos(':', item2.Caption) - 1, 1))
          else
            Result:=CompareText(item1.Caption, item2.Caption)
        end
        else
          if (nType1 > nType2) then
            Result:=1
          else
            Result:=-1;
      end;
    Exit;
  end;

  // now there are only files and folders left
  if ((lItemIs1 and (not ITEMIS_FLAGS)) = FILEBROWSER_ITEMIS_FOLDER) then
    nType1:=0
  else
    nType1:=1;
  if ((lItemIs2 and (not ITEMIS_FLAGS)) = FILEBROWSER_ITEMIS_FOLDER) then
    nType2:=0
  else
    nType2:=1;

  // handle the easy cases
  if ((nType1 = 0) and (nType2 = 1)) then begin
    Result:=-1;
    Exit;
  end;
  if ((nType1 = 1) and (nType2 = 0)) then begin
    Result:=1;
    Exit;
  end;
  // (treat the cmp(file, file) case first)
  if ((nType1 = 1) and (nType2 = 1)) then begin

    // sort by name?
    if ((nSortMode and FILEBROWSER_SORT_FILE) <> 0) then begin
      Result:=CompareText(item1.Caption, item2.Caption);
    end
    else
      // sort by size?
      if ((nSortMode and FILEBROWSER_SORT_SIZE) <> 0) then begin
        qDiff:=pItemInfo1^.qFileSize - pItemInfo2^.qFileSize;
        if (qDiff > 0) then
          Result:=1
        else
          if (qDiff < 0) then
            Result:=-1
          else
            Result:=0;
      end
      else
        // sort by (64bit nanosec) time?
        if ((nSortMode and FILEBROWSER_SORT_TIME) <> 0) then begin
          qDiff:=pItemInfo1^.qFileTime - pItemInfo2^.qFileTime;
          if (qDiff > 0) then
            Result:=1
          else
            if (qDiff < 0) then
              Result:=-1
            else
              Result:=0;
        end
        else
          // sort by the type (extension dependant)?
          if ((nSortMode and FILEBROWSER_SORT_TYPE) <> 0) then begin
            // FXIME: how to improve this timeconsuming comparison?
            Result:=CompareText(
                      AnsiUpperCase(
                        TStrPlus.ExtractFileExtension(item1.Caption)),
                      AnsiUpperCase(
                        TStrPlus.ExtractFileExtension(item2.Caption)));
          end
          else
            // sort by the attributes?
            if ((nSortMode and FILEBROWSER_SORT_ATTR) <> 0) then begin
              // FIXME: how to sort attributes?!?!
              Result:=Integer(pItemInfo1^.lAttrTypePIDL) -
                      Integer(pItemInfo2^.lAttrTypePIDL);
            end
            else
              // sort by the real name?
              if ((nSortMode and FILEBROWSER_SORT_REALNAME) <> 0) then begin
                Result:=CompareText(item1.SubItems[COL_REALNAME - 1],
                                    item2.SubItems[COL_REALNAME - 1]);
              end
              else
                // sort by the real size?
                if ((nSortMode and FILEBROWSER_SORT_REALSIZE) <> 0)then begin
                  qDiff:=pItemInfo1^.qRealFileSize - pItemInfo2^.qRealFileSize;
                  if (qDiff > 0) then
                    Result:=1
                  else
                    if (qDiff < 0) then
                      Result:=-1
                    else
                      Result:=0;
                end
                else
                  // (this should never happen)
                  Result:=0;

    // if we have equal cases we sort by the name (should always work)
    if (Result = 0) then
      Result:=CompareText(item1.Caption, item2.Caption);

    // change the direction, i. n.
    if ((nSortMode and FILEBROWSER_SORT_UPWARDS) <> 0) then
      Result:=Result * -1;
    Exit;
  end;

  // last case: cmp(dir,dir), same as above
  if ((nSortMode and FILEBROWSER_SORT_FILE) <> 0) then begin
    Result:=CompareText(item1.Caption, item2.Caption);
  end
  else
    if ((nSortMode and FILEBROWSER_SORT_TIME) <> 0) then begin
      qDiff:=pItemInfo1^.qFileTime - pItemInfo2^.qFileTime;
      if (qDiff > 0) then
        Result:=1
      else
        if (qDiff < 0) then
          Result:=-1
        else
          Result:=0;
    end
    else
      if ((nSortMode and FILEBROWSER_SORT_ATTR) <> 0) then begin
        Result:=Integer(pItemInfo1^.lAttrTypePIDL) -
                Integer(pItemInfo2^.lAttrTypePIDL);
      end
      else
         Result:=0;
  if (Result = 0) then
    Result:=CompareText(item1.Caption, item2.Caption);
  if ((nSortMode and FILEBROWSER_SORT_UPWARDS) <> 0) then
    Result:=Result * -1;
end;




procedure TFileBrowser.UpdateContentInfo;
begin
  if (m_blPureNetwork) then
    m_sContentInfo:=m_sr.Get(FB_CFG_ID, 'NETWORK')
  else begin
    try
      m_sContentInfo:=Format(m_sr.Get(FB_CFG_ID, 'ST_INFO'),
                             [TStrPlusI.Sepa1000(m_sr, m_qNumOfBytes),
                              TStrPlusI.Sepa1000(m_sr,
                               TFileSupport.GetDiskFreeBytes(
                                 TStrPlus.RootPath(m_sPath), m_sr))]);
    except
      on EFileSupportError do
        m_sContentInfo:=Format(m_sr.Get(FB_CFG_ID, 'ST_INFO'),
                               [TStrPlusI.Sepa1000(m_sr, m_qNumOfBytes),
                                m_sr.Get(FB_CFG_ID, 'DUNNO')])
    end;
  end;
end;



// our modulo when we do the progress callback
const
  CALLBACK_MODULO = 23;


procedure TFileBrowser.Refresh(blUpdateHistory : Boolean = True);
var
  nNumOfObjs    : Integer;
  nSortMode     : Integer;
  blInterrupt   : Boolean;
  blDrivesUp    : Boolean;
  blAddDirOK    : Boolean;
  blUOLDir      : Boolean;
  hSearch       : THandle;
  dta           : TWin32FindData;
  newItem       : TListItem;
  fileInfo      : TSHFileInfo;
  pItemInfo     : PFileBrowserItemInfo;
  throwThis     : EFileBrowserError;
  sTypeInfo     : String;
  sRTLPath      : String;
  sDirPath      : String;
  // all the stuff needed for network browsing using the shell...
  ulCeltFetched : ULONG;
  desktopFolder : IShellFolder;
  enumIDList    : IEnumIDList;
  itemIDList    : PItemIDList;


// local sub to add drives (and the global network, if possible)
procedure AddDrives;
var
  cDrive          : Char;
  lDriveType      : WORD32;
  sDriveRoot      : String;
  networkRootPIDL : PItemIDList;
begin
  for cDrive:='A' to 'Z' do begin
    // build the drive root
    sDriveRoot:=cDrive + ':\';
    lDriveType:=GetDriveType(PChar(sDriveRoot));

    // drive exists?
    if ((lDriveType <> 0) and (lDriveType <> 1)) then begin

      // insert a new item
      newItem:=m_listView.Items.Add;
      pItemInfo:=AddItemInfo;
      newItem.Data:=pItemInfo;

      // fill out the item info
      with pItemInfo^ do begin
        lItemIs:=FILEBROWSER_ITEMIS_DRIVE;
        lAttrTypePIDL:=lDriveType;
      end;

      // get the shell icon and description for the drive
      SHGetFileInfo(PChar(sDriveRoot),
                    0,
                    fileInfo,
                    SizeOf(fileInfo),
                    SHGFI_SMALLICON or SHGFI_ICON or SHGFI_DISPLAYNAME or
                    SHGFI_SYSICONINDEX);
      newItem.Caption:=fileinfo.szDisplayName;
      newItem.ImageIndex:=fileinfo.iIcon;
    end;
  end;

  // add the network root icon, if possible
  if (SHGetSpecialFolderLocation(m_listView.Handle,
                                 CSIDL_NETWORK,
                                 networkRootPIDL) <> NOERROR) then
    // (no network, no problems)
    Exit;
  // time to fetch the icon
  SHGetFileInfo(PChar(networkRootPIDL),
                0,
                fileInfo,
                SizeOf(fileInfo),
                SHGFI_PIDL or SHGFI_SYSICONINDEX  or SHGFI_TYPENAME or
                SHGFI_DISPLAYNAME);
  newItem:=m_listView.Items.Add;
  newItem.Caption:=fileinfo.szDisplayName;
  newItem.ImageIndex:=fileinfo.iIcon;

  // add the item info
  pItemInfo:=AddItemInfo;
  pItemInfo.lItemIs:=FILEBROWSER_ITEMIS_NETWORKROOT;
  pItemInfo.lAttrTypePIDL:=WORD32(networkRootPIDL);
  newItem.Data:=pItemInfo;
end;

begin

  // suspend any possible scanning action
  SuspendBFAScanner;

  // stop the auto refresh
  AutoRefresh(False);

  // prepare the associated listview
  m_listView.Items.BeginUpdate;

  ClearItemInfos;
  m_listView.Items.Clear;

  // assume success
  throwThis:=Nil;
  blInterrupt:=False;

  // no breaks, please
  m_callBack.SetSignal(FILEBROWSER_CALLBACK_NOBREAK);

  // reset all counters
  m_nNumOfFiles:=0;
  m_nNumOfFolders:=0;
  NoSelect;
  nNumOfObjs:=0; // (this one is only used for the progress callback)
  m_qNumOfBytes:=0; // (and this for the content description)

  // get the absolute path
  SetCurrentDir(m_sPath);
  m_sPath:=GetCurrentDir;

  // assume a non-readonly drive
  m_blReadOnlyDrive:=False;

  // add the drives at the start, i.n.
  blDrivesUp:=m_config.GetBooleanOption(FB_CFGID_PLACEDRIVESFIRST);
  if (blDrivesUp) then
    AddDrives;

  // the following part is interruptable
  hSearch:=0;
  try
    // don't search for files and folders if we're in a pure network path
    if (m_blPureNetWork) then begin

      // get all network subfolders
      SHGetDesktopFolder(desktopFolder);
      if (desktopFolder.BindToObject(MakeAbsolutePIDL,
                                     Nil,
                                     IID_IShellFolder,
                                     Pointer(desktopFolder)) <> NOERROR) then
        throwThis:=EFileBrowserError.Create(m_sr.Get(FB_CFG_ID,
                                                     'ERR02'));
      if (throwThis = Nil) then
        if (desktopFolder.EnumObjects(m_listView.Handle,
                                      SHCONTF_FOLDERS,
                                      enumIDList) <> NOERROR) then
          throwThis:=EFileBrowserError.Create(m_sr.Get(FB_CFG_ID,
                                                       'ERR02'));
      if (throwThis = Nil) then
        while (enumIDList.Next(1,
                               itemIDList,
                               ulCeltFetched) = NOERROR) do begin
          // get the icon information
          SHGetFileInfo(PChar(MakeAbsolutePIDL(itemIDList)),
                        0,
                        fileInfo,
                        SizeOf(fileInfo),
                        SHGFI_PIDL or SHGFI_SYSICONINDEX or SHGFI_TYPENAME or
                        SHGFI_DISPLAYNAME);

          // add a new row
          newItem:=m_listView.Items.Add;
          pItemInfo:=AddItemInfo;
          newItem.Data:=pItemInfo;
          newItem.Caption:=fileInfo.szDisplayName;
          newItem.ImageIndex:=fileInfo.iIcon;
          with pItemInfo^ do begin
            lItemIs:=FILEBROWSER_ITEMIS_NETWORKUNIT;
            lAttrTypePIDL:=WORD32(itemIDList);
          end;
        end;

    end
    else begin

      // prepare the attribute exclude mask
      sRTLPath:=TStrPlus.RTLPath(m_sPath);
      hSearch:=FindFirstFile(PChar(sRTLPath + m_sSelection), dta);
      if (hSearch = INVALID_HANDLE_VALUE) then begin
        // no files at all?
        if (GetLastError <> ERROR_FILE_NOT_FOUND) then
          throwThis:=EFileBrowserWarning.Create(m_sr.Get(FB_CFG_ID,
                                                         'NOFILES'));
      end
      else begin
        // the classic file searcher
        repeat
          // does the found object match into the attribut exclusion?
          if ((m_nExclAttrMask and dta.dwFileAttributes) = 0) then begin

            // is it a directory or a file?
            if ((dta.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) =
                FILE_ATTRIBUTE_DIRECTORY) then begin

              // don't accept the '.' path
              if (StrComp(dta.cFilename, '.') <> 0) then begin

                // check the special '..' case
                blAddDirOK:=True;
                blUOLDir:=(StrComp(dta.cFilename, '..') = 0);
                if (blUOLDir) then begin
                  if (not CanGoUp) then
                    blAddDirOK:=False
                  else
                    // get the 'real' path
                    sDirPath:=TStrPlus.ParentPath(m_sPath);
                end
                else
                  sDirPath:=sRTLPath + dta.cFileName;

                // add the directory now
                if (blAddDirOK) then begin
                  // create a new item
                  newItem:=m_listView.Items.Add;
                  pItemInfo:=AddItemInfo;
                  newItem.Data:=pItemInfo;

                  // we don't use an icon lookup for folders, so get the icon
                  newItem.ImageIndex:=m_iconLookup.GetIndex(sDirPath,
                                                            sTypeInfo,
                                                            True);
                  // fill out the current row
                  newItem.Caption:=dta.cFilename;
                  with newItem.SubItems do begin
                    Add(''); // (for the scanner, ignored)
                    Add(''); // (directory has no size)
                    Add(''); // (for the scanner, ignored)
                    Add(TStrPlusI.Win32FileTimeToStr(m_sr,
                                                     dta.ftLastWriteTime));
                    Add(TStrPlusI.FileAttrToStr(dta.dwFileAttributes));
                    Add(sTypeInfo);
                  end;

                  // complete the item info
                  with pItemInfo^ do begin
                    // up-one-level 'path' gets a special code
                    if (blUOLDir) then
                      lItemIs:=FILEBROWSER_ITEMIS_UPONELEVEL
                    else
                      lItemIs:=FILEBROWSER_ITEMIS_FOLDER;
                    qFileTime:=MakeWORD64(dta.ftLastWriteTime.dwLowDateTime,
                                          dta.ftLastWriteTime.dwHighDateTime);
                    lAttrTypePIDL:=dta.dwFileAttributes;
                  end;

                  // increase the counters, if necessary
                  if (not blUOLDir) then begin
                    Inc(nNumOfObjs);
                    Inc(m_nNumOfFolders);
                  end;
                end;
              end;
            end
            else begin
              // create a new item
              newItem:=m_listView.Items.Add;
              pItemInfo:=AddItemInfo;
              newItem.Data:=pItemInfo;

              // here (for files) we use the icon lookup
              newItem.ImageIndex:=m_iconLookup.GetIndex(sRTLPath +
                                                        dta.cFileName,
                                                        sTypeInfo);
              // complete the item info
              with pItemInfo^ do begin
                lItemIs:=FILEBROWSER_ITEMIS_FILE;
                qRealFileSize:=-1;  // (unknown yet, set by the scanner)
                qFileSize:=MakeWORD64(dta.nFileSizeLow, dta.nFileSizeHigh);
                qFileTime:=MakeWORD64(dta.ftLastWriteTime.dwLowDateTime,
                                      dta.ftLastWriteTime.dwHighDateTime);
                lAttrTypePIDL:=dta.dwFileAttributes;
              end;

              // fill out the current row
              newItem.Caption:=dta.cFilename;
              with newItem.SubItems do begin
                Add('');  // (prepare for the scanner)
                Add(TStrPlusI.Sepa1000(m_sr, pItemInfo^.qFileSize));
                Add('');  // (prepared for the scanner)
                Add(TStrPlusI.Win32FileTimeToStr(m_sr, dta.ftLastWriteTime));
                Add(TStrPlus.FileAttrToStr(dta.dwFileAttributes));
                Add(sTypeInfo);
              end;

              // increase the counters
              Inc(nNumOfObjs);
              Inc(m_nNumOfFiles);
              Inc(m_qNumOfBytes, pItemInfo^.qFileSize);
            end;
          end;

          // report the search progress, i. n.
          if ((nNumOfObjs mod CALLBACK_MODULO) = 0) then begin
            m_callBack.SetMessage(Format(m_sr.Get(FB_CFG_ID,
                                                  'BROWSING'),
                                         [nNumOfObjs]));
            m_callBack.CallBack;
          end;

        until (Windows.FindNextFile(hSearch, dta) = FALSE);

        // no more files or a real error?
        if (GetLastError <> ERROR_NO_MORE_FILES) then
          throwThis:=EFileBrowserWarning.Create(m_sr.Get(FB_CFG_ID,
                                                'ERR01'));

        // the search ends here
        Windows.FindClose(hSearch);
      end;

    end;
  except
    on ECallBackInterrupt do begin
      // clean up
      if (hSearch <> 0) then
        Windows.FindClose(hSearch);
      // set the interrupt flag
      blInterrupt:=True;
    end;
  end;

  // add the drives at the end, i. n.
  if (not blDrivesUp) then
    AddDrives;

  // change the view style
  ChangeViewStyle;

  // clear the icon index lookup (not needed anymore until next time)
  m_iconLookup.ClearTable;

  // check for a readonly drive (currently we only detect CD-ROM drives)
  if (GetDriveType(PChar(TStrPlus.RootPath(m_sPath))) = DRIVE_CDROM) then
    m_blReadOnlyDrive:=True;

  // create the content info string
  UpdateContentInfo;

  // sort the list, i. n.
  try
    nSortMode:=m_config.GetIntegerOption(FB_CFGID_SORT);
    if ((nSortMode and FILEBROWSER_SORT_UNSORTED) = 0) then begin
      // (show first) what we're doing
      m_callBack.SetMessage(m_sr.Get(FB_CFG_ID, 'SORTING'));
      m_callBack.CallBack;
      // (set the drives first flag, if necessary)
      if (m_config.GetBooleanOption(FB_CFGID_PLACEDRIVESFIRST)) then
        nSortMode:=nSortMode or FILEBROWSER_SORTEX_DRIVESFIRST;

      m_listView.CustomSort(@CompareItems, nSortMode);
    end;
  except
    on ECallBackInterrupt do
      blInterrupt:=True;
  end;

  // show the results
  m_listView.Items.EndUpdate;
  m_listView.Color:=clWindow;

  // back to normal callbacks (tricky)
  m_callBack.GetSignal;

  // (re)start the scanner
  if (m_blScannerReady) then begin
    try
      ResumeBFAScanner;
    except
      on ebse : EBFAScannerError do begin
        // (FIXME: we ignore errors right here, ok so?)
      end;
    end;
  end;

  // add the current path to the history
  if ((not m_blPureNetwork) and (blUpdateHistory)) then begin
    m_hist.Add(GetCurrentPath,
               HISTORY_ADD_NODOUBLE);
  end;

  // (re)enable the auto refresh
  if (m_config.GetBooleanOption(FB_CFGID_AUTOREFRESH) and
      (not m_blPureNetwork)) then
    AutoRefresh(True);

  // store the current drive path, i.n.
  if (m_sPath[2] = ':') then
    m_config.SetStringOption(FB_CFGID_DRIVEPATH_PREFIX + m_sPath[1],
                             m_sPath);

  // interrupted?
  if (blInterrupt) then
    raise EFileBrowserInterrupt.Create(m_sr.Get(FB_CFG_ID, 'INTERRUPT'));

  // error occured?
  if (throwThis <> Nil) then
    raise throwThis;
end;


procedure TFileBrowser.Layout;
var
  blState : Boolean;
begin
  // set all properties
  with m_listView, m_config do begin
    blState:=GetBooleanOption(FB_CFGID_FLATVIEW);
    FlatScrollBars:=blState;
    if (blState) then
      BorderStyle:=bsNone
    else
      BorderStyle:=bsSingle;

    HotTrack:=GetBooleanOption(FB_CFGID_HOTTRACKING);
    if (GetBooleanOption(FB_CFGID_HTMLSTYLE)) then
      // (the fun never ends...)
      HotTrackStyles:=[htHandPoint, htUnderlineCold, htUnderlineHot]
    else
      HotTrackStyles:=[];

    IconOptions.AutoArrange:=GetBooleanOption(FB_CFGID_AUTOARRANGE);
    GridLines:=GetBooleanOption(FB_CFGID_GRIDLINES);

    with Font do begin
      Name:=GetStringOption(FB_CFGID_FONTNAME);
      Size:=GetIntegerOption(FB_CFGID_FONTSIZE);
      Style:=TFontStyles(Byte(GetIntegerOption(FB_CFGID_FONTSTYLE))); // crazy
      Color:=TColor(GetIntegerOption(FB_CFGID_FONTCOLOR));
    end;
  end;
end;


procedure TFileBrowser.ChangeViewStyle;
var
  nActStyle : Integer;
  nSetStyle : Integer;
begin
  // get the current style
  nActStyle:=Integer(m_listView.ViewStyle);
  nSetStyle:=m_config.GetIntegerOption(FB_CFGID_VIEWSTYLE);

  // no need to change?
  if (nActStyle = nSetStyle) then
    Exit;

  // due to auto arrange the first column may change in non-report views, so
  // we take the chance to save...
  if (nActStyle = Integer(vsReport)) then begin
    m_config.FixIntegerOption(FB_CFGID_COLNAME,
                              m_listView.Columns.Items[0].Width);
    // set the (one and only) column to autosize now
    m_listView.Columns.Items[0].Width:=ColumnTextWidth;
  end
  else
    // ...or reload it (due to the same reason)
    m_listView.Columns.Items[0].Width:=
      m_config.GetIntegerOption(FB_CFGID_COLNAME);

  // now we're ready to change the style
  m_listView.ViewStyle:=TViewStyle(nSetStyle);

end;


function TFileBrowser.GetCurrentPath(blAll : Boolean = True) : String;
begin
  if (m_blPureNetwork) then
    Result:=''
  else begin
    if (blAll) then begin
      Result:=TStrPlus.RTLPath(m_sPath);
      if (Result <> '') then
        Result:=Result + m_sSelection;
    end
    else begin
      Result:=m_sPath;
    end;
  end;
end;




function TFileBrowser.HandleDoubleClick : Integer;
var
  blNetworkRoot : Boolean;
  cDriveChar    : Char;
  sTemp         : String;
  sFullPathed   : String;
  selectedItem  : TListItem;
  pItemData     : PFileBrowserItemInfo;
  networkPath   : array[0..MAX_PATH] of Char;
begin
  // set the default result
  Result:=FILEBROWSER_DBLCLICK_NOP;

  // get the pointer to the selected item's data
  selectedItem:=m_listView.Selected;
  if (selectedItem = Nil) then
    // (definitely a caller error)
    Exit;
  pItemData:=selectedItem.Data;

  with pItemData^ do begin

    // is it a network node?
    blNetworkRoot:=((lItemIs and FILEBROWSER_ITEMIS_NETWORKROOT) =
                    FILEBROWSER_ITEMIS_NETWORKROOT);
    if (((lItemIs and FILEBROWSER_ITEMIS_NETWORKUNIT) =
         FILEBROWSER_ITEMIS_NETWORKUNIT) or blNetworkRoot) then begin

      // push the new PIDL on the network stack
      if (blNetworkRoot) then begin
        // (reset the network stack if the user clicked on the root)
        m_nNetworkDepth:=1;
        m_blPureNetWork:=True;
        m_networkPIDLs[0]:=Pointer(lAttrTypePIDL);
      end
      else begin
        // does the item obtain a network file path?
        m_blPureNetWork:=True;
        if (SHGetPathFromIDList(makeAbsolutePIDL(Pointer(lAttrTypePIDL)),
                                @networkPath) = TRUE) then begin
          sTemp:=String(PChar(@networkPath));
          if (Copy(sTemp, 1, 2) = '\\') then
            m_blPureNetWork:=False;
        end;
        if (m_blPureNetwork) then begin
          // no, enter the next network level
          m_networkPIDLs[m_nNetworkDepth]:=Pointer(lAttrTypePIDL);
          Inc(m_nNetworkDepth);
        end
        else begin
          // yes, leave the pure network view, the PIDLs still remain on the
          // stack for the case that the user wants to go up again
          m_sPath:=sTemp;
          m_sSelection:='*.*';
        end;
      end;
      // refresh the list and quit
      Refresh;
      Result:=FILEBROWSER_DBLCLICK_CHANGEDPATH;
      Exit;
    end;

    // creare the full path
    sFullPathed:=TStrPlus.RTLPath(m_sPath) + selectedItem.Caption;

    // is it a file?
    if ((lItemIs and FILEBROWSER_ITEMIS_FILE) =
        FILEBROWSER_ITEMIS_FILE) then begin

      // must we view (.BFA) or execute (.BFJ) it?
      if (CompareText(TStrPlus.ExtractFileExtension(selectedItem.Caption),
                      BFAFILE_EXTENSION) = 0) then begin

        Result:=FILEBROWSER_DBLCLICK_BFAREQUEST;

      end
      else begin

        if (CompareText(TStrPlus.ExtractFileExtension(selectedItem.Caption),
                        BFJFILE_EXTENSION) = 0) then begin

          Result:=FILEBROWSER_DBLCLICK_BFJREQUEST;

        end
        else begin
          // try to execute the file via the shell
          try
            // show what we're doing
            m_callBack.SetMessage(Format(m_sr.Get(FB_CFG_ID, 'EXEC'),
                                         [selectedItem.Caption]));
            m_callBack.CallBack;
          except
            on ECallbackInterrupt do
              // ok, let it be
              Exit;
          end;
          try
            TFileSupport.OpenFile(sFullPathed,
                                  m_listView.Handle,
                                  m_sr);
          except
            on efse : EFileSupportError do begin
              raise EFileBrowserError.Create(efse.Message);
            end;
          end;
          Result:=FILEBROWSER_DBLCLICK_EXECUTED;
        end;
      end;

      // file handling done
      Exit;
    end;

    // perhaps a folder?
    if ((lItemIs and FILEBROWSER_ITEMIS_FOLDER) =
         FILEBROWSER_ITEMIS_FOLDER) then begin
      // change to the new path
      m_sPath:=sFullPathed;
      // (set the last drive path, i. n.)
      if (m_sPath[2] = ':') then
        m_sLastDrivePath:=m_sPath;
      Refresh;
      Result:=FILEBROWSER_DBLCLICK_CHANGEDPATH;
      Exit;
    end;

    // the up-one-level 'path'?
    if ((lItemIs and FILEBROWSER_ITEMIS_UPONELEVEL) =
         FILEBROWSER_ITEMIS_UPONELEVEL) then begin
      UpOneLevel;
      Result:=FILEBROWSER_DBLCLICK_CHANGEDPATH;
      Exit;
    end;

    // drive?
    if ((lItemIs and FILEBROWSER_ITEMIS_DRIVE) =
         FILEBROWSER_ITEMIS_DRIVE) then begin

      // get the drive character directly from the caption (FIXME: this isn't
      // a very smart solution...)
      sTemp:=selectedItem.Caption;
      cDriveChar:=UpCase(sTemp[Pos(':', sTemp) - 1]);
      // (we try to change the path in all cases)
      Result:=FILEBROWSER_DBLCLICK_CHANGEDPATH;

      // can we return to a previous drive path?
      if (Length(m_sPath) >= 2) then
        if (m_sPath[2] = ':') then begin
          if (UpCase(m_sPath[1]) = cDriveChar) then
            ChangePath(cDriveChar + ':\' + m_sSelection)  // back to the root
          else begin
            sTemp:=m_config.GetStringOption(FB_CFGID_DRIVEPATH_PREFIX +
                                            cDriveChar);
            if (sTemp <> '') then begin
              // check out first if the path still exists,
              // otherwise change to the root
              if (not DirectoryExists(TStrPlus.PurePath(sTemp))) then
                sTemp:=TStrPlus.RootPath(sTemp);
              ChangePath(TStrPlus.RTLPath(sTemp) + m_sSelection)
            end
            else
              ChangePath(cDriveChar + ':\' + m_sSelection);
          end;
          Exit;
        end;

      // there's no other destination for us than the root of the drive
      ChangePath(cDriveChar + ':\' + m_sSelection);
    end;


  end;

end;


function TFileBrowser.CanGoUp : Boolean;
begin

//  debd('m_nNetworkDepth', m_nNetworkDepth);

  // are we in a pure network environment?
  if (m_blPureNetwork) then
    Result:=(m_nNetworkDepth > 0)
  else begin
    // we have a valid path, let's check if we can go up in the file system
    if (TStrPlus.ParentPath(m_sPath) <> '') then
      Result:=True
    else
      // not in the file system, but how about up in the network?
      Result:=(m_nNetworkDepth > 0);
  end;
end;



procedure TFileBrowser.UpOneLevel;
var
  sUpperPath : String;
begin
  // if we can't go up avoid any further operations
  if (not CanGoUp) then
    Exit;

  // not in a pure network environment?
  if (not m_blPureNetwork) then begin

    // is there an upper path we can join?
    sUpperPath:=TStrPlus.ParentPath(m_sPath);
    if (sUpperPath <> '') then begin
      // yes, do so
      m_sPath:=sUpperPath;
      Refresh;
    end
    else
      // check if we can reenter the network
      if (m_nNetworkDepth > 0) then begin
        // refresh
        m_blPureNetwork:=True;
        Refresh;
      end;
  end
  else begin
    // check if we are just one level below the global network
    if (m_nNetworkDepth = 1) then begin
      // we are, so try to get back to the latest drive path or the default one
      // (this will throw us out of the pure network automatically)
      if (m_sLastDrivePath = '') then
        JoinDefaultPath
      else
        ChangePath(TStrPlus.RTLPath(m_sLastDrivePath) + m_sSelection);
    end
    else begin
      // pop the network stack
      Dec(m_nNetworkDepth);
      // refresh the view
      Refresh;
    end;
  end;
end;


procedure TFileBrowser.JoinDefaultPath;
begin
  // the path of the program executabe should always be valid
  ChangePath(ExtractFilePath(Application.ExeName));
end;

function TFileBrowser.GetNumOfFiles : Integer;
begin
  Result:=m_nNumOfFiles;
end;

function TFileBrowser.GetNumOfFolders : Integer;
begin
  Result:=m_nNumOfFolders;
end;

function TFileBrowser.GetNumOfSelObjs : Integer;
begin
  Result:=m_nNumOfSelFiles + m_nNumOfSelDirs + m_nNumOfSelDrives;
end;

function TFileBrowser.GetNumOfSelFiles : Integer;
begin
  Result:=m_nNumOfSelFiles;
end;

function TFileBrowser.GetNumOfSelDirs : Integer;
begin
  Result:=m_nNumOfSelDirs;
end;

function TFileBrowser.GetNumOfSelDrives : Integer;
begin
  Result:=m_nNumOfSelDrives;
end;

function TFileBrowser.GetNumOfSelOther : Integer;
begin
  Result:=m_nNumOfSelOther;
end;

function TFileBrowser.GetNumOfSelBytes : WORD64;
begin
  Result:=m_qNumOfSelBytes;
end;

function TFileBrowser.GetContentInfo : String;
begin
  Result:=m_sContentInfo;
end;

function TFileBrowser.GetSelectedInfo : String;
begin
  Result:=Format(m_sr.Get(FB_CFG_ID, 'SEL_INFO'),
                [TStrPlusI.Sepa1000(m_sr, GetNumOfSelObjs),
                 TStrPlusI.Sepa1000(m_sr, GetNumOfSelBytes)]);
end;

function TFileBrowser.GetCountInfo : String;
begin
  Result:=Format(m_sr.Get(FB_CFG_ID, 'FF_INFO'),
                 [TStrPlusI.Sepa1000(m_sr, GetNumOfFolders),
                  TStrPlusI.Sepa1000(m_sr, GetNumOfFiles)]);
end;


procedure TFileBrowser.HandleSelection(item: TListItem;
                                       blState : Boolean);
begin

  if (not m_blHandleSelection) then
    Exit;

  with PFileBrowserItemInfo(item.Data)^ do begin

    // file?
    if ((lItemIs and FILEBROWSER_ITEMIS_FILE) =
         FILEBROWSER_ITEMIS_FILE) then begin
      if (blState) then begin
        Inc(m_nNumOfSelFiles);
        Inc(m_qNumOfSelBytes, qFileSize);
      end
      else begin
        Dec(m_nNumOfSelFiles);
        Dec(m_qNumOfSelBytes, qFileSize);
      end;
      Exit;
    end;

    // directory?
    if ((lItemIs and FILEBROWSER_ITEMIS_FOLDER) <> 0) then begin
      if (blState) then
        Inc(m_nNumOfSelDirs)
      else
        Dec(m_nNumOfSelDirs);
      Exit;
    end;

    // drive?
    if ((lItemIs and FILEBROWSER_ITEMIS_DRIVE) <> 0) then begin
      if (blState) then
        Inc(m_nNumOfSelDrives)
      else
        Dec(m_nNumOfSelDrives);
      Exit;
    end;

    // something other...
    if (blState) then
      Inc(m_nNumOfSelOther)
    else
      Dec(m_nNumOfSelOther);
  end;
end;


procedure TFileBrowser.HandleColumnClick(col : TListColumn);
var
  nOldSort : Integer;
  nNewSort : Integer;
begin
  // show what we want to do
  try
    m_callBack.SetMessage(m_sr.Get(FB_CFG_ID, 'SORTING'));
    m_callBack.CallBack;
  except
    on ECallBackInterrupt do
      Exit;
  end;

  // stop any running scanner
  SuspendBFAScanner;

  // stop auto refreshing
  AutoRefresh(False);

  // set the new sort code
  case (col.Index) of

    COL_FILENAME : nNewSort:=FILEBROWSER_SORT_FILE;
    COL_REALNAME : nNewSort:=FILEBROWSER_SORT_REALNAME;
    COL_SIZE     : nNewSort:=FILEBROWSER_SORT_SIZE;
    COL_REALSIZE : nNewSort:=FILEBROWSER_SORT_REALSIZE;
    COL_TIME     : nNewSort:=FILEBROWSER_SORT_TIME;
    COL_ATTR     : nNewSort:=FILEBROWSER_SORT_ATTR;
    COL_TYPE     : nNewSort:=FILEBROWSER_SORT_TYPE;
  else
    // (this should never happen!)
    nNewSort:=FILEBROWSER_SORT_UNSORTED;
  end;

  // get the old sort code
  nOldSort:=m_config.GetIntegerOption(FB_CFGID_SORT);

  // just a change in the direction?
  if ((nOldSort and nNewSort) = nNewSort) then
    // yes, just inverse the direction bit
    if ((nOldSort and FILEBROWSER_SORT_UPWARDS) = 0) then
      nNewSort:=nNewSort or FILEBROWSER_SORT_UPWARDS;

  // store the new sort code
  m_config.FixIntegerOption(FB_CFGID_SORT, nNewSort);

  // now sort that stuff (adding the drives first bit, if necessary)
  if (m_config.GetBooleanOption(FB_CFGID_PLACEDRIVESFIRST)) then
    nNewSort:=nNewSort or FILEBROWSER_SORTEX_DRIVESFIRST;
  m_listView.CustomSort(@CompareItems, nNewSort);

  // new coum headers
  ColumnsRelabel;

  // restart the scanner
  ResumeBFAScanner;

  // (re)enable the auto refresh
  if (m_config.GetBooleanOption(FB_CFGID_AUTOREFRESH)) then
    AutoRefresh(True);
end;


// internal selection codes
const
  SELECT_ALL       = 0;
  SELECT_FILES     = 1;
  SELECT_FOLDERS   = 2;
  SELECT_ENCRYPTED = 3;
  SELECT_DECRYPTED = 4;
  SELECT_BYSTRING  = 5;


procedure TFileBrowser.Selector(nSelCode : Integer;
                                sSubStr : String = '';
                                blCaseSens : Boolean = False);
var
  nI        : Integer;
  nUpIdx    : Integer;
  lItemIs   : WORD32;
  pItemInfo : PFileBrowserItemInfo;
  item      : TListItem;
begin
  NoSelect;
  // scan for files and folders
  with m_listView do begin

    Items.BeginUpdate;
    nUpIdx:=Items.Count - 1;
    Selected:=Nil;

    for nI:=0 to nUpIdx do begin
      item:=Items[nI];
      pItemInfo:=PFileBrowserItemInfo(item.Data);
      lItemIs:=pItemInfo^.lItemIs;

      // select what?
      case nSelCode of

        SELECT_ALL : begin
          if ((lItemIs and FILEBROWSER_ITEMIS_FILE) <> 0) then begin
            item.Selected:=True;
            Inc(m_nNumOfSelFiles);
            Inc(m_qNumOfSelBytes, pItemInfo^.qFileSize);
          end
          else
            if ((lItemIs and FILEBROWSER_ITEMIS_FOLDER) <> 0) then begin
              item.Selected:=True;
              Inc(m_nNumOfSelDirs);
            end;
        end;

        SELECT_FILES : begin
          if ((lItemIs and FILEBROWSER_ITEMIS_FILE) <> 0) then begin
            item.Selected:=True;
            Inc(m_nNumOfSelFiles);
            Inc(m_qNumOfSelBytes, pItemInfo^.qFileSize);
          end;
        end;

        SELECT_FOLDERS : begin
          if ((lItemIs and FILEBROWSER_ITEMIS_FOLDER) <> 0) then begin
            item.Selected:=True;
            Inc(m_nNumOfSelDirs);
          end;
        end;

        SELECT_ENCRYPTED : begin
          // encrypted means either a set encrypted-flag or the .BFA extension
          if ((lItemIs and FILEBROWSER_ITEMIS_FILE) <> 0) then begin
            if ((CompareText(
                   TStrPlus.ExtractFileExtension(item.Caption),
                   BFAFILE_EXTENSION) = 0)
                or ((lItemIs and FILEBROWSER_ITEMIS_ENCRYPTED) <> 0))
              then begin
              item.Selected:=True;
              Inc(m_nNumOfSelFiles);
              Inc(m_qNumOfSelBytes, pItemInfo^.qFileSize);
            end;
          end;
        end;

        SELECT_DECRYPTED : begin
          if ((lItemIs and FILEBROWSER_ITEMIS_FILE) <> 0) then begin
            if ((CompareText(
                   TStrPlus.ExtractFileExtension(item.Caption),
                   BFAFILE_EXTENSION) <> 0)
                and ((lItemIs and FILEBROWSER_ITEMIS_ENCRYPTED) = 0))
              then begin
              item.Selected:=True;
              Inc(m_nNumOfSelFiles);
              Inc(m_qNumOfSelBytes, pItemInfo^.qFileSize);
            end;
          end;
        end;

        SELECT_BYSTRING : begin
          if (not blCaseSens) then
            if (Pos(Uppercase(sSubStr), UpperCase(item.Caption)) > 0) then
              item.Selected:=True
          else
            if (Pos(sSubStr, item.Caption) > 0) then
              item.Selected:=True;
          if (item.Selected) then begin
             Inc(m_nNumOfSelFiles);
             Inc(m_qNumOfSelBytes, pItemInfo^.qFileSize);
          end;
        end;

      end; (* OF CASE *)
    end; (* OF FOR *)
    Items.EndUpdate;
  end; (* OF WITH *)
end;


procedure TFileBrowser.SelectAll;
begin
  Selector(SELECT_ALL);
end;

procedure TFileBrowser.SelectFiles;
begin
  Selector(SELECT_FILES);
end;

procedure TFileBrowser.SelectFolders;
begin
  Selector(SELECT_FOLDERS);
end;

procedure TFileBrowser.SelectEncrypted;
begin
  Selector(SELECT_ENCRYPTED);
end;

procedure TFileBrowser.SelectDecrypted;
begin
  Selector(SELECT_DECRYPTED);
end;

procedure TFileBrowser.SelectByString(sCont : String;
                                      blCaseSens : Boolean = False);
begin
  Selector(SELECT_BYSTRING, sCont, blCaseSens);
end;



procedure TFileBrowser.CopyObjects(sDestPath : String;
                                   msgCB : TMessageCallBack);
var
  nI            : Integer;
  nCount        : Integer;
  nCopied       : Integer;
  nErrors       : Integer;
  blOverwrite   : Boolean;
  blNoOverwrite : BOOL;
  sSource       : String;
  sTarget       : String;
  sSourcePath   : String;
  sActFile      : String;
  sMessage      : String;
  item          : TListItem;
  throwThis     : Exception;
begin
  // source and destination path equal?
  if (CompareText(sDestPath, TStrPlus.PurePath(m_sPath)) = 0) then
    raise EFileBrowserError.Create(m_sr.Get(FB_CFG_ID, 'ERR20'));

  // get the progress message
  sMessage:=m_sr.Get(FB_CFG_ID, 'MSG_COPY');

  // copy all selected files
  sDestPath:=TStrPlus.RTLPath(sDestPath);
  sSourcePath:=TStrPlus.RTLPath(m_sPath);
  blNoOverwrite:=TRUE;
  nCopied:=0;
  nErrors:=0;
  nI:=0;
  nCount:=m_listView.Items.Count;
  throwThis:=Nil;
  while ((nI < nCount) and (throwThis = Nil)) do begin
    item:=m_listView.Items[nI];
    if (item.Selected and
        ((PFileBrowserItemInfo(Item.Data)^.lItemIs and
          FILEBROWSER_ITEMIS_FILE) <> 0)) then begin

      try
        // show what we're doing
        sActFile:=item.Caption;
        m_callBack.SetMessage(Format(sMessage, [sActFile]));
        m_callBack.CallBack;

        // now copy that file
        sSource:=sSourcePath + sActFile;
        sTarget:=sDestPath + sActFile;
        if (CopyFile(PChar(sSource),
                     PChar(sTarget),
                     blNoOverwrite) = FALSE) then begin
          if (GetLastError = ERROR_FILE_EXISTS) then begin

            // file exists, ask for overwrite permission
            m_listView.Items.EndUpdate;  // (avoid dead browser window)
            msgCB.SetStyle(MCB_STYLE_YESNOALLCANCEL);
            msgCB.SetKindOf(MCB_KINDOF_CONFIRMATION);
            msgCB.SetMessage(Format(m_sr.Get(FB_CFG_ID, 'ERR22'),
                                    [sTarget,
                                     TStrPlusI.Sepa1000(m_sr, 
                                       TFileSupport.GetFile64Len(sTarget,
                                                                 m_sr)),
                                     item.Caption,
                                     TStrPlusI.Sepa1000(m_sr, 
                                       TFileSupport.GetFile64Len(sSource,
                                                                 m_sr))]));
            msgCB.CallBack;
            case msgCB.GetResult of
              MCB_RES_YES : blOverwrite:=True;
              MCB_RES_ALL : begin
                blOverwrite:=True;
                blNoOverwrite:=FALSE;
              end;
            else
              blOverwrite:=False;
            end;
            // (an interrupt doesn't matter here)
            m_listView.Items.BeginUpdate;

            // second try
            if (blOverwrite) then
              if (CopyFile(PChar(sSource),
                           PChar(sTarget),
                           FALSE) = FALSE) then
                Inc(nErrors)
              else
                Inc(nCopied);
          end
          else
            Inc(nErrors)
        end
        else
          Inc(nCopied);
      except
        on ECallBackInterrupt do
          throwThis:=EFileBrowserInterrupt.Create(Format(m_sr.Get(FB_CFG_ID,
                                                                  'ERR21'),
                                                         [nCopied]));
        on EFileSupportError do
          Inc(nErrors);
      end;
    end;
    Inc(nI);
  end;

  // clean up
  NoSelect;

  // errors?
  if ((nErrors > 0) and (throwThis = Nil)) then
    throwThis:=EFileBrowserWarning.Create(Format(m_sr.Get(FB_CFG_ID, 'ERR23'),
                                                          [nErrors]));
  // throw exeception, i. n.
  if (throwThis <> Nil) then
    raise throwThis;
end;


procedure TFileBrowser.MoveObjects(sDestPath : String;
                                   msgCB : TMessageCallBack);
var
  lItemIs        : WORD32;
  dwLastError    : DWORD;
  nI             : Integer;
  nCount         : Integer;
  nMoved         : Integer;
  nSaveMoved     : Integer;
  nErrors        : Integer;
  blOverwrite    : Boolean;
  blOverwriteAll : Boolean;
  blIsFile       : Boolean;
  sSource        : String;
  sTarget        : String;
  sSourcePath    : String;
  sActObject     : String;
  sMessage       : String;
  pItemInfo      : PFileBrowserItemInfo;
  item           : TListItem;
  throwThis      : Exception;
begin
  // source and destination path equal?
  if (CompareText(sDestPath, TStrPlus.PurePath(m_sPath)) = 0) then
    raise EFileBrowserError.Create(m_sr.Get(FB_CFG_ID, 'ERR20'));

  // we can only move directories to the same drive
  if ((CompareText(TStrPlus.RootPath(sDestPath),
                   TStrPlus.RootPath(m_sPath)) <> 0) and
       (m_nNumOfSelDirs > 0)) then
    raise EFileBrowserError.Create(m_sr.Get(FB_CFG_ID, 'ERR24'));

  // scanner off
  SuspendBFAScanner;

  // deactivate the auto refresh
  AutoRefresh(False);

  // get the progress message
  sMessage:=m_sr.Get(FB_CFG_ID, 'MSG_MOVE');

  // move all selected files and folders
  m_listView.Items.BeginUpdate;
  sDestPath:=TStrPlus.RTLPath(sDestPath);
  sSourcePath:=TStrPlus.RTLPath(m_sPath);
  nMoved:=0;
  nErrors:=0;
  nI:=0;
  nCount:=m_listView.Items.Count;
  blOverwriteAll:=False;
  throwThis:=Nil;
  while ((nI < nCount) and (throwThis = Nil)) do begin

    item:=m_listView.Items[nI];
    pItemInfo:=PFileBrowserItemInfo(Item.Data);
    lItemIs:=pItemInfo^.lItemIs;
    blIsFile:=((lItemIs and FILEBROWSER_ITEMIS_FILE) <> 0);
    nSaveMoved:=nMoved;
    if (item.Selected and
        (blIsFile or
         ((lItemIs and FILEBROWSER_ITEMIS_FOLDER) <> 0))) then begin

      try
        // show what we're doing
        sActObject:=item.Caption;
        m_callBack.SetMessage(Format(sMessage, [sActObject]));
        m_callBack.CallBack;

        // now copy that file
        sSource:=sSourcePath + sActObject;
        sTarget:=sDestPath + sActObject;
        if (MoveFile(PChar(sSource),
                     PChar(sTarget)) = FALSE) then begin
          dwLastError:=GetLastError;
          if ((dwLastError = ERROR_FILE_EXISTS) or
              (dwLastError = ERROR_ALREADY_EXISTS)) then begin

            // (don't care about existing folders)
            if (blIsFile) then begin

              // file exists, ask for overwrite permission, i.n.
              if (not blOverWriteAll) then begin
                msgCB.SetStyle(MCB_STYLE_YESNOALLCANCEL);
                msgCB.SetKindOf(MCB_KINDOF_CONFIRMATION);
                m_listView.Items.EndUpdate;
                msgCB.SetMessage(Format(m_sr.Get(FB_CFG_ID, 'ERR22'),
                                        [sTarget,
                                         TStrPlusI.Sepa1000(m_sr,
                                           TFileSupport.GetFile64Len(sTarget,
                                                                     m_sr)),
                                         item.Caption,
                                         TStrPlusI.Sepa1000(m_sr,
                                           TFileSupport
                                             .GetFile64Len(sSource,
                                                           m_sr))]));
                msgCB.CallBack;
                case msgCB.GetResult of
                  MCB_RES_YES : blOverwrite:=True;
                  MCB_RES_ALL : begin
                    blOverwrite:=True;
                    blOverwriteAll:=True;
                  end;
                else
                  blOverwrite:=False;
                end;
                m_listView.Items.BeginUpdate;
              end
                else blOverwrite:=True;

              // second try
              if (blOverwrite) then begin
                // (do it the hard way)
                SetFileAttributes(PChar(sTarget), 0);
                DeleteFile(PChar(sTarget));
                if (MoveFile(PChar(sSource),
                             PChar(sTarget)) = FALSE) then
                  Inc(nErrors)
                else
                  Inc(nMoved);
              end
              else
                Inc(nErrors)
            end;
          end
          else
            Inc(nErrors);
        end
        else
          Inc(nMoved);
      except
        on ECallBackInterrupt do
          throwThis:=EFileBrowserInterrupt.Create(Format(m_sr.Get(FB_CFG_ID,
                                                                  'ERR25'),
                                                         [nMoved]));
        on EFileSupportError do
          Inc(nErrors);
      end;
    end;

    // delete the moved item from the browser and update the counting, i. n.
    if (nSaveMoved < nMoved) then begin
      RemoveItem(nI);
      Dec(nCount);
    end
    else
      Inc(nI);
  end;
  m_listView.Items.EndUpdate;

  // clean up
  NoSelect;
  UpdateContentInfo;

  // restart the scanner
  ResumeBFAScanner;

  // (re)enable the auto refresh
  if (m_config.GetBooleanOption(FB_CFGID_AUTOREFRESH)) then
    AutoRefresh(True);

  // errors?
  if ((nErrors > 0) and (throwThis = Nil)) then
    throwThis:=EFileBrowserWarning.Create(Format(m_sr.Get(FB_CFG_ID, 'ERR26'),
                                                          [nErrors]));
  // throw exeception, i. n.
  if (throwThis <> Nil) then
    raise throwThis;
end;


// callback used for recursive directory deleteing
type
  TFileBrowserPSCB = class(TPathSearchCallBack)
  private
    m_nCopyStart : Integer;
  public
    constructor Create(callBackObj : TObject); override;
    procedure CallBack; override;
  end;


constructor TFileBrowserPSCB.Create(callBackObj : TObject);
begin
  // just to speed up things
  inherited Create(callBackObj);
  with callBackObj as TFileBrowser do begin
    m_nCopyStart:=Length(m_sPath) + 1;
    if (m_sPath[m_nCopyStart - 1] <> '\') then
      Inc(m_nCopyStart);
  end;
end;


procedure TFileBrowserPSCB.CallBack;
begin
  with m_callBackObj as TFileBrowser do begin
    // show the relative path only (no file/directory counters currently)
    m_callBack.SetMessage(Format(m_sr.Get(FB_CFG_ID, 'MSG_DELETE'),
                                 [Copy(m_sActFile,
                                       m_nCopyStart,  // (should be ok)
                                       Length(m_sActFile) -
                                         m_nCopyStart + 1)]));
    // map it
    m_callBack.CallBack;
  end;
end;


procedure TFileBrowser.DeleteObjects;
var
  lItemIs      : WORD32;
  nI           : Integer;
  nCount       : Integer;
  nDeleted     : Integer;
  nSaveDeleted : Integer;
  nErrors      : Integer;
  blIsFile     : Boolean;
  sActObject   : String;
  sDeleteThis  : String;
  sDeletePath  : String;
  sMessage     : String;
  pItemInfo    : PFileBrowserItemInfo;
  item         : TListItem;
  pathKiller   : TPathToKill;
  pathKillCB   : TFileBrowserPSCB;
  throwThis    : Exception;
begin

  // scanner off
  SuspendBFAScanner;

  // deactivate the auto refresh
  AutoRefresh(False);

  // get the progress message
  sMessage:=m_sr.Get(FB_CFG_ID, 'MSG_DELETE');

  // remove all selected files and folders
  m_listView.Items.BeginUpdate;
  sDeletePath:=TStrPlus.RTLPath(m_sPath);
  nDeleted:=0;
  nErrors:=0;
  nI:=0;
  nCount:=m_listView.Items.Count;
  throwThis:=Nil;
  while ((nI < nCount) and (throwThis = Nil)) do begin

    item:=m_listView.Items[nI];
    pItemInfo:=PFileBrowserItemInfo(Item.Data);
    lItemIs:=pItemInfo^.lItemIs;
    blIsFile:=((lItemIs and FILEBROWSER_ITEMIS_FILE) <> 0);
    nSaveDeleted:=nDeleted;
    if (item.Selected and
        (blIsFile or
         ((lItemIs and FILEBROWSER_ITEMIS_FOLDER) <> 0))) then begin

      try
        // delete that thing
        sActObject:=item.Caption;
        sDeleteThis:=sDeletePath + sActObject;
        if (not blIsFile) then begin
          // directories need a more complicated removal
          pathKiller:=TPathToKill.Create(sDeleteThis, PATHKILL_ERASEALL, m_sr);
          pathKillCB:=TFileBrowserPSCB.Create(Self);
          try
            pathKiller.Erase(pathKillCB);
            Inc(nDeleted);
          except
            on EPathSearchInterrupted do
              // (will be catched below soon)
              raise ECallBackInterrupt.Create('interrupt');
            on EPathSearchError do
              Inc(nErrors);
          end;
        end
        else begin
          // show the single file deleting
          m_callBack.SetMessage(Format(sMessage, [sActObject]));
          m_callBack.CallBack;

          // remove the file now
          SetFileAttributes(PChar(sDeleteThis), 0);
          if (DeleteFile(PChar(sDeleteThis)) = FALSE) then
            Inc(nErrors)
          else
            Inc(nDeleted);
         end;
      except
        on ECallBackInterrupt do
          throwThis:=EFileBrowserInterrupt.Create(Format(m_sr.Get(FB_CFG_ID,
                                                                  'ERR27'),
                                                         [nDeleted]));
        on EFileSupportError do
          Inc(nErrors);
      end;
    end;

    // delete the deleted item from the browser and update the counting, i. n.
    if (nSaveDeleted < nDeleted) then begin
      RemoveItem(nI);
      Dec(nCount);
    end
    else
      Inc(nI);
  end;
  m_listView.Items.EndUpdate;

  // clean up
  NoSelect;
  UpdateContentInfo;

  // restart the scanner
  ResumeBFAScanner;

  // (re)enable the auto refresh
  if (m_config.GetBooleanOption(FB_CFGID_AUTOREFRESH)) then
    AutoRefresh(True);

  // errors?
  if ((nErrors > 0) and (throwThis = Nil)) then
    throwThis:=EFileBrowserWarning.Create(Format(m_sr.Get(FB_CFG_ID, 'ERR28'),
                                                          [nErrors]));
  // throw exeception, i. n.
  if (throwThis <> Nil) then
    raise throwThis;
end;



procedure TFileBrowser.HandleDragStart(nMode : Integer);
begin
  // get the selected items
  m_nDragDropMode:=nMode;
  case m_nDragDropMode of
    FILEBROWSER_DRAG_COPY : begin
      if (GetNumOfSelObjs <> m_nNumOfSelFiles) then begin
        m_nDragDropMode:=FILEBROWSER_DRAG_INVALID;
        Exit;
      end;
    end;
    FILEBROWSER_DRAG_MOVE : begin
      if (GetNumOfSelObjs <> (m_nNumOfSelFiles + m_nNumOfSelDirs)) then begin
        m_nDragDropMode:=FILEBROWSER_DRAG_INVALID;
        Exit;
      end;
    end;
  else
    m_nDragDropMode:=FILEBROWSER_DRAG_INVALID;
  end;
end;


function TFileBrowser.GetDropTarget(nX, nY : Integer) : TListItem;
var
  lItemIs   : WORD32;
  pItemInfo : PFileBrowserItemInfo;
begin
  // assume nothing good
  Result:=Nil;

  // quit immediately if the drop and drop was invalid from the beginning
  if (m_nDragDropMode = FILEBROWSER_DRAG_INVALID) then
    Exit;

  // over an item?
  Result:=m_listView.GetItemAt(nX, nY);
  if (Result = Nil) then
    Exit;

  // we can only copy or move to a folder (".." also allowed) or a
  // (non CD-ROM) drive
  pItemInfo:=PFileBrowserItemInfo(Result.Data);
  lItemIs:=pItemInfo^.lItemIs;
  if (((lItemIs and FILEBROWSER_ITEMIS_FOLDER) <> 0) or
      ((lItemIs and FILEBROWSER_ITEMIS_UPONELEVEL) <> 0)) then begin
    if (m_blReadOnlyDrive) then
      Result:=Nil;
    Exit;
  end;
  if (((lItemIs and FILEBROWSER_ITEMIS_DRIVE) <> 0) and
      (pItemInfo^.lAttrTypePIDL <> DRIVE_CDROM)) then
    Exit;
  Result:=Nil;
end;



function TFileBrowser.HandleDragOver(nX, nY : Integer) : Boolean;
begin
  Result:=(GetDropTarget(nX, nY) <> Nil);
end;


procedure TFileBrowser.HandleDragDrop(nX, nY : Integer;
                                      msgCB : TMessageCallBack);
var
  lItemIs   : WORD32;
  actItem   : TListItem;
  sDestPath : String;
begin
  // operation allowed?
  actItem:=GetDropTarget(nX, nY);
  if (actItem = Nil) then
    Exit;

  // get the destination path
  lItemIs:=PFileBrowserItemInfo(actItem.Data)^.lItemIs;
  if (((lItemIs and FILEBROWSER_ITEMIS_FOLDER) <> 0) or
      ((lItemIs and FILEBROWSER_ITEMIS_UPONELEVEL) <> 0)) then begin
    sDestPath:=TStrPlus.RTLPath(m_sPath) + actItem.Caption
  end
  else
    sDestPath:=Copy(actItem.Caption, Pos(':', actItem.Caption) - 1, 2) + '\';

  // execute and clean up afterwards
  try
    case m_nDragDropMode of
      FILEBROWSER_DRAG_COPY : CopyObjects(sDestPath, msgCB);
      FILEBROWSER_DRAG_MOVE : MoveObjects(sDestPath, msgCB);
    end;
  except
    on EFileBrowserError do begin
      // clean up before tunneling the message through
      NoSelect;
      raise;
    end;
  end;
  NoSelect;
end;


function TFileBrowser.GetSelectedObject(blWithPath : Boolean = False) : String;
var
   item : TListItem;
begin
  // only one selected file for folder is valid
  if (m_listView.SelCount <> 1) then
    raise EFileBrowserError.Create('more than one/no object was selected');
  item:=m_listView.Selected;
  if ((PFileBrowserItemInfo(item.Data)^.lItemIs and
       (FILEBROWSER_ITEMIS_FILE or FILEBROWSER_ITEMIS_FOLDER)) = 0) then
    raise EFileBrowserError.Create('selected item is not a file or folder');
  if (blWithPath) then
    Result:=TStrPlus.RTLPath(m_sPath) + item.Caption
  else
    Result:=item.Caption;
end;


function TFileBrowser.IsReadOnlyDrive : Boolean;
begin
  Result:=m_blReadOnlyDrive;
end;


procedure TFileBrowser.RenameObjects(renReq : TFileBrowserRenameRequest;
                                     errCB : TMessageCallBack);
var
  nI, nCount  : Integer;
  dwSaveAttr  : DWORD;
  dwLastError : DWORD;
  lItemIs     : WORD32;
  sOldName    : String;
  sNewName    : String;
  sMoveName   : String;
  sTypeInfo   : String;
  item        : TListItem;
begin
  // work through all items
  nI:=0;
  AutoRefresh(False);
  SuspendBFAScanner;
  nCount:=m_listView.Items.Count;
  while (nI < nCount) do begin

    // selected file or folder?
    item:=m_listView.Items[nI];
    lItemIs:=PFileBrowserItemInfo(item.Data)^.lItemIs;
    if ((item.Selected) and
        ((lItemIs and (FILEBROWSER_ITEMIS_FILE or
                       FILEBROWSER_ITEMIS_FOLDER)) <> 0)) then begin

      // request the new name (user may cancel the whole operation!)
      sOldName:=item.Caption;
      if (not renReq.Request(sOldName, sNewName)) then
        Exit;
      sOldName:=TStrPlus.RTLPath(m_sPath) + sOldName;

      // don't trust the given name, cut off leading and ending spaces
      sNewName:=Trim(sNewName);
      sMoveName:=TStrPlus.RTLPath(m_sPath) + sNewName;

      // rename now (transfer the attributes, too)
      dwSaveAttr:=GetFileAttributes(PChar(sOldName));
      SetFileAttributes(PChar(sOldName), 0);  // (enable access)
      if (MoveFile(PChar(sOldName), PChar(sMoveName)) = FALSE) then begin

        // if the file name already exists then we give the user another
        // chance to enter a valid file name
        dwLastError:=GetLastError;
        SetFileAttributes(PChar(sOldName), dwSaveAttr);

        // report the error (user break may occur)
        try
          if (dwLastError = ERROR_ALREADY_EXISTS) then begin
            errCB.SetStyle(MCB_STYLE_YESNOCANCEL);
            errCB.SetKindOf(MCB_KINDOF_CONFIRMATION);
            errCB.SetMessage(Format(m_sr.Get(FB_CFG_ID, 'ERR41'),
                                             [sNewName]));
            errCB.CallBack;
            if (errCB.GetResult = MCB_RES_YES) then
              Dec(nI);  // (tricky, yes)
          end
          else begin
            errCB.SetStyle(MCB_STYLE_YESNO);
            errCB.SetMessage(Format(m_sr.Get(FB_CFG_ID, 'ERR42'),
                                             [sOldName, sNewName]));
            errCB.CallBack;
            if (errCB.GetResult = MCB_RES_NO) then begin
              if (m_config.GetBooleanOption(FB_CFGID_AUTOREFRESH)) then
                AutoRefresh(True);
              ResumeBFAScanner;
              Exit;
            end;
          end;
        except
          on ECallBackInterrupt do begin
            if (m_config.GetBooleanOption(FB_CFGID_AUTOREFRESH)) then
              AutoRefresh(True);
            ResumeBFAScanner;
            Exit;
          end;
        end;
      end
      else begin
        SetFileAttributes(PChar(sMoveName), dwSaveAttr);

        // now get the new icon and type for the renamed file/folder
        with item do begin
          Caption:=sNewName;
          ImageIndex:=m_iconLookup.GetIndex(sMoveName,
                                            sTypeInfo,
                                            True);
          SubItems[COL_TYPE - 1]:=sTypeInfo;
        end;
      end;
    end;

    Inc(nI);
  end;
  if (m_config.GetBooleanOption(FB_CFGID_AUTOREFRESH)) then
    AutoRefresh(True);
  ResumeBFAScanner;
end;


procedure TFileBrowser.NewFolders(names : TStringList);
var
  nI           : Integer;
  nPos         : Integer;
  blAbsPath    : Boolean;
  blBaseExists : Boolean;
  pItemInfo    : PFileBrowserItemInfo;
  sName        : String;
  sRTLRoot     : String;
  sNewFolder   : String;
  sTypeInfo    : String;
  throwThis    : EFileBrowserError;
  item         : TListItem;
  hSearch      : THandle;
  findData     : TWin32FindData;
begin

  nI:=0;
  throwThis:=Nil;

  while ((throwThis = Nil) and (nI < names.Count)) do begin

    sName:= names.Strings[nI];
    Inc(nI);

    // check before creating the folder(s)
    blAbsPath:=((Copy(sName, 1, 1) = '\') or (Copy(sName, 2, 1) = ':'));
    if (blAbsPath) then
      sRTLRoot:=''
    else
      sRTLRoot:=TStrPlus.RTLPath(m_sPath);
    sName:=Trim(sName);

    // success?
    AutoRefresh(False);
    throwThis:=Nil;
    if (blAbsPath) then
    begin

      // now make them (even if the whole structure already exists
      // it won't cause an error)
      ForceDirectories(sName);

      // if we have an absolute path we don't need to create a new folder icon
      // (FIXME: perhaps some clever routine to detect a possible change?)
      if (not DirectoryExists(sName)) then
        throwThis:=EFileBrowserError.Create(Format(m_sr.Get(FB_CFG_ID, 'ERR50'),
                                                   [sName]));
    end
    else begin
      // detect the nearest folder to the root (which will also create a new
      // folder icon)
      nPos:=Pos('\', sName);
      if (nPos = 0) then
        sNewFolder:=sName
      else
        sNewFolder:=Copy(sName, 1, nPos - 1);

      // check if the 'base' folder already exist (needed below)
      blBaseExists:=DirectoryExists(sRTLRoot + sNewFolder);

      // (same as above)
      ForceDirectories(sRTLRoot + sName);

      // does this folder exist?
      if (not DirectoryExists(sRTLRoot + sNewFolder)) then
      begin
        // no folder, no fun
        throwThis:=EFileBrowserError.Create(Format(m_sr.Get(FB_CFG_ID, 'ERR50'),
                                                 [sName]))
      end
      else
      begin

        if (not blBaseExists) then
        begin

          // we can now create the icon, place it at the end of the current
          // directory list
          item:=m_listView.Items.Insert(GetInsertPos(True) + 1);
          item.SubItems.BeginUpdate;
          with item do begin
            Caption:=sNewFolder;
            ImageIndex:=m_iconLookup.GetIndex(sRTLRoot + sNewFolder,
                                              sTypeInfo,
                                              True);
            // (a folder has no size)
            SubItems.Add('');
            // this is weird, but there's no other way: get the creation time
            // and the attributes of created folder via a search operation
            hSearch:=FindFirstFile(PChar(sRTLRoot + sNewFolder),
                                   findData);
            if (hSearch = INVALID_HANDLE_VALUE) then
            begin
              // (this should never happen)
              SubItems.Add('???');
              SubItems.Add('???');
              SubItems.Add('???');
              with findData do begin
                ftCreationTime.dwLowDateTime:=0;  // (the need for cheat)
                ftLastWriteTime.dwHighDateTime:=0;
                dwFileAttributes:=0;
              end;
            end
            else
            begin
              Windows.FindClose(hSearch);
              SubItems.Add(TStrPlusI.Win32FileTimeToStr(
                             m_sr,
                             findData.ftCreationTime));
              SubItems.Add(TStrPlus.FileAttrToStr(findData.dwFileAttributes));
              SubItems.Add(sTypeInfo);
            end;

            // don't forget to add an item info
            pItemInfo:=AddItemInfo;
            with pItemInfo^ do
            begin
              lItemIs:=FILEBROWSER_ITEMIS_FOLDER;
              qFileTime:=MakeWORD64(findData.ftCreationTime.dwLowDateTime,
                                    findData.ftCreationTime.dwHighDateTime);
              lAttrTypePIDL:=findData.dwFileAttributes;
            end;
            Data:=pItemInfo;

            // we have to update the browser, too
            Inc(m_nNumOfFolders);
            UpdateContentInfo;
          end;
          
          item.SubItems.EndUpdate;
        end;

        // full path completely created?
        if (not DirectoryExists(sRTLRoot + sName)) then
        begin
          // (this will only cause a warning)
          throwThis:=EFileBrowserWarning.Create(Format(m_sr.Get(FB_CFG_ID,
                                                                'ERR51'),
                                                       [sName]));
        end;                                                       
      end;
    end;
    if (m_config.GetBooleanOption(FB_CFGID_AUTOREFRESH)) then
      AutoRefresh(True);
  end;

  names.Destroy;

  // error/warning to report?
  if (throwThis <> Nil) then
    raise throwThis;
end;


function TFileBrowser.CheckSelection(nType : Integer) : Boolean;
var
  nCheck : Integer;
begin
  // assume a failure
  Result:=False;

  // check the readonly flag first
  if (((nType and FILEBROWSER_SELECTED_NOTREADONLY) <> 0) and
      IsReadOnlyDrive) then
    Exit;

  // don't accept others or no selections
  if ((GetNumOfSelOther > 0) or (GetNumOfSelObjs = 0)) then
    Exit;

  // now check the single flags
  nCheck:=0;
  if (GetNumOfSelDrives > 0) then begin
    nCheck:=nCheck or FILEBROWSER_SELECTED_SINGLEDRIVE;
    // is the (one and only) selected drive formattable?
    if (1 = GetNumOfSelDrives) then
      if (0 <> (nCheck and FILEBROWSER_SELECTED_SINGLEDRIVE)) then begin
        case PFileBrowserItemInfo(m_listView.Selected.Data)^.lAttrTypePIDL of
          DRIVE_REMOVABLE, DRIVE_FIXED, DRIVE_RAMDISK:
            nCheck:=nCheck or FILEBROWSER_SELECTED_FORMATTABLE;
        end;
      end;
  end;
  if (GetNumOfSelDirs > 0) then
    nCheck:=nCheck or FILEBROWSER_SELECTED_SINGLEFOLDER;
  if (GetNumOfSelFiles > 0) then
    nCheck:=nCheck or FILEBROWSER_SELECTED_SINGLEFILE;
  if ((nType and nCheck) <> 0) then begin
    Result:=(GetNumOfSelObjs = 1);
    Exit;
  end;

  // do the final check
  nCheck:=0;
  if (GetNumOfSelDrives > 0) then
    nCheck:=nCheck or FILEBROWSER_SELECTED_DRIVES;
  if (GetNumOfSelDirs > 0) then
    nCheck:=nCheck or FILEBROWSER_SELECTED_FOLDERS;
  if (GetNumOfSelFiles > 0) then
    nCheck:=nCheck or FILEBROWSER_SELECTED_FILES;
  Result:=((nCheck and nType) = nCheck);

end;


procedure TFileBrowser.ResumeBFAScanner;
begin
  if (not m_blScannerReady) then
    Exit;
  if (m_scanner <> Nil) then
    SuspendBFAScanner;
  try
    // cipher available?
    m_scanner:=TBFAScanner.Create(Self,
                                  m_scanKey,
                                  m_bfaSupport.GetCipherManager
                                              .GetCurrentCipher,
                                  m_config.GetBooleanOption(
                                    FB_CFGID_SCANBFAFILESONLY),
                                  m_config.GetBooleanOption(
                                    FB_CFGID_REPLACESCANICONS),
                                  m_sr);
  except
    on ebse : EBFAScannerError do begin
      m_scanner:=Nil;
      raise EFileBrowserError.Create(ebse.Message);
    end;
  end;
  m_scanner.Resume;
  if (m_saveScanner <> Nil) then begin  // don't ask why - it's the law!
    m_saveScanner.Destroy;
    m_saveScanner:=Nil;
  end;
end;


procedure TFileBrowser.SuspendBFAScanner;
begin
  if (not m_blScannerReady) then
    Exit;
  if (m_scanner <> Nil) then begin
    if (m_scanner.AccessRunningFlag) then begin
      m_scanner.Terminate;
      m_scanner.WaitFor;
    end;
    m_saveScanner:=m_scanner;
    m_scanner:=Nil;
  end;
end;

procedure TFileBrowser.StartBFAScanner;
var
  newKey : TKeyMemory;
begin
  // get a key
  newKey:=m_bfaSupport.GetPasswordInput.Execute(PASSWINPUT_MODE_SCANNER);
  if (newKey = Nil) then
    raise EFileBrowserInterrupt.Create(m_sr.Get(FB_CFG_ID, 'USERBREAK'));
  m_scanKey:=newKey;

  m_blScannerReady:=True;
  ResumeBFAScanner;
end;

procedure TFileBrowser.StopBFAScanner;
begin
  SuspendBFAScanner;
  if (m_scanKey <> Nil) then begin
    m_scanKey.Destroy;
    m_scanKey:=Nil;
  end;
  m_blScannerReady:=False;
end;

function TFileBrowser.IsBFAScannerReady : Boolean;
begin
  Result:=m_blScannerReady;
end;


procedure TFileBrowser.GetSelectedFilesAndFolders(files : TStringList;
                                                  folders : TStringList);
var
  nI      : Integer;
  nCount  : Integer;
  lItemIs : WORD32;
  sPath   : String;
  item    : TListItem;
begin
  // FIXME: how can we make this search AFAP?

  sPath:=TStrPlus.RTLPath(m_sPath);
  nI:=0;
  nCount:=m_listView.Items.Count;
  files.BeginUpdate;
  folders.BeginUpdate;
  while (nI < nCount) do begin

    // selected file or folder?
    item:=m_listView.Items[nI];
    if (item.Selected) then begin

      lItemIs:=PFileBrowserItemInfo(item.Data)^.lItemIs;
      if ((lItemIs and FILEBROWSER_ITEMIS_FILE) <> 0) then
        files.Add(sPath + item.Caption)
      else
        if ((lItemIs and FILEBROWSER_ITEMIS_FOLDER) <> 0) then
          folders.Add(sPath + item.Caption);
    end;
    Inc(nI);
  end;
  folders.EndUpdate;
  files.EndUpdate;
end;


function TFileBrowser.GetSingleSelectedFile : String;
var
  selItem : TListItem;
  lItemIs : WORD32;
begin
  Result:='';
  selItem:=m_listView.Selected;
  if (selItem = Nil) then
    Exit;
  lItemIs:=PFileBrowserItemInfo(selItem.Data)^.lItemIs;
  if ((lItemIs and FILEBROWSER_ITEMIS_FILE) <> 0) then begin
    Result:=TStrPlus.RTLPath(m_sPath) + selItem.Caption;
  end;
end;



procedure TFileBrowser.AutoRefresh(blActive : Boolean);
begin
  if (m_changesNotifier <> Nil) then begin
    m_changesNotifier.Shutdown;
    m_changesNotifier.Free;
    m_changesNotifier:=Nil;
  end;

  if (blActive) then begin
    m_changesNotifier:=TFileChangesNotifier.Create(Self);
    m_changesNotifier.Resume;
  end;
end;


procedure TFileBrowser.DoAutoRefresh(blState : Boolean);
begin
  if (blState) then begin
    if (m_changesNotifier = Nil) then
      AutoRefresh(True);
  end
  else begin
    if (m_changesNotifier <> Nil) then
      AutoRefresh(False);
  end;
end;

function TFileBrowser.IsAutoRefreshActive : Boolean;
begin
  Result:=(m_changesNotifier <> Nil);
end;


procedure TFileBrowser.Update(blSort : Boolean = False);
const
  FLAG_UNCHECKED = 0;
  FLAG_CHECKED   = 100;
  FLAG_REMOVE    = 101;
  FLAG_IGNORE    = 200;
var
  qFileSize : WORD64;
  qFileTime : WORD64;
  lItemIs   : WORD32;
  lTemp     : WORD32;
  nI        : Integer;
  nUpIdx    : Integer;
  sFullPath : String;
  hSearch   : THandle;
  dta       : TWin32FindData;
  actItem   : TListItem;
  fileInfo  : TSHFileInfo;
  pItemInfo : PFileBrowserItemInfo;

// to add a new file or folder to the list
procedure AddItem;
var
  newItem      : TListItem;
  sTypeInfo    : String;
  pNewItemInfo : PFileBrowserItemInfo;
begin

  newItem:=m_listView.Items.Insert(GetInsertPos);

  pNewItemInfo:=AddItemInfo;
  newItem.Data:=pNewItemInfo;

  newItem.Caption:=dta.cFileName;
  pNewItemInfo^.lReserved0:=FLAG_IGNORE;
  pNewItemInfo^.lItemIs:=lItemIs;
  pNewItemInfo^.lAttrTypePIDL:=dta.dwFileAttributes;
  pNewItemInfo^.qFileTime:=MakeWORD64(dta.ftLastWriteTime.dwLowDateTime,
                                      dta.ftLastWriteTime.dwHighDateTime);

  if (lItemIs = FILEBROWSER_ITEMIS_FILE) then begin
    pNewItemInfo^.qRealFileSize:=-1;
    pNewItemInfo^.qFileSize:=qFileSize;
    newItem.ImageIndex:=m_iconLookup.GetIndex(sFullPath, sTypeInfo);
    with newItem.SubItems do begin
      Add('');
      Add(TStrPlusI.Sepa1000(m_sr, pNewItemInfo^.qFileSize));
      Add('');
    end;
    Inc(m_nNumOfFiles);
    Inc(m_qNumOfBytes, qFileSize);
  end
  else begin
    newItem.ImageIndex:=m_iconLookup.GetIndex(sFullPath, sTypeInfo, True);
    with newItem.SubItems do begin
      Add('');
      Add('');
      Add('');
    end;
    Inc(m_nNumOfFolders);
  end;
  with newItem.SubItems do begin
    Add(TStrPlusI.Win32FileTimeToStr(m_sr, dta.ftLastWriteTime));
    Add(TStrPlus.FileAttrToStr(dta.dwFileAttributes));
    Add(sTypeInfo);
  end;
end;

begin

  // nothing to do in pure network view
  if (m_blPureNetWork) then
    Exit;

  // suspend any possible scanning action
  SuspendBFAScanner;

  // prepare the item flags
  nUpIdx:=m_listView.Items.Count - 1;
  for nI:=0 to nUpIdx do begin
    pItemInfo:=PFileBrowserItemInfo(m_listView.Items[nI].Data);
    if ((pItemInfo^.lItemIs and
         (FILEBROWSER_ITEMIS_FILE or FILEBROWSER_ITEMIS_FOLDER)) <> 0) then
      pItemInfo^.lReserved0:=FLAG_UNCHECKED
    else
      pItemInfo^.lReserved0:=FLAG_IGNORE;
  end;

  // lock the list view
  m_listView.Items.BeginUpdate;

  // check for new or lost items
  hSearch:=Windows.FindFirstFile(PChar(GetCurrentPath), dta);
  if (hSearch <> INVALID_HANDLE_VALUE) then begin

    repeat

      // skip special folders
      if ((dta.cFileName <> '..') and
          (StrComp(dta.cFileName, '.') <> 0)) then begin

        // what kind of item do we have?
        if ((dta.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0) then
          lItemIs:=FILEBROWSER_ITEMIS_FOLDER
        else
          lItemIs:=FILEBROWSER_ITEMIS_FILE;

        // build the full pathname for further purposes
        sFullPath:=TStrPlus.RTLPath(m_sPath) + dta.cFileName;

        // search for an exisiting entry
        nUpIdx:=m_listView.Items.Count;
        nI:=0;
        while (nI < nUpIdx) do begin
          actItem:=m_listView.Items[nI];
          pItemInfo:=PFileBrowserItemInfo(actItem.Data);
          if ((pItemInfo^.lItemIs and
               (FILEBROWSER_ITEMIS_FILE or FILEBROWSER_ITEMIS_FOLDER)
              ) <> 0) then begin
            if (pItemInfo^.lReserved0 = FLAG_UNCHECKED) then begin
              if (CompareText(dta.cFileName, actItem.Caption) = 0) then
                Break;
            end;
          end;
          Inc(nI);
        end;

        // build the file size
        qFileSize:=MakeWORD64(dta.nFileSizeLow, dta.nFileSizeHigh);

        // in the list?
        if (nI = nUpIdx) then begin

          // no, but does the attribute qualify the item to be added?
          if ((m_nExclAttrMask and dta.dwFileAttributes) = 0) then
            AddItem;

        end
        else begin

          // get the current item
          actItem:=m_listView.Items[nI];
          pItemInfo:=PFileBrowserItemInfo(actItem.Data);
          pItemInfo^.lReserved0:=FLAG_CHECKED; // (may be overwritten below)

          // check for a file/folder change first (a very rare
          // situation, of course)
          if ((pItemInfo^.lItemIs and
              (FILEBROWSER_ITEMIS_FILE or FILEBROWSER_ITEMIS_FOLDER))
              <> lItemIs) then begin

            // replace by remove + new
            pItemInfo^.lReserved0:=FLAG_REMOVE;
            AddItem;

          end
          else begin

            // attributes still the same?
            if (pItemInfo^.lAttrTypePIDL <> dta.dwFileAttributes) then begin

              // need to show the item any longer?
              if ((m_nExclAttrMask and dta.dwFileAttributes) = 0) then begin
                pItemInfo^.lAttrTypePIDL:=dta.dwFileAttributes;
                actItem.SubItems[COL_ATTR - 1]:=
                  TStrPlus.FileAttrToStr(dta.dwFileAttributes);
              end
              else begin
                pItemInfo^.lReserved0:=FLAG_REMOVE;
              end;
            end;

            if (pItemInfo^.lReserved0 <> FLAG_REMOVE) then begin

              // file (if any) size changed?
              if (lItemIs = FILEBROWSER_ITEMIS_FILE) then begin
                if (pItemInfo^.qFileSize <> qFileSize) then begin

                  // modify the content state
                  Dec(m_qNumOfBytes, pItemInfo^.qFileSize);
                  Inc(m_qNumOfBytes, qFileSize);
                  if (actItem.Selected) then begin
                    Dec(m_qNumOfSelBytes, pItemInfo^.qFileSize);
                    Inc(m_qNumOfSelBytes, qFileSize);
                  end;

                  // show the new size
                  pItemInfo^.qFileSize:=qFileSize;
                  actItem.SubItems[COL_SIZE - 1]:=
                    TStrPlusI.Sepa1000(m_sr, qFileSize);

                  // this guy has to be scanned again
                  pItemInfo^.lItemIs:=FILEBROWSER_ITEMIS_FILE;

                  // (clear all necessary columns and reset the icon)
                  actItem.SubItems[COL_REALNAME - 1]:='';
                  actItem.SubItems[COL_REALSIZE - 1]:='';
                  SHGetFileInfo(PChar(sFullPath), 0, fileInfo, SizeOf(fileInfo),
                                SHGFI_SMALLICON or SHGFI_ICON or
                                SHGFI_DISPLAYNAME or SHGFI_SYSICONINDEX);
                  actItem.ImageIndex:=fileInfo.iIcon;
                end;

              end;

              // date+time changed?
              qFileTime:=MakeWORD64(dta.ftLastWriteTime.dwLowDateTime,
                                    dta.ftLastWriteTime.dwHighDateTime);
              if (pItemInfo^.qFileTime <> qFileTime) then begin
                pItemInfo^.qFileTime:=qFileTime;
                actItem.SubItems[COL_TIME - 1]:=
                  TStrPlusI.Win32FileTimeToStr(m_sr, dta.ftLastWriteTime)
              end;

            end;


          end; (* OF FILE-FOLDER XCHANGE *)
        end; (* OF IN THE LIST *)

      end; (* OF IF *)
    until (Windows.FindNextFile(hSearch, dta) = FALSE);
  end;
  Windows.FindClose(hSearch);

  // now remove all nonexisting items
  nUpIdx:=m_listView.Items.Count;
  nI:=0;
  while (nI < nUpIdx) do begin
    pItemInfo:=PFileBrowserItemInfo(m_listView.Items[nI].Data);
    lTemp:=pItemInfo^.lReserved0;
    if ((lTemp = FLAG_REMOVE) or (lTemp = FLAG_UNCHECKED)) then begin
      RemoveItem(nI);
      Dec(nUpIdx);
    end
    else begin
      Inc(nI);
    end;
  end;

  // sort, if necessary
  if (blSort) then begin
    m_listView.CustomSort(@CompareItems,
                          m_config.GetIntegerOption(FB_CFGID_SORT));
  end;

  // modifications finished
  m_listView.Items.EndUpdate;

  // invalidate the icon cache
  m_iconLookup.ClearTable;

  // send the changes signal
  UpdateContentInfo;
  if (GetNumOfSelObjs = 0) then
    m_callBack.SetMessage(GetContentInfo)
  else
    m_callBack.SetMessage(GetSelectedInfo);
  try
    m_callBack.CallBack;
  except
    on ECallBackInterrupt do begin
      // (don't care about any interrupt here)
    end;
  end;

  // (re)start the scanner
  ResumeBFAScanner;

end;


function TFileBrowser.GetInsertPos(blBestForDir : Boolean = False) : Integer;
var
  nI        : Integer;
  nLen      : Integer;
  pItemInfo : PFileBrowserItemInfo;

function IsDrive(nIdx : Integer) : Boolean;
begin
  pItemInfo:=PFileBrowserItemInfo(m_listView.Items[nIdx].Data);
  Result:=((pItemInfo^.lItemIs and
           (FILEBROWSER_ITEMIS_DRIVE or FILEBROWSER_ITEMIS_NETWORKROOT)) <> 0);
end;

function IsFolder(nIdx : Integer) : Boolean;
begin
  pItemInfo:=PFileBrowserItemInfo(m_listView.Items[nIdx].Data);
  Result:=((pItemInfo^.lItemIs and FILEBROWSER_ITEMIS_FOLDER) <> 0);
end;

begin
  nLen:=m_listView.Items.Count;

  // finding the best new folder position needs a whole scan of the
  // current browser view
  if (blBestForDir) then begin
    Result:=0;

    for nI:=0 to (nLen - 1) do begin
      if (IsFolder(nI)) then
        Result:=nI;
    end;

    Exit;
  end;

  // we play an outsider role here, trying to find the drive list at the
  // beginning (files will be inserted at the end of the list) or at the end
  // (items are going to be inserted before the drives)

  if (m_config.GetBooleanOption(FB_CFGID_PLACEDRIVESFIRST)) then begin
    Result:=nLen;
  end
  else begin
    Result:=nLen;
    if (Result = 0) then
      Exit
    else
      Dec(Result);
    // (jump over the network icon)
    if (not IsDrive(Result)) then
      Dec(Result);
    if (Result < 1) then begin  // (don't take any chances)
      Result:=0
    end
    else begin
      repeat
        Dec(Result);
        if (Result < 0) then Break;
      until (not IsDrive(Result));
      Inc(Result);
    end;
  end;

end;


function TFileBrowser.IsReadOnlyPath : Boolean;
var
  sPath : String;
begin
  // FIXME: currently we're only checking for CD-ROM drives, but it might
  //        ne interesting to know readonly network paths, too
  Result:=False;
  sPath:=GetCurrentPath(False);
  if (Length(sPath) > 1) then
    if (sPath[2] = ':') then
      Result:=(GetDriveType(PChar(sPath[1] + ':\')) = DRIVE_CDROM);
end;



//////////////////////////// TBFAScanner ////////////////////////////

const
  NO_FILE_SIZE = -1; // to detect an error


procedure TBFAScanner.GetCountOfItems;
begin
  m_nItemsCount:=m_fbrowser.m_listView.Items.Count;
end;


procedure TBFAScanner.SetFileItem;
begin
  with m_fbrowser.m_listView.Items[m_nActIndex] do begin

    if ((m_qRealFileSize <> NO_FILE_SIZE) and m_blReplIco) then begin
      ImageIndex:=m_nImageIndex;
      // real file type are put in brackets
      SubItems[COL_TYPE - 1]:='(' + m_sRealType + ')';
    end;

    SubItems[COL_REALNAME - 1]:=m_sRealFileName;
    if (m_qRealFileSize = NO_FILE_SIZE) then
      SubItems[COL_REALSIZE - 1]:=''
    else
      SubItems[COL_REALSIZE - 1]:=TStrPlusI.Sepa1000(m_sr, m_qRealFileSize);
    with PFileBrowserItemInfo(Data)^ do begin
      lItemIs:=m_lItemIs;
      qRealFileSize:=m_qRealFileSize;
    end;
  end;
end;

procedure TBFAScanner.GetFileItem;
begin
  with m_fbrowser.m_listView do begin
    m_lItemIs:=PFileBrowserItemInfo(Items[m_nActIndex].Data)^.lItemIs;
    m_sCaption:=Items[m_nActIndex].Caption;
    // file?
    if ((m_lItemIs and FILEBROWSER_ITEMIS_FILE) <> 0) then begin
      // already scanned?
      if ((m_lItemIs and FILEBROWSER_ITEMIS_SCANNED) = 0) then begin
        // check extension for .BFA?
        if (m_blBFAOnly) then begin
          m_blResult:=(CompareText(TStrPlus.ExtractFileExtension(m_sCaption),
                                   BFAFILE_EXTENSION) = 0);
        end
        else
          m_blResult:=True;
      end
      else
        m_blResult:=False;
    end
    else
      m_blResult:=False;
  end;
end;

constructor TBFAScanner.Create(fbrowser : TFileBrowser;
                               scanKey : TKeyMemory;
                               sCipherName : String;
                               blScanBFAFilesOnly : Boolean;
                               blReplaceIcons : Boolean;
                               sr : TStrRes);
begin
  m_bfaFile:=Nil;
  m_blBFAOnly:=blScanBFAFilesOnly;
  m_blReplIco:=blReplaceIcons;
  InitializeCriticalSection(m_critSect);
  m_scanKey:=scanKey;
  m_sr:=sr;
  inherited Create(True);
  try
    m_bfaFile:=TBFAFile.Create(sCipherName, m_sr);
  except
    on ebfe : EBFAFileError do
      raise EBFAScannerError.Create(ebfe.Message);
  end;
  m_fbrowser:=fbrowser;
  AccessRunningFlag(False, False);
end;

destructor TBFAScanner.Destroy;
var
  saveCritSect : TRTLCriticalSection;
  saveBfaFile  : TBFAFile;
begin
  // (FIXME: do we need these saves?)
  saveCritSect:=m_critSect;
  saveBfaFile:=m_bfaFile;
  inherited Destroy;
  if (saveBfaFile <> Nil) then
    saveBfaFile.Destroy;
  DeleteCriticalSection(saveCritSect);
end;


procedure TBFAScanner.Execute;
var
  blScanned : Boolean;
  sTemp     : String;
  sFileName : String;
  setup     : TBFAFileDecryptSetup;
  bres      : TBFAFileDecryptResult;
  iil       : TIconIndexLookup;
  sr        : TStrRes;
begin
  AccessRunningFlag(False, True);
  Synchronize(GetCountOfItems);
  setup:=TBFAFileDecryptSetup.Create;
  with setup do begin
    // (we just set all parameters, although most of them won't be used)
    SetPassword(m_scanKey);
    SetTargetPath('');
    SetRemoveSource(False);
    SetOverwriteExisting(False);
    SetNoCache(False);
    SetIgnoreCRC32(False);
    SetRestorePath(False);
    SetNoKeyCheck(False);
    SetAcceptTruncated(True);
    SetFileInfoOnly(True); // (this is the most important flag!)
    SetDirectHeaderInfo(False);
  end;

  // we use an icon index lookup
  iil:=TIconIndexLookup.Create;

  sr:=m_fbrowser.m_sr; // (we can do this thanks to the unit scope)
  m_nActIndex:=0;
  while ((m_nActIndex < m_nItemsCount) and (not Terminated)) do begin

    // one of our synchonizes which seem to have problems after a restared
    // scanner: in such a case its recommended to call
    // Application.ProcessMessages() before (place this call well to avoid
    // unexpcted message handling!)
    Synchronize(GetFileItem);

    if (m_blResult) then begin

      // build the file name
      sFileName:=TStrPlus.RTLPath(ExtractFilePath(m_fbrowser.GetCurrentPath)) +
                 m_sCaption;

      m_qRealFileSize:=NO_FILE_SIZE;  // (assume an error)
      blScanned:=False;
      try
        // get the real file name and size
        bres:=m_bfaFile.Decrypt(sFileName,
                                setup);
        sTemp:=bres.GetOriginalFileName;
        m_qRealFileSize:=bres.GetBytesWritten;
        bres.Destroy;

        // change the flag
        m_lItemIs:=m_lItemIs or FILEBROWSER_ITEMIS_ENCRYPTED;
        blScanned:=True;

        // try to get the icon (and type)
        m_nImageIndex:=iil.GetIndexByExtension(
                         TStrPlus.ExtractFileExtension(sTemp),
                         m_sRealType);

      except
        // (we try to visualize some errors, if a common error occured we
        //  won't mark the file as scanned to give the user a second chance)
        on EBFAFileNoCryptFile      do begin sTemp:=''; blScanned:=True; end;
        on EBFAFileWrongPassword    do sTemp:=sr.Get(FB_CFG_ID, 'SCANERR01');
        on EBFAFileWrongCipherType  do sTemp:=sr.Get(FB_CFG_ID, 'SCANERR02');
        on EBFAFileFileNotFound     do sTemp:=sr.Get(FB_CFG_ID, 'SCANERR03');
        on EBFAFileUnknownFormat    do sTemp:=sr.Get(FB_CFG_ID, 'SCANERR04');
        on ebff : EBFAFileFatal do begin
          // we break on fatal errors
          setup.Destroy;
          AccessRunningFlag(False, False);
          with m_fbrowser.m_msgCB do begin
            SetStyle(MCB_STYLE_OK);
            SetKindOf(MCB_KINDOF_ERROR);
            SetMessage(Format(sr.Get(FB_CFG_ID, 'SCANFATAL'),
                              [sFileName, ebff.Message]));
            CallBack;
          end;
          Exit;
        end;
        on ebfe : EBFAFileError do
          sTemp:=Format(sr.Get(FB_CFG_ID, 'SCANERR05'), [ebfe.Message]);
      end;
      m_sRealFileName:=sTemp;
      if (blScanned) then
        m_lItemIs:=m_lItemIs or FILEBROWSER_ITEMIS_SCANNED;
       Synchronize(SetFileItem);
    end;

    // next browser item
    Inc(m_nActIndex);

    // we should read the current number of items again (possible move/delete)
    Synchronize(GetCountOfItems);
  end;
  setup.Destroy;
  iil.Destroy;

  AccessRunningFlag(False, False);
end;

function TBFAScanner.AccessRunningFlag(blRead : Boolean = True;
                                       blNewValue : Boolean = False) : Boolean;
begin
  EnterCriticalSection(m_critSect);
  if (not blRead) then
    m_blRunning:=blNewValue;
  Result:=m_blRunning;
  LeaveCriticalSection(m_critSect);
end;


//////////////////////////// TFileChangesNotifier ////////////////////////////


// those are the changes we're looking for
const CHANGE_FLAGS = FILE_NOTIFY_CHANGE_FILE_NAME or
                     FILE_NOTIFY_CHANGE_DIR_NAME or
                     FILE_NOTIFY_CHANGE_ATTRIBUTES or
                     FILE_NOTIFY_CHANGE_SIZE or
                     FILE_NOTIFY_CHANGE_LAST_WRITE;

// number of milliseconds to wait before a change actually causes a refresh                     
const FLOOD_WAIT_MILLIS = 2000;


constructor TFileChangesNotifier.Create(fbrowser : TFileBrowser);
begin
  inherited Create(True);

  // first entry is the actual wait handle
  m_notifies[0]:=FindFirstChangeNotification(
    PChar(fbrowser.GetCurrentPath(False)),
    FALSE,
    CHANGE_FLAGS);

  // second entry is an event, which allows us a quick shutdown
  m_notifies[1]:=CreateEvent(Nil, FALSE, FALSE, Nil);

  m_fbrowser:=fbrowser;
 
  FreeOnTerminate:=False;
end;


destructor TFileChangesNotifier.Destroy;
begin
  // (in case the thread's still running)
  Shutdown;

  // clean up all the open handles
  
  if (m_notifies[0] <> INVALID_HANDLE_VALUE) then
    FindCloseChangeNotification(m_notifies[0]);

  if (m_notifies[1] <> 0) then
    CloseHandle(m_notifies[1]);
end;


procedure TFileChangesNotifier.RefreshBrowser;
begin
  with m_fbrowser do begin
    GetListView.Cursor:=crHourGlass;
    GetListView.Refresh;
    Update;
    GetListView.Cursor:=crDefault;
  end;
end;


procedure TFileChangesNotifier.Shutdown;
begin

  // bring down the thread by signaling the event and also setting the terminate
  // flag, so both blocking and executed code will exit as early as possible

  Terminate;

  if (m_notifies[1] <> INVALID_HANDLE_VALUE) then begin
    SetEvent(m_notifies[1]);

    WaitFor;

    CloseHandle(m_notifies[1]);
    m_notifies[1]:=THandle(0);
  end;
end;


procedure TFileChangesNotifier.Execute;
begin
  if (m_notifies[0] = INVALID_HANDLE_VALUE) then
    Exit;

  // run until something makes us exit
  while (not Terminated) do begin

    // we poll here, better than waiting for a change signal infinite times
    // (otherwise the thread will have to be shut down the hard way which is
    //  neither fine during the program run nor at the shutdown)
    if (WaitForMultipleObjects(
      2,
      @m_notifies,
      FALSE,
      INFINITE) = WAIT_OBJECT_0) then begin

      // aborted?
      if (Terminated) then
        Exit;

      // we don't refresh immediately, but wait for a certain amount of time
      // in case more changes are coming in

      FindNextChangeNotification(m_notifies[0]);

      while (WaitForMultipleObjects(
        2,
        @m_notifies,
        FALSE,
        FLOOD_WAIT_MILLIS) = WAIT_OBJECT_0) do begin

        // aborted?
        if (Terminated) then
          Exit;

        // we woke up because of another notification, so we need to rest again
        // and hope for the next idle period to come
        FindNextChangeNotification(m_notifies[0]);
      end;

      // aborted?
      if (Terminated) then
        Exit;

      // finally we are allowed to take control over the file browser
      Synchronize(RefreshBrowser);

    end  
    else begin

      // something else woke us up, which means we have to leave
      Exit;
    end;
  end;
end;

//////////////////////////// TFileBrowser ////////////////////////////

procedure TFileBrowser.ChooseFont(fntDlg : TFontDialog);
begin
  with fntDlg do begin
    // set the existing font
    with Font, m_config do begin
      Name:=GetStringOption(FB_CFGID_FONTNAME);
      Size:=GetIntegerOption(FB_CFGID_FONTSIZE);
      Style:=TFontStyles(Byte(GetIntegerOption(FB_CFGID_FONTSTYLE)));
      Color:=TColor(GetIntegerOption(FB_CFGID_FONTCOLOR));
    end;

    // offer selection
    if (not Execute) then
      Exit;

    // store and set the new font
    with Font, m_config do begin
      FixStringOption(FB_CFGID_FONTNAME, Name);
      FixIntegerOption(FB_CFGID_FONTSIZE, Size);
      FixIntegerOption(FB_CFGID_FONTSTYLE, Integer(Byte(Style)));
      FixIntegerOption(FB_CFGID_FONTCOLOR, Integer(Color));
    end;
    with m_listview.Font do begin
      Name:=Font.Name;
      Size:=Font.Size;
      Style:=Font.Style;
      Color:=Font.Color;
    end;
  end;
end;


function TFileBrowser.GetLastValidPath : String;
begin

  // (FIXME: we take advantage that the current path is not overwritten when
  //         chaniging to a pure network environment, hope that will be the
  //         case in the future, too)

  Result:=TStrPlus.RTLPath(m_sPath) + m_sSelection;

end;


procedure TFileBrowser.ColumnsRelabel;
var
  nIndex     : Integer;
  nSaveWidth : Integer;
  nSort      : Integer;
  cSortSign  : Char;
begin
  m_listView.Columns.BeginUpdate;

  with m_listView, m_sr do begin
    Columns[0].Caption:=Get(FB_CFG_ID, FB_CFGID_COLNAME);
    Columns[1].Caption:=Get(FB_CFG_ID, FB_CFGID_COLREALNAME);
    Columns[2].Caption:=Get(FB_CFG_ID, FB_CFGID_COLSIZE);
    Columns[3].Caption:=Get(FB_CFG_ID, FB_CFGID_COLREALSIZE);
    Columns[4].Caption:=Get(FB_CFG_ID, FB_CFGID_COLTIME);
    Columns[5].Caption:=Get(FB_CFG_ID, FB_CFGID_COLATTR);
    Columns[6].Caption:=Get(FB_CFG_ID, FB_CFGID_COLTYPE);

    nSort:=m_config.GetIntegerOption(FB_CFGID_SORT);

    if ((nSort and FILEBROWSER_SORT_UPWARDS) = FILEBROWSER_SORT_UPWARDS) then
      cSortSign:='-'
    else
      cSortSign:='+';

    // (FXIME: bitmasked, of course, but this is easier to do)
    case (nSort and $0ffff) of
      FILEBROWSER_SORT_FILE      : nIndex:=0;
      FILEBROWSER_SORT_REALNAME  : nIndex:=1;
      FILEBROWSER_SORT_SIZE      : nIndex:=2;
      FILEBROWSER_SORT_REALSIZE  : nIndex:=3;
      FILEBROWSER_SORT_TIME      : nIndex:=4;
      FILEBROWSER_SORT_ATTR      : nIndex:=5;
      FILEBROWSER_SORT_TYPE      : nIndex:=6;
    else
      nIndex:=-1;
    end;
    if (nIndex <> -1) then begin
      nSaveWidth:=Columns[nIndex].Width;
      Columns[nIndex].Caption:=Columns[nIndex].Caption + ' ' + cSortSign;
      Columns[nIndex].Width:=nSaveWidth;
    end;
  end;

  m_listView.Columns.EndUpdate;

end;


function TFileBrowser.GetHistory : THistory;
begin
  Result:=m_hist;
end;


function TFileBrowser.GetListView : TListView;
begin
  Result:=m_listView;
end;

type
  TSHFormatDriveProc = function(
    hWnd : HWND;Drive, fmtID, Options : Word) : LongInt; stdcall;

var
  _hShell32Dll : THandle;
  _SHFormatDrive : TSHFormatDriveProc;

const
  SHFMT_ID_DEFAULT = 65535;

function TFileBrowser.FormatDrive : Boolean;
var
  cDrive : Char;
  sCaption : String;
  item : TListItem;
  pInfo : PFileBrowserItemInfo;
begin

  Result:=False;

  if (not FormatSupported) then Exit;
  if (1 <> m_listView.SelCount) then Exit;

  item:=m_listView.Selected;
  if (Nil = item) then Exit;

  pInfo:=PFileBrowserItemInfo(item.Data);

  if (FILEBROWSER_ITEMIS_DRIVE <>
     (FILEBROWSER_ITEMIS_DRIVE and pInfo^.lItemIs)) then Exit;

  sCaption:=item.Caption;
  cDrive:=UpCase(sCaption[Pos(':', sCaption) - 1]);
  if (('A' > cDrive) or ('Z' < cDrive)) then Exit;

  _ShFormatDrive(m_listView.Handle,
                 Ord(cDrive) - Ord('A'),
                 SHFMT_ID_DEFAULT,
                 0);
  Result:=True;
end;


class function TFileBrowser.FormatSupported : Boolean;
begin
  Result:=(0 <> _hShell32Dll);
end;


initialization
  // SHFormatDrive() might not be available on older systems, so we have to
  // check for it and load it dynamically
  _hShell32Dll:=Windows.LoadLibrary('shell32.dll');
  if (0 <> _hShell32Dll) then begin
    _SHFormatDrive:=Windows.GetProcAddress(_hShell32Dll, 'SHFormatDrive');
    if (not Assigned(_SHFormatDrive)) then begin
      Windows.FreeLibrary(_hShell32Dll);
      _hShell32Dll:=0;
    end;
  end;

finalization
  if (0 <> _hShell32Dll) then Windows.FreeLibrary(_hShell32Dll);

end.
