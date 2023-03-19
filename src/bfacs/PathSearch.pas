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
  implementations of path handling classes
}


unit PathSearch;


interface
uses Classes,
     Windows,
     SysUtils,
     ProgressCallBack,
     bfacslib,
     CallBack,
     StringRes,
     IntLists;


// exceptions for path searching
type
  EPathSearchError       = class(Exception);
  EPathSearchNoFiles     = class(EPathSearchError);
  EPathSearchInterrupted = class(EPathSearchError);


// simple callback class used by the path processors,
// doesn't touch the message of its parent class
type
  TPathSearchCallBack = class(TCallBack)
  protected
    m_nNumOfDirs  : Integer;
    m_nNumOfFiles : Integer;
    m_sActFile    : String;
  public

    // sets the callback object, init. the internals
    // -> object to modify during the callback
    constructor Create(setcallbackobj : TObject); override;

    // sets the number of found directories
    // -> new number of found directories
    procedure SetNumOfDirs(nSetNumOfDirs : Integer);

    // returns the number of found directories
    // <- number of found directories
    function GetNumOfDirs : Integer;

    // increases the number of found directories
    procedure IncNumOfDirs;

    // sets the number of found files
    // -> new number of found files
    procedure SetNumOfFiles(nSetNumOfFiles : Integer);

    // returns the number of found files
    // <- number of found files
    function GetNumOfFiles : Integer;

    // increases the number of found files
    procedure IncNumOfFiles;

    // sets the actual processed file name
    // -> new actual processed file name
    procedure SetActFile(sSetActFile : String);

    // returns the actual processed file name (plus path)
    // <- actual processed file name
    function GetActFile : String;
  end;


// constants to set the strength of killing paths
const
  // erase all, even system/hidden/writeprotected files
  PATHKILL_ERASEALL  = 0;
  // erase all normal files
  PATHKILL_DELETE    = 1;
  // just erase the folder (and subfolders) if there's no single file left
  PATHKILL_EMPTYDIRS = 2;


// class for deleting a complete path
type
  TPathToKill = class
  protected
    m_nLevel      : Integer;
    m_sPathToKill : String;
    m_sr          : TStrRes;

  public
    // copies the name of the path to kill, sets formal delete level
    // -> name of the path to kill
    // -> string resources
    constructor Create(sSetPathToKill : String;
                       sr : TStrRes); overload;

    // copies the name of the path to kill, allows a special level of erasing
    // -> name of the path to kill
    // -> the delete level (see PATHKILL_xxx constants)
    // -> string resources
    constructor Create(sSetPathToKill : String;
                       nLevel : Integer;
                       sr : TStrRes); overload;

    // kills the path
    // -> to report the progress and detect interrupts
    //    (supports the "changed" flag, may be Nil)
    // exception: EPathSearchError       - error occured
    // exception: EPathSearchInterrupted - interrupt detected
    procedure Erase(progressCall : TPathSearchCallBack = Nil);
  end;



// class to register files in a path tree
type
  TPathSearch = class
  private
    // members
    m_theDirs       : TStringList; // to store the directories
    m_theFiles      : TStringList; // to store the files
    m_nFileOutIndex : Integer;     // for reading files
    m_nDirOutIndex  : Integer;     // for reading directories
    m_nNumOfFiles   : Integer;     // counters...
    m_nNumOfDirs    : Integer;
    m_qNumOfBytes   : WORD64;
    m_sr            : TStrRes;

  protected
    // returns an RTL path by its index
    // -> index of path
    // <- RTL path
    function BuildPath(nIndex : Integer) : String;

  public

    // creates the lists
    // -> string resources
    constructor Create(sr : TStrRes);

    // frees the lists
    destructor Destroy; override;

    // reads a tree or just a single directory, overwrites the old content
    // -> new path/selection (only absolute paths allowed)
    // -> search attribute (exclude mask)
    // -> recursive flag
    // -> progress callback object (supports the "changed" flag)
    // -> if the progress callback object
    //                            should be reset
    // -> to store the file sizes externally (may be Nil)
    // exception: EPathSearchInterrupted - interrupt detected
    procedure RegisterFiles(sPath : String;
                            nExclAttr : Integer;
                            blRecursive : Boolean;
                            progressCall : TPathSearchCallBack;
                            blResetProgressCall : Boolean;
                            fileSizesKeeper : TWORD64List = Nil);

    // gets the first file out of the list
    // <- first file (plus complete path), empty if no files in list
    function GetFirstFile : String;

    // gets the next file out of the list (first the file from the trees will
    // be put out, then the single files, just in case for if the file sizes
    // keeper wants to be used in combination with GetFirstFile/GetNextFile)
    // <- file (plus complete path), empty if no more files
    function GetNextFile : String;

    // gets the first directory out of the list
    // <- first directory (RTL), empty if no directories were found
    function GetFirstDir : String;

    // gets the next directory out of the list
    // <- directory (RTL), empty if no more directories
    function GetNextDir : String;

    // returns the number of registered files
    // <- number of registered files
    function GetNumOfFiles : Integer;

    // returns the number of registered directories
    // <- number of registered directories
    function GetNumOfDirs : Integer;

    // returns the number of bytes of all registered files
    // <- the number of bytes of all registered files
    function GetNumOfBytes : WORD64;
  end;




// container keeping multiple TPathSeatch instances plus single filenames
type
  TPathSearchContainer = class
  private
    // members
    m_singleFiles  : TStringList; // single filenames are stored here
    m_treeList     : TList;       // here we keep all trees
    m_nFileTree    : Integer;     // for reading the filenames...
    m_nFileIndex   : Integer;
    m_nDirTree     : Integer;     // for reading the directories...
    m_nDirIndex    : Integer;     //
    m_nNumOfFiles  : Integer;     // counters...
    m_nNumOfDirs   : Integer;
    m_qNumOfBytes  : WORD64;
    m_sr           : TStrRes;

  public

    // creates the container lists
    // -> string resources
    constructor Create(sr : TStrRes);

    // destroy all internal objects
    destructor Destroy; override;

    // resets the whole object container
    procedure Clear;

    // adds a single file
    // -> complete file path
    // -> progress callback object
    // -> to store the file sizes externally
    // exception: EPathSearchNoFiles - file not found
    // exception: EPathSearchInterrupted - interrupt detected
    procedure AddSingleFile(sFileName : String;
                            progressCall : TPathSearchCallBack = Nil;
                            fileSizesKeeper : TWORD64List = Nil);

    // adds a complete path tree
    // -> path to search
    // -> attribute mask (exclusive)
    // -> recursive search flag
    // -> progress callback object (reset by the caller)
    // -> to store the file sizes externally
    // exception: EPathSearchError       - error occured
    // exception: EPathSearchNoFiles     - no files found
    // exception: EPathSearchInterrupted - interrupt detected
    procedure AddTree(sPath : String;
                      nExclAttr : Integer;
                      blRecursive : Boolean;
                      progressCall : TPathSearchCallBack;
                      fileSizesKeeper : TWORD64List = Nil);

    // returns the first filename out of the container
    // <- first registered file name with complete path
    function GetFirstFile : String;

    // returns the next filename out of the container
    // <- next file name with complete path (empty if no more files)
    function GetNextFile : String;

    // returns the first directory out of the container
    // <- first registered directory
    function GetFirstDir : String;

    // returns the next directory out of the container
    // <- next directory name (empty if no more directories)
    function GetNextDir : String;

    // returns the number of all registered files in the container
    // <- number of registered files
    function GetNumOfFiles : Integer;

    // returns the number of all registered directories, the most useless
    // member function of this class
    // <- number of registered files
    function GetNumOfDirs : Integer;

    // returns the number of bytes of all registered files in the container
    // <- numbers of bytes of all files together
    function GetNumOfBytes : WORD64;
  end;



// the toplevel class for searching files, does the whole action in one
// step, providing informative callbacks
type
  TFileSearcher = class
  public
    // members
    m_files     : TStringList;
    m_folders   : TStringList;
    m_nExclAttr : Integer;
    m_sr        : TStrRes;

  public

    // constructor, sets up all search parameters
    // -> single files to look for
    // -> directories to scan for files
    // -> search exclusive attribute (folders only)
    // -> string resources
    constructor Create(files : TStringList;
                       folders : TStringList;
                       nExclAttr : Integer;
                       sr : TStrRes);

    // searches
    // -> progress callback
    // -> where to return the names of nonexisting files
    // -> to store the file sizes externally (may be Nil)
    // <- all registered files (may be empty, of course)
    // exception: EPathSearchError       - error occured
    // exception: EPathSearchInterrupted - interrupt detected
    function Search(progress : TPathSearchCallBack;
                    var nonExistingFiles : TStringList;
                    fileSizesKeeper : TWORD64List = Nil) : TPathSearchContainer;
  end;




implementation
uses FileCtrl,
     StringPlus,
//     Globals,
//     StringPlusI,
     FileSupp;


//////////////////////////// TPathSearchCallBack ////////////////////////////

constructor TPathSearchCallBack.Create(setcallbackobj : TObject);
begin
  inherited Create(setcallbackobj);
  m_nNumOfDirs:=0;
  m_nNumOfFiles:=0;
  m_sActFile:='';
end;


procedure TPathSearchCallBack.SetNumOfDirs(nSetNumOfDirs : Integer);
begin
  m_nNumOfDirs:=nSetNumOfDirs;
end;


function TPathSearchCallBack.GetNumOfDirs : Integer;
begin
  Result:=m_nNumOfDirs;
end;


procedure TPathSearchCallBack.IncNumOfDirs;
begin
  Inc(m_nNumOfDirs);
end;


procedure TPathSearchCallBack.SetNumOfFiles(nSetNumOfFiles : Integer);
begin
  m_nNumOfFiles:=nSetNumOfFiles;
end;


function TPathSearchCallBack.GetNumOfFiles : Integer;
begin
  Result:=m_nNumOfFiles;
end;


procedure TPathSearchCallBack.IncNumOfFiles;
begin
  Inc(m_nNumOfFiles);
end;


procedure TPathSearchCallBack.SetActFile(sSetActFile : String);
begin
  m_sActFile:=sSetActFile;
end;


function TPathSearchCallBack.GetActFile : String;
begin
  Result:=m_sActFile;
end;


//////////////////////////// TPathToKill ////////////////////////////

constructor TPathToKill.Create(sSetPathToKill : String;
                               sr : TStrRes);
begin
  m_sPathToKill:=sSetPathToKill;
  m_nLevel:=PATHKILL_DELETE;
  m_sr:=sr;
end;


constructor TPathToKill.Create(sSetPathToKill : String;
                               nLevel : Integer;
                               sr : TStrRes);
begin
  m_sPathToKill:=sSetPathToKill;
  m_nLevel:=nLevel;
  m_sr:=sr;
end;


procedure TPathToKill.Erase(progressCall : TPathSearchCallBack = Nil);
var
  blWasError     : Boolean;
  blWasInterrupt : Boolean;


procedure RecKill(sDirToKill : String);
var
  nResult   : Integer;
  sFileName : String;
  dta       : TSearchRec;
begin
  // start searching
  nResult:=FindFirst(sDirToKill + '*.*', faAnyFile, dta);
  while ((nResult = 0) and (not blWasError)) do begin

    // new directory found?
    if ((dta.Attr and faDirectory) = faDirectory) then begin
      // ignore self pointing paths
      if (dta.Name <> '.')  and (dta.Name <> '..') then begin
        if (progressCall <> Nil) then
          progressCall.IncNumOfDirs;
        RecKill(sDirToKill + dta.Name + '\');
      end;
    end
    else begin
      // it's a file, are we allowed to delete it?
      case m_nLevel of
        PATHKILL_EMPTYDIRS :
          raise EPathSearchError.Create(m_sr.Get('PATHSEARCH', '004'));

        PATHKILL_DELETE : begin
          // check if the file is nothing other than an archived one
          if ((dta.Attr and (not faArchive)) <> 0) then
            raise EPathSearchError.Create(m_sr.Get('PATHSEARCH', '005'));
        end;
      end;

      // call the progress object, if necessary
      sFileName:=sDirToKill + dta.Name;
      try
        if (progressCall <> Nil) then begin
          with progressCall do begin
            IncNumOfFiles;
            SetActFile(sFileName);
            CallBack;
            SetChanged(False);
          end;
        end;

        // reset the file attributes to be sure
        SetFileAttributes(PChar(sFileName), 0);

        // delete it
        if (Windows.DeleteFile(PChar(sFileName)) = FALSE) then
          blWasError:=True;
      except
        on ECallBackInterrupt do begin
          blWasError:=True;
          blWasInterrupt:=True;
        end;
      end;
    end;
    nResult:=FindNext(dta);
  end;
  SysUtils.FindClose(dta);

  // reset the directory attributes
  SetFileAttributes(PChar(sDirToKill), 0);

  // kill the directory
  if (not RemoveDir(sDirToKill)) then
    blWasError:=True;

  // error?
  if (nResult <> ERROR_NO_MORE_FILES) then
    blWasError:=True;
end;

begin
  // empty?
  if (m_sPathToKill = '') then
    Exit;

  // prepare the callback object
  if (progressCall <> Nil) then begin
    with progressCall do begin
      SetNumOfFiles(0);
      SetNumOfDirs(0);
      SetChanged(True);
    end;
  end;

  // we need a RTL path
  m_sPathToKill:=TStrPlus.RTLPath(m_sPathToKill);

  // start searching and killing
  blWasError:=False;
  blWasInterrupt:=False;
  RecKill(m_sPathToKill);
  if (blWasInterrupt) then
    raise EPathSearchInterrupted.Create(m_sr.Get('PATHSEARCH', '000'));
  if (blWasError) then
    raise EPathSearchError.Create(m_sr.Get('PATHSEARCH', '001'));
end;



//////////////////////////// TPathSearch ////////////////////////////

constructor TPathSearch.Create(sr : TStrRes);
begin
  m_theDirs:=TStringList.Create;
  m_theFiles:=TStringList.Create;
  m_qNumOfBytes:=0;
  m_sr:=sr;
end;


destructor TPathSearch.Destroy;
begin
  m_theDirs.Destroy;
  m_theFiles.Destroy;
end;


procedure TPathSearch.RegisterFiles(sPath : String;
                                    nExclAttr : Integer;
                                    blRecursive : Boolean;
                                    progressCall : TPathSearchCallBack;
                                    blResetProgressCall : Boolean;
                                    fileSizesKeeper : TWORD64List = Nil);
var
  blWasInterrupt : Boolean;
  sSelection     : String;
  sBaseDir       : String;

// internal recursive search routine
procedure RecSearch(sDirToSearch : String);
var
  qFileSize  : WORD64;
  nThisIndex : Integer;
  blDone     : Boolean;
  findHandle : THandle;
  sFileName  : String;
  dta        : TWin32FindData;
begin

  // save our directories index (for filename storing)
  nThisIndex:=m_nNumOfDirs;

  // start searching
  findHandle:=FindFirstFile(PChar(sDirToSearch + sSelection), dta);
  if (findHandle <> INVALID_HANDLE_VALUE) then begin

    blDone:=False;
    while (not blDone) do begin
      sFileName:=String(PChar(@dta.cFileName[0]));

      // new directory found?
      if ((dta.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) =
           FILE_ATTRIBUTE_DIRECTORY) then begin

         // ignore self-pointing paths
         if ((sFileName <> '.') and (sFileName <> '..')) then
           // if we are in recursive mode add the directory and enter it
           if (blRecursive) then begin
             m_theDirs.Add(sFileName + '\');
             Inc(m_nNumOfDirs);
             progressCall.IncNumOfDirs;
             m_theDirs.Objects[m_nNumOfDirs]:=Pointer(nThisIndex);
             RecSearch(sDirToSearch + sFileName + '\');
           end;
        // (otherwise it'll be ignored)
      end
      else begin
        // add if the file matches the exclude mask
        if ((dta.dwFileAttributes and nExclAttr) = 0) then begin
          m_theFiles.Add(sFileName);
          m_theFiles.Objects[m_nNumOfFiles]:=Pointer(nThisIndex);
          Inc(m_nNumOfFiles);
          qFileSize:=MakeWORD64(dta.nFileSizeLow, dta.nFileSizeHigh);
          Inc(m_qNumOfBytes, qFileSize);
          // (store the file size, i.n.)
          if (fileSizesKeeper <> Nil) then
            fileSizesKeeper.Add(qFileSize);
        end;

        // report to the progress object
        progressCall.IncNumOfFiles;
        progressCall.SetActFile(sDirToSearch + sFileName);
        try
          progressCall.CallBack;
          progressCall.SetChanged(False);
        except
          on ECallBackInterrupt do begin
            blDone:=True;
            blWasInterrupt:=True;
          end;
        end;
      end;
      if (FindNextFile(findhandle, dta) = FALSE) then
        blDone:=True;
    end;
    Windows.FindClose(findhandle);
  end;

end;
begin
  // empty is error
  if (sPath = '') then
    Exit;

  // extract base directory and the selection
  sSelection:='*.*';
  if (TStrPlus.GetLastChar(sPath) <> '\') then begin
    if DirectoryExists(sPath) then
      sBaseDir:=sPath + '\'
    else begin
      sBaseDir:=ExtractFilePath(sPath);
      if (TStrPlus.GetLastChar(sBaseDir) <> '\') then
        sBaseDir:=sBaseDir + '\';
      sSelection:=ExtractFileName(sPath);
    end;
  end
  else sBaseDir:=sPath;

  // clear list and counters
  m_theDirs.BeginUpdate;
  m_theFiles.BeginUpdate;
  m_theDirs.Clear;
  m_theFiles.Clear;
  m_nNumOfFiles:=0;
  m_nNumOfDirs:=0;
  m_qNumOfBytes:=0;

  // store the base directory (as rtl path)
  m_theDirs.Add(sBaseDir);
  m_theDirs.Objects[0]:=Pointer(-1);

  // reset the callback object, i.n.
  if (blResetProgressCall) then begin
    progressCall.SetNumOfFiles(0);
    progressCall.SetNumOfDirs(0);
  end;
  progressCall.SetChanged(True);

  // start searching
  blWasInterrupt:=False;
  RecSearch(sBaseDir);
  m_theFiles.EndUpdate;
  m_theDirs.EndUpdate;

  if (blWasInterrupt) then
    raise EPathSearchInterrupted.Create(m_sr.Get('PATHSEARCH', '000'));
end;


function TPathSearch.GetFirstFile : String;
begin

  // reset the index
  m_nFileOutIndex:=0;

  // save code by using this trick
  Result:=GetNextFile;
end;


function TPathSearch.GetNextFile : String;
begin
  // no more files?
  if (m_nFileOutIndex >= m_nNumOfFiles) then
    Result:=''
  else begin
    Result:=BuildPath(Integer(m_theFiles.Objects[m_nFileOutIndex])) +
            m_theFiles.Strings[m_nFileOutIndex];
    Inc(m_nFileOutIndex);
  end;
end;


function TPathSearch.GetFirstDir : String;
begin
  // no directories?
  if (m_nNumOfDirs = 0) then
    Result:='';

  // reset the index
  m_nDirOutIndex:=0;

  // (skip the base directory)
  Result:=GetNextDir;
end;


function TPathSearch.GetNextDir : String;
begin
  // no more directories?
  if (m_nDirOutIndex >= m_nNumOfDirs) then
    Result:=''
  else begin
    Inc(m_nDirOutIndex);
    Result:=BuildPath(m_nDirOutIndex);
  end;
end;


function TPathSearch.GetNumOfFiles : Integer;
begin
  Result:=m_nNumOfFiles;
end;


function TPathSearch.GetNumOfDirs : Integer;
begin
  // the base path doesn't count
  Result:=m_nNumOfDirs - 1;
end;


function TPathSearch.GetNumOfBytes : WORD64;
begin
  Result:=m_qNumOfBytes;
end;


function TPathSearch.BuildPath(nIndex : Integer) : String;
begin
  Result:='';
  repeat
    Result:=m_theDirs.Strings[nIndex] + Result;
    nIndex:=Integer(m_theDirs.Objects[nIndex]);
  until (nIndex = -1);
end;

//////////////////////////// TPathSearchContainer ////////////////////////////


constructor TPathSearchContainer.Create(sr : TStrRes);
begin
  m_singleFiles:=TStringList.Create;
  m_treeList:=TList.Create;
  m_qNumOfBytes:=0;
  m_sr:=sr;
  Clear;
end;


destructor TPathSearchContainer.Destroy;
var
  nI : Integer;
begin
  // destroy the lists
  m_singleFiles.Destroy;

  // detroy every single directory keeper
  nI:=0;
  while (nI < m_treeList.Count) do begin
    TPathSearch(m_treeList.Items[nI]).Destroy;
    Inc(nI);
  end;
  m_treeList.Destroy;
end;


procedure TPathSearchContainer.Clear;
begin
  // clear the lists
  m_singleFiles.Clear;
  m_treeList.Clear;

  // reset the counters
  m_nFileTree:=0;
  m_nFileIndex:=0;
  m_nNumOfFiles:=0;
  m_nNumOfDirs:=0;
  m_qNumOfBytes:=0;
end;



procedure TPathSearchContainer.AddTree(sPath : String;
                                       nExclAttr : Integer;
                                       blRecursive : Boolean;
                                       progressCall : TPathSearchCallBack;
                                       fileSizesKeeper : TWORD64List = Nil);
var
  newPath : TPathSearch;
begin
  // create a new tree
  newPath:=TPathSearch.Create(m_sr);

  // search for files
  try
    newPath.RegisterFiles(sPath,
                          nExclAttr,
                          blRecursive,
                          progressCall,
                          False,
                          fileSizesKeeper);
  except
    on EPathSearchError do begin
      newPath.Destroy;
      raise;
    end;
  end;

  // no files found?
  if (newPath.GetNumOfFiles = 0) then begin
    newPath.Destroy;
    raise EPathSearchNoFiles.Create(m_sr.Get('PATHSEARCH', '002'));
  end;

  // add tree object to the list
  m_treeList.Add(newPath);

  // increase the counters
  m_nNumOfFiles:=m_nNumOfFiles + newPath.GetNumOfFiles;
  m_nNumOfDirs:=m_nNumOfDirs + newPath.GetNumOfDirs;
  Inc(m_qNumOfBytes, newPath.GetNumOfBytes);
end;


procedure TPathSearchContainer.AddSingleFile(sFileName : String;
                                             progressCall
                                               : TPathSearchCallBack = Nil;
                                             fileSizesKeeper
                                               : TWORD64List = Nil);
var
  qFileSize : WORD64;
begin
  // get the size of the file (use the API functions for that)
  try
    qFileSize:=TFileSupport.GetFile64Len(sFileName, m_sr);

    // show what we found
    if (progressCall <> Nil) then
      with progressCall do begin
        IncNumOfFiles;
        SetActFile(sFileName);
        CallBack;
      end;

    // increase the counters
    Inc(m_nNumOfFiles);
    Inc(m_qNumOfBytes, qFileSize);
    if (fileSizesKeeper <> Nil) then
      fileSizesKeeper.Add(qFileSize);
    m_singleFiles.Add(sFileName);

  except
    on EFileSupportError do
      raise EPathSearchNoFiles.Create(Format(m_sr.Get('PATHSEARCH', '003'),
                                             [sFileName]));

    on ecbi : ECallBackInterrupt do
      raise EPathSearchInterrupted.Create(ecbi.Message);

  end;
end;


function TPathSearchContainer.GetFirstFile : String;
begin
  // reset the position counters
  m_nFileTree:=0;
  m_nFileIndex:=0;

  // just use the function above now
  Result:=GetNextFile;
end;


function TPathSearchContainer.GetNextFile : String;
var
  actTree : TPathSearch;
begin
  // tree to read out?
  if (m_nFileTree < m_treeList.Count) then begin
    actTree:=m_treeList.Items[m_nFileTree];

    // out of files in the actual tree?
    if (m_nFileIndex < actTree.GetNumOfFiles) then begin
      if (m_nFileIndex = 0) then
        Result:=actTree.GetFirstFile
      else
        Result:=actTree.GetNextFile;
      Inc(m_nFileIndex);
      Exit;
    end
    else begin
      Inc(m_nFileTree);

      // out of trees? (remember that we don't accept empty trees)
      if (m_nFileTree < m_treeList.Count) then begin
        actTree:=m_treeList.Items[m_nFileTree];
        Result:=actTree.GetFirstFile;
        m_nFileIndex:=1;
        Exit;
      end
      else
        m_nFileIndex:=0; // now reading single files
    end;
  end;

  // no more single files?
  if (m_nFileIndex < m_singleFiles.Count) then begin
    Result:=m_singleFiles.Strings[m_nFileIndex];
    Inc(m_nFileIndex);
  end
  else
    Result:='';
end;


function TPathSearchContainer.GetFirstDir : String;
begin
  // reset the counter
  m_nDirTree:=0;
  m_nDirIndex:=0;

  // just use the function above now
  Result:=GetNextDir;
end;


function TPathSearchContainer.GetNextDir : String;
var
  actTree : TPathSearch;
begin
  // tree to read out?
  if (m_nDirTree < m_treeList.Count) then begin
    actTree:=m_treeList.Items[m_nFileTree];

    // out of files in the actual tree?
    if (m_nDirIndex < actTree.GetNumOfDirs) then begin
      if (m_nDirIndex = 0) then
        Result:=actTree.GetFirstDir
      else
        Result:=actTree.GetNextDir;
      Inc(m_nDirIndex);
    end
    else begin
      Inc(m_nDirTree);

      // out of trees?
      if (m_nDirTree < m_treeList.Count) then begin
        actTree:=m_treeList.Items[m_nDirTree];
        Result:=actTree.GetFirstDir;
        m_nDirIndex:=1;
      end
      else
        Result:='';
    end;
  end
  else
    Result:='';
end;



function TPathSearchContainer.GetNumOfFiles : Integer;
begin
  Result:=m_nNumOfFiles;
end;


function TPathSearchContainer.GetNumOfDirs : Integer;
begin
  Result:=m_nNumOfDirs;
end;

function TPathSearchContainer.GetNumOfBytes : WORD64;
begin
  Result:=m_qNumOfBytes;
end;



//////////////////////////// TFileSearcher ////////////////////////////


constructor TFileSearcher.Create(files : TStringList;
                                 folders : TStringList;
                                 nExclAttr : Integer;
                                 sr : TStrRes);
begin
  m_files:=files;
  m_folders:=folders;
  m_nExclAttr:=nExclAttr;
  m_sr:=sr;
end;


function TFileSearcher.Search(progress : TPathSearchCallBack;
                              var nonExistingFiles : TStringList;
                              fileSizesKeeper : TWORD64List = Nil)
                                : TPathSearchContainer;
var
  nI : Integer;
begin

  // searching for files...
  Result:=TPathSearchContainer.Create(m_sr);

  // first add the folders
  nI:=0;
  while (nI < m_folders.Count) do begin
    try
      progress.SetMessage(Format(m_sr.Get('PATHSEARCH', '005'),
                                 [m_folders.Strings[nI]]));
      Result.AddTree(m_folders.Strings[nI],
                     m_nExclAttr,
                     True,
                     progress,
                     fileSizesKeeper);
    except
      on EPathSearchInterrupted do begin
        Result.Destroy;
        raise;
      end;
      on epse : EPathSearchError do
        // (we skip over "no files" errors)
        if (not (epse is EPathSearchNoFiles)) then begin
          Result.Destroy;
          raise;
        end;
    end;
    Inc(nI);
  end;

  // now add the single files
  progress.SetMessage(m_sr.Get('PATHSEARCH', '007'));
  try
    progress.SetSimpleMessageState(True);
    progress.CallBack;
    progress.SetSimpleMessageState(False);
  except
    on ECallBackInterrupt do begin
      Result.Destroy;
      raise EPathSearchInterrupted.Create(m_sr.Get('PATHSEARCH', '000'));
    end;
  end;
  nI:=0;
  nonExistingFiles.BeginUpdate;
  while (nI < m_files.Count) do begin
    try
      Result.AddSingleFile(m_files.Strings[nI],
                           progress,
                           fileSizesKeeper);
    except
      // we have a file not found case
      on EPathSearchNoFiles do
        nonExistingFiles.Add(m_files.Strings[nI]);
    end;
    Inc(nI);
  end;
  nonExistingFiles.EndUpdate;
end;



end.

