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
  general progress callback for file handling
}

unit FilesProgress;


interface
uses SysUtils,
     bfacslib,
     ProgressCallBack;


// the progress callback class
type
  TFilesProgress = class(TProgressCallBack)
  protected
    // members
    m_nNumOfFiles : Integer;
    m_nFileNumber : Integer;
    m_sFileName   : String;
  public

    // gets the number of files together
    // <- number of files
    function GetNumOfFiles : Integer;

    // sets the number of files together
    // -> new number of files
    procedure SetNumOfFiles(nNumOfFiles : Integer);

    // gets the current file number
    // <- file number
    function GetFileNumber : Integer;

    // sets the current file number
    // -> new file number
    procedure SetFileNumber(nFileNumber : Integer);

    // gets the name of the currently processed file
    // <- file name
    function GetFileName : String;

    // sets the name of the currently processed file
    // -> new file name
    procedure SetFileName(sFileName : String);

  end;



implementation


//////////////////////////// TFilesProgress ////////////////////////////

function TFilesProgress.GetNumOfFiles : Integer;
begin
  Result:=m_nNumOfFiles;
end;

procedure TFilesProgress.SetNumOfFiles(nNumOfFiles : Integer);
begin
  m_nNumOfFiles:=nNumOfFiles;
end;

function TFilesProgress.GetFileNumber : Integer;
begin
  Result:=m_nFileNumber;
end;

procedure TFilesProgress.SetFileNumber(nFileNumber : Integer);
begin
  m_nFileNumber:=nFileNumber;
end;

function TFilesProgress.GetFileName : String;
begin
  Result:=m_sFileName;
end;

procedure TFilesProgress.SetFileName(sFileName : String);
begin
  m_sFileName:=sFileName;
end;

end.
