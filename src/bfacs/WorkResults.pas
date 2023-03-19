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
  base class for results
}

unit WorkResults;

{$I config.inc}

interface
uses Classes,
     IntLists;

// result types
const
  WORKRESULTS_TYPE_SUCCESS = 0;
  WORKRESULTS_TYPE_ERROR   = 1;
  WORKRESULTS_TYPE_WARNING = 2;
  WORKRESULTS_TYPE_SKIP    = 3;
  WORKRESULTS_TYPE_BREAK   = 4;


// column alignments
const
  WORKRESULTS_COLALIGN_LEFT   = 0;
  WORKRESULTS_COLALIGN_RIGHT  = 1;
  WORKRESULTS_COLALIGN_MIDDLE = 2;


// information structure for columns
type
  TWorkResultsColumnInfos = class
  protected
    m_alignments       : array of Integer;
    m_widthPercentages : array of Integer;
    m_captions         : array of String;
  public

    // constructor
    constructor Create;

    // destructor
    destructor Destroy; override;

    // gets the number of columns
    // -> number of columns
    function GetCount : Integer;

    // clears all column infos
    procedure Clear;

    // adds a column info
    // -> alignment (see WORKRESULTS_COLALIGN_xxx)
    // -> percentage the column width in a list (-1 equals don't care)
    // -> caption of the column header
    procedure AddInfo(nAlignment : Integer;
                      nWidthPercentage : Integer;
                      const sCaption : String);

    // gets the alignment of a column info
    // -> column index
    // <- alignment
    function GetAlignment(nIdx : Integer) : Integer;

    // gets the percentage the column width of a column info
    // -> column index
    // <- percentage the column width
    function GetWidthPercentage(nIdx : Integer) : Integer;

    // gets the caption of the column header
    // -> column index
    // <- column header caption
    function GetCaption(nIdx : Integer) : String;

  end;



// the base class, an abstract one
type
  TWorkResults = class
  protected
    // members
    m_nErrors        : Integer;
    m_nWarnings      : Integer;
    m_nSkips         : Integer;
    m_results        : TList;
    m_errorMessages  : TStringList;
    m_types          : TIntegerList;
    m_blInterrupt    : Boolean;
    m_blErrMessExist : Boolean;

  public
    // constructor, should be overloaded by the inherited classes, but must
    // be called if the errors and warning counters are used
    constructor Create; virtual;

    // destructor
    destructor Destroy; override;

    // gets the dimension (number of columns)
    // <- dimension
    function GetDimension : Integer; virtual; abstract;

    // gets the list column information
    // <- column infos
    function GetColInfos : TWorkResultsColumnInfos; virtual; abstract;

    // gets the number of results
    // <- number of results
    function GetCount : Integer; virtual;

    // renders a result into a table row
    // -> number of result
    // -> array where to store the entries (correct sized!)
    // <- table row
    procedure Render(nIndex : Integer;
                     var rows : array of String); virtual; abstract;

    // renders a complete report (this method may be heavily time- and memory
    // consuming when rendering large reports!)
    // -> separator string placed between the columns
    // <- the report
    function RenderReport(const sSeparator : String) : String; virtual;

    // gets the number of errors
    // <- number of errors
    function GetNumOfErrors : Integer; virtual;

    // gets the number of warnings
    // <- number of warnings
    function GetNumOfWarnings : Integer; virtual;

    // gets the number of skips
    // <- number of warnings
    function GetNumOfSkips : Integer; virtual;

    // adds a result
    // -> the result object
    // -> result type, see WORKRESULTS_TYPE_xxx
    // -> error message
    procedure Add(toAdd  : TObject;
                  nType : Integer;
                  const sErrorMessage : String = ''); virtual;

    // checks what kind of type an entry is
    // -> number of the result to check
    // <- entry type
    function GetEntryType(nIndex : Integer) : Integer; virtual;

    // sets the flag to detect an interrupt
    procedure SetInterrupt(blInterrupt : Boolean); virtual;

    // gets the flag to detect an interrupt
    function GetInterrupt : Boolean; virtual;

    // checks if error messages are existing
    // <- True: they are / False: not
    function ErrorMessagesExist : Boolean;
  end;


implementation
uses StringPlus;


//////////////////////// TWorkResultsColumnInfos ////////////////////////



constructor TWorkResultsColumnInfos.Create;
begin
  Clear;
end;

destructor TWorkResultsColumnInfos.Destroy; 
begin
  Finalize(m_alignments);
  Finalize(m_widthPercentages);
  Finalize(m_captions);
end;

function TWorkResultsColumnInfos.GetCount : Integer;
begin
  Result:=Length(m_alignments);
end;

procedure TWorkResultsColumnInfos.Clear;
begin
  SetLength(m_alignments, 0);
  SetLength(m_widthPercentages, 0);
  SetLength(m_captions, 0);
end;

procedure TWorkResultsColumnInfos.AddInfo(nAlignment : Integer;
                                          nWidthPercentage : Integer;
                                          const sCaption : String);
var
  nI : Integer;
begin
  nI:=GetCount;

  Inc(nI);
  SetLength(m_alignments, nI);
  SetLength(m_widthPercentages, nI);
  SetLength(m_captions, nI);

  Dec(nI);
  m_alignments[nI]:=nAlignment;
  m_widthPercentages[nI]:=nWidthPercentage;
  m_captions[nI]:=sCaption;
end;

function TWorkResultsColumnInfos.GetAlignment(nIdx : Integer) : Integer;
begin
  Result:=m_alignments[nIdx];
end;

function TWorkResultsColumnInfos.GetWidthPercentage(nIdx : Integer) : Integer;
begin
  Result:=m_widthPercentages[nIdx];
end;

function TWorkResultsColumnInfos.GetCaption(nIdx : Integer) : String;
begin
  Result:=m_captions[nIdx];
end;




//////////////////////////// TWorkResults ////////////////////////////

constructor TWorkResults.Create;
begin
  m_nErrors:=0;
  m_nWarnings:=0;
  m_nSkips:=0;
  m_results:=TList.Create;
  m_types:=TIntegerList.Create;
  m_errorMessages:=TStringList.Create;
  m_blInterrupt:=False;
  m_blErrMessExist:=False;
end;

destructor TWorkResults.Destroy;
var
  nI : Integer;
begin
  // free all result objects (whatever they are)
  m_errorMessages.Destroy;
  m_types.Destroy;
  for nI:=0 to (m_results.Count - 1) do
    if (m_results.Items[nI] <> Nil) then
      TObject(m_results.Items[nI]).Destroy;
  m_results.Destroy;
end;


function TWorkResults.GetCount : Integer;
begin
  result:=m_results.Count;
end;


function TWorkResults.GetNumOfErrors : Integer;
begin
  Result:=m_nErrors;
end;


function TWorkResults.GetNumOfWarnings : Integer;
begin
  Result:=m_nWarnings;
end;


function TWorkResults.GetNumOfSkips : Integer;
begin
  Result:=m_nSkips;
end;


procedure TWorkResults.Add(toAdd : TObject;
                           nType : Integer;
                           const sErrorMessage : String = '');
begin
  // add the result
  m_results.Add(toAdd);
  m_types.Add(nType);
  m_errorMessages.Add(sErrorMessage);
  case nType of
    WORKRESULTS_TYPE_ERROR   : Inc(m_nErrors);
    WORKRESULTS_TYPE_WARNING : Inc(m_nWarnings);
    WORKRESULTS_TYPE_SKIP    : Inc(m_nSkips);
  end;
  if (sErrorMessage <> '') then
    m_blErrMessExist:=True;
end;


function TWorkResults.RenderReport(const sSeparator : String) : String;
var
  nI         : Integer;
  nJ         : Integer;
  nRowSize   : Integer;
  nCount     : Integer;
  nActLen    : Integer;
  nMaxLen    : Integer;
  nNumOfCols : Integer;
  nNumOfRows : Integer;
  nLineLen   : Integer;
  nBufSize   : Integer;
  nBufPos    : Integer;
  nSpaces    : Integer;
  nSpaceRest : Integer;
  sLine      : String;
  sPart      : String;
  keeper     : array of String;
  maxColLens : array of Integer;
  buf        : array of Char;
  arow       : TStringList;
  actRow     : TStringList;
  rows       : TList;
  colInfos   : TWorkResultsColumnInfos;

procedure CleanUp;
var
  nX : Integer;
begin
  for nX:=0 to (nCount - 1) do
    TStringList(rows.Items[nX]).Destroy;
  rows.Destroy;
  Finalize(buf);
  Finalize(maxColLens);
  colInfos.Destroy;
end;

begin
  // build a report linewise
  rows:=TList.Create;
  nCount:=GetCount;
  nRowSize:=GetDimension;
  SetLength(keeper, nRowSize);
  for nI:=0 to (nCount - 1) do begin
    Render(nI, keeper);
    arow:=TStringList.Create;
    arow.BeginUpdate;
    for nJ:=0 to (nRowSize - 1) do
      arow.Add(keeper[nJ]);
    arow.EndUpdate;
    rows.Add(arow);
  end;
  Finalize(keeper);

  // make the report...
  colInfos:=GetColInfos;

  // get the number of rows
  nNumOfRows:=rows.Count;
  if (nNumOfRows = 0) then begin
    // no rows, no fun
    Result:='';
    CleanUp;
    Exit;
  end;

  // the first row tells us the number of columns
  actRow:=TStringList(rows[0]);
  nNumOfCols:=actRow.Count;
  SetLength(maxColLens, nNumOfCols);

  // now scan for the columns with the max. size
  nLineLen:=0;
  for nI:=0 to (nNumOfCols - 1) do begin

    nMaxLen:=0;
    for nJ:=0 to (nNumOfRows - 1) do begin
      actRow:=TStringList(rows[nJ]);
      nActLen:=Length(actRow.Strings[nI]);
      if (nActLen > nMaxLen) then
        nMaxLen:=nActLen;
    end;
    maxColLens[nI]:=nMaxLen;
    Inc(nLineLen, nMaxLen);
  end;

  // we build the whole stuff via a buffer (the 2 is for the linebreaks)
  if (nNumOfCols > 1) then
    Inc(nLineLen, Length(sSeparator) * (nNumOfCols - 1));
  Inc(nLineLen, 2);
  nBufSize:=(nLineLen * nNumOfRows) + 1; // the one is for the #0
  SetLength(buf, nBufSize);

  // good, now make the report
  nBufPos:=0;
  for nI:=0 to (nNumOfRows - 1) do begin

    // build a line
    sLine:='';
    actRow:=TStringList(rows[nI]);
    for nJ:=0 to (nNumOfCols - 1) do begin

      sPart:=actRow.Strings[nJ];

      // fill up with spaces, left or right aligned
      nSpaces:=maxColLens[nJ] - Length(sPart);
      case colInfos.GetAlignment(nJ) of
        WORKRESULTS_COLALIGN_LEFT  :
          sPart:=sPart + TStrPlus.MulChar(' ', nSpaces);
        WORKRESULTS_COLALIGN_RIGHT :
          sPart:=TStrPlus.MulChar(' ', nSpaces) + sPart;
      else  // (center)
        nSpaceRest:=nSpaces and 1;
        nSpaces:=nSpaces shr 1;
        sPart:=TStrPlus.MulChar(' ', nSpaces ) + sPart +
               TStrPlus.MulChar(' ', (nSpaces + nSpaceRest));
      end;
 
      // a linebreak at the end, i. n.
      if (nJ = (nNumOfCols - 1)) then begin
        if (nI < nNumOfRows - 1) then
          sLine:=sLine + sPart + #13#10
        else
          sLine:=sLine + sPart;
      end
      else begin
        // add it with the seperator in between
        sLine:=sLine + sPart + sSeparator;
      end;
   end;

    // copy this line to the buffer
    nLineLen:=Length(sLine);
    Move(sLine[1], buf[nBufPos], nLineLen);
    Inc(nBufPos, nLineLen);
  end;

  // pass the result, clean up
  buf[nBufPos]:=#0;
  Result:=String(PChar(@buf[0]));
  CleanUp;
end;


function TWorkResults.GetEntryType(nIndex : Integer) : Integer;
begin
  Result:=m_types.Get(nIndex);
end;


procedure TWorkResults.SetInterrupt(blInterrupt : Boolean);
begin
  m_blInterrupt:=blInterrupt;
end;


function TWorkResults.GetInterrupt : Boolean;
begin
  Result:=m_blInterrupt;
end;


function TWorkResults.ErrorMessagesExist : Boolean;
begin
  Result:=m_blErrMessExist;
end;


end.
