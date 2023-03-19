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
  prototype of a list of (real) checkboxes
}

unit CheckBoxList;

interface
uses
  StdCtrls,
  Controls,
  Classes,
  CallBack;


// callback class to react on checkbox changes
type
  TCheckBoxListCallBack = class(TCallBack)
  private
    // members
    m_cb  : TCheckBox;
    m_obj : TObject;

    // sets the checkbox which fired the event
    // -> check box reference
    procedure SetCheckBox(cb : TCheckBox);

    // sets the object linked to the checkbox
    // -> check box object
    procedure SetObject(obj : TObject);

  public

    // gets the eventfiring checkbox
    // <- check box (reference)
    function GetCheckBox : TCheckBox;

    // gets the check box object
    // <- check box obj. (ref. only, of course)
    function GetObject : TObject;
  end;


type
  TCheckBoxList = class

  private
    // members
    m_nLeft    : Integer;
    m_nTop     : Integer;
    m_nSpace   : Integer;
    m_blSorted : Boolean;
    m_parent   : TWinControl;
    m_callBack : TCheckBoxListCallBack;
    m_boxes    : TList;
    m_objects  : TList;

    // handler for change events
    procedure OnClick(Sender : TObject);

  public
    // constructor
    // -> control (TPanel in a TScrollBox is recommended)
    // -> callback for change events (may be Nil)
    // -> True sort the list / False: don't sort
    constructor Create(parent : TWinControl;
                       callBack : TCheckBoxListCallBack = Nil;
                       blSorted : Boolean = False);

    // destructor
    destructor Destroy; override;

    // adds a checkbox
    // -> linked object (may be Nil)
    // <- the new checkbox (ref.)
    function Add(obj : TObject = Nil) : TCheckBox;

    // gets a check box _reference_
    // -> index of the checkbox
    function Get(nIndex : Integer) : TCheckBox;

    // gets a _reference_ to a linked object
    // -> index of the object
    function GetObject(nIndex : Integer) : TObject;

    // changes the layout (the value -1 equals "ignore, don't change"),
    // rearranges the controls (might be necessary to call ResizeParent() after)
    // -> left distance
    // -> top distance
    // -> space between the boxes
    procedure Layout(nLeft : Integer = -1;
                     nTop : Integer = -1;
                     nSpace : Integer = -1);

    // resizes the parent to fit for the list (changes the _height_ only!)
    procedure ResizeParent;

    // sorts the list
    procedure Sort;

    // clears the list
    procedure Clear;

    // gets the number of checkboxes in the list
    // <- the number of checkboxes
    function GetCount : Integer;
  end;


implementation
uses SysUtils;


//////////////////////// TCheckBoxListCallBack ////////////////////////


procedure TCheckBoxListCallBack.SetCheckBox(cb : TCheckBox);
begin
  m_cb:=cb;
end;

procedure TCheckBoxListCallBack.SetObject(obj : TObject);
begin
  m_obj:=obj;
end;

function TCheckBoxListCallBack.GetCheckBox : TCheckBox;
begin
  Result:=m_cb;
end;

function TCheckBoxListCallBack.GetObject : TObject;
begin
  Result:=m_obj;
end;


//////////////////////// TCheckBoxList ////////////////////////


constructor TCheckBoxList.Create(parent : TWinControl;
                                 callBack : TCheckBoxListCallBack = Nil;
                                 blSorted : Boolean = False);
begin
  m_parent:=parent;
  m_callBack:=callBack;
  m_blSorted:=blSorted;

  m_boxes:=TList.Create;
  m_objects:=TList.Create;

  // set a default layout
  Layout(10, 10, 20);
end;


destructor TCheckBoxList.Destroy;
var
  nI, nC : Integer;
begin
  // freeing the objects is a job for the caller, of course
  nC:=m_objects.Count - 1;
  for nI:=0 to nC do begin
    TObject(m_objects.Items[nI]).Destroy;
  end;
  m_objects.Destroy;
  // (FIXME: we don't destroy the controls - done by the parent, correct?)
  m_boxes.Destroy;
end;


function TCheckBoxList.Add(obj : TObject = Nil) : TCheckBox;
begin

  // make the new checkbox
  Result:=TCheckBox.Create(m_parent);

  // set a default layout
  Result.Width:=m_parent.ClientWidth - m_nLeft;
  Result.Caption:=''; // (just to be sure)
  Result.Left:=m_nLeft;
  Result.Top:=m_nTop + (m_boxes.Count * m_nSpace);
  Result.ParentColor:=True;
  Result.AllowGrayed:=False;
  Result.OnClick:=OnClick; // route the events to our routine

  // attach the checkbox to the parent control
  m_parent.InsertControl(Result);

  // add box (plus object to the list(s)
  m_boxes.Add(Result);
  m_objects.Add(obj);

  // sort, if necessary (FIXME: rather inefficient, searching the right index
  // will speed up things much more)
  if (m_blSorted) then
    Sort;
end;


function TCheckBoxList.Get(nIndex : Integer) : TCheckBox;
begin
  Result:=m_boxes.Items[nIndex];
end;


function TCheckBoxList.GetObject(nIndex : Integer) : TObject;
begin
  Result:=m_objects.Items[nIndex];
end;


procedure TCheckBoxList.Layout(nLeft : Integer = -1;
                               nTop : Integer = -1;
                               nSpace : Integer = -1);
var
  nI : Integer;
  nY : Integer;
  cb : TCheckBox;
begin
  // copy all necessary parameters
  if (nLeft <> -1) then
    m_nLeft:=nLeft;
  if (nTop <> -1) then
    m_nTop:=nTop;
  if (nSpace <> -1) then
    m_nSpace:=nSpace;

  // do layout, quite easy
  nY:=m_nTop;
  for nI:=0 to (m_boxes.Count - 1) do begin
    cb:=m_boxes.Items[nI];
    cb.Left:=m_nLeft;
    cb.Top:=nY;
    Inc(nY, m_nSpace);
  end;

end;


procedure TCheckBoxList.ResizeParent;
begin
  m_parent.ClientHeight:=(m_nTop shl 1) +  // (mul 2 just looks better, no?)
                         (m_nSpace * m_boxes.Count);
end;


procedure TCheckBoxList.Sort;
var
  nI, nJ   : Integer;
  nMaximal : Integer;
  nOffset  : Integer;
  nLimit   : Integer;
  nSwap    : Integer;

procedure SwapBoxes(nIdx1, nIdx2 : Integer);
begin
  m_boxes.Exchange(nIdx1, nIdx2);
  m_objects.Exchange(nIdx1, nIdx2);
end;

function CompareBoxes(nIdx1, nIdx2 : Integer) : Integer;
begin
  Result:=CompareText(TCheckBox(m_boxes.Items[nIdx1]).Caption,
                      TCheckBox(m_boxes.Items[nIdx2]).Caption)
end;

begin

  // (using the shellsort algorithm)
  nMaximal:=m_boxes.Count - 1;
  nOffset:=nMaximal shr 1;
  while (nOffset > 0) do begin
    nLimit:=nMaximal - nOffset;

    repeat
      nSwap:=0;
      for nI:=0 to nLimit do begin
        nJ:=nI + nOffset;
        if (CompareBoxes(nI, nJ) > 0) then begin
          SwapBoxes(nI, nJ);
          nSwap:=nI;
        end;
      end;
      nLimit:=nSwap - nOffset;
    until (nSwap = 0);

    nOffset:=nOffset shr 1;
  end;

  // show the sorted items
  Layout;

  // reset the tab orders
  for nI:=0 to (GetCount - 1) do
    TCheckBox(m_boxes.Items[nI]).TabOrder:=nI;
end;


procedure TCheckBoxList.Clear;
var
  nI : Integer;
begin
  for nI:=0 to (m_boxes.Count - 1) do begin
    m_parent.RemoveControl(m_boxes.Items[nI]);
    TCheckBox(m_boxes.Items[nI]).Destroy;
  end;
  m_boxes.Clear;
  m_objects.Clear;
end;


procedure TCheckBoxList.OnClick(Sender : TObject);
var
  nI : Integer;
begin
  if (m_callBack <> Nil) then begin
    // search for the selected box (direct reference comparison)
    for nI:=0 to (m_boxes.Count - 1) do
      if (Sender = m_boxes.Items[nI]) then begin
        m_callBack.SetCheckBox(m_boxes.Items[nI]);
        m_callBack.SetObject(m_objects.Items[nI]);
        m_callBack.CallBack;
        Break;
      end;
    // (...this should never happen)
  end;
end;


function TCheckBoxList.GetCount : Integer;
begin
  Result:=m_boxes.Count;
end;


end.
