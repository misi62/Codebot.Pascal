
(********************************************************)
(*                                                      *)
(*  Private Distribution of Codebot Class Library       *)
(*  Version: 2.00.00                                    *)
(*  Date: February 2010                                 *)
(*                                                      *)
(********************************************************)

unit XMLObjects;

interface

{$I CODEBOT.INC}

uses
  ActiveX;

type
  INodes = interface;
  INode = interface;
  IAttributes = interface;
  IAttribute = interface;

{ IDocumentInterface }

  IDocumentInterface = interface
    ['{53D4F287-A420-4792-B176-231A89229CE4}']
    function GetController: IUnknown;
    property Controller: IUnknown read GetController;
  end;

{ ITextInterface }

  ITextInterface = interface(IDocumentInterface)
    ['{31144DC6-AB12-4FE2-BF39-277E58ACC4B4}']
    function GetText: string;
    procedure SetText(Value: string);
    property Text: string read GetText write SetText;
  end;

{ IDocument }

  IDocument = interface(ITextInterface)
    ['{37F774E7-7F10-4E23-9EE8-5674C37FB984}']
    function GetNodes: INodes;
    function GetRoot: INode;
    procedure SetRoot(Node: INode);
    function GetStyleSheet: string;
    procedure SetStyleSheet(Value: string);
    function CreateNode(const Name: string): INode;
    function ForceRoot(const Name: string): INode;
    procedure Instruct(const Target, Data: string);
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    function Transform(StyleSheet: IDocument): string;
    property Nodes: INodes read GetNodes;
    property Root: INode read GetRoot write SetRoot;
    property StyleSheet: string read GetStyleSheet write SetStyleSheet;
  end;

{ INodes }

  INodes = interface(IDocumentInterface)
    ['{97F84504-798A-4FD7-A8A5-98C12F4EB9E1}']
    function GetCount: Integer;
    procedure SetNode(Index: Integer; Node: INode);
    function GetNode(Index: Integer): INode;
    function GetParent: INode;
    function Add(const Name: string): INode;
    procedure Append(Node: INode);
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Insert(Index: Integer; Node: INode);
    procedure Move(CurIndex, NewIndex: Integer);
    procedure Remove(Node: INode);
    procedure Replace(OldNode, NewNode: INode);
    property Count: Integer read GetCount;
    property Node[Index: Integer]: INode read GetNode write SetNode; default;
    property Parent: INode read GetParent;
  end;

{ IElement }

  IElement = interface(ITextInterface)
    ['{C8C58739-1CCA-4969-88D3-201FF5187D73}']
    function GetDocument: IDocument;
    function GetParent: INode;
    function GetName: string;
    function GetValue: string;
    procedure SetValue(const Value: string);
    property Document: IDocument read GetDocument;
    property Parent: INode read GetParent;
    property Name: string read GetName;
    property Value: string read GetValue write SetValue;
  end;

  { IFiler }

  IFiler = interface
    ['{4F115D4C-B730-497C-9061-B40946CB8DD9}']
    procedure ReadBinary(const Name: string; Stream: IStream);
    procedure WriteBinary(const Name: string; Stream: IStream);
    function ReadBool(const Name: string; Default: Boolean = False; Stored: Boolean = True): Boolean;
    procedure WriteBool(const Name: string; Value: Boolean);
    function ReadDate(const Name: string; const Default: TDateTime = 0; Stored: Boolean = True): TDateTime;
    procedure WriteDate(const Name: string; const Value: TDateTime);
    function ReadDateTime(const Name: string; const Default: TDateTime = 0; Stored: Boolean = True): TDateTime;
    procedure WriteDateTime(const Name: string; const Value: TDateTime);
    function ReadTime(const Name: string; const Default: TDateTime = 0; Stored: Boolean = True): TDateTime;
    procedure WriteTime(const Name: string; const Value: TDateTime);
    function ReadFloat(const Name: string; const Default: Double = 0; Stored: Boolean = True): Double;
    procedure WriteFloat(const Name: string; const Value: Double);
    function ReadInteger(const Name: string; Default: Integer = 0; Stored: Boolean = True): Integer;
    procedure WriteInteger(const Name: string; Value: Integer);
    function ReadString(const Name: string; const Default: string = ''; Stored: Boolean = True): string;
    procedure WriteString(const Name: string; const Value: string);
    function ReadObject: TObject; overload;
    function ReadObject(const Name: string; const Default: TObject = nil; Stored: Boolean = True): TObject; overload;
    procedure WriteObject(Value: TObject); overload;
    procedure WriteObject(const Name: string; Value: TObject); overload;
    function Read(const Name: string): Variant;
    procedure Write(const Name: string; const Value: Variant);
  end;

{ INode }

  INode = interface(IElement)
    ['{5B1F94C0-3487-4421-8CCD-1DE8113C2C8E}']
    function GetAttributes: IAttributes;
    function GetFiler: IFiler;
    function GetNodes: INodes;
    function Clone(Deep: Boolean = True): INode;
    function FindAttribute(const Name: string): IAttribute;
    function FindNode(const Name: string): INode;
    function FindNodes(const Name: string): INodes;
    function ForceNode(const Name: string): INode;
    property Attributes: IAttributes read GetAttributes;
    property Filer: IFiler read GetFiler;
    property Nodes: INodes read GetNodes;
  end;

{ IAttributes }

  IAttributes = interface(IDocumentInterface)
    ['{8EB6647F-211C-4786-B7A2-0B58591B0300}']
    function GetCount: Integer;
    function GetFiler: IFiler;
    function GetNamedAttribute(const Name: string): IAttribute;
    function GetIndexedAttribute(Index: Integer): IAttribute;
    function Add(const Name: string): IAttribute;
    procedure Clear;
    procedure Remove(Node: IAttribute);
    property Count: Integer read GetCount;
    property Filer: IFiler read GetFiler;
    property NamedAttribute[const Name: string]: IAttribute read GetNamedAttribute; default;
    property IndexedAttribute[Index: Integer]: IAttribute read GetIndexedAttribute;
  end;

{ IAttribute }

  IAttribute = interface(IElement)
    ['{DFDB3B47-B64E-49D9-88AB-8D490C69D661}']
  end;

function Beautify(const XML: string): string; overload;
procedure Beautify(const XML: string; const FileName: string); overload;

var
  CreateDocument: function: IDocument;

implementation

uses
  XMLParser, QuickString;

function Beautify(const XML: string): string;
var
  D: IDocument;
begin
  D := CreateDocument;
  D.Text := QuickReplace(XML,  '><', '>'#13#10'<');
  Result := QuickReplace(D.Text, #9, '  ');
end;

procedure Beautify(const XML: string; const FileName: string);
var
  D: IDocument;
  S: string;
begin
  D := CreateDocument;
  S := QuickReplace(XML,  '><', '>'#13#10'<');
  S := QuickReplace(S, #9, '  ');
  D.Text := S;
  D.SaveToFile(FileName);
end;

initialization
  CreateDocument := MSXMLCreateDocument;
end.
