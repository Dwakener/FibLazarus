{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2010 Embarcadero Technologies, Inc. }
{                                                       }
{   Copyright and license exceptions noted in source    }
{                                                       }
{*******************************************************}

unit System; { Predefined constants, types, procedures, }
             { and functions (such as True, Integer, or }
             { Writeln) do not have actual declarations.}
             { Instead they are built into the compiler }
             { and are treated as if they were declared }
             { at the beginning of the System unit.     }

{$H+,I-,R-,O+,W-}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNSAFE_TYPE OFF}

{ L- should never be specified.

  The IDE needs to find DebugHook (through the C++
  compiler sometimes) for integrated debugging to
  function properly.

  ILINK will generate debug info for DebugHook if
  the object module has not been compiled with debug info.

  ILINK will not generate debug info for DebugHook if
  the object module has been compiled with debug info.

  Thus, the Pascal compiler must be responsible for
  generating the debug information for that symbol
  when a debug-enabled object file is produced.
}

interface

{$IFDEF MACOSX}
// Stack alignment code is nightmarish to manage in assembler, so:
{$DEFINE PUREPASCAL}
{$ENDIF MACOSX}

(* You can use RTLVersion in $IF expressions to test the runtime library
  version level independently of the compiler version level.
  Example:  {$IF RTLVersion >= 16.2} ... {$IFEND}                  *)

const
  RTLVersion = 21.00;

{$EXTERNALSYM CompilerVersion}

(*
const
  CompilerVersion = 0.0;

  CompilerVersion is assigned a value by the compiler when
  the system unit is compiled.  It indicates the revision level of the
  compiler features / language syntax, which may advance independently of
  the RTLVersion.  CompilerVersion can be tested in $IF expressions and
  should be used instead of testing for the VERxxx conditional define.
  Always test for greater than or less than a known revision level.
  It's a bad idea to test for a specific revision level.
*)

{$IFDEF DECLARE_GPL}
(* The existence of the GPL symbol indicates that the System unit
  and the rest of the Delphi runtime library were compiled for use
  and distribution under the terms of the GNU General Public License (GPL).
  Under the terms of the GPL, all applications compiled with the
  GPL version of the Delphi runtime library must also be distributed
  under the terms of the GPL.
  For more information about the GNU GPL, see
  http://www.gnu.org/copyleft/gpl.html

  The GPL symbol does not exist in the Delphi runtime library
  purchased for commercial/proprietary software development.

  If your source code needs to know which licensing model it is being
  compiled into, you can use {$IF DECLARED(GPL)}...{$IFEND} to
  test for the existence of the GPL symbol.  The value of the
  symbol itself is not significant.   *)

const
  GPL = True;
{$ENDIF}

{ Delphi built-in types for .hpp/.obj support }
{   Most of built-in types are defined in sysmac.h }
{   Pointer types should be mangled by the compiler for constness}
{$EXTERNALSYM Boolean     'bool'             } {$OBJTYPENAME Boolean    'Bo'}
{$NODEFINE    ShortInt    'ShortInt'         } {$OBJTYPENAME ShortInt   'Bzc'} { signed char }
{-EXTERNALSYM ShortInt    'signed char'      } {-OBJTYPENAME ShortInt   'Bzc'}
{$EXTERNALSYM SmallInt    'short'            } {$OBJTYPENAME SmallInt   'Bs'}
{$EXTERNALSYM Integer     'int'              } {$OBJTYPENAME Integer    'Bi'}
{$NODEFINE    Byte        'Byte'             } {$OBJTYPENAME Byte       'Buc'} { unsigned char }
{$NODEFINE    Word        'Word'             } {$OBJTYPENAME Word       'Bus'} { unsigned short }
{$EXTERNALSYM Cardinal    'unsigned'         } {$OBJTYPENAME Cardinal   'Bui'}
{$EXTERNALSYM Int64       '__int64'          } {$OBJTYPENAME Int64      'Bj'}
{$EXTERNALSYM UInt64      'unsigned __int64' } {$OBJTYPENAME UInt64     'Buj'}
{$EXTERNALSYM NativeInt   'int'              } {$OBJTYPENAME NativeInt  'Bi'}
{$EXTERNALSYM NativeUInt  'unsigned'         } {$OBJTYPENAME NativeUInt 'Bui'}
{$EXTERNALSYM Single      'float'            } {$OBJTYPENAME Single     'Bf'}
{$EXTERNALSYM Double      'double'           } {$OBJTYPENAME Double     'Bd'}
{$NODEFINE    Extended    'Extended'         } {$OBJTYPENAME Extended   'Bg'} { long double }
{$NODEFINE    Currency    'Currency'    'CurrencyBase'    } {$OBJTYPENAME Currency    'NCurrency'}
{$NODEFINE    Comp        'Comp'        'CompBase'        } {$OBJTYPENAME Comp        'NComp'}
{$EXTERNALSYM Real        'double'                        } {$OBJTYPENAME Real        'Bd'}
{$NODEFINE    ShortString 'ShortString' 'ShortStringBase' } {$OBJTYPENAME ShortString 'N%SmallString$iuc$255%'}
{$NODEFINE    OpenString  'OpenString'       } {$OBJTYPENAME OpenString 'Bxpc'} { char * const }
{$NODEFINE    File        'file'             } {$OBJTYPENAME File       'Nfile'}
{$NODEFINE    Text        'TextFile'         } {$OBJTYPENAME Text       'NTextfile'}
{$NODEFINE    ByteBool    'ByteBool'         } {$OBJTYPENAME ByteBool   'Buc'} { unsigned char }
{$NODEFINE    WordBool    'WordBool'         } {$OBJTYPENAME WordBool   'Bus'} { unsigned short }
{$EXTERNALSYM LongBool    'BOOL'             } {$OBJTYPENAME LongBool   'Bi'}  { int } { from windef.h }
{$NODEFINE    Real48      } { not supported in C++ }
{$EXTERNALSYM Pointer     'void *'    }
{$NODEFINE    PWideChar   'WideChar *'}
{$EXTERNALSYM PAnsiChar   'char *'    }
{$NODEFINE    Variant     } { defined in sysvari.h }
{$NODEFINE    OleVariant  } { defined in sysvari.h }
{$NODEFINE    LongInt     } { alias of Integer     }
{$NODEFINE    LongWord    } { alias of Cardinal    }
{$NODEFINE    TextFile    } { alias of Text        }
{$IFDEF UNICODE}
  {$EXTERNALSYM AnsiChar     'char'          } {$OBJTYPENAME AnsiChar 'Bc'}
  {$IFDEF LINUX}
    {$NODEFINE  Char         'WideChar'      } {$OBJTYPENAME Char     'Bus'} { unsigned short }
                                               {-OBJTYPENAME Char     'BCs'} { char16_t }
  {$ELSE}
    {$NODEFINE  Char         'WideChar'      } {$OBJTYPENAME Char     'Bb'}  { wchar_t }
                                               {-OBJTYPENAME Char     'BCs'} { char16_t }
  {$ENDIF}
  {$NODEFINE    string       'UnicodeString' } {$OBJTYPENAME string   'NUnicodeString'} { defined in vcl/ustring.h }
  {-NODEFINE    string       'String'        } {$OBJTYPENAME string   'NUnicodeString'} { defined in vcl/ustring.h }
  {$NODEFINE    AnsiString   } { defined in vcl/dstring.h }
  {$NODEFINE    WideString   } { defined in vcl/wstring.h }
  {$NODEFINE    PChar        } { alias of PWideChar  }
  {$NODEFINE    WideChar     } { alias of Char       }
  {$NODEFINE    UnicodeString} { alias of string     }
{$ELSE}
  {$EXTERNALSYM Char         'char'          } {$OBJTYPENAME Char     'Bc'}
  {$IFDEF LINUX}
    {$NODEFINE  WideChar     'WideChar'      } {$OBJTYPENAME WideChar 'Bus'} { unsigned short }
                                               {-OBJTYPENAME WideChar 'BCs'} { char16_t }
  {$ELSE}
    {$NODEFINE  WideChar     'WideChar'      } {$OBJTYPENAME WideChar 'Bb'}  { wchar_t }
                                               {-OBJTYPENAME WideChar 'BCs'} { char16_t }
  {$ENDIF}
  {$NODEFINE    string       'AnsiString'    } { defined in vcl/dstring.h }
  {-NODEFINE    string       'String'        } { defined in vcl/dstring.h }
  {$NODEFINE    WideString   } { defined in vcl/wstring.h }
  {$NODEFINE    UnicodeString} { defined in vcl/ustring.h }
  {$NODEFINE    PChar        } { alias of PAnsiChar  }
  {$NODEFINE    AnsiChar     } { alias of Char       }
  {$NODEFINE    AnsiString   } { alias of string     }
{$ENDIF}

(*$HPPEMIT 'namespace System' *)
(*$HPPEMIT '{' *)
(*$HPPEMIT '  typedef Shortint ShortInt;' *)
(*$HPPEMIT '  typedef Smallint SmallInt;' *)
(*$HPPEMIT '  typedef Longint LongInt;' *)
(*$HPPEMIT '}' *)

type
  CppLongInt  = type LongInt;  {$EXTERNALSYM CppLongInt  'long'         } {$OBJTYPENAME CppLongInt  'Bl'}
  CppULongInt = type LongWord; {$EXTERNALSYM CppULongInt 'unsigned long'} {$OBJTYPENAME CppULongInt 'Bul'}

{ Useful alias types }
type
  Int8   = ShortInt;
  Int16  = SmallInt;
  Int32  = Int;
  UInt8  = Byte;
  UInt16 = Word;
  UInt32 = Cardinal;

const
{ Variant type codes (wtypes.h) }

  varEmpty    = $0000; { vt_empty        0 }
  varNull     = $0001; { vt_null         1 }
  varSmallint = $0002; { vt_i2           2 }
  varInteger  = $0003; { vt_i4           3 }
  varSingle   = $0004; { vt_r4           4 }
  varDouble   = $0005; { vt_r8           5 }
  varCurrency = $0006; { vt_cy           6 }
  varDate     = $0007; { vt_date         7 }
  varOleStr   = $0008; { vt_bstr         8 }
  varDispatch = $0009; { vt_dispatch     9 }
  varError    = $000A; { vt_error       10 }
  varBoolean  = $000B; { vt_bool        11 }
  varVariant  = $000C; { vt_variant     12 }
  varUnknown  = $000D; { vt_unknown     13 }
//varDecimal  = $000E; { vt_decimal     14 } {UNSUPPORTED as of v6.x code base}
//varUndef0F  = $000F; { undefined      15 } {UNSUPPORTED per Microsoft}
  varShortInt = $0010; { vt_i1          16 }
  varByte     = $0011; { vt_ui1         17 }
  varWord     = $0012; { vt_ui2         18 }
  varLongWord = $0013; { vt_ui4         19 }
  varInt64    = $0014; { vt_i8          20 }
  varUInt64   = $0015; { vt_ui8         21 }
{  if adding new items, update Variants' varLast, BaseTypeMap and OpTypeMap }

  varStrArg   = $0048; { vt_clsid        72 }
  varString   = $0100; { Pascal string  256 } {not OLE compatible }
  varAny      = $0101; { Corba any      257 } {not OLE compatible }
  varUString  = $0102; { Unicode string 258 } {not OLE compatible}
  // custom types range from $110 (272) to $7FF (2047)

  varTypeMask = $0FFF;
  varArray    = $2000;
  varByRef    = $4000;

{ TVarRec.VType values }

  vtInteger       = 0;
  vtBoolean       = 1;
  vtChar          = 2;
  vtExtended      = 3;
  vtString        = 4;
  vtPointer       = 5;
  vtPChar         = 6;
  vtObject        = 7;
  vtClass         = 8;
  vtWideChar      = 9;
  vtPWideChar     = 10;
  vtAnsiString    = 11;
  vtCurrency      = 12;
  vtVariant       = 13;
  vtInterface     = 14;
  vtWideString    = 15;
  vtInt64         = 16;
  vtUnicodeString = 17;

{ Virtual method table entries }
  vmtSelfPtr           = -88;
  vmtIntfTable         = -84;
  vmtAutoTable         = -80;
  vmtInitTable         = -76;
  vmtTypeInfo          = -72;
  vmtFieldTable        = -68;
  vmtMethodTable       = -64;
  vmtDynamicTable      = -60;
  vmtClassName         = -56;
  vmtInstanceSize      = -52;
  vmtParent            = -48;
  vmtEquals            = -44 deprecated 'Use VMTOFFSET in asm code';
  vmtGetHashCode       = -40 deprecated 'Use VMTOFFSET in asm code';
  vmtToString          = -36 deprecated 'Use VMTOFFSET in asm code';
  vmtSafeCallException = -32 deprecated 'Use VMTOFFSET in asm code';
  vmtAfterConstruction = -28 deprecated 'Use VMTOFFSET in asm code';
  vmtBeforeDestruction = -24 deprecated 'Use VMTOFFSET in asm code';
  vmtDispatch          = -20 deprecated 'Use VMTOFFSET in asm code';
  vmtDefaultHandler    = -16 deprecated 'Use VMTOFFSET in asm code';
  vmtNewInstance       = -12 deprecated 'Use VMTOFFSET in asm code';
  vmtFreeInstance      = -8 deprecated 'Use VMTOFFSET in asm code';
  vmtDestroy           = -4 deprecated 'Use VMTOFFSET in asm code';

  vmtQueryInterface    = 0 deprecated 'Use VMTOFFSET in asm code';
  vmtAddRef            = 4 deprecated 'Use VMTOFFSET in asm code';
  vmtRelease           = 8 deprecated 'Use VMTOFFSET in asm code';
  vmtCreateObject      = 12 deprecated 'Use VMTOFFSET in asm code';

  { Hidden TObject field info }
  hfFieldSize          = 4;
  hfMonitorOffset      = 0;

{ RTTI Visibility }
type
  TVisibilityClasses = set of (vcPrivate, vcProtected, vcPublic, vcPublished);

const
  { These constants represent the default settings built into the compiler.
    For classes, these settings are normally inherited from TObject. }
  DefaultMethodRttiVisibility = [vcPublic, vcPublished];
  DefaultFieldRttiVisibility = [vcPrivate..vcPublished];
  DefaultPropertyRttiVisibility = [vcPublic, vcPublished];

type
  { Default RTTI settings }
  {$RTTI INHERIT
      METHODS(DefaultMethodRttiVisibility)
      FIELDS(DefaultFieldRttiVisibility)
      PROPERTIES(DefaultPropertyRttiVisibility)}

  { Minimal RTTI generation henceforth in this file }
  {.$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}

  TArray<T> = array of T;

  TObject = class;
  {$NODEFINE TObject}   { defined in systobj.h }

  TClass = class of TObject;
  {$NODEFINE TClass}    { defined in systobj.h }

  HRESULT = type Longint;  { from wtypes.h }
  {$EXTERNALSYM HRESULT} {$OBJTYPENAME HRESULT 'Bl'} { long }

  PGUID = ^TGUID;
  TGUID = packed record
    D1: LongWord;
    D2: Word;
    D3: Word;
    D4: array[0..7] of Byte;
  end;
  {$NODEFINE PGUID}             { defined in sysmac.h }
  {$EXTERNALSYM TGUID 'GUID' }  { defined in sysmac.h }
  {$OBJTYPENAME TGUID 'N_GUID'}
  { Type 'GUID' in C++ is alias of '_GUID' and defined in guiddef.h (wtypes.h) }

  PInterfaceEntry = ^TInterfaceEntry;
  TInterfaceEntry = packed record
    IID: TGUID;
    VTable: Pointer;
    IOffset: Integer;
    ImplGetter: Integer;
  end;
  {-NODEFINE PInterfaceEntry}   { defined in systobj.h }
  {-NODEFINE TInterfaceEntry}   { defined in systobj.h }

  PInterfaceTable = ^TInterfaceTable;
  TInterfaceTable = packed record
    EntryCount: Integer;
    Entries: array[0..9999] of TInterfaceEntry;
  end;
  {-NODEFINE PInterfaceTable}   { defined in systobj.h }
  {-NODEFINE TInterfaceTable}   { defined in systobj.h }

  TMethod = record
    Code, Data: Pointer;
  end;
  {-NODEFINE TMethod}           { defined in sysmac.h }

{ TObject.Dispatch accepts any data type as its Message parameter.  The
  first 2 bytes of the data are taken as the message id to search for
  in the object's message methods.  TDispatchMessage is an example of
  such a structure with a word field for the message id.
}
  TDispatchMessage = record
    MsgID: Word;
  end;

  TObject = class
  public
    constructor Create;
    procedure Free;
    class function InitInstance(Instance: Pointer): TObject;
    procedure CleanupInstance;
    function ClassType: TClass; inline;
    class function ClassName: string;
    class function ClassNameIs(const Name: string): Boolean;
    class function ClassParent: TClass;
    class function ClassInfo: Pointer; inline;
    class function InstanceSize: Longint; inline;
    class function InheritsFrom(AClass: TClass): Boolean;
    class function MethodAddress(const Name: ShortString): Pointer; overload;
    class function MethodAddress(const Name: string): Pointer; overload;
    class function MethodName(Address: Pointer): string;
    function FieldAddress(const Name: ShortString): Pointer; overload;
    function FieldAddress(const Name: string): Pointer; overload;
    function GetInterface(const IID: TGUID; out Obj): Boolean;
    class function GetInterfaceEntry(const IID: TGUID): PInterfaceEntry;
    class function GetInterfaceTable: PInterfaceTable;
    class function UnitName: string;
    function Equals(Obj: TObject): Boolean; virtual;
    function GetHashCode: Integer; virtual;
    function ToString: string; virtual;
    function SafeCallException(ExceptObject: TObject;
      ExceptAddr: Pointer): HResult; virtual;
    procedure AfterConstruction; virtual;
    procedure BeforeDestruction; virtual;
    procedure Dispatch(var Message); virtual;
    procedure DefaultHandler(var Message); virtual;
    class function NewInstance: TObject; virtual;
    procedure FreeInstance; virtual;
    destructor Destroy; virtual;
  end;

  PPMonitor = ^PMonitor;
  PMonitor = ^TMonitor;
  TMonitor = record
  strict private
    type
      PWaitingThread = ^TWaitingThread;
      TWaitingThread = record
        Next: PWaitingThread;
        Thread: Cardinal;
        WaitEvent: Pointer;
      end;
    var
      FLockCount: Integer;
      FRecursionCount: Integer;
      FOwningThread: Cardinal;
      FLockEvent: Pointer;
      FSpinCount: Integer;
      FWaitQueue: PWaitingThread;
    procedure QueueWaiter(var WaitingThread: TWaitingThread);
    procedure RemoveWaiter(var WaitingThread: TWaitingThread);
    function DequeueWaiter: PWaitingThread;
    function GetEvent: Pointer;
    function CheckOwningThread: Cardinal;
    class procedure CheckMonitorSupport; static; inline;
    class function Create: PMonitor; static;
    // Make sure the following Destroy overload is always
    // listed first since it is called from an asm block
    // and there is no overload-resolution done from an
    // assembler symbol reference
    class procedure Destroy(AObject: TObject); overload; static;
    class function GetFieldAddress(AObject: TObject): PPMonitor; inline; static;
    class function GetMonitor(AObject: TObject): PMonitor; static;
    procedure Destroy; overload;
    function Enter(Timeout: Cardinal): Boolean; overload;
    procedure Exit; overload;
    function TryEnter: Boolean; overload;
    function Wait(Timeout: Cardinal): Boolean; overload;
    procedure Pulse; overload;
    procedure PulseAll; overload;
  public
    class procedure SetSpinCount(AObject: TObject; ASpinCount: Integer); static;
    class procedure Enter(AObject: TObject); overload; static; inline;
    class function Enter(AObject: TObject; Timeout: Cardinal): Boolean; overload; static;
    class procedure Exit(AObject: TObject); overload; static;
    class function TryEnter(AObject: TObject): Boolean; overload; static;
    class function Wait(AObject: TObject; Timeout: Cardinal): Boolean; overload; static;
    class procedure Pulse(AObject: TObject); overload; static;
    class procedure PulseAll(AObject: TObject); overload; static;
  end;

const
  INFINITE = Cardinal($FFFFFFFF);       {$EXTERNALSYM INFINITE}

function MonitorEnter(AObject: TObject; Timeout: Cardinal = INFINITE): Boolean; inline;
function MonitorTryEnter(AObject: TObject): Boolean; inline;
procedure MonitorExit(AObject: TObject); inline;
function MonitorWait(AObject: TObject; Timeout: Cardinal): Boolean; inline;
procedure MonitorPulse(AObject: TObject); inline;
procedure MonitorPulseAll(AObject: TObject); inline;
procedure MemoryBarrier;

procedure YieldProcessor; {$EXTERNALSYM YieldProcessor }

const
  S_OK = 0;                             {$EXTERNALSYM S_OK}
  S_FALSE = $00000001;                  {$EXTERNALSYM S_FALSE}
  E_NOINTERFACE = HRESULT($80004002);   {$EXTERNALSYM E_NOINTERFACE}
  E_UNEXPECTED = HRESULT($8000FFFF);    {$EXTERNALSYM E_UNEXPECTED}
  E_NOTIMPL = HRESULT($80004001);       {$EXTERNALSYM E_NOTIMPL}

type
  IInterface = interface
    ['{00000000-0000-0000-C000-000000000046}']
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;
  {$NODEFINE IInterface}        { defined in sysmac.h }

  IUnknown = IInterface;
  {$EXTERNALSYM IUnknown}       { from unknwn.h or sysmac.h }
{$M+}
  IInvokable = interface(IInterface)
  end;
{$M-}
  {$NODEFINE IInvokable}        { defined in sysmac.h }

  IEnumerator = interface(IInterface)
    function GetCurrent: TObject;
    function MoveNext: Boolean;
    procedure Reset;
    property Current: TObject read GetCurrent;
  end;

  IEnumerable = interface(IInterface)
    function GetEnumerator: IEnumerator;
  end;

  IEnumerator<T> = interface(IEnumerator)
    function GetCurrent: T;
    property Current: T read GetCurrent;
  end;

  IEnumerable<T> = interface(IEnumerable)
    function GetEnumerator: IEnumerator<T>;
  end;

  IComparable = interface(IInterface)
    function CompareTo(Obj: TObject): Integer;
  end;

  IComparable<T> = interface(IComparable)
    function CompareTo(Value: T): Integer;
  end;

  IEquatable<T> = interface(IInterface)
    function Equals(Value: T): Boolean;
  end;

  IDispatch = interface(IUnknown)
    ['{00020400-0000-0000-C000-000000000046}']
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
  end;
  {$EXTERNALSYM IDispatch}      { from oaidl.h (oleauto.h) }

{ TInterfacedObject provides a threadsafe default implementation
  of IInterface.  You should use TInterfaceObject as the base class
  of objects implementing interfaces.  }

  TInterfacedObject = class(TObject, IInterface)
  protected
    FRefCount: Integer;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;
    property RefCount: Integer read FRefCount;
  end;
  {$NODEFINE TInterfacedObject}         { defined in systobj.h }

  TInterfacedClass = class of TInterfacedObject;

{ TAggregatedObject and TContainedObject are suitable base
  classes for interfaced objects intended to be aggregated
  or contained in an outer controlling object.  When using
  the "implements" syntax on an interface property in
  an outer object class declaration, use these types
  to implement the inner object.

  Interfaces implemented by aggregated objects on behalf of
  the controller should not be distinguishable from other
  interfaces provided by the controller.  Aggregated objects
  must not maintain their own reference count - they must
  have the same lifetime as their controller.  To achieve this,
  aggregated objects reflect the reference count methods
  to the controller.

  TAggregatedObject simply reflects QueryInterface calls to
  its controller.  From such an aggregated object, one can
  obtain any interface that the controller supports, and
  only interfaces that the controller supports.  This is
  useful for implementing a controller class that uses one
  or more internal objects to implement the interfaces declared
  on the controller class.  Aggregation promotes implementation
  sharing across the object hierarchy.

  TAggregatedObject is what most aggregate objects should
  inherit from, especially when used in conjunction with
  the "implements" syntax.  }

  TAggregatedObject = class(TObject)
  private
    FController: Pointer;  // weak reference to controller
    function GetController: IInterface;
  protected
    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create(const Controller: IInterface);
    property Controller: IInterface read GetController;
  end;
  {$NODEFINE TAggregatedObject} { defined in systobj.h }

  { TContainedObject is an aggregated object that isolates
    QueryInterface on the aggregate from the controller.
    TContainedObject will return only interfaces that the
    contained object itself implements, not interfaces
    that the controller implements.  This is useful for
    implementing nodes that are attached to a controller and
    have the same lifetime as the controller, but whose
    interface identity is separate from the controller.
    You might do this if you don't want the consumers of
    an aggregated interface to have access to other interfaces
    implemented by the controller - forced encapsulation.
    This is a less common case than TAggregatedObject.  }

  TContainedObject = class(TAggregatedObject, IInterface)
  protected
    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
  end;
  {$NODEFINE TContainedObject}  { defined in systobj.h }

  TClassHelperBase = class(TInterfacedObject, IInterface)
  protected
    FInstance: TObject;
    constructor _Create(Instance: TObject);
  end;

  TClassHelperBaseClass = class of TClassHelperBase;
  {$NODEFINE TClassHelperBaseClass}
  {$NODEFINE TClassHelperBase}

  { The base class for all custom attributes. Attribute
    instances created by the RTTI unit are owned by those
    members to which they apply. }
  TCustomAttribute = class(TObject)
  end;
  {$NODEFINE TCustomAttribute}

  PShortString = ^ShortString;
  PAnsiString = ^AnsiString;
  PWideString = ^WideString;
  PUnicodeString = ^UnicodeString;
  {$IFNDEF UNICODE}
  PString = PAnsiString;
  {$ELSE}
  PString = PUnicodeString;
  {$ENDIF}
  {$NODEFINE PShortString}      { defined in sysmac.h }
  {$NODEFINE PAnsiString}       { defined in sysmac.h }
  {$NODEFINE PWideString}       { defined in sysmac.h }
  {$NODEFINE PUnicodeString}    { defined in sysmac.h }
  {$NODEFINE PString}           { defined in sysmac.h }

  UCS2Char = WideChar;
  PUCS2Char = PWideChar;
  UCS4Char = type LongWord;
  {$NODEFINE UCS4Char}          { defined in sysmac.h }
  {-OBJTYPENAME UCS4Char 'BCt'}
  PUCS4Char = ^UCS4Char;
  {$NODEFINE PUCS4Char}         { defined in sysmac.h }

  TUCS4CharArray = array [0..$effffff] of UCS4Char;
  PUCS4CharArray = ^TUCS4CharArray;
  {$NODEFINE TUCS4CharArray}
  (*$HPPEMIT 'namespace System' *)
  (*$HPPEMIT '{' *)
  (*$HPPEMIT '  typedef UCS4Char* TUCS4CharArray;' *)
  (*$HPPEMIT '}' *)

  UCS4String = array of UCS4Char;
  {$NODEFINE UCS4String}        { defined in sysmac.h }

  UTF8String = type AnsiString(65001);
  PUTF8String = ^UTF8String;

  RawByteString = type AnsiString($ffff);
  PRawByteString = ^RawByteString;

  IntegerArray  = array[0..$effffff] of Integer;
  PIntegerArray = ^IntegerArray;
  {$NODEFINE IntegerArray}
  (*$HPPEMIT 'namespace System' *)
  (*$HPPEMIT '{' *)
  (*$HPPEMIT '  typedef int* IntegerArray;' *)
  (*$HPPEMIT '}' *)

  PointerArray = array [0..512*1024*1024 - 2] of Pointer;
  PPointerArray = ^PointerArray;
  {$NODEFINE PointerArray}
  (*$HPPEMIT 'namespace System' *)
  (*$HPPEMIT '{' *)
  (*$HPPEMIT '  typedef void *PointerArray;' *)
  (*$HPPEMIT '}' *)

  TBoundArray = array of Integer;

  TPCharArray = packed array[0..(MaxLongint div SizeOf(PChar))-1] of PChar;
  PPCharArray = ^TPCharArray;
  {$NODEFINE TPCharArray}
  (*$HPPEMIT 'namespace System' *)
  (*$HPPEMIT '{' *)
  (*$HPPEMIT '  typedef PChar TPCharArray;' *)
  (*$HPPEMIT '}' *)

  PLongint      = ^Longint;
  (*$HPPEMIT 'namespace System' *)
  (*$HPPEMIT '{' *)
  (*$HPPEMIT '  typedef int *PLongInt;' *)
  (*$HPPEMIT '  typedef PLongInt PLongint;' *)
  (*$HPPEMIT '}' *)
  {$NODEFINE PLongint}

  PInteger      = ^Integer;     {$NODEFINE PInteger}    { defined in sysmac.h }
  PCardinal     = ^Cardinal;
  PWord         = ^Word;
  PSmallInt     = ^SmallInt;    {$NODEFINE PSmallInt}   { defined in sysmac.h }
  {$POINTERMATH ON}
  PByte         = ^Byte;        {$NODEFINE PByte}       { defined in sysmac.h }
  {$POINTERMATH OFF}
  PShortInt     = ^ShortInt;    {$NODEFINE PShortInt}   { defined in sysmac.h }
  PInt64        = ^Int64;       {$NODEFINE PInt64}      { defined in sysmac.h }
  PUInt64       = ^UInt64;
  PLongWord     = ^LongWord;    {$NODEFINE PLongWord}   { defined in sysmac.h }
  PSingle       = ^Single;      {$NODEFINE PSingle}     { defined in sysmac.h }
  PDouble       = ^Double;      {$NODEFINE PDouble}     { defined in sysmac.h }
  PDate         = ^Double;
  PDispatch     = ^IDispatch;
  PPDispatch    = ^PDispatch;
  {$NODEFINE PDispatch}  // due to avoid compile error
  {$NODEFINE PPDispatch} // due to avoid compile error
  PError        = ^LongWord;
  PWordBool     = ^WordBool;
  PUnknown      = ^IUnknown;
  PPUnknown     = ^PUnknown;
  PPWideChar    = ^PWideChar;
  PPAnsiChar    = ^PAnsiChar;
  {$IFNDEF UNICODE}
  PPChar = PPAnsiChar;
  {$ELSE}
  PPChar = PPWideChar;
  {$ENDIF}                      {$NODEFINE PPChar}      { defined in sysmac.h }
  PExtended     = ^Extended;    {$NODEFINE PExtended}   { defined in sysmac.h }
  PComp         = ^Comp;
  PCurrency     = ^Currency;    {$NODEFINE PCurrency}   { defined in sysmac.h }
  PVariant      = ^Variant;     {$NODEFINE PVariant}    { defined in sysmac.h }
  POleVariant   = ^OleVariant;  {$NODEFINE POleVariant} { defined in sysmac.h }
  PPointer      = ^Pointer;     {$NODEFINE PPointer}    { defined in sysmac.h }
  PBoolean      = ^Boolean;     {$NODEFINE PBoolean}    { defined in sysmac.h }

  TDateTime = type Double;
  PDateTime = ^TDateTime;
  {$NODEFINE TDateTime 'TDateTime' 'TDateTimeBase'}     { defined in systdate.h }
  {$OBJTYPENAME TDateTime 'NTDateTime' }

  TDate = type TDateTime;
  TTime = type TDateTime;
  {$NODEFINE TDate 'TDate' 'TDateTimeBase'}
  {$NODEFINE TTime 'TTime' 'TDateTimeBase'}
  {$OBJTYPENAME TDate 'NTDateTime' }
  {$OBJTYPENAME TTime 'NTDateTime' }
  (*$HPPEMIT 'namespace System' *)
  (*$HPPEMIT '{' *)
  (*$HPPEMIT '    typedef System::TDateTime TDate;' *)
  (*$HPPEMIT '    typedef System::TDateTime TTime;' *)
  (*$HPPEMIT '}' *)

  THandle = LongWord;
  {$NODEFINE THandle}

  TVarArrayBound = packed record
    ElementCount: Integer;
    LowBound: Integer;
  end;
  TVarArrayBoundArray = array [0..0] of TVarArrayBound;
  PVarArrayBoundArray = ^TVarArrayBoundArray;
  TVarArrayCoorArray = array [0..0] of Integer;
  PVarArrayCoorArray = ^TVarArrayCoorArray;
  {$NODEFINE TVarArrayBound}      { defined in sysclass.h }
  {$NODEFINE TVarArrayBoundArray} { defined in sysmac.h }
  {$NODEFINE PVarArrayBoundArray} { defined in sysmac.h }
  {$NODEFINE TVarArrayCoorArray}  { defined in sysmac.h }
  {$NODEFINE PVarArrayCoorArray}  { defined in sysmac.h }

  PVarArray = ^TVarArray;
  TVarArray = packed record
    DimCount: Word;
    Flags: Word;
    ElementSize: Integer;
    LockCount: Integer;
    Data: Pointer;
    Bounds: TVarArrayBoundArray;
  end;
  {$NODEFINE PVarArray} { defined in sysclass.h }
  {$NODEFINE TVarArray} { defined in sysclass.h }

  TVarType = Word;
  PVarData = ^TVarData;
  TVarData = packed record
    case Integer of
      0: (VType: TVarType;
          case Integer of
            0: (Reserved1: Word;
                case Integer of
                  0: (Reserved2, Reserved3: Word;
                      case Integer of
                        varSmallInt: (VSmallInt: SmallInt);
                        varInteger:  (VInteger: Integer);
                        varSingle:   (VSingle: Single);
                        varDouble:   (VDouble: Double);
                        varCurrency: (VCurrency: Currency);
                        varDate:     (VDate: TDateTime);
                        varOleStr:   (VOleStr: PWideChar);
                        varDispatch: (VDispatch: Pointer);
                        varError:    (VError: HRESULT);
                        varBoolean:  (VBoolean: WordBool);
                        varUnknown:  (VUnknown: Pointer);
                        varShortInt: (VShortInt: ShortInt);
                        varByte:     (VByte: Byte);
                        varWord:     (VWord: Word);
                        varLongWord: (VLongWord: LongWord);
                        varInt64:    (VInt64: Int64);
                        varUInt64:   (VUInt64: UInt64);
                        varString:   (VString: Pointer);
                        varAny:      (VAny: Pointer);
                        varArray:    (VArray: PVarArray);
                        varByRef:    (VPointer: Pointer);
                        varUString:  (VUString: Pointer);
                     );
                  1: (VLongs: array[0..2] of LongInt);
               );
            2: (VWords: array [0..6] of Word);
            3: (VBytes: array [0..13] of Byte);
          );
      1: (RawData: array [0..3] of LongInt);
  end;
  {$EXTERNALSYM TVarData}
  {$EXTERNALSYM PVarData}

type
  TVarOp = Integer;

const
  opAdd =        0;
  opSubtract =   1;
  opMultiply =   2;
  opDivide =     3;
  opIntDivide =  4;
  opModulus =    5;
  opShiftLeft =  6;
  opShiftRight = 7;
  opAnd =        8;
  opOr =         9;
  opXor =        10;
  opCompare =    11;
  opNegate =     12;
  opNot =        13;

  opCmpEQ =      14;
  opCmpNE =      15;
  opCmpLT =      16;
  opCmpLE =      17;
  opCmpGT =      18;
  opCmpGE =      19;

  {The number of small block types employed by the default memory manager}
  NumSmallBlockTypes = 55;

type
  { Dispatch call descriptor }
  PCallDesc = ^TCallDesc;
  TCallDesc = packed record
    CallType: Byte;
    ArgCount: Byte;
    NamedArgCount: Byte;
    ArgTypes: array[0..255] of Byte;
  end;
  {$NODEFINE PCallDesc} { defined in systvar.h }
  {$NODEFINE TCallDesc} { defined in systvar.h }

  PDispDesc = ^TDispDesc;
  TDispDesc = packed record
    DispID: Integer;
    ResType: Byte;
    CallDesc: TCallDesc;
  end;
  {$NODEFINE PDispDesc} { defined in systvar.h }
  {$NODEFINE TDispDesc} { defined in systvar.h }

  PVariantManager = ^TVariantManager;
  TVariantManager = record
    VarClear: procedure(var V : Variant);
    VarCopy: procedure(var Dest: Variant; const Source: Variant);
    VarCopyNoInd: procedure; // ARGS PLEASE!
    VarCast: procedure(var Dest: Variant; const Source: Variant; VarType: Integer);
    VarCastOle: procedure(var Dest: Variant; const Source: Variant; VarType: Integer);

    VarToInt: function(const V: Variant): Integer;
    VarToInt64: function(const V: Variant): Int64;
    VarToBool: function(const V: Variant): Boolean;
    VarToReal: function(const V: Variant): Extended;
    VarToCurr: function(const V: Variant): Currency;
    VarToPStr: procedure(var S; const V: Variant);
    VarToLStr: procedure(var S: string; const V: Variant);
    VarToWStr: procedure(var S: WideString; const V: Variant);
    VarToIntf: procedure(var Unknown: IInterface; const V: Variant);
    VarToDisp: procedure(var Dispatch: IDispatch; const V: Variant);
    VarToDynArray: procedure(var DynArray: Pointer; const V: Variant; TypeInfo: Pointer);

    VarFromInt: procedure(var V: Variant; const Value: Integer; const Range: ShortInt);
    VarFromInt64: procedure(var V: Variant; const Value: Int64);
    VarFromBool: procedure(var V: Variant; const Value: Boolean);
    VarFromReal: procedure; // var V: Variant; const Value: Real
    VarFromTDateTime: procedure; // var V: Variant; const Value: TDateTime
    VarFromCurr: procedure; // var V: Variant; const Value: Currency
    VarFromPStr: procedure(var V: Variant; const Value: ShortString);
    VarFromLStr: procedure(var V: Variant; const Value: string);
    VarFromWStr: procedure(var V: Variant; const Value: WideString);
    VarFromIntf: procedure(var V: Variant; const Value: IInterface);
    VarFromDisp: procedure(var V: Variant; const Value: IDispatch);
    VarFromDynArray: procedure(var V: Variant; const DynArray: Pointer; TypeInfo: Pointer);
    OleVarFromPStr: procedure(var V: OleVariant; const Value: ShortString);
    OleVarFromLStr: procedure(var V: OleVariant; const Value: string);
    OleVarFromVar: procedure(var V: OleVariant; const Value: Variant);
    OleVarFromInt: procedure(var V: OleVariant; const Value: Integer; const Range: ShortInt);
    OleVarFromInt64: procedure(var V: OleVariant; const Value: Int64);

    VarOp: procedure(var Left: Variant; const Right: Variant; OpCode: TVarOp);
    VarCmp: procedure(const Left, Right: TVarData; const OpCode: TVarOp); { result is set in the flags }
    VarNeg: procedure(var V: Variant);
    VarNot: procedure(var V: Variant);

    DispInvoke: procedure(Dest: PVarData; const Source: TVarData;
      CallDesc: PCallDesc; Params: Pointer); cdecl;
    VarAddRef: procedure(var V: Variant);

    VarArrayRedim: procedure(var A : Variant; HighBound: Integer);
    VarArrayGet: function(var A: Variant; IndexCount: Integer;
      Indices: Integer): Variant; cdecl;
    VarArrayPut: procedure(var A: Variant; const Value: Variant;
      IndexCount: Integer; Indices: Integer); cdecl;

    WriteVariant: function(var T: Text; const V: Variant; Width: Integer): Pointer;
    Write0Variant: function(var T: Text; const V: Variant): Pointer;
  end deprecated;
  {$NODEFINE PVariantManager}   { defined in sysclass.h }
  {$NODEFINE TVariantManager}   { defined in sysclass.h }

  { Dynamic array support }
  PDynArrayTypeInfo = ^TDynArrayTypeInfo;
  {$EXTERNALSYM PDynArrayTypeInfo}
  TDynArrayTypeInfo = packed record
    kind: Byte;
    name: string[0];
    elSize: Longint;
    elType: ^PDynArrayTypeInfo;
    varType: Integer;
  end;
  {$EXTERNALSYM TDynArrayTypeInfo}

  PVarRec = ^TVarRec;
  TVarRec = record { do not pack this record; it is compiler-generated }
    case Byte of
      vtInteger:       (VInteger: Integer; VType: Byte);
      vtBoolean:       (VBoolean: Boolean);
      vtChar:          (VChar: AnsiChar);
      vtExtended:      (VExtended: PExtended);
      vtString:        (VString: PShortString);
      vtPointer:       (VPointer: Pointer);
      vtPChar:         (VPChar: PAnsiChar);
      vtObject:        (VObject: TObject);
      vtClass:         (VClass: TClass);
      vtWideChar:      (VWideChar: WideChar);
      vtPWideChar:     (VPWideChar: PWideChar);
      vtAnsiString:    (VAnsiString: Pointer);
      vtCurrency:      (VCurrency: PCurrency);
      vtVariant:       (VVariant: PVariant);
      vtInterface:     (VInterface: Pointer);
      vtWideString:    (VWideString: Pointer);
      vtInt64:         (VInt64: PInt64);
      vtUnicodeString: (VUnicodeString: Pointer);
  end;
  {$NODEFINE PVarRec}   { defined in systvar.h }
  {$NODEFINE TVarRec}   { defined in systvar.h }

  {The old memory manager structure (for backward compatibility)}
  PMemoryManager = ^TMemoryManager;
  TMemoryManager = record
    GetMem: function(Size: Integer): Pointer;
    FreeMem: function(P: Pointer): Integer;
    ReallocMem: function(P: Pointer; Size: Integer): Pointer;
  end deprecated 'Use TMemoryManagerEx';
  {$NODEFINE PMemoryManager}    { defined in sysclass.h }
  {$NODEFINE TMemoryManager}    { defined in sysclass.h }

  {The new memory manager structure with expanded functionality}
  PMemoryManagerEx = ^TMemoryManagerEx;
  TMemoryManagerEx = record
    {The basic (required) memory manager functionality}
    GetMem: function(Size: Integer): Pointer;
    FreeMem: function(P: Pointer): Integer;
    ReallocMem: function(P: Pointer; Size: Integer): Pointer;
    {Extended (optional) functionality.}
    AllocMem: function(Size: Cardinal): Pointer;
    RegisterExpectedMemoryLeak: function(P: Pointer): Boolean;
    UnregisterExpectedMemoryLeak: function(P: Pointer): Boolean;
  end;
  {$NODEFINE PMemoryManagerEx}  { defined in sysclass.h }
  {$NODEFINE TMemoryManagerEx}  { defined in sysclass.h }

  THeapStatus = record
    TotalAddrSpace: Cardinal;
    TotalUncommitted: Cardinal;
    TotalCommitted: Cardinal;
    TotalAllocated: Cardinal;
    TotalFree: Cardinal;
    FreeSmall: Cardinal;
    FreeBig: Cardinal;
    Unused: Cardinal;
    Overhead: Cardinal;
    HeapErrorCode: Cardinal;
  end deprecated;
  {$NODEFINE THeapStatus}       { defined in sysclass.h }

  TSmallBlockTypeState = packed record
    {The internal size of the block type}
    InternalBlockSize: Cardinal;
    {Useable block size: The number of non-reserved bytes inside the block.}
    UseableBlockSize: Cardinal;
    {The number of allocated blocks}
    AllocatedBlockCount: Cardinal;
    {The total address space reserved for this block type (both allocated and
     free blocks)}
    ReservedAddressSpace: Cardinal;
  end;
  TSmallBlockTypeStates = array[0..NumSmallBlockTypes - 1] of TSmallBlockTypeState;
  {$NODEFINE TSmallBlockTypeState}      { defined in sysclass.h }
  {$NODEFINE TSmallBlockTypeStates}     { defined in sysclass.h }

  {The structure returned by GetMemoryManagerState}
  TMemoryManagerState = packed record
    {Small block type states}
    SmallBlockTypeStates: TSmallBlockTypeStates;
    {Medium block stats}
    AllocatedMediumBlockCount: Cardinal;
    TotalAllocatedMediumBlockSize: Cardinal;
    ReservedMediumBlockAddressSpace: Cardinal;
    {Large block stats}
    AllocatedLargeBlockCount: Cardinal;
    TotalAllocatedLargeBlockSize: Cardinal;
    ReservedLargeBlockAddressSpace: Cardinal;
  end;
  {$NODEFINE TMemoryManagerState}       { defined in sysclass.h }

  PMonitorSupport = ^TMonitorSupport;
  TMonitorSupport = record
    // Obtain a synchronization object - usually an auto-reset event or semaphore
    NewSyncObject: function: Pointer;
    // Free the synchronization object obtained from NewSyncObject
    FreeSyncObject: procedure (SyncObject: Pointer);
    // Obtain a wait object - usually an auto-reset event or semaphore - these should be cached
    NewWaitObject: function: Pointer;
    // Return the wait object from NewWaitObject back to the cache
    FreeWaitObject: procedure (WaitObject: Pointer);
    // Wait for either a SyncObject or WaitObject and optionally signal another object, or just signal an object
    // o WaitAndOrSignalObject(nil, Obj, Timeout); - Wait for <Timeout> time or until <Obj> is signaled
    // o WaitAndOrSignalObject(Obj, nil, 0); - Signal <Obj> and return. Timeout and WaitObject params ignored.
    // o WaitAndOrSignalObject(Obj, Wait, Timeout); - Atomically Signal <Obj> and wait for <Timeout> time or until <Wait> is signaled
    WaitAndOrSignalObject: function (SignalObject, WaitObject: Pointer; Timeout: Cardinal): Cardinal;
  end;

  {Memory map}
  TChunkStatus = (csUnallocated, csAllocated, csReserved,
    csSysAllocated, csSysReserved);
  {$EXTERNALSYM TChunkStatus}           { defined in sysmac.h }
  TMemoryMap = array[0..65535] of TChunkStatus;
  {$EXTERNALSYM TMemoryMap}             { defined in sysmac.h }

  {Block alignment options}
  TMinimumBlockAlignment = (mba8Byte, mba16Byte);
  {$EXTERNALSYM TMinimumBlockAlignment} { defined in sysmac.h }

{$IFDEF PC_MAPPED_EXCEPTIONS}
  PUnwinder = ^TUnwinder;
  TUnwinder = record
    RaiseException: function(Exc: Pointer): LongBool; cdecl;
    RegisterIPLookup: function(fn: Pointer; StartAddr, EndAddr: LongInt; Context: Pointer; GOT: LongInt): LongBool; cdecl;
    UnregisterIPLookup: procedure(StartAddr: LongInt) cdecl;
    DelphiLookup: function(Addr: LongInt; Context: Pointer): Pointer; cdecl;
    ClosestHandler: function(Context: Pointer): LongWord; cdecl;
  end;
  {$NODEFINE TUnwinder}                 { defined in sysmac.h }
{$ENDIF PC_MAPPED_EXCEPTIONS}

  PackageUnitEntry = packed record
    Init, FInit : Pointer;
  end;

  { Compiler generated table to be processed sequentially to init & finit all package units }
  { Init: 0..Max-1; Final: Last Initialized..0                                              }
  UnitEntryTable = array [0..9999999] of PackageUnitEntry;
  PUnitEntryTable = ^UnitEntryTable;
  { Pointer in this table is PPTypeInfo, except when it's not; if the value is 1,
    then it's a "unit boundary" marker, indicating that following types are in
    the next unit along in the TPackageTypeInfo.UnitNames unit name list sequence. }
  TTypeTable = array[0..MaxInt div SizeOf(Pointer) - 1] of Pointer;
  PTypeTable = ^TTypeTable;

  {$NODEFINE UnitEntryTable}
  (*$HPPEMIT 'namespace System' *)
  (*$HPPEMIT '{' *)
  (*$HPPEMIT '    typedef struct PackageUnitEntry UnitEntryTable;' *)
  (*$HPPEMIT '}' *)

  PPackageTypeInfo = ^TPackageTypeInfo;
  TPackageTypeInfo = record
    TypeCount: Integer;
    TypeTable: PTypeTable;
    UnitCount: Integer;
    UnitNames: PShortString; { concatenation of Pascal strings, one for each unit }
  end;

  PackageInfoTable = record
    UnitCount : Integer;      { number of entries in UnitInfo array; always > 0 }
    UnitInfo : PUnitEntryTable;
    TypeInfo: TPackageTypeInfo;
  end;

  PackageInfo = ^PackageInfoTable;

  { Each package exports a '@GetPackageInfoTable' which can be used to retrieve }
  { the table which contains compiler generated information about the package DLL }
  GetPackageInfoTable = function : PackageInfo;

{$IFDEF DEBUG_FUNCTIONS}
{ Inspector Query; implementation in GETMEM.INC; no need to conditionalize that }
  THeapBlock = record
    Start: Pointer;
    Size: Cardinal;
  end;
  {$NODEFINE THeapBlock}                { defined in sysclass.h }

  THeapBlockArray = array of THeapBlock;
  TObjectArray = array of TObject;
  {$NODEFINE THeapBlockArray}           { defined in sysmac.h }
  {$NODEFINE TObjectArray}              { defined in sysmac.h }

function GetHeapBlocks: THeapBlockArray;
function FindObjects(AClass: TClass; FindDerived: Boolean): TObjectArray;
{ Inspector Query }
{$ENDIF}

{
  When an exception is thrown, the exception object that is thrown is destroyed
  automatically when the except clause which handles the exception is exited.
  There are some cases in which an application may wish to acquire the thrown
  object and keep it alive after the except clause is exited.  For this purpose,
  we have added the AcquireExceptionObject and ReleaseExceptionObject functions.
  These functions maintain a reference count on the most current exception object,
  allowing applications to legitimately obtain references.  If the reference count
  for an exception that is being thrown is positive when the except clause is exited,
  then the thrown object is not destroyed by the RTL, but assumed to be in control
  of the application.  It is then the application's responsibility to destroy the
  thrown object.  If the reference count is zero, then the RTL will destroy the
  thrown object when the except clause is exited.
}
function AcquireExceptionObject: Pointer;
procedure ReleaseExceptionObject;

{$IFDEF PC_MAPPED_EXCEPTIONS}
procedure GetUnwinder(var Dest: TUnwinder);
procedure SetUnwinder(const NewUnwinder: TUnwinder);
function IsUnwinderSet: Boolean;

//function SysRegisterIPLookup(ModuleHandle, StartAddr, EndAddr: LongInt; Context: Pointer; GOT: LongInt): LongBool;
{
  Do NOT call these functions.  They are for internal use only:
    SysRegisterIPLookup
    SysUnregisterIPLookup
    BlockOSExceptions
    UnblockOSExceptions
    AreOSExceptionsBlocked
}
function SysRegisterIPLookup(StartAddr, EndAddr: LongInt; Context: Pointer; GOT: LongInt): LongBool;
procedure SysUnregisterIPLookup(StartAddr: LongInt);
//function SysAddressIsInPCMap(Addr: LongInt): Boolean;
function SysClosestDelphiHandler(Context: Pointer): LongWord;
procedure BlockOSExceptions;
procedure UnblockOSExceptions;
function AreOSExceptionsBlocked: Boolean;

{$ELSE !PC_MAPPED_EXCEPTIONS}
// These functions are not portable.  Use AcquireExceptionObject above instead
function RaiseList: Pointer; deprecated 'Use AcquireExceptionObject';  { Stack of current exception objects }
function SetRaiseList(NewPtr: Pointer): Pointer; deprecated 'Use AcquireExceptionObject';  { returns previous value }
{$ENDIF !PC_MAPPED_EXCEPTIONS}

function ExceptObject: TObject;
function ExceptAddr: Pointer;

{
  Coverage support.  These are internal use structures referenced by compiler
  helper functions for QA coverage support.
}
type
    TCVModInfo = packed record
        ModName: PAnsiChar;
        LibName: PAnsiChar;
        UserData: Pointer;
        end;
    PCVModInfo = ^TCVModInfo;

{$EXTERNALSYM _CVR_PROBE}
procedure _CVR_PROBE(mi: PCVModInfo; probeNum: Cardinal); cdecl;
{$EXTERNALSYM _CVR_STMTPROBE}
function _CVR_STMTPROBE(mi: PCVModInfo; probeNum: Cardinal; TrueFalse: Cardinal): Boolean; cdecl;

procedure SetInOutRes(NewValue: Integer);

type
  TAssertErrorProc = procedure (const Message, Filename: string;
    LineNumber: Integer; ErrorAddr: Pointer);
  TSafeCallErrorProc = procedure (ErrorCode: HResult; ErrorAddr: Pointer);
  {$NODEFINE TAssertErrorProc}          { defined in sysmac.h }
  {$NODEFINE TSafeCallErrorProc}        { defined in sysmac.h }

{$IFDEF DEBUG}
{
  This variable is just for debugging the exception handling system.  See
  _DbgExcNotify for the usage.
}
var
  ExcNotificationProc : procedure(NotificationKind: Integer;
                                  ExceptionObject: Pointer;
                                  ExceptionName: PShortString;
                                  ExceptionLocation: Pointer;
                                  HandlerAddr: Pointer) = nil;
{$ENDIF}

var
  DispCallByIDProc: Pointer;
  ExceptProc: Pointer;    { Unhandled exception handler }
  ErrorProc: procedure (ErrorCode: Byte; ErrorAddr: Pointer);     { Error handler procedure }
{$IFDEF MSWINDOWS}
  ExceptClsProc: Pointer; { Map an OS Exception to a Delphi class reference }
  ExceptObjProc: Pointer; { Map an OS Exception to a Delphi class instance }
  RaiseExceptionProc: Pointer;
  RTLUnwindProc: Pointer;
  RaiseExceptObjProc: Pointer; { notify of the raise of an exception object }
  ExceptionAcquired: Pointer; { notification that a given exception object has been "acquired" (C++)}
{$ENDIF}
  ExceptionClass: TClass; { Exception base class (must be Exception) }
  SafeCallErrorProc: TSafeCallErrorProc; { Safecall error handler }
  AssertErrorProc: TAssertErrorProc; { Assertion error handler }
  ExitProcessProc: procedure; { Hook to be called just before the process actually exits }
  AbstractErrorProc: procedure; { Abstract method error handler }
  HPrevInst: LongWord deprecated;    { Handle of previous instance - HPrevInst cannot be tested for multiple instances in Win32}
  MainInstance: LongWord;   { Handle of the main(.EXE) HInstance }
  {$NODEFINE MainInstance}
  (*$HPPEMIT 'namespace System' *)
  (*$HPPEMIT '{' *)
  (*$HPPEMIT '    extern PACKAGE HINSTANCE MainInstance;' *)
  (*$HPPEMIT '}' *)
  MainThreadID: LongWord;   { ThreadID of thread that module was initialized in }
  IsLibrary: Boolean;       { True if module is a DLL }
  CmdShow: Integer platform;       { CmdShow parameter for CreateWindow }
  CmdLine: PChar platform;         { Command line pointer }
  InitProc: Pointer;        { Last installed initialization procedure }
  ExitCode: Integer = 0;    { Program result }
  ExitProc: Pointer;        { Last installed exit procedure }
  ErrorAddr: Pointer = nil; { Address of run-time error }
  RandSeed: Longint = 0;    { Base for random number generator }
  IsConsole: Boolean;       { True if compiled as console app }
  IsMultiThread: Boolean;   { True if more than one thread }
  FileMode: Byte = 2;       { Standard mode for opening files }
{$IF defined(LINUX) or defined(MACOSX)}
  FileAccessRights: Integer platform; { Default access rights for opening files }
  ArgCount: Integer platform;
  ArgValues: PPChar platform;
{$IFEND}
  Test8086: Byte;         { CPU family (minus one) See consts below }
  Test8087: Byte = 3;     { assume 80387 FPU or OS supplied FPU emulation }
  TestFDIV: Shortint;     { -1: Flawed Pentium, 0: Not determined, 1: Ok }
  CPUCount: Integer;       { Number of CPU Cores detected }
  Input: Text;            { Standard input }
  Output: Text;           { Standard output }
  ErrOutput: Text;        { Standard error output }
  envp: PPChar platform;

  VarClearProc:  procedure (var v: TVarData) = nil; // for internal use only
  VarAddRefProc: procedure (var v: TVarData) = nil; // for internal use only
  VarCopyProc:   procedure (var Dest: TVarData; const Source: TVarData) = nil; // for internal use only
  VarToLStrProc: procedure (var Dest: AnsiString; const Source: TVarData) = nil;   // for internal use only
  VarToWStrProc: procedure (var Dest: WideString; const Source: TVarData) = nil;   // for internal use only

  MonitorSupport: PMonitorSupport;
  {$EXTERNALSYM MonitorSupport}

const
  CPUi386     = 2;
  CPUi486     = 3;
  CPUPentium  = 4;

var
  Default8087CW: Word = $1332;{ Default 8087 control word.  FPU control
                                register is set to this value.
                                CAUTION:  Setting this to an invalid value
                                could cause unpredictable behavior. }

  HeapAllocFlags: Word platform = 2;   { Heap allocation flags, gmem_Moveable }
  DebugHook: Byte platform = 0;        { 1 to notify debugger of non-Delphi exceptions
                                >1 to notify debugger of exception unwinding }
  JITEnable: Byte platform = 0;        { 1 to call UnhandledExceptionFilter if the exception
                                is not a Pascal exception.
                                >1 to call UnhandledExceptionFilter for all exceptions }
  NoErrMsg: Boolean platform = False;  { True causes the base RTL to not display the message box
                                when a run-time error occurs }
{$IF defined(LINUX) or defined(MACOSX)}
                              { CoreDumpEnabled = True will cause unhandled
                                exceptions and runtime errors to raise a
                                SIGABRT signal, which will cause the OS to
                                coredump the process address space.  This can
                                be useful for postmortem debugging. }
  CoreDumpEnabled: Boolean platform = False;
{$IFEND}
  DefaultSystemCodePage: Integer;
  DefaultUnicodeCodePage: Integer; { Used by _NewUnicodeString to set the codePage field of strRec }
{$IFDEF MSWINDOWS}
  UTF8CompareLocale: Cardinal;
{$ENDIF MSWINDOWS}

type
  TTextLineBreakStyle = (tlbsLF, tlbsCRLF);
  {$NODEFINE TTextLineBreakStyle}       { defined in sysmac.h }

var   { Text output line break handling.  Default value for all text files }
  DefaultTextLineBreakStyle: TTextLineBreakStyle = {$IFDEF LINUX} tlbsLF {$ENDIF}
                                                 {$IFDEF MSWINDOWS} tlbsCRLF {$ENDIF}
                                                 {$IFDEF MACOSX} tlbsLF {$ENDIF};
const
   sLineBreak = {$IFDEF LINUX} AnsiChar(#10) {$ENDIF}
      {$IFDEF MSWINDOWS} AnsiString(#13#10) {$ENDIF}
      {$IFDEF MACOSX} AnsiChar(#10) {$ENDIF};

type
  HRSRC = THandle;              { from windef.h / winnt.h }
  TResourceHandle = HRSRC;   // make an opaque handle type
  HINST = THandle;              { HINSTANCE from widnef.h }
  HMODULE = HINST;              { from windef.h }
  HGLOBAL = THandle;            { from windef.h }
  {$EXTERNALSYM HRSRC}
  {$NODEFINE HINST}
  {$EXTERNALSYM HMODULE}
  {$EXTERNALSYM HGLOBAL}
  {$NODEFINE MainInstance}


{$IFDEF ELF}
{ ELF resources }
function FindResource(ModuleHandle: HMODULE; ResourceName, ResourceType: PChar): TResourceHandle;
function LoadResource(ModuleHandle: HMODULE; ResHandle: TResourceHandle): HGLOBAL;
function SizeofResource(ModuleHandle: HMODULE; ResHandle: TResourceHandle): Integer;
function LockResource(ResData: HGLOBAL): Pointer;
function UnlockResource(ResData: HGLOBAL): LongBool;
function FreeResource(ResData: HGLOBAL): LongBool;
{$ENDIF}

{ Memory manager support }

procedure GetMemoryManager(var MemMgr: TMemoryManager); overload; deprecated;
procedure SetMemoryManager(const MemMgr: TMemoryManager); overload; deprecated;
procedure GetMemoryManager(var MemMgrEx: TMemoryManagerEx); overload;
procedure SetMemoryManager(const MemMgrEx: TMemoryManagerEx); overload;
function IsMemoryManagerSet: Boolean;

function SysGetMem(Size: Integer): Pointer;
function SysFreeMem(P: Pointer): Integer;
function SysReallocMem(P: Pointer; Size: Integer): Pointer;
function SysAllocMem(Size: Cardinal): Pointer;
function SysRegisterExpectedMemoryLeak(P: Pointer): Boolean;
function SysUnregisterExpectedMemoryLeak(P: Pointer): Boolean;

{ AllocMem allocates a block of the given size on the heap. Each byte in
  the allocated buffer is set to zero. To dispose the buffer, use the
  FreeMem standard procedure. }

function AllocMem(Size: Cardinal): Pointer;

var

  AllocMemCount: Integer deprecated; {Unsupported}
  AllocMemSize: Integer deprecated; {Unsupported}

{Set this variable to true to report memory leaks on shutdown. This setting
 has no effect if this module is sharing a memory manager owned by another
 module.}
  ReportMemoryLeaksOnShutdown: Boolean;

{Set this variable to true to employ a "busy waiting" loop instead of putting
 the thread to sleep if a thread contention occurs inside the memory manager.
 This may improve performance on multi-CPU systems with a relatively low thread
 count, but will hurt performance otherwise.}
  NeverSleepOnMMThreadContention: Boolean;

{$IFDEF MSWINDOWS}
function GetHeapStatus: THeapStatus; platform; deprecated; {Unsupported}

{Returns information about the current state of the memory manager}
procedure GetMemoryManagerState(var AMemoryManagerState: TMemoryManagerState);

{Gets the state of every 64K block in the 4GB address space}
procedure GetMemoryMap(var AMemoryMap: TMemoryMap);

{Registers expected memory leaks. Returns true on success. The list of leaked
 blocks is limited in size, so failure is possible if the list is full.}
function RegisterExpectedMemoryLeak(P: Pointer): boolean;

{Removes expected memory leaks. Returns true if the previously registered leak
 was found and removed.}
function UnregisterExpectedMemoryLeak(P: Pointer): boolean;

{Set the minimum block alignment. In the current implementation blocks >=160
 bytes will always be at least 16 byte aligned, even if only 8-byte alignment
 (the default) is required.}
function GetMinimumBlockAlignment: TMinimumBlockAlignment;
procedure SetMinimumBlockAlignment(AMinimumBlockAlignment: TMinimumBlockAlignment);

{Searches the current process for a shared memory manager. If no memory has
 been allocated using this memory manager it will switch to using the shared
 memory manager instead. Returns true if another memory manager was found and
 this module is now sharing it.}
function AttemptToUseSharedMemoryManager: Boolean;

{Makes this memory manager available for sharing to other modules in the
 current process. Only one memory manager may be shared per process, so this
 function may fail.}
function ShareMemoryManager: Boolean;

{$ENDIF}

{ Thread support }
type
  TThreadFunc = function(Parameter: Pointer): Integer;
  {$NODEFINE TThreadFunc}       { defined in sysmac.h }

{$IFDEF POSIX}

{$IFDEF LINUX}
type
  TSize_T = Cardinal;

  TSchedParam = record
    sched_priority: Integer;
  end;
  {$NODEFINE TSchedParam}       { defined in sysmac.h }

  pthread_attr_t = record
    __detachstate,
    __schedpolicy: Integer;
    __schedparam: TSchedParam;
    __inheritsched,
    __scope: Integer;
    __guardsize: TSize_T;
    __stackaddr_set: Integer;
    __stackaddr: Pointer;
    __stacksize: TSize_T;
  end;
  {$EXTERNALSYM pthread_attr_t}
{$ENDIF LINUX}
{$IFDEF MACOSX}
const
   PTHREAD_ATTR_SIZE = 36;
type
   pthread_attr_t = record
      __sig: Longint;
      opaque: array [0..PTHREAD_ATTR_SIZE] of Byte;
   end;
  {$EXTERNALSYM pthread_attr_t}
{$ENDIF MAXOSX}

type
  TThreadAttr = pthread_attr_t;
  PThreadAttr = ^TThreadAttr;
  {$NODEFINE PThreadAttr}       { defined in sysmac.h }

  TBeginThreadProc = function (Attribute: PThreadAttr;
    ThreadFunc: TThreadFunc; Parameter: Pointer;
    var ThreadId: Cardinal): Integer;
  TEndThreadProc = procedure(ExitCode: Integer);
  {$NODEFINE TBeginThreadProc}  { defined in sysmac.h }
  {$NODEFINE TEndThreadProc}    { defined in sysmac.h }

var
  BeginThreadProc: TBeginThreadProc = nil;
  EndThreadProc: TEndThreadProc = nil;
{$ENDIF POSIX}

{$IFDEF MSWINDOWS}

type
  TSystemThreadFuncProc = function(ThreadFunc: TThreadFunc; Parameter: Pointer): Pointer;
  TSystemThreadEndProc = procedure(ExitCode: Integer);
  {$NODEFINE TSystemThreadFuncProc}
  {$NODEFINE TSystemThreadEndProc}

  (*$HPPEMIT 'namespace System' *)
  (*$HPPEMIT '{' *)
  (*$HPPEMIT '  typedef void * (__fastcall * TSystemThreadFuncProc)(void *, void * );' *)
  (*$HPPEMIT '  typedef void (__fastcall * TSystemThreadEndProc)(int);' *)
  (*$HPPEMIT '}' *)

var
  // SystemThreadFuncProc and SystemThreadEndProc are set during the startup
  // code by the C++ RTL when running in a C++Builder VCL application.
  SystemThreadFuncProc: TSystemThreadFuncProc = nil;
  SystemThreadEndProc: TSystemThreadEndProc = nil;

function BeginThread(SecurityAttributes: Pointer; StackSize: LongWord;
  ThreadFunc: TThreadFunc; Parameter: Pointer; CreationFlags: LongWord;
  var ThreadId: LongWord): Integer;
{$ENDIF}
{$IFDEF POSIX}
function BeginThread(Attribute: PThreadAttr; ThreadFunc: TThreadFunc;
                     Parameter: Pointer; var ThreadId: Cardinal): Integer;

{$ENDIF}
procedure EndThread(ExitCode: Integer);

{ Standard procedures and functions }

const
{ File mode magic numbers }

  fmClosed = $D7B0;
  fmInput  = $D7B1;
  fmOutput = $D7B2;
  fmInOut  = $D7B3;

{ Text file flags         }
  tfCRLF   = $1;    // Dos compatibility flag, for CR+LF line breaks and EOF checks

type
{ Typed-file and untyped-file record }

  TFileRec = packed record (* must match the size the compiler generates: 592 bytes *)
    Handle: Integer;
    Mode: Word;
    Flags: Word;
    case Byte of
      0: (RecSize: Cardinal);   //  files of record
      1: (BufSize: Cardinal;    //  text files
          BufPos: Cardinal;
          BufEnd: Cardinal;
          BufPtr: PAnsiChar;
          OpenFunc: Pointer;
          InOutFunc: Pointer;
          FlushFunc: Pointer;
          CloseFunc: Pointer;
          UserData: array[1..32] of Byte;
          {$IFNDEF UNICODE}
          Name: array[0..259] of AnsiChar;
          {$ELSE}
          Name: array[0..259] of WideChar;
          {$ENDIF}
      );
  end;

{ Text file record structure used for Text files }
  PTextBuf = ^TTextBuf;
  TTextBuf = array[0..127] of AnsiChar;
  TTextRec = packed record (* must match the size the compiler generates: 720 bytes *)
    Handle: Integer;       (* must overlay with TFileRec *)
    Mode: Word;
    Flags: Word;
    BufSize: Cardinal;
    BufPos: Cardinal;
    BufEnd: Cardinal;
    BufPtr: PAnsiChar;
    OpenFunc: Pointer;
    InOutFunc: Pointer;
    FlushFunc: Pointer;
    CloseFunc: Pointer;
    UserData: array[1..32] of Byte;
    {$IFNDEF UNICODE}
    Name: array[0..259] of AnsiChar;
    {$ELSE}
    Name: array[0..259] of WideChar;
    {$ENDIF}
    Buffer: TTextBuf;
  end;

  TTextIOFunc = function (var F: TTextRec): Integer;
  TFileIOFunc = function (var F: TFileRec): Integer;

procedure SetLineBreakStyle(var T: Text; Style: TTextLineBreakStyle);

procedure ChDir(const S: string); overload;
procedure ChDir(P: PChar); overload;
function Flush(var t: Text): Integer;
procedure _LGetDir(D: Byte; var S: AnsiString);
procedure _WGetDir(D: Byte; var S: WideString);
procedure _SGetDir(D: Byte; var S: ShortString);
function IOResult: Integer;
procedure MkDir(const S: string); overload;
procedure MkDir(P: PChar); overload;
procedure Move(const Source; var Dest; Count: Integer);
procedure MoveChars(const Source; var Dest; Length: Integer); inline;
function ParamCount: Integer;
function ParamStr(Index: Integer): string;
procedure RmDir(const S: string); overload;
procedure RmDir(P: PChar); overload;
function UpCase(Ch: AnsiChar): AnsiChar; overload; inline;
function UpCase(Ch: WideChar): WideChar; overload; inline;

{ random functions }
procedure Randomize;

function Random(const ARange: Integer): Integer; overload;
function Random: Extended; overload;


{ Control 8087 control word }

procedure Set8087CW(NewCW: Word);
function Get8087CW: Word;

{ Wide character support procedures and functions for C++ }
{ These functions should not be used in Delphi code!
 (conversion is implicit in Delphi code)      }

function WideCharToString(Source: PWideChar): UnicodeString;
function WideCharLenToString(Source: PWideChar; SourceLen: Integer): UnicodeString;
procedure WideCharToStrVar(Source: PWideChar; var Dest: UnicodeString);
procedure WideCharLenToStrVar(Source: PWideChar; SourceLen: Integer;
  var Dest: UnicodeString); overload;
procedure WideCharLenToStrVar(Source: PWideChar; SourceLen: Integer;
  var Dest: AnsiString); overload;
function StringToWideChar(const Source: UnicodeString; Dest: PWideChar;
  DestSize: Integer): PWideChar;

{ PUCS4Chars returns a pointer to the UCS4 char data in the
  UCS4String array, or a pointer to a null char if UCS4String is empty }

function PUCS4Chars(const S: UCS4String): PUCS4Char;

{ Widestring <-> UCS4 conversion }

function WideStringToUCS4String(const S: WideString): UCS4String;
function UCS4StringToWideString(const S: UCS4String): WideString;

{ PAnsiChar/PWideChar Unicode <-> UTF8 conversion }

// UnicodeToUTF8(3):
// UTF8ToUnicode(3):
// Scans the source data to find the null terminator, up to MaxBytes
// Dest must have MaxBytes available in Dest.
// MaxDestBytes includes the null terminator (last char in the buffer will be set to null)
// Function result includes the null terminator.

function UnicodeToUtf8(Dest: PAnsiChar; Source: PWideChar; MaxBytes: Integer): Integer; overload; deprecated;
function Utf8ToUnicode(Dest: PWideChar; Source: PAnsiChar; MaxChars: Integer): Integer; overload; deprecated;

// UnicodeToUtf8(4):
// UTF8ToUnicode(4):
// MaxDestBytes includes the null terminator (last char in the buffer will be set to null)
// Function result includes the null terminator.
// Nulls in the source data are not considered terminators - SourceChars must be accurate

function UnicodeToUtf8(Dest: PAnsiChar; MaxDestBytes: Cardinal; Source: PWideChar; SourceChars: Cardinal): Cardinal; overload;
function Utf8ToUnicode(Dest: PWideChar; MaxDestChars: Cardinal; Source: PAnsiChar; SourceBytes: Cardinal): Cardinal; overload;

{ WideString <-> UTF8 conversion }

function UTF8Encode(const WS: WideString): RawByteString; overload;
function UTF8Encode(const US: UnicodeString): RawByteString; overload;
function UTF8Encode(const A: RawByteString): RawByteString; overload;
function UTF8EncodeToShortString(const WS: WideString): ShortString; overload;
function UTF8EncodeToShortString(const US: UnicodeString): ShortString; overload;
function UTF8EncodeToShortString(const A: RawByteString): ShortString; overload;
function UTF8Decode(const S: RawByteString): WideString; deprecated 'Use UTF8ToWideString or UTF8ToString';
function UTF8ToWideString(const S: RawByteString): WideString; inline;
function UTF8ToUnicodeString(const S: RawByteString): UnicodeString; overload;
function UTF8ToUnicodeString(const S: ShortString): UnicodeString; overload;
function UTF8ToUnicodeString(const S: PAnsiChar): UnicodeString; overload;
function UTF8ToString(const S: RawByteString): string; inline; overload;
function UTF8ToString(const S: ShortString): string; inline; overload;
function UTF8ToString(const S: PAnsiChar): string; inline; overload;
function UTF8ToString(const S: array of AnsiChar): string; overload;
//function UTF8ToUnicodeString(const S: ShortString): UnicodeString; overload;

{ Ansi <-> UTF8 conversion }

function AnsiToUtf8(const S: string): RawByteString;
function Utf8ToAnsi(const S: RawByteString): string;

{ OLE string support procedures and functions }

function OleStrToString(Source: PWideChar): UnicodeString;
procedure OleStrToStrVar(Source: PWideChar; var Dest: AnsiString); overload;
procedure OleStrToStrVar(Source: PWideChar; var Dest: UnicodeString); overload;
function StringToOleStr(const Source: AnsiString): PWideChar; overload;
function StringToOleStr(const Source: UnicodeString): PWideChar; overload;

{ Variant manager support procedures and functions (obsolete - see Variants.pas) }

procedure GetVariantManager(var VarMgr: TVariantManager); deprecated;
procedure SetVariantManager(const VarMgr: TVariantManager); deprecated;
function IsVariantManagerSet: Boolean; deprecated;

{ Interface dispatch support }

procedure _IntfDispCall; cdecl; // ARGS PLEASE!
procedure _IntfVarCall; cdecl; // ARGS PLEASE!

{ Dynamic method dispatch support }

function GetDynaMethod(vmt: TClass; selector: Smallint): Pointer;

{ Package/Module registration and unregistration }

type
  PLibModule = ^TLibModule;
  TLibModule = record
    Next: PLibModule;
    Instance: LongWord;
    CodeInstance: LongWord;
    DataInstance: LongWord;
    ResInstance: LongWord;
    TypeInfo: PPackageTypeInfo;
    Reserved: Integer;
    {$IF defined(LINUX) or defined(MACOSX)}
    InstanceVar: Pointer platform;
    InitTable: Pointer platform;
    {$IFEND LINUX or MACOSX}
{$IFDEF LINUX}
    GOT: LongWord platform;
{$ENDIF LINUX}
{$IFDEF PC_MAPPED_EXCEPTIONS}
    CodeSegStart: LongWord platform;
    CodeSegEnd: LongWord platform;
{$ENDIF PC_MAPPED_EXCEPTIONS}
  end;
  {$NODEFINE PLibModule}                { defined in sysmac.h }
  {$NODEFINE TLibModule}                { defined in sysclass.h }

  TEnumModuleFunc = function (HInstance: Integer; Data: Pointer): Boolean;
  {$NODEFINE TEnumModuleFunc}           { defined in sysmac.h }
  TEnumModuleFuncLW = function (HInstance: LongWord; Data: Pointer): Boolean;
  {$NODEFINE TEnumModuleFuncLW}         { defined in sysmac.h }
  TModuleUnloadProc = procedure (HInstance: Integer);
  {$NODEFINE TModuleUnloadProc}         { defined in sysmac.h }
  TModuleUnloadProcLW = procedure (HInstance: LongWord);
  {$NODEFINE TModuleUnloadProcLW}       { defined in sysmac.h }

  PModuleUnloadRec = ^TModuleUnloadRec;
  TModuleUnloadRec = record
    Next: PModuleUnloadRec;
    Proc: TModuleUnloadProcLW;
  end;
  {$NODEFINE PModuleUnloadRec}          { defined in sysclass.h }
  {$NODEFINE TModuleUnloadRec}          { defined in sysclass.h }

var
  LibModuleList: PLibModule = nil;
  ModuleUnloadList: PModuleUnloadRec = nil;

procedure RegisterModule(LibModule: PLibModule);
procedure UnregisterModule(LibModule: PLibModule);
function FindHInstance(Address: Pointer): LongWord;
function FindClassHInstance(ClassType: TClass): LongWord;
function FindResourceHInstance(Instance: LongWord): LongWord;
{$IFDEF MSWINDOWS}
function GetResourceModuleName(HostAppName, ModuleName: string): string;
{$ENDIF}
function LoadResourceModule(ModuleName: PChar; CheckOwner: Boolean = True): LongWord;
procedure EnumModules(Func: TEnumModuleFunc; Data: Pointer); overload;
procedure EnumResourceModules(Func: TEnumModuleFunc; Data: Pointer); overload;
procedure EnumModules(Func: TEnumModuleFuncLW; Data: Pointer); overload;
procedure EnumResourceModules(Func: TEnumModuleFuncLW; Data: Pointer); overload;
procedure AddModuleUnloadProc(Proc: TModuleUnloadProc); overload;
procedure RemoveModuleUnloadProc(Proc: TModuleUnloadProc); overload;
procedure AddModuleUnloadProc(Proc: TModuleUnloadProcLW); overload;
procedure RemoveModuleUnloadProc(Proc: TModuleUnloadProcLW); overload;
{$IFDEF LINUX}
{ Given an HMODULE, this function will return its fully qualified name.  There is
  no direct equivalent in Linux so this function provides that capability. }
function GetModuleFileName(Module: HMODULE; Buffer: PChar; BufLen: Integer): Integer;
{$ENDIF}

{ ResString support function/record }

type
  PResStringRec = ^TResStringRec;
  TResStringRec = packed record
    Module: ^Cardinal;
    Identifier: Integer;
  end;
  {$NODEFINE PResStringRec}     { defined in sysmac.h }
  {$NODEFINE TResStringRec}     { defined in sysclass.h }

function LoadResString(ResStringRec: PResStringRec): string;

function Int(const X: Extended): Extended;
function Frac(const X: Extended): Extended;
function Exp(const X: Extended): Extended;
function Cos(const X: Extended): Extended;
function Sin(const X: Extended): Extended;
function Ln(const X: Extended): Extended;
function ArcTan(const X: Extended): Extended;
function Sqrt(const X: Extended): Extended;

{ Procedures and functions that need compiler magic }

procedure _ROUND;
procedure _TRUNC;

procedure _AbstractError;
procedure _Assert(const Message, Filename: String; LineNumber: Integer);
function _Append(var t: TTextRec): Integer;
function _Assign(var t: TTextRec; const s: PChar): Integer;
function _BlockRead(var f: TFileRec; buffer: Pointer; recCnt: Longint; var recsRead: Longint): Longint;
function  _BlockWrite(var f: TFileRec; buffer: Pointer; recCnt: Longint; var recsWritten: Longint): Longint;
function _Close(var t: TTextRec): Integer;
procedure _PStrCat;
procedure _PStrNCat;
procedure _PStrCpy(Dest: PShortString; Source: PShortString);
procedure _PStrNCpy(Dest: PShortString; Source: PShortString; MaxLen: Byte);
function _EofFile(var f: TFileRec): Boolean;
function _EofText(var t: TTextRec): Boolean;
function _Eoln(var t: TTextRec): Boolean;
procedure _Erase(var f: TFileRec);
{$IFDEF TRIAL_EDITION}
procedure _Expired;
{$ENDIF}
function _FilePos(var f: TFileRec): Longint;
function _FileSize(var f: TFileRec): Longint;
procedure _FillChar(var Dest; count: Integer; Value: Char);
function _FreeMem(P: Pointer): Integer;
function _GetMem(Size: Integer): Pointer;
function _ReallocMem(var P: Pointer; NewSize: Integer): Pointer;
procedure _Halt(Code: Integer);
procedure _Halt0;
{$IFDEF TRIAL_EDITION}
{$IFDEF MSWINDOWS}
function _InitUnitPrep: Int64;
{$ENDIF}
{$IFDEF LINUX}
function _InitUnitPrep: Integer;
{$ENDIF}
{$ENDIF}
procedure Mark; deprecated;
procedure _PStrCmp;
procedure _AStrCmp;
procedure _WStrLCmp;
function _ReadRec(var f: TFileRec; Buffer: Pointer): Integer;
function _ReadChar(var t: TTextRec): AnsiChar;
function _ReadLong(var t: TTextRec): Longint;
procedure _ReadString(var t: TTextRec; s: PShortString; maxLen: Longint);
procedure _ReadCString(var t: TTextRec; s: PAnsiChar; maxLen: Longint);
procedure _ReadLString(var t: TTextRec; var s: AnsiString; CodePage: Word);
procedure _ReadWString(var t: TTextRec; var s: WideString);
procedure _ReadWCString(var t: TTextRec; s: PWideChar; maxBytes: Longint);
function _ReadWChar(var t: TTextRec): WideChar;
function _ReadExt(var t: TTextRec): Extended;
procedure _ReadLn(var t: TTextRec);
procedure _Rename(var f: TFileRec; newName: PChar);
procedure Release; deprecated;
function _ResetText(var t: TTextRec): Integer;
function _ResetFile(var f: TFileRec; recSize: Longint): Integer;
function _RewritText(var t: TTextRec): Integer;
function _RewritFile(var f: TFileRec; recSize: Longint): Integer;
procedure _RunError(errorCode: Byte);
procedure _Run0Error;
procedure _Seek(var f: TFileRec; recNum: Cardinal);
function _SeekEof(var t: TTextRec): Boolean;
function _SeekEoln(var t: TTextRec): Boolean;
procedure _SetTextBuf(var t: TTextRec; p: Pointer; size: Longint);
procedure _StrLong(val, width: Longint; s: PShortString);
procedure _Str0Long(val: Longint; s: PShortString);
procedure _Truncate(var f: TFileRec);
function _ValLong(const s: string; var code: Integer): Longint;
{$IFDEF PC_MAPPED_EXCEPTIONS}
procedure _UnhandledException;
{$ENDIF PC_MAPPED_EXCEPTIONS}
function _WriteRec(var f: TFileRec; buffer: Pointer): Pointer;
function _WriteChar(var t: TTextRec; c: AnsiChar; width: Integer): Pointer;
function _Write0Char(var t: TTextRec; c: AnsiChar): Pointer;
function _WriteBool(var t: TTextRec; val: Boolean; width: Longint): Pointer;
function _Write0Bool(var t: TTextRec; val: Boolean): Pointer;
function _WriteLong(var t: TTextRec; val, width: Longint): Pointer;
function _Write0Long(var t: TTextRec; val: Longint): Pointer;
function _WriteString(var t: TTextRec; const s: ShortString; width: Longint): Pointer;
function _Write0String(var t: TTextRec; const s: ShortString): Pointer;
function _WriteCString(var t: TTextRec; s: PAnsiChar; width: Longint): Pointer;
function _Write0CString(var t: TTextRec; s: PAnsiChar): Pointer;
function _Write0LString(var t: TTextRec; const s: AnsiString): Pointer;
function _WriteLString(var t: TTextRec; const s: AnsiString; width: Longint): Pointer;
function _Write0WString(var t: TTextRec; const s: WideString): Pointer;
function _WriteWString(var t: TTextRec; const s: WideString; width: Longint): Pointer;
function _WriteWCString(var t: TTextRec; s: PWideChar; width: Longint): Pointer;
function _Write0WCString(var t: TTextRec; s: PWideChar): Pointer;
function _WriteWChar(var t: TTextRec; c: WideChar; width: Integer): Pointer;
function _Write0WChar(var t: TTextRec; c: WideChar): Pointer;
function _WriteVariant(var T: TTextRec; const V: TVarData; Width: Integer): Pointer;
function _Write0Variant(var T: TTextRec; const V: TVarData): Pointer;
procedure _Write2Ext;
procedure _Write1Ext;
procedure _Write0Ext;
function _WriteLn(var t: TTextRec): Pointer;

procedure __CToPasStr(Dest: PShortString; const Source: PAnsiChar);
procedure __CLenToPasStr(Dest: PShortString; const Source: PAnsiChar; MaxLen: Integer);
procedure __ArrayToPasStr(Dest: PShortString; const Source: PAnsiChar; Len: Integer);
procedure __PasToCStr(const Source: PShortString; const Dest: PAnsiChar);

procedure __IOTest;
function _Flush(var t: TTextRec): Integer;

procedure _SetElem;
procedure _SetRange;
procedure _SetEq;
procedure _SetLe;
procedure _SetIntersect;
procedure _SetIntersect3; { BEG only }
procedure _SetUnion;
procedure _SetUnion3; { BEG only }
procedure _SetSub;
procedure _SetSub3; { BEG only }
procedure _SetExpand;

procedure _Str2Ext;
procedure _Str0Ext;
procedure _Str1Ext;
procedure _ValExt;
procedure _Pow10;
procedure _Real2Ext;
procedure _Ext2Real;

procedure _ObjSetup;
procedure _ObjCopy;
procedure _Fail;
procedure _BoundErr;
procedure _IntOver;

{ Module initialization context.  For internal use only. }

type
  PInitContext = ^TInitContext;
  TInitContext = record
    OuterContext:   PInitContext;     { saved InitContext   }
{$IFNDEF PC_MAPPED_EXCEPTIONS}
    ExcFrame:       Pointer;          { bottom exc handler  }
{$ENDIF}
    InitTable:      PackageInfo;      { unit init info      }
    InitCount:      Integer;          { how far we got      }
    Module:         PLibModule;       { ptr to module desc  }
    DLLSaveEBP:     Pointer;          { saved regs for DLLs }
    DLLSaveEBX:     Pointer;          { saved regs for DLLs }
    DLLSaveESI:     Pointer;          { saved regs for DLLs }
    DLLSaveEDI:     Pointer;          { saved regs for DLLs }
{$IFDEF MSWINDOWS}
    ExitProcessTLS: procedure;        { Shutdown for TLS    }
{$ENDIF}
    DLLInitState:   Byte;             { 0 = package, 1 = DLL shutdown, 2 = DLL startup }
    ThreadID:  LongWord;              { Initializing Thread }
  end platform;

type
  TDLLProc = procedure (Reason: Integer);
  // TDLLProcEx provides the reserved param returned by WinNT
  TDLLProcEx = procedure (Reason: Integer; Reserved: Integer);
  {$NODEFINE TDLLProc}          { defined in sysmac.h }
  {$NODEFINE TDLLProcEx}        { defined in sysmac.h }

{$IF defined(LINUX) or defined(MACOSX)}
procedure _StartExe(InitTable: PackageInfo; Module: PLibModule; Argc: Integer; Argv: Pointer);
procedure _StartLib(Context: PInitContext; Module: PLibModule; DLLProc: TDLLProcEx);
{$IFEND LINUX or MACOSX}
{$IFDEF MSWINDOWS}
procedure _StartExe(InitTable: PackageInfo; Module: PLibModule);
procedure _StartLib;
{$ENDIF}
procedure _PackageLoad(const Table : PackageInfo; Module: PLibModule);
procedure _PackageUnload(const Table : PackageInfo; Module: PLibModule);
procedure _InitResStrings;
procedure _InitResStringImports;
procedure _InitImports;
{$IFDEF MSWINDOWS}
procedure _InitWideStrings;
{$ENDIF}

function _ClassCreate(AClass: TClass; Alloc: Boolean): TObject;
procedure _ClassDestroy(Instance: TObject);
function _AfterConstruction(Instance: TObject): TObject;
function _BeforeDestruction(Instance: TObject; OuterMost: ShortInt): TObject;
function _IsClass(Child: TObject; Parent: TClass): Boolean;
function _AsClass(Child: TObject; Parent: TClass): TObject;
function _GetHelperIntf(Instance: TObject; HelperClass: TClass): IInterface;
function _IntfAsClass(const Intf: IInterface; Parent: TClass): TObject;
function _SafeIntfAsClass(const Intf: IInterface; Parent: TClass): TObject;
function _IntfIsClass(const Intf: IInterface; Parent: TClass): Boolean;

{$IFDEF PC_MAPPED_EXCEPTIONS}
procedure _RaiseAtExcept;
//procedure _DestroyException(Exc: PRaisedException);
procedure _DestroyException;
{$ENDIF PC_MAPPED_EXCEPTIONS}
procedure _RaiseExcept;
procedure _RaiseAgain;
procedure _DoneExcept;
{$IFNDEF PC_MAPPED_EXCEPTIONS}
procedure _TryFinallyExit;
{$ENDIF PC_MAPPED_EXCEPTIONS}
procedure _HandleAnyException;
procedure _HandleFinally;
procedure _HandleOnException;
{$IFDEF PC_MAPPED_EXCEPTIONS}
procedure _HandleOnExceptionPIC;
{$ENDIF PC_MAPPED_EXCEPTIONS}
procedure _HandleAutoException;
{$IFDEF PC_MAPPED_EXCEPTIONS}
procedure _ClassHandleException;
{$ENDIF PC_MAPPED_EXCEPTIONS}

procedure _CallDynaInst;
procedure _CallDynaClass;
procedure _FindDynaInst;
procedure _FindDynaClass;

procedure _LStrClr(var S);
procedure _LStrArrayClr(var StrArray; cnt: longint);
procedure _LStrAsg(var dest; const source);
procedure _LStrLAsg(var dest; const source);
procedure _LStrFromPCharLen(var Dest: AnsiString; Source: PAnsiChar; Length: Integer; CodePage: Word);
procedure _LStrFromPWCharLen(var Dest: AnsiString; Source: PWideChar; Length: Integer; CodePage: Word);
procedure SetAnsiString(Dest: PAnsiString; Source: PWideChar; Length: Integer; CodePage: Word);
procedure _LStrFromChar(var Dest: AnsiString; Source: AnsiChar; CodePage: Word);
procedure _LStrFromWChar(var Dest: AnsiString; Source: WideChar; CodePage: Word);
procedure _LStrFromPChar(var Dest: AnsiString; Source: PAnsiChar; CodePage: Word);
procedure _LStrFromPWChar(var Dest: AnsiString; Source: PWideChar; CodePage: Word);
procedure _LStrFromString(var Dest: AnsiString; const Source: ShortString; CodePage: Word);
procedure _LStrFromArray(var Dest: AnsiString; Source: PAnsiChar; Length: Integer; CodePage: Word);
procedure _LStrFromWArray(var Dest: AnsiString; Source: PWideChar; Length: Integer; CodePage: Word);
procedure _LStrFromWStr(var Dest: AnsiString; const Source: WideString; CodePage: Word);
procedure _LStrFromUStr(var Dest: AnsiString; const Source: UnicodeString; CodePage: Word);
procedure _LStrToString{(var Dest: ShortString; const Source: AnsiString; MaxLen: Integer)};
function _LStrLen(const S: AnsiString): Longint; inline;
procedure _LStrCat{var dest: AnsiString; source: AnsiString};
procedure _LStrCat3{var dest:AnsiString; source1: AnsiString; source2: AnsiString};
procedure _LStrCatN{var dest:AnsiString; argCnt: Integer; ...};
procedure _LStrCmp{left: AnsiString; right: AnsiString};
procedure _LStrEqual{const Left, Right: AnsiString};
function _LStrAddRef(var str): Pointer;
function _LStrToPChar(const s: AnsiString): PAnsiChar;
procedure _Copy{ s : ShortString; index, count : Integer ) : ShortString};
procedure _Delete{ var s : openstring; index, count : Integer };
procedure _Insert{ source : ShortString; var s : openstring; index : Integer };
procedure _Pos{ substr : ShortString; s : ShortString ) : Integer};
procedure _SetLength(s: PShortString; newLength: Byte);
procedure _SetString(s: PShortString; buffer: PAnsiChar; len: Byte);

function _PCharLen(P: PAnsiChar): Longint;

procedure UniqueString(var str: AnsiString); overload;
procedure UniqueString(var str: WideString); overload;
procedure _UniqueStringA(var str: AnsiString);
procedure _UniqueStringW(var str: WideString);

function StringElementSize(const S: UnicodeString): Word; overload; inline;
function StringElementSize(const S: RawByteString): Word; overload; inline;
function StringCodePage(const S: UnicodeString): Word; overload; inline;
function StringCodePage(const S: RawByteString): Word; overload; inline;
function StringRefCount(const S: UnicodeString): Longint; overload; inline;
function StringRefCount(const S: RawByteString): Longint; overload; inline;
procedure SetCodePage(var S: RawByteString; CodePage: Word; Convert: Boolean = True);
function _EnsureUnicodeString(var S: UnicodeString): Pointer; inline;
function _EnsureAnsiString(var S: AnsiString; CodePage: Word): Pointer; inline;

procedure _LStrCopy  { const s : AnsiString; index, count : Integer) : AnsiString};
procedure _LStrDelete{ var s : AnsiString; index, count : Integer };
procedure _LStrInsert{ const source : AnsiString; var s : AnsiString; index : Integer };
procedure _LStrPos{ const substr : AnsiString; const s : AnsiString ) : Integer};
procedure _LStrSetLength{ var str: AnsiString; newLength: Integer; CodePage: Word };
function _NewAnsiString(length: Longint; CodePage: Word): Pointer;      { for debugger & some RTL usage }
function _NewWideString(CharLength: Longint): Pointer;

procedure _WStrClr(var S);
procedure _WStrArrayClr(var StrArray; Count: Integer);
procedure _WStrAsg(var Dest: WideString; const Source: WideString);
procedure _WStrLAsg(var Dest: WideString; const Source: WideString);
function _WStrToPWChar(const S: WideString): PWideChar;
function _WStrLen(const S: WideString): Longint; inline;
procedure _WStrFromPCharLen(var Dest: WideString; Source: PAnsiChar; Length: Integer);
procedure _WStrFromPWCharLen(var Dest: WideString; Source: PWideChar; CharLength: Integer);
procedure _WStrFromChar(var Dest: WideString; Source: AnsiChar);
procedure _WStrFromWChar(var Dest: WideString; Source: WideChar);
procedure _WStrFromPChar(var Dest: WideString; Source: PAnsiChar);
procedure _WStrFromPWChar(var Dest: WideString; Source: PWideChar);
procedure _WStrFromString(var Dest: WideString; const Source: ShortString);
procedure _WStrFromArray(var Dest: WideString; Source: PAnsiChar; Length: Integer);
procedure _WStrFromWArray(var Dest: WideString; Source: PWideChar; Length: Integer);
procedure _WStrFromLStr(var Dest: WideString; const Source: AnsiString);
procedure _WStrFromUStr(var Dest: WideString; const Source: UnicodeString);
procedure _WStrToString(Dest: PShortString; const Source: WideString; MaxLen: Integer);
procedure _WStrCat(var Dest: WideString; const Source: WideString);
procedure _WStrCat3(var Dest: WideString; const Source1, Source2: WideString);
procedure _WStrCatN{var dest:WideString; argCnt: Integer; ...};
procedure _WStrCmp{left: WideString; right: WideString};
procedure _WStrEqual{const Left, Right: WideString};
function _WStrCopy(const S: WideString; Index, Count: Integer): WideString;
procedure _WStrDelete(var S: WideString; Index, Count: Integer);
procedure _WStrInsert(const Source: WideString; var Dest: WideString; Index: Integer);
procedure _WStrSetLength(var S: WideString; NewLength: Integer);
function _WStrAddRef(var str: WideString): Pointer;
procedure _WCharToString(Dest: PShortString; const Source: WideChar; MaxLen: Integer);

function _PWCharLen(P: PWideChar): Longint;

{ UnicodeString helper functions }

function _UStrAddRef(str: Pointer { UnicodeString }): Pointer;
procedure _UStrClr(var S);
procedure _UStrArrayClr(var StrArray; Count: Integer);
procedure _UStrAsg(var Dest: UnicodeString; const Source: UnicodeString); // globals (need copy)
procedure _UStrLAsg(var Dest: UnicodeString; const Source: UnicodeString); // locals
function _UStrToPWChar(const S: UnicodeString): PWideChar;
procedure _UStrFromPCharLen(var Dest: UnicodeString; Source: PAnsiChar; Length: Integer);
procedure _UStrFromPWCharLen(var Dest: UnicodeString; Source: PWideChar; CharLength: Integer);
procedure _UStrFromChar(var Dest: UnicodeString; Source: AnsiChar);
procedure _UStrFromWChar(var Dest: UnicodeString; Source: WideChar);
procedure _UStrFromPChar(var Dest: UnicodeString; Source: PAnsiChar);
procedure _UStrFromPWChar(var Dest: UnicodeString; Source: PWideChar);
procedure _UStrFromArray(var Dest: UnicodeString; Source: PAnsiChar; Length: Integer);
procedure _UStrFromWArray(var Dest: UnicodeString; Source: PWideChar; Length: Integer);
procedure _UStrFromLStr(var Dest: UnicodeString; const Source: AnsiString);
procedure _UStrFromWStr(var Dest: UnicodeString; const Source: WideString);
procedure _UStrToString(Dest: PShortString; const Source: UnicodeString; MaxLen: Integer);
procedure _UStrFromString(var Dest: UnicodeString; const Source: ShortString);
function _UStrLen(const S: UnicodeString): Integer; inline;
procedure _UStrSetLength(var S: UnicodeString; NewLength: Integer);
procedure _UStrCat(var Dest: UnicodeString; const Source: UnicodeString);
procedure _UStrCat3(var Dest: UnicodeString; const Source1, Source2: UnicodeString);
procedure _UStrCatN{var dest:UnicodeString; argCnt: Integer; ...};
procedure _UStrCmp{left: UnicodeString; right: UnicodeString};
procedure _UStrEqual{const Left, Right: UnicodeString};
function _UStrCopy(const S: UnicodeString; Index, Count: Integer): UnicodeString;
procedure _UStrDelete(var S: UnicodeString; Index, Count: Integer);
procedure _UStrInsert(const Source: UnicodeString; var Dest: UnicodeString; Index: Integer);

function _InternalUStrFromLStr(var Dest: UnicodeString; const Source: AnsiString): Pointer;
function _InternalLStrFromUStr(var Dest: AnsiString; const Source: UnicodeString; CodePage: Word): Pointer;

function UnicodeStringToUCS4String(const S: UnicodeString): UCS4String;
function UCS4StringToUnicodeString(const S: UCS4String): UnicodeString;

//function UTF8Encode(const WS: UnicodeString): UTF8String;
//function UTF8Decode(const S: UTF8String): UnicodeString;

procedure _ReadUString(var t: TTextRec; var s: UnicodeString);
function _Write0UString(var t: TTextRec; const s: UnicodeString): Pointer;
function _WriteUString(var t: TTextRec; const s: UnicodeString; width: Longint): Pointer;

procedure _InitUnicodeStrings;

procedure UniqueString(var str: UnicodeString); overload;
procedure _UniqueStringU(var str: UnicodeString);

function _NewUnicodeString(CharLength: Longint): Pointer;

function Pos(const substr, str: UnicodeString): Integer; overload;
//function StringOfChar(ch: WideChar; Count: Integer): UnicodeString; overload;

procedure _UGetDir(D: Byte; var S: UnicodeString);


procedure InitializeArray(p: Pointer; typeInfo: Pointer; elemCount: Cardinal);
procedure _Initialize(p: Pointer; typeInfo: Pointer);
procedure _InitializeArray(p: Pointer; typeInfo: Pointer; elemCount: Cardinal);
procedure _InitializeRecord(p: Pointer; typeInfo: Pointer);
procedure _Finalize(p: Pointer; typeInfo: Pointer);
procedure FinalizeArray(p, typeInfo: Pointer; cnt: Cardinal);
procedure _FinalizeArray(p: Pointer; typeInfo: Pointer; elemCount: Cardinal);
procedure _FinalizeRecord(P: Pointer; typeInfo: Pointer);
procedure _AddRef;
procedure _AddRefArray;
procedure _AddRefRecord;
procedure CopyArray(dest, source, typeInfo: Pointer; cnt: Integer);
procedure _CopyArray;
procedure _CopyRecord;
procedure _CopyObject;

function _New(size: Longint; typeInfo: Pointer): Pointer;
procedure _Dispose(p: Pointer; typeInfo: Pointer);

{ 64-bit Integer helper routines }
procedure __llmul;
procedure __lldiv;
procedure __lludiv;
procedure __llmod;
procedure __llmulo;
procedure __lldivo;
procedure __llmodo;
procedure __llumod;
procedure __llshl;
procedure __llushr;
procedure _WriteInt64;
procedure _Write0Int64;
procedure _WriteUInt64;
procedure _Write0UInt64;
procedure _ReadInt64;
function _StrInt64(val: Int64; width: Integer): ShortString;
function _Str0Int64(val: Int64): ShortString;
function _ValInt64(const s: string; var code: Integer): Int64;
function _StrUInt64(val: UInt64; width: Integer): ShortString;
function _Str0UInt64(val: Int64): ShortString;

{ Dynamic array helper functions }

procedure _DynArrayHigh;
procedure _DynArrayClear(var a: Pointer; typeInfo: Pointer);
procedure _DynArrayLength;
procedure _DynArraySetLength;
procedure _DynArrayCopy(a: Pointer; typeInfo: Pointer; var Result: Pointer);
procedure _DynArrayCopyRange(a: Pointer; typeInfo: Pointer; index, count : Integer; var Result: Pointer);
procedure _DynArrayAsg;
procedure _DynArrayAddRef;

procedure DynArrayClear(var a: Pointer; typeInfo: Pointer);
procedure DynArraySetLength(var a: Pointer; typeInfo: Pointer; dimCnt: Longint; lengthVec: PLongint);
function DynArrayDim(typeInfo: PDynArrayTypeInfo): Integer;
function DynArraySize(a: Pointer): Integer;
{$NODEFINE DynArrayDim}

function _IntfClear(var Dest: IInterface): Pointer;
procedure _IntfCopy(var Dest: IInterface; const Source: IInterface);
procedure _IntfCast(var Dest: IInterface; const Source: IInterface; const IID: TGUID);
procedure _IntfAddRef(const Dest: IInterface);

{$IFDEF MSWINDOWS}
procedure _FSafeDivide;
procedure _FSafeDivideR;
{$ENDIF}

function _CheckAutoResult(ResultCode: HResult): HResult;

procedure FPower10;

procedure TextStart; deprecated;

// Conversion utility routines for C++ convenience.  Not for Delphi code.
function  CompToDouble(Value: Comp): Double; cdecl;
procedure DoubleToComp(Value: Double; var Result: Comp); cdecl;
function  CompToCurrency(Value: Comp): Currency; cdecl;
procedure CurrencyToComp(Value: Currency; var Result: Comp); cdecl;

function GetMemory(Size: Integer): Pointer; cdecl;
function FreeMemory(P: Pointer): Integer; cdecl;
function ReallocMemory(P: Pointer; Size: Integer): Pointer; cdecl;

function Pos(const substr, str: RawByteString): Integer; overload;
function Pos(const substr, str: WideString): Integer; overload;

function StringOfChar(ch: AnsiChar; Count: Integer): AnsiString; overload;
function StringOfChar(ch: WideChar; Count: Integer): UnicodeString; overload;

{ Internal runtime error codes }

type
  TRuntimeError = (reNone, reOutOfMemory, reInvalidPtr, reDivByZero,
  reRangeError, reIntOverflow, reInvalidOp, reZeroDivide, reOverflow,
  reUnderflow, reInvalidCast, reAccessViolation, rePrivInstruction,
  reControlBreak, reStackOverflow,
  { reVar* used in Variants.pas }
  reVarTypeCast, reVarInvalidOp,
  reVarDispatch, reVarArrayCreate, reVarNotArray, reVarArrayBounds,
  reAssertionFailed,
  reExternalException, { not used here; in SysUtils }
  reIntfCastError, reSafeCallError,
  reMonitorNotLocked, reNoMonitorSupport
{$IFDEF LINUX}
  , reQuit
{$ENDIF}
{$IFDEF POSIX}
  , reCodesetConversion
{$ENDIF POSIX}
{$IFDEF MACOSX}
  , reMacNotImplemented { not implemented for Mac OS/X yet, but shouod be }
{$ENDIF}
  );
{$NODEFINE TRuntimeError}

procedure Error(errorCode: TRuntimeError);
{$NODEFINE Error}

{ GetLastError returns the last error reported by an OS API call.  Calling
  this function usually resets the OS error state.
}

function GetLastError: Integer; {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
{$EXTERNALSYM GetLastError}

{ SetLastError writes to the thread local storage area read by GetLastError. }

procedure SetLastError(ErrorCode: Integer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF}

{$IFDEF LINUX}
{  To improve performance, some RTL routines cache module handles and data
   derived from modules.  If an application dynamically loads and unloads
   shared object libraries, packages, or resource packages, it is possible for
   the handle of the newly loaded module to match the handle of a recently
   unloaded module.  The resource caches have no way to detect when this happens.

   To address this issue, the RTL maintains an internal counter that is
   incremented every time a module is loaded or unloaded using RTL functions
   (like LoadPackage).  This provides a cache version level signature that
   can detect when modules have been cycled but have the same handle.

   If you load or unload modules "by hand" using dlopen or dlclose, you must call
   InvalidateModuleCache after each load or unload so that the RTL module handle
   caches will refresh themselves properly the next time they are used.  This is
   especially important if you manually tinker with the LibModuleList list of
   loaded modules, or manually add or remove resource modules in the nodes
   of that list.

   ModuleCacheID returns the "current generation" or version number kept by
   the RTL.  You can use this to implement your own refresh-on-next-use
   (passive) module handle caches as the RTL does.  The value changes each
   time InvalidateModuleCache is called.
}

function ModuleCacheID: Cardinal;
procedure InvalidateModuleCache;
{$ENDIF}

{$IFDEF MSWINDOWS}
procedure SetMultiByteConversionCodePage(CodePage: Integer);
function GetUILanguages(const LANGID: WORD): string;
function GetLocaleOverride(AppName: string): string;

type
  PImageThunkData = ^TImageThunkData;
  TImageThunkData = record
    case Byte of
      0: (ForwarderString: LongWord); // PBYTE
      1: (_Function: LongWord);       // PLongWord Function -> _Function
      2: (Ordinal: LongWord);
      3: (AddressOfData: LongWord);   // PIMAGE_IMPORT_BY_NAME
  end;
  {$EXTERNALSYM TImageThunkData}
  {$EXTERNALSYM PImageThunkData}

  ImgDelayDescr = record
    grAttrs:     LongWord;          { attributes                        }
    szName:      PAnsiChar;         { pointer to dll name               }
    hmod:        HMODULE;           { address of module handle          }
    pIAT:        PImageThunkData;   { address of the IAT                }
    pINT:        PImageThunkData;   { address of the INT                }
    pBoundIAT:   PImageThunkData;   { address of the optional bound IAT }
    pUnloadIAT:  PImageThunkData;   { address of optional copy of
                                       original IAT                     }
    dwTimeStamp: LongWord;          { 0 if not bound,                   }
                                    { O.W. date/time stamp of DLL bound
                                       to (Old BIND)                    }
  end;
  {$EXTERNALSYM ImgDelayDescr}
  TImgDelayDescr = ImgDelayDescr;
  PImgDelayDescr = ^TImgDelayDescr;

{ Delay load import hook notifications }

  dliNotification = (
    dliNoteStartProcessing,        { used to bypass or note helper only     }
    dliNotePreLoadLibrary,         { called just before LoadLibrary, can    }
                                   {  override w/ new HMODULE return val    }
    dliNotePreGetProcAddress,      { called just before GetProcAddress, can }
                                   {  override w/ new Proc address return   }
                                   {  value                                 }
    dliFailLoadLibrary,            { failed to load library, fix it by      }
                                   {  returning a valid HMODULE             }
    dliFailGetProcAddress,         { failed to get proc address, fix it by  }
                                   {  returning a valid Proc address        }
    dliNoteEndProcessing           { called after all processing is done,   }
                                   {  no bypass possible at this point      }
                                   {  except by raise, or
                                       RaiseException.                       }
  );
  {$EXTERNALSYM dliNotification}

  DelayLoadProc = record
    fImportByName:      LongBool;
    case Byte of
      0: (szProcName:   PAnsiChar);
      1: (dwOrdinal:    LongWord);
  end;
  {$EXTERNALSYM DelayLoadProc}
  TDelayLoadProc = DelayLoadProc;
  PDelayLoadProc = ^TDelayLoadProc;

  DelayLoadInfo = record
    cb:          LongWord;       { size of structure                 }
    pidd:        PImgDelayDescr; { raw form of data (everything is
                                   there)                            }
    ppfn:        Pointer;        { points to address of function to
                                   load                              }
    szDll:       PAnsiChar;      { name of dll                       }
    dlp:         TDelayLoadProc; { name or ordinal of procedure      }
    hmodCur:     HMODULE;        { the hInstance of the library we
                                   have loaded                       }
    pfnCur:      Pointer;        { the actual function that will be
                                   called                            }
    dwLastError: LongWord;       { error received (if an error
                                   notification)                     }
  end;
  {$EXTERNALSYM DelayLoadInfo}
  TDelayLoadInfo = DelayLoadInfo;
  PDelayLoadInfo = ^TDelayLoadInfo;

  DelayedLoadHook = function (dliNotify: dliNotification; pdli: PDelayLoadInfo): Pointer; stdcall;
  {$EXTERNALSYM DelayedLoadHook}
  TDelayedLoadHook = DelayedLoadHook;

procedure ___pfnDliNotifyHook;
procedure ___pfnDliFailureHook;
procedure __delayLoadHelper;
procedure __FUnloadDelayLoadedDLL;

{ Unload support }

var
  UnloadDelayLoadedDLLPtr: Pointer = @__FUnloadDelayLoadedDLL;
  DelayLoadHelper: Pointer = @__delayLoadHelper;
  pfnDliNotifyHook: Pointer = @___pfnDliNotifyHook;
  pfnDliFailureHook: Pointer = @___pfnDliFailureHook;

{ Hook pointers }

{ The "notify hook" gets called for every call to the
   delay load helper.  This allows a user to hook every call and
   skip the delay load helper entirely.

   dliNotify =
   (
       dliNoteStartProcessing   or
       dliNotePreLoadLibrary    or
       dliNotePreGetProcAddress or
       dliNoteEndProcessing
   )

   on this call.
}

function SetDliNotifyHook(HookProc: TDelayedLoadHook): TDelayedLoadHook;
function DliNotifyHook: TDelayedLoadHook;
{$EXTERNALSYM SetDliNotifyHook}
{$EXTERNALSYM DliNotifyHook}

{ This is the failure hook,

   dliNotify =
   (
       dliFailLoadLibrary       or
       dliFailGetProcAddress
   )
}
function SetDliFailureHook(HookProc: TDelayedLoadHook): TDelayedLoadHook;
function DliFailureHook: TDelayedLoadHook;
{$EXTERNALSYM SetDliFailureHook}
{$EXTERNALSYM DliFailureHook}

{ takes a pointer to a name to unload, or NULL to unload all the delay load dlls in the list. }

procedure UnloadDelayLoadedDLL(szDll: PAnsiChar); stdcall;

procedure _delayLoadHelper;
{$ENDIF}

(* =================================================================== *)

implementation

uses
  SysInit;

{$IFDEF POSIX}
{$I PosixAPIs.INC }
{$ENDIF}

{$IFDEF MACOSX}
{ Mac OS/X ABI requires stack to be aligned to 16 bytes at the
  point of a function call. }
{$DEFINE ALIGN_STACK}
{$ENDIF}

type
  PStrRec = ^StrRec;
  StrRec = packed record
    codePage: Word;
    elemSize: Word;
    refCnt: Longint;
    length: Longint;
  end;

const
  skew = SizeOf(StrRec);
  rOff = SizeOf(StrRec); { codePage offset }
  overHead = SizeOf(StrRec) + SizeOf(Char);
  CP_UTF8 = 65001;
  CP_UTF16 = 1200;
  STATUS_WAIT_0 = Cardinal($00000000);
  WAIT_OBJECT_0 = (STATUS_WAIT_0 + 0);
  ObjCastGUID: TGUID = '{CEDF24DE-80A4-447D-8C75-EB871DC121FD}';

{ This procedure should be at the very beginning of the }
{ text segment. It used to be used by _RunError to find    }
{ start address of the text segment, but is not used anymore.  }

procedure TextStart;
begin
end;

{$IFDEF PIC}
function GetGOT: LongWord; export;
begin
  asm
  MOV Result,EBX
  end;
end;
{$ENDIF}

{$IFDEF PC_MAPPED_EXCEPTIONS}
const
  UNWINDFI_TOPOFSTACK =   $BE00EF00;

type
  UNWINDPROC  = Pointer;

{$IFDEF MSWINDOWS}
const
  unwind = 'unwind.dll';

function UnwindRegisterIPLookup(fn: UNWINDPROC; StartAddr, EndAddr: LongInt; Context: Pointer; GOT: LongInt): LongBool; cdecl;
  external unwind name '__BorUnwind_RegisterIPLookup';

function UnwindDelphiLookup(Addr: LongInt; Context: Pointer): UNWINDPROC; cdecl;
  external unwind name '__BorUnwind_DelphiLookup';

function UnwindRaiseException(Exc: Pointer): LongBool; cdecl;
  external unwind name '__BorUnwind_RaiseException';

function UnwindClosestHandler(Context: Pointer): LongWord; cdecl;
  external unwind name '__BorUnwind_ClosestDelphiHandler';
{$ENDIF}
{$IF defined(LINUX) or defined(MACOSX)}
{$IFDEF LINUX}
const
  unwind = 'libborunwind.so.6';
{$ENDIF LINUX}
{$IFDEF MACOSX}
const
  unwind = 'libborunwind.dylib';
{$ENDIF MACOSX}
//{$DEFINE STATIC_UNWIND}

{$IFDEF STATIC_UNWIND}
function _BorUnwind_RegisterIPLookup(fn: UNWINDPROC; StartAddr, EndAddr: LongInt; Context: Pointer; GOT: LongInt): LongBool; cdecl;
  external;

procedure _BorUnwind_UnregisterIPLookup(StartAddr: LongInt); cdecl;  external;

function _BorUnwind_DelphiLookup(Addr: LongInt; Context: Pointer): UNWINDPROC; cdecl;  external;

function _BorUnwind_RaiseException(Exc: Pointer): LongBool; cdecl;  external;

//function _BorUnwind_AddressIsInPCMap(Addr: LongInt): LongBool; cdecl; external;
function _BorUnwind_ClosestDelphiHandler(Context: Pointer): LongWord; cdecl; external;
{$ELSE !STATIC_UNWIND}

function _BorUnwind_RegisterIPLookup(fn: UNWINDPROC; StartAddr, EndAddr: LongInt; Context: Pointer; GOT: LongInt): LongBool; cdecl;
  external unwind name _U + '_BorUnwind_RegisterIPLookup';

procedure _BorUnwind_UnregisterIPLookup(StartAddr: LongInt); cdecl;
  external unwind name _U + '_BorUnwind_UnregisterIPLookup';

function _BorUnwind_DelphiLookup(Addr: LongInt; Context: Pointer): UNWINDPROC; cdecl;
  external unwind name _U + '_BorUnwind_DelphiLookup';

function _BorUnwind_RaiseException(Exc: Pointer): LongBool; cdecl;
  external unwind name _U + '_BorUnwind_RaiseException';

function _BorUnwind_ClosestDelphiHandler(Context: Pointer): LongWord; cdecl;
  external unwind name _U + '_BorUnwind_ClosestDelphiHandler';
{$ENDIF !STATIC_UNWIND}
{$IFEND LINUX or MACOSX}
{$ENDIF PC_MAPPED_EXCEPTIONS}

const { copied from xx.h }
  cContinuable        = 0;
  cNonContinuable     = 1;
  cUnwinding          = 2;
  cUnwindingForExit   = 4;
  cUnwindInProgress   = cUnwinding or cUnwindingForExit;
  cDelphiException    = $0EEDFADE;
  cDelphiReRaise      = $0EEDFADF;
  cDelphiExcept       = $0EEDFAE0;
  cDelphiFinally      = $0EEDFAE1;
  cDelphiTerminate    = $0EEDFAE2;
  cDelphiUnhandled    = $0EEDFAE3;
  cNonDelphiException = $0EEDFAE4;
  cDelphiExitFinally  = $0EEDFAE5;
  cCppException       = $0EEFFACE; { used by BCB }
  EXCEPTION_CONTINUE_SEARCH    = 0;
  EXCEPTION_EXECUTE_HANDLER    = 1;
  EXCEPTION_CONTINUE_EXECUTION = -1;

{$IFDEF PC_MAPPED_EXCEPTIONS}
const
  excIsBeingHandled     = $00000001;
  excIsBeingReRaised    = $00000002;
{$ENDIF}

type
  JmpInstruction =
  packed record
    opCode:   Byte;
    distance: Longint;
  end;
  TExcDescEntry =
  record
    vTable:  Pointer;
    handler: Pointer;
  end;
  PExcDesc = ^TExcDesc;
  TExcDesc =
  packed record
{$IFNDEF PC_MAPPED_EXCEPTIONS}
    jmp: JmpInstruction;
{$ENDIF}
    case Integer of
    0:      (instructions: array [0..0] of Byte);
    1{...}: (cnt: Integer; excTab: array [0..0{cnt-1}] of TExcDescEntry);
  end;

{$IFNDEF PC_MAPPED_EXCEPTIONS}
  PExcFrame = ^TExcFrame;
  TExcFrame = record
    next: PExcFrame;
    desc: PExcDesc;
    hEBP: Pointer;
    case Integer of
    0:  ( );
    1:  ( ConstructedObject: Pointer );
    2:  ( SelfOfMethod: Pointer );
  end;

  PExceptionRecord = ^TExceptionRecord;
  TExceptionRecord =
  record
    ExceptionCode        : LongWord;
    ExceptionFlags       : LongWord;
    OuterException       : PExceptionRecord;
    ExceptionAddress     : Pointer;
    NumberParameters     : Longint;
    case {IsOsException:} Boolean of
    True:  (ExceptionInformation : array [0..14] of Longint);
    False: (ExceptAddr: Pointer; ExceptObject: Pointer);
  end;
{$ENDIF}

{$IFDEF PC_MAPPED_EXCEPTIONS}
const
  UW_EXC_CLASS_BORLANDCPP = $FBEE0001;
  UW_EXC_CLASS_BORLANDDELPHI = $FBEE0101;

type
  // The following _Unwind_* types represent unwind.h
  _Unwind_Word = LongWord;
  _Unwind_Exception_Cleanup_Fn = Pointer;
  _Unwind_Exception = packed record
    exception_class: _Unwind_Word;
    exception_cleanup: _Unwind_Exception_Cleanup_Fn;
    private_1: _Unwind_Word;
    private_2: _Unwind_Word;
  end;

  PRaisedException = ^TRaisedException;
  TRaisedException = packed record
    RefCount: Integer;
    ExceptObject: TObject;
    ExceptionAddr: Pointer;
    HandlerEBP: LongWord;
    Flags: LongWord;
    Cleanup: Pointer;
    Prev: PRaisedException;
    ReleaseProc: Pointer;
  end;
{$ELSE}
  PRaiseFrame = ^TRaiseFrame;
  TRaiseFrame = packed record
    NextRaise: PRaiseFrame;
    ExceptAddr: Pointer;
    ExceptObject: TObject;
    ExceptionRecord: PExceptionRecord;
  end;
{$ENDIF}

const
  cCR = $0D;
  cLF = $0A;
  cEOF = $1A;

{$IFDEF LINUX}
function __errno_location: PInteger; cdecl;
  external libc name '__errno_location';
{$ENDIF LINUX}
{$IFDEF MACOSX}
function __errno_location: PInteger; cdecl;
  external libc name '__error';
{$ENDIF MACOSX}

{$IF defined(LINUX) or defined(MACOSX)}
function GetLastError: Integer;
begin
  Result := __errno_location^;
end;

procedure SetLastError(ErrorCode: Integer);
begin
  __errno_location^ := ErrorCode;
end;
{$IFEND LINUX or MACOSX}

{$IFDEF PUREPASCAL}
{$MESSAGE WARN 'Look at the implementation of InterlockedExchange'}
function InterlockedExchange(var P: Pointer; V: Pointer): Pointer;
asm
 LOCK CMPXCHG [EAX],EDX
end;
{$ENDIF PUREPASCAL}

function InterlockedAdd(var Addend: Integer; Increment: Integer): Integer;
asm
      MOV   ECX,EAX
      MOV   EAX,EDX
 LOCK XADD  [ECX],EAX
      ADD   EAX,EDX
end;

function InterlockedIncrement(var Addend: Integer): Integer;
asm
      MOV   EDX,1
      XCHG  EAX,EDX
 LOCK XADD  [EDX],EAX
      INC   EAX
end;

function InterlockedDecrement(var Addend: Integer): Integer;
asm
      MOV   EDX,-1
      XCHG  EAX,EDX
 LOCK XADD  [EDX],EAX
      DEC   EAX
end;

function InterlockedCompareExchangePointer(var Destination: Pointer; Exchange: Pointer; Comparand: Pointer): Pointer;
asm
      XCHG    EAX,ECX
 LOCK CMPXCHG [ECX],EDX
end;

function InterlockedCompareExchange(var Destination: Integer; Exchange: Integer; Comparand: Integer): Integer;
asm
      XCHG    EAX,ECX
 LOCK CMPXCHG [ECX],EDX
end;

{$IF defined(LINUX) or defined(MACOSX)}
//function InterlockedIncrement(var I: Integer): Integer;
//asm
//      MOV   EDX,1
//      XCHG  EAX,EDX
// LOCK XADD  [EDX],EAX
//      INC   EAX
//end;

//function InterlockedDecrement(var I: Integer): Integer;
//asm
//      MOV   EDX,-1
//      XCHG  EAX,EDX
// LOCK XADD  [EDX],EAX
//      DEC   EAX
//end;

var
  ModuleCacheVersion: Cardinal = 0;

function ModuleCacheID: Cardinal;
begin
  Result := ModuleCacheVersion;
end;

procedure InvalidateModuleCache;
begin
  InterlockedIncrement(Integer(ModuleCacheVersion));
end;
{$IFEND LINUX or MACOSX}

{$IFDEF MSWINDOWS}

{$I WindowsAPIs.INC}

function GetCmdShow: Integer;
var
  SI: TStartupInfo;
begin
  Result := 10;                  { SW_SHOWDEFAULT }
  SI.cb := SizeOf(TStartupInfo);
  GetStartupInfo(SI);
  if SI.dwFlags and 1 <> 0 then  { STARTF_USESHOWWINDOW }
    Result := SI.wShowWindow;
end;

{$ENDIF} // MSWindows

function WCharFromChar(WCharDest: PWideChar; DestChars: Integer; const CharSource: PAnsiChar; SrcBytes: Integer; CodePage: Integer): Integer; forward;
function CharFromWChar(CharDest: PAnsiChar; DestBytes: Integer; const WCharSource: PWideChar; SrcChars: Integer; CodePage: Integer): Integer; overload; forward;
function CharFromWChar(CharDest: PAnsiChar; DestBytes: Integer; const WCharSource: PWideChar; SrcChars: Integer): Integer; overload; forward;

{ ----------------------------------------------------- }
{       Memory manager                                  }
{ ----------------------------------------------------- }

{$IFDEF MSWINDOWS}
{$I GETMEM.INC }
{$ENDIF}

{$IFDEF POSIX}
function SysGetMem(Size: Integer): Pointer;
begin
  Result := __malloc(size);
end;

function SysFreeMem(P: Pointer): Integer;
begin
  __free(P);
  Result := 0;
end;

function SysReallocMem(P: Pointer; Size: Integer): Pointer;
begin
  Result := __realloc(P, Size);
end;


function SysAllocMem(Size: Cardinal): Pointer;
begin
  Result := SysGetMem(Size);
  if Result <> nil then
    FillChar(Result^, Size, 0);
end;

function SysRegisterExpectedMemoryLeak(P: Pointer): Boolean;
begin
  // not implemented for POSIX
  Result := False;
end;

function SysUnregisterExpectedMemoryLeak(P: Pointer): Boolean;
begin
  // not implemented for POSIX
  Result := False;
end;

{$ENDIF POSIX}

var
  MemoryManager: TMemoryManagerEx = (
    GetMem: SysGetMem;
    FreeMem: SysFreeMem;
    ReallocMem: SysReallocMem;
    AllocMem: SysAllocMem;
    RegisterExpectedmemoryLeak: SysRegisterExpectedMemoryLeak;
    UnregisterExpectedmemoryLeak: SysUnregisterExpectedMemoryLeak);

function AllocMem(Size: Cardinal): Pointer;
{$IFDEF PUREPASCAL}
begin
  if Size > 0 then
  begin
    Result := MemoryManager.AllocMem(Size);
    if Result = nil then
      Error(reOutOfMemory);
  end
  else
    Result := nil;
end;
{$ELSE}
asm
        TEST    EAX,EAX
        JZ      @@allocmemdone
        CALL    MemoryManager.AllocMem
        TEST    EAX,EAX
        JZ      @@allocmemerror
@@allocmemdone:
        DB      $F3
        RET
@@allocmemerror:
        MOV     AL,reOutOfMemory
        JMP     Error
end;
{$ENDIF}

function RegisterExpectedMemoryLeak(P: Pointer): boolean;
begin
  Result := (P <> nil) and MemoryManager.RegisterExpectedMemoryLeak(P);
end;

function UnregisterExpectedMemoryLeak(P: Pointer): boolean;
begin
  Result := (P <> nil) and MemoryManager.UnregisterExpectedMemoryLeak(P);
end;

{$IFDEF PC_MAPPED_EXCEPTIONS}
var
//  Unwinder: TUnwinder = (
//    RaiseException: UnwindRaiseException;
//    RegisterIPLookup: UnwindRegisterIPLookup;
//    UnregisterIPLookup: UnwindUnregisterIPLookup;
//    DelphiLookup: UnwindDelphiLookup);
  Unwinder: TUnwinder;

{$IFDEF STATIC_UNWIND}
{$IFDEF PIC}
{$L 'objs/arith.pic.o'}
{$L 'objs/diag.pic.o'}
{$L 'objs/delphiuw.pic.o'}
{$L 'objs/unwind.pic.o'}
{$ELSE}
{$L 'objs/arith.o'}
{$L 'objs/diag.o'}
{$L 'objs/delphiuw.o'}
{$L 'objs/unwind.o'}
{$ENDIF}
procedure Arith_RdUnsigned; external;
procedure Arith_RdSigned; external;
procedure __assert_fail; cdecl; external libc name '__assert_fail';
procedure malloc; cdecl; external libc name 'malloc';
procedure memset; cdecl; external libc name 'memset';
procedure strchr; cdecl; external libc name 'strchr';
procedure strncpy; cdecl; external libc name 'strncpy';
procedure strcpy; cdecl; external libc name 'strcpy';
procedure strcmp; cdecl; external libc name 'strcmp';
procedure printf; cdecl; external libc name 'printf';
procedure free; cdecl; external libc name 'free';
procedure getenv; cdecl; external libc name 'getenv';
procedure strtok; cdecl; external libc name 'strtok';
procedure strdup; cdecl; external libc name 'strdup';
procedure __strdup; cdecl; external libc name '__strdup';
procedure fopen; cdecl; external libc name 'fopen';
procedure fdopen; cdecl; external libc name 'fdopen';
procedure time; cdecl; external libc name 'time';
procedure ctime; cdecl; external libc name 'ctime';
procedure fclose; cdecl; external libc name 'fclose';
procedure fprintf; cdecl; external libc name 'fprintf';
procedure vfprintf; cdecl; external libc name 'vfprintf';
procedure fflush; cdecl; external libc name 'fflush';
procedure dup; cdecl; external libc name 'dup';
procedure debug_init; external;
procedure debug_print; external;
procedure debug_class_enabled; external;
procedure debug_continue; external;
{$ENDIF}
{$ENDIF}

function _GetMem(Size: Integer): Pointer;
{$IFDEF PUREPASCAL}
{$IF Defined(DEBUG) and Defined(LINUX)}
var
  Signature: PLongInt;
{$IFEND}
begin
  if Size > 0 then
  begin
{$IF Defined(DEBUG) and Defined(LINUX)}
    Signature := PLongInt(MemoryManager.GetMem(Size + 4));
    if Signature = nil then
      Error(reOutOfMemory);
    Signature^ := 0;
    Result := Pointer(LongInt(Signature) + 4);
{$ELSE}
    Result := MemoryManager.GetMem(Size);
    if Result = nil then
      Error(reOutOfMemory);
{$IFEND}
  end
  else
    Result := nil;
end;
{$ELSE}
asm
        TEST    EAX,EAX
        JLE     @@negativeorzerosize
{$IFDEF ALIGN_STACK}
        SUB	ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    MemoryManager.GetMem
{$IFDEF ALIGN_STACK}
        ADD	ESP, 12
{$ENDIF ALIGN_STACK}
        TEST    EAX,EAX
        JZ      @@getmemerror
        DB      $F3
        RET
@@getmemerror:
        MOV     AL,reOutOfMemory
        JMP     Error
@@negativeorzerosize:
        XOR     EAX, EAX
        DB      $F3
end;
{$ENDIF}

const
  FreeMemorySignature = Longint($FBEEFBEE);

function _FreeMem(P: Pointer): Integer;
{$IFDEF PUREPASCAL}
{$IF Defined(DEBUG) and Defined(LINUX)}
var
  Signature: PLongInt;
{$IFEND}
begin
  if P <> nil then
  begin
{$IF Defined(DEBUG) and Defined(LINUX)}
    Signature := PLongInt(LongInt(P) - 4);
    if Signature^ <> 0 then
      Error(reInvalidPtr);
    Signature^ := FreeMemorySignature;
    Result := MemoryManager.Freemem(Pointer(Signature));
{$ELSE}
    Result := MemoryManager.FreeMem(P);
{$IFEND}
    if Result <> 0 then
      Error(reInvalidPtr);
  end
  else
    Result := 0;
end;
{$ELSE}
asm
        TEST    EAX,EAX
        JZ      @@freememdone
{$IFDEF ALIGN_STACK}
        SUB	ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    MemoryManager.FreeMem
{$IFDEF ALIGN_STACK}
        ADD	ESP, 12
{$ENDIF ALIGN_STACK}
        TEST    EAX,EAX
        JNZ     @@freememerror
@@freememdone:
        DB      $F3
        RET
@@freememerror:
        MOV     AL,reInvalidPtr
        JMP     ERROR
end;
{$ENDIF}

// POSIX is not quite what we want here - it's more non-assembler
{$IFDEF POSIX}
function _ReallocMem(var P: Pointer; NewSize: Integer): Pointer;
{$IFDEF DEBUG}
var
  Temp: Pointer;
{$ENDIF DEBUG}
begin
  if P <> nil then
  begin
{$IFDEF DEBUG}
    Temp := Pointer(LongInt(P) - 4);
    if NewSize > 0 then
    begin
      Temp := MemoryManager.ReallocMem(Temp, NewSize + 4);
      Result := Pointer(LongInt(Temp) + 4);
    end
    else
    begin
      MemoryManager.FreeMem(Temp);
      Result := nil;
    end;
{$ELSE !DEBUG}
    if NewSize > 0 then
    begin
      Result := MemoryManager.ReallocMem(P, NewSize);
    end
    else
    begin
      if MemoryManager.FreeMem(P) <> 0 then
        Error(reInvalidPtr);
      Result := nil;
    end;
{$ENDIF !DEBUG}
    P := Result;
  end else
  begin
    Result := _GetMem(NewSize);
    P := Result;
  end;
end;
{$ELSEIF Defined(MSWINDOWS)}
function _ReallocMem(var P: Pointer; NewSize: Integer): Pointer;
asm
        MOV     ECX,[EAX]
        TEST    ECX,ECX
        JE      @@alloc
        TEST    EDX,EDX
        JE      @@free
@@resize:
        PUSH    EAX
        MOV     EAX,ECX
        CALL    MemoryManager.ReallocMem
        POP     ECX
        OR      EAX,EAX
        JE      @@allocError
        MOV     [ECX],EAX
        RET
@@freeError:
        MOV     AL,reInvalidPtr
        JMP     Error
@@free:
        MOV     [EAX],EDX
        MOV     EAX,ECX
        CALL    MemoryManager.FreeMem
        OR      EAX,EAX
        JNE     @@freeError
        RET
@@allocError:
        MOV     AL,reOutOfMemory
        JMP     Error
@@alloc:
        TEST    EDX,EDX
        JE      @@exit
        PUSH    EAX
        MOV     EAX,EDX
        CALL    MemoryManager.GetMem
        POP     ECX
        OR      EAX,EAX
        JE      @@allocError
        MOV     [ECX],EAX
@@exit:
end;
{$IFEND}

{The default AllocMem implementation - for older memory managers that do not
 implement this themselves.}
function DefaultAllocMem(Size: Cardinal): Pointer;
begin
  Result := MemoryManager.GetMem(Size);
  if (Result <> nil) then
    FillChar(Result^, Size, 0)
end;

{The default (do nothing) leak registration function for backward compatibility
 with older memory managers.}
function DefaultRegisterAndUnregisterExpectedMemoryLeak(P: Pointer): boolean;
begin
  Result := False;
end;

{Backward compatible GetMemoryManager implementation}
procedure GetMemoryManager(var MemMgr: TMemoryManager);
begin
  MemMgr.GetMem := MemoryManager.GetMem;
  MemMgr.FreeMem := MemoryManager.FreeMem;
  MemMgr.ReallocMem := MemoryManager.ReallocMem;
end;

{Backward compatible SetMemoryManager implementation}
procedure SetMemoryManager(const MemMgr: TMemoryManager);
begin
  MemoryManager.GetMem := MemMgr.GetMem;
  MemoryManager.FreeMem := MemMgr.FreeMem;
  MemoryManager.ReallocMem := MemMgr.ReallocMem;
  MemoryManager.AllocMem := DefaultAllocMem;
  MemoryManager.RegisterExpectedMemoryLeak :=
    DefaultRegisterAndUnregisterExpectedMemoryLeak;
  MemoryManager.UnregisterExpectedMemoryLeak :=
    DefaultRegisterAndUnregisterExpectedMemoryLeak;
end;

procedure GetMemoryManager(var MemMgrEx: TMemoryManagerEx);
begin
  MemMgrEx := MemoryManager;
end;

procedure SetMemoryManager(const MemMgrEx: TMemoryManagerEx);
begin
  MemoryManager := MemMgrEx;
end;

function IsMemoryManagerSet: Boolean;
begin
  with MemoryManager do
    Result := (@GetMem <> @SysGetMem) or (@FreeMem <> @SysFreeMem) or
      (@ReallocMem <> @SysReallocMem) or (@AllocMem <> @SysAllocMem) or
      (@RegisterExpectedMemoryLeak <> @SysRegisterExpectedMemoryLeak) or
      (@UnregisterExpectedMemoryLeak <> @SysUnregisterExpectedMemoryLeak);
end;

{$IFDEF PC_MAPPED_EXCEPTIONS}
procedure GetUnwinder(var Dest: TUnwinder);
begin
  Dest := Unwinder;
end;

procedure SetUnwinder(const NewUnwinder: TUnwinder);
begin
  Unwinder := NewUnwinder;
end;

function IsUnwinderSet: Boolean;
begin
  with Unwinder do
    Result := (@RaiseException <> @_BorUnwind_RaiseException) or
      (@RegisterIPLookup <> @_BorUnwind_RegisterIPLookup) or
      (@UnregisterIPLookup <> @_BorUnwind_UnregisterIPLookup) or
      (@DelphiLookup <> @_BorUnwind_DelphiLookup);
end;

procedure InitUnwinder;
var
  Addr: Pointer;
begin
  {
    We look to see if we can find a dynamic version of the unwinder.  This
    will be the case if the application used ShareExcept.pas.  If it is
    present, then we fire it up.  Otherwise, we use our static copy.
  }
  Addr := dlsym(0, '_BorUnwind_RegisterIPLookup');
  if Addr <> nil then
  begin
    Unwinder.RegisterIPLookup := Addr;
    Addr := dlsym(0, '_BorUnwind_UnregisterIPLookup');
    Unwinder.UnregisterIPLookup := Addr;
    Addr := dlsym(0, '_BorUnwind_RaiseException');
    Unwinder.RaiseException := Addr;
    Addr := dlsym(0, '_BorUnwind_DelphiLookup');
    Unwinder.DelphiLookup := Addr;
    Addr := dlsym(0, '_BorUnwind_ClosestDelphiHandler');
    Unwinder.ClosestHandler := Addr;
  end
  else
  begin
    dlerror;   // clear error state;  dlsym doesn't
    Unwinder.RegisterIPLookup := _BorUnwind_RegisterIPLookup;
    Unwinder.DelphiLookup := _BorUnwind_DelphiLookup;
    Unwinder.UnregisterIPLookup := _BorUnwind_UnregisterIPLookup;
    Unwinder.RaiseException := _BorUnwind_RaiseException;
    Unwinder.ClosestHandler := _BorUnwind_ClosestDelphiHandler;
  end;
end;

function SysClosestDelphiHandler(Context: Pointer): LongWord;
begin
  if not Assigned(Unwinder.ClosestHandler) then
    InitUnwinder;
  Result := Unwinder.ClosestHandler(Context);
end;

function SysRegisterIPLookup(StartAddr, EndAddr: LongInt; Context: Pointer; GOT: LongInt): LongBool;
begin
//  xxx
  if not Assigned(Unwinder.RegisterIPLookup) then
  begin
    InitUnwinder;
//    Unwinder.RegisterIPLookup := UnwindRegisterIPLookup;
//    Unwinder.DelphiLookup := UnwindDelphiLookup;
  end;
  Result := Unwinder.RegisterIPLookup(@Unwinder.DelphiLookup, StartAddr, EndAddr, Context, GOT);
end;

procedure SysUnregisterIPLookup(StartAddr: LongInt);
begin
//  if not Assigned(Unwinder.UnregisterIPLookup) then
//    Unwinder.UnregisterIPLookup := UnwindUnregisterIPLookup;
  Unwinder.UnregisterIPLookup(StartAddr);
end;

function SysRaiseException(Exc: Pointer): LongBool; export;
var
  uexc: _Unwind_Exception;
begin
  uexc.exception_class := UW_EXC_CLASS_BORLANDDELPHI;
  uexc.private_1 := _Unwind_Word(Exc);
  uexc.private_2 := 0;
  Result := Unwinder.RaiseException(@uexc);
end;


//  SysRaiseCPPException
//    Called to reraise a C++ exception that is unwinding through pascal code.
function SysRaiseCPPException(Exc: Pointer; priv2: Pointer; cls: LongWord): LongBool;
var
  uexc: _Unwind_Exception;
begin
  uexc.exception_class := cls;
  uexc.private_1 := _Unwind_Word(Exc);
  uexc.private_2 := _Unwind_Word(priv2);
  Result := Unwinder.RaiseException(@uexc);
end;

const
  MAX_NESTED_EXCEPTIONS = 16;
{$ENDIF PC_MAPPED_EXCEPTIONS}

threadvar
{$IFDEF PC_MAPPED_EXCEPTIONS}
  ExceptionObjects: array[0..MAX_NESTED_EXCEPTIONS-1] of TRaisedException;
  ExceptionObjectCount: Integer;
  OSExceptionsBlocked: Integer;
  ExceptionList: PRaisedException;
{$ELSE !PC_MAPPED_EXCEPTIONS}
  RaiseListPtr: pointer;
{$ENDIF !PC_MAPPED_EXCEPTIONS}

threadvar
  InOutRes: Integer;

{$IFDEF PUREPASCAL}
var
  notimpl: array [0..15] of Char = 'not implemented'#10;

procedure NotImplemented;
begin
  __write (2, @notimpl, 16);
  Halt;
end;
{$ENDIF}

{$IFDEF PC_MAPPED_EXCEPTIONS}
procedure BlockOSExceptions;
asm
        PUSH    EAX
        PUSH    EDX
        CALL    SysInit.@GetTLS
        MOV     [EAX].OSExceptionsBlocked, 1
        POP     EDX
        POP     EAX
end;

procedure UnblockOSExceptions;
asm
        CALL    SysInit.@GetTLS
        MOV     [EAX].OSExceptionsBlocked, 0
end;

// Access to a TLS variable.  Note the comment in BeginThread before
// you change the implementation of this function.
function AreOSExceptionsBlocked: Boolean;
asm
        CALL    SysInit.@GetTLS
        MOV     EAX, [EAX].OSExceptionsBlocked
end;

const
  TRAISEDEXCEPTION_SIZE = SizeOf(TRaisedException);

function CurrentException: PRaisedException;
asm
        CALL    SysInit.@GetTLS
        LEA     EDX, [EAX].ExceptionObjects
        MOV     EAX, [EAX].ExceptionObjectCount
        OR      EAX, EAX
        JE      @@Done
        DEC     EAX
        IMUL    EAX, TRAISEDEXCEPTION_SIZE
        ADD     EAX, EDX
        JMP     @@Exit
@@Done:
        CALL    SysInit.@GetTLS
        MOV     EAX,[EAX].ExceptionList
@@Exit:
end;

function CurrentPrivateException: PRaisedException;
asm
        CALL    SysInit.@GetTLS
        LEA     EDX, [EAX].ExceptionObjects
        MOV     EAX, [EAX].ExceptionObjectCount
        OR      EAX, EAX
        JE      @@Done
        DEC     EAX
        IMUL    EAX, TRAISEDEXCEPTION_SIZE
        ADD     EAX, EDX
@@Done:
end;

{
  In the interests of code size here, this function is slightly overloaded.
  It is responsible for freeing up the current exception record on the
  exception stack, and it conditionally returns the thrown object to the
  caller.  If the object has been acquired through AcquireExceptionObject,
  we don't return the thrown object.
}
function FreeException: Pointer;
asm
        CALL    CurrentPrivateException
        OR      EAX, EAX
        JE      @@Error
        { EAX -> the TRaisedException }
        XOR     ECX, ECX
        { If the exception object has been referenced, we don't return it. }
        CMP     [EAX].TRaisedException.RefCount, 0
        JA      @@GotObject
        MOV     ECX, [EAX].TRaisedException.ExceptObject
@@GotObject:
        PUSH    ECX
        CALL    SysInit.@GetTLS
        POP     ECX
        DEC     [EAX].ExceptionObjectCount
        MOV     EAX, ECX
        RET
@@Error:
        CALL    SysInit.@GetTLS
        MOV     EAX, [EAX].ExceptionList
        CALL    [EAX].TRaisedException.Cleanup
        RET
end;

procedure ReleaseDelphiException;
begin
  FreeException;
end;

function AllocateException(Exception: Pointer; ExceptionAddr: Pointer): PRaisedException;
asm
{$IFDEF PIC}
        PUSH    EBX
        PUSH    EAX
        PUSH    EDX
        CALL    GetGOT
        MOV     EBX,EAX
{$ELSE !PIC}
        PUSH    EAX
        PUSH    EDX
{$ENDIF !PIC}
        CALL    SysInit.@GetTLS
        CMP     [EAX].ExceptionObjectCount, MAX_NESTED_EXCEPTIONS-1
        JE      @@TooManyNestedExceptions
        INC     [EAX].ExceptionObjectCount
        CALL    CurrentException
        POP     EDX
        POP     ECX
        MOV     [EAX].TRaisedException.ExceptObject, ECX
        MOV     [EAX].TRaisedException.ExceptionAddr, EDX
        MOV     [EAX].TRaisedException.RefCount, 0
        MOV     [EAX].TRaisedException.HandlerEBP, $FFFFFFFF
        MOV     [EAX].TRaisedException.Flags, 0
        MOV     [EAX].TRaisedException.Prev, 0
{$IFDEF PIC}
        LEA     EDX,[EBX].OFFSET FreeException
{$ELSE !PIC}
        LEA     EDX, FreeException
{$ENDIF !PIC}
        MOV     [EAX].TRaisedException.Cleanup, EDX
{$IFDEF PIC}
        LEA     EDX,[EBX].OFFSET FreeException
        LEA     EDX, ReleaseDelphiException
{$ELSE !PIC}
        LEA     EDX, ReleaseDelphiException
{$ENDIF !PIC}
        MOV     [EAX].TRaisedException.ReleaseProc, EDX
{$IFDEF PIC}
        POP     EBX
{$ENDIF PIC}
        RET
@@TooManyNestedExceptions:
        MOV     EAX, 231
        JMP     _RunError
end;

function AcquireExceptionObject: Pointer;
asm
        CALL    CurrentException
        OR      EAX, EAX
        JE      @@Error
        INC     [EAX].TRaisedException.RefCount
        MOV     EAX, [EAX].TRaisedException.ExceptObject
        RET
@@Error:
   RET // windows version doesn't generate an error, and Halt0 calls this always
        { This happens if there is no exception pending }
//        JMP     _Run0Error
end;

procedure ReleaseExceptionObject;
asm
        CALL    CurrentException
        OR      EAX, EAX
        JE      @@Error
        CMP     [EAX].TRaisedException.RefCount, 0
        JE      @@Error
        DEC     [EAX].TRaisedException.RefCount
        RET
@@Error:

{
  This happens if there is no exception pending, or
  if the reference count on a pending exception is
  zero.
}
        JMP   _Run0Error
end;

function ExceptObject: TObject;
var
  Exc: PRaisedException;
begin
  Exc := CurrentException;
  if Exc <> nil then
    Result := TObject(Exc^.ExceptObject)
  else
    Result := nil;
end;

{ Return current exception address }

function ExceptAddr: Pointer;
var
  Exc: PRaisedException;
begin
  Exc := CurrentException;
  if Exc <> nil then
    Result := Exc^.ExceptionAddr
  else
    Result := nil;
end;
{$ELSE !PC_MAPPED_EXCEPTIONS}  {not PC_MAPPED_EXCEPTIONS}

function ExceptObject: TObject;
begin
  if RaiseListPtr <> nil then
    Result := PRaiseFrame(RaiseListPtr)^.ExceptObject
  else
    Result := nil;
end;

{ Return current exception address }

function ExceptAddr: Pointer;
begin
  if RaiseListPtr <> nil then
    Result := PRaiseFrame(RaiseListPtr)^.ExceptAddr
  else
    Result := nil;
end;

function AcquireExceptionObject: Pointer;
type
  ExceptionAcquiredProc = procedure (Obj: Pointer);
var
  RaiseFrame: PRaiseFrame;
begin
  RaiseFrame := RaiseListPtr;
  if RaiseFrame <> nil then
  begin
    Result := RaiseFrame^.ExceptObject;
    RaiseFrame^.ExceptObject := nil;
    if Assigned(ExceptionAcquired) then
      ExceptionAcquiredProc(ExceptionAcquired)(Result);
  end
  else
    Result := nil;
end;

procedure ReleaseExceptionObject;
begin
end;

function RaiseList: Pointer;
begin
  Result := RaiseListPtr;
end;

function SetRaiseList(NewPtr: Pointer): Pointer;
asm
        PUSH    EAX
        CALL    SysInit.@GetTLS
        MOV     EDX, [EAX].RaiseListPtr
        POP     [EAX].RaiseListPtr
        MOV     EAX, EDX
end;
{$ENDIF !PC_MAPPED_EXCEPTIONS}

{
  Coverage helper glue - just go directly to the external coverage
  library.  NEVER put code in here, because we sometimes want to run
  coverage analysis on the System unit.
}
{
    Note: names are wrong for linux, but we'll be fixing that soon.
}
procedure _CVR_PROBE; external 'coverage.dll' name '__CVR_PROBE';
function _CVR_STMTPROBE; external 'coverage.dll' name '__CVR_STMTPROBE';

{ ----------------------------------------------------- }
{    local functions & procedures of the system unit    }
{ ----------------------------------------------------- }

procedure RunErrorAt(ErrCode: Integer; ErrorAtAddr: Pointer);
begin
  ErrorAddr := ErrorAtAddr;
  _Halt(ErrCode);
end;

procedure ErrorAt(ErrorCode: Byte; ErrorAddr: Pointer);

const
  reMap: array [TRunTimeError] of Byte = (
    0,   { reNone }
    203, { reOutOfMemory }
    204, { reInvalidPtr }
    200, { reDivByZero }
    201, { reRangeError }
{   210    Abstract error }
    215, { reIntOverflow }
    207, { reInvalidOp }
    200, { reZeroDivide }
    205, { reOverflow }
    206, { reUnderflow }
    219, { reInvalidCast }
    216, { reAccessViolation }
    218, { rePrivInstruction }
    217, { reControlBreak }
    202, { reStackOverflow }
    220, { reVarTypeCast }
    221, { reVarInvalidOp }
    222, { reVarDispatch }
    223, { reVarArrayCreate }
    224, { reVarNotArray }
    225, { reVarArrayBounds }
{   226    Thread init failure }
    227, { reAssertionFailed }
    0,   { reExternalException not used here; in SysUtils }
    228, { reIntfCastError }
    229, { reSafeCallError }
    235, { reMonitorNotLocked }
    236  { reNoMonitorSupport }
{$IFDEF PC_MAPPED_EXCEPTIONS}
{   230   Reserved by the compiler for unhandled exceptions }
{   231   Too many nested exceptions }
{$ENDIF PC_MAPPED_EXCEPTIONS}
{$IFDEF LINUX}
{   232   Fatal signal raised on a non-Delphi thread }
    , 233 { reQuit }
{$ENDIF LINUX}
{$IFDEF POSIX}
    , 234  { reCodesetConversion }
{$ENDIF POSIX}
{$IFDEF MACOSX}
    , 237 { reMacNotImplemented }
{$ENDIF MACOSX}
);

begin
  errorCode := errorCode and 127;
  if Assigned(ErrorProc) then
    ErrorProc(errorCode, ErrorAddr);
  if errorCode = 0 then
    errorCode := InOutRes
  else if errorCode <= Byte(High(TRuntimeError)) then
    errorCode := reMap[TRunTimeError(errorCode)];
  RunErrorAt(errorCode, ErrorAddr);
end;

procedure Error(errorCode: TRuntimeError);
asm
        AND     EAX,127
        MOV     EDX,[ESP]
        JMP     ErrorAt
end;

procedure       __IOTest;
asm
        PUSH    EAX
        PUSH    EDX
        PUSH    ECX
        CALL    SysInit.@GetTLS
        CMP     [EAX].InOutRes,0
        POP     ECX
        POP     EDX
        POP     EAX
        JNE     @error
        RET
@error:
        XOR     EAX,EAX
        JMP     Error
end;

procedure SetInOutRes(NewValue: Integer);
begin
  InOutRes := NewValue;
end;

procedure InOutError;
begin
  SetInOutRes(GetLastError);
end;

procedure ChDir(const S: string);
begin
  // U-OK
  ChDir(PChar(S));
end;

procedure ChDir(P: PChar);
{$IFDEF POSIX}
var
   us: UTF8String;
{$ENDIF POSIX}
begin
  // U-OK
{$IFDEF MSWINDOWS}
  if not SetCurrentDirectory(P) then
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  us := UTF8String(P);
  if __chdir(PAnsiChar(us)) <> 0 then
{$ENDIF POSIX}
    InOutError;
end;

procedure       _Copy{ s : ShortString; index, count : Integer ) : ShortString};
asm
{     ->EAX     Source string                   }
{       EDX     index                           }
{       ECX     count                           }
{       [ESP+4] Pointer to result string        }

        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX
        MOV     EDI,[ESP+8+4]

        XOR     EAX,EAX
        OR      AL,[ESI]
        JZ      @@srcEmpty

{       limit index to satisfy 1 <= index <= Length(src) }

        TEST    EDX,EDX
        JLE     @@smallInx
        CMP     EDX,EAX
        JG      @@bigInx
@@cont1:

{       limit count to satisfy 0 <= count <= Length(src) - index + 1    }

        SUB     EAX,EDX { calculate Length(src) - index + 1     }
        INC     EAX
        TEST    ECX,ECX
        JL      @@smallCount
        CMP     ECX,EAX
        JG      @@bigCount
@@cont2:

        ADD     ESI,EDX

        MOV     [EDI],CL
        INC     EDI
        REP     MOVSB
        JMP     @@exit

@@smallInx:
        MOV     EDX,1
        JMP     @@cont1
@@bigInx:
{       MOV     EDX,EAX
        JMP     @@cont1 }
@@smallCount:
        XOR     ECX,ECX
        JMP     @@cont2
@@bigCount:
        MOV     ECX,EAX
        JMP     @@cont2
@@srcEmpty:
        MOV     [EDI],AL
@@exit:
        POP     EDI
        POP     ESI
    RET 4
end;

procedure       _Delete{ var s : openstring; index, count : Integer };
asm
{     ->EAX     Pointer to s    }
{       EDX     index           }
{       ECX     count           }

        PUSH    ESI
        PUSH    EDI

        MOV     EDI,EAX

        XOR     EAX,EAX
        MOV     AL,[EDI]

{       if index not in [1 .. Length(s)] do nothing     }

        TEST    EDX,EDX
        JLE     @@exit
        CMP     EDX,EAX
        JG      @@exit

{       limit count to [0 .. Length(s) - index + 1]     }

        TEST    ECX,ECX
        JLE     @@exit
        SUB     EAX,EDX         { calculate Length(s) - index + 1       }
        INC     EAX
        CMP     ECX,EAX
        JLE     @@1
        MOV     ECX,EAX
@@1:
        SUB     [EDI],CL        { reduce Length(s) by count                     }
        ADD     EDI,EDX         { point EDI to first char to be deleted }
        LEA     ESI,[EDI+ECX]   { point ESI to first char to be preserved       }
        SUB     EAX,ECX         { #chars = Length(s) - index + 1 - count        }
        MOV     ECX,EAX

        REP     MOVSB

@@exit:
        POP     EDI
        POP     ESI
end;

procedure _LGetDir(D: Byte; var S: AnsiString);
{$IFDEF MSWINDOWS}
var
  Drive: array[0..3] of AnsiChar;
  DirBuf, SaveBuf: array[0..MAX_PATH] of AnsiChar;
begin
  if D <> 0 then
  begin
    Drive[0] := AnsiChar(Chr(D + Ord('A') - 1));
    Drive[1] := ':';
    Drive[2] := #0;
    GetCurrentDirectoryA(SizeOf(SaveBuf), SaveBuf);
    SetCurrentDirectoryA(Drive);
  end;
  GetCurrentDirectoryA(SizeOf(DirBuf), DirBuf);
  if D <> 0 then SetCurrentDirectoryA(SaveBuf);
  S := DirBuf;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
var
  DirBuf: array[0..MAX_PATH] of AnsiChar;
begin
  __getcwd(DirBuf, sizeof(DirBuf));
  S := string(DirBuf);
{$ENDIF POSIX}
end;

procedure _WGetDir(D: Byte; var S: WideString);
{$IFDEF MSWINDOWS}
var
  Drive: array[0..3] of WideChar;
  DirBuf, SaveBuf: array[0..MAX_PATH] of WideChar;
begin
  if D <> 0 then
  begin
    Drive[0] := WideChar(Chr(D + Ord('A') - 1));
    Drive[1] := ':';
    Drive[2] := #0;
    GetCurrentDirectoryW(Length(SaveBuf), SaveBuf);
    SetCurrentDirectoryW(Drive);
  end;
  GetCurrentDirectoryW(Length(DirBuf), DirBuf);
  if D <> 0 then SetCurrentDirectoryW(SaveBuf);
  S := DirBuf;
{$ENDIF MSWINDOWS}
{$IF defined(LINUX) or defined(MACOSX)}
var
   U: UnicodeString;
begin
   _UGetDir(D, U);
   S := U;
{$IFEND}
end;

procedure _SGetDir(D: Byte; var S: ShortString);
var
  L: AnsiString;
begin
  _LGetDir(D, L);
  S := L;
end;

procedure       _Insert{ source : ShortString; var s : openstring; index : Integer };
asm
{     ->EAX     Pointer to source string        }
{       EDX     Pointer to destination string   }
{       ECX     Length of destination string    }
{       [ESP+4] Index                   }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    ECX
        MOV     ECX,[ESP+16+4]
        SUB     ESP,512         { VAR buf: ARRAY [0..511] of Char       }

        MOV     EBX,EDX         { save pointer to s for later   }
        MOV     ESI,EDX

        XOR     EDX,EDX
        MOV     DL,[ESI]
        INC     ESI

{       limit index to [1 .. Length(s)+1]       }

        INC     EDX
        TEST    ECX,ECX
        JLE     @@smallInx
        CMP     ECX,EDX
        JG      @@bigInx
@@cont1:
        DEC     EDX     { EDX = Length(s)               }
                        { EAX = Pointer to src  }
                        { ESI = EBX = Pointer to s      }
                        { ECX = Index           }

{       copy index-1 chars from s to buf        }

        MOV     EDI,ESP
        DEC     ECX
        SUB     EDX,ECX { EDX = remaining length of s   }
        REP     MOVSB

{       copy Length(src) chars from src to buf  }

        XCHG    EAX,ESI { save pointer into s, point ESI to src         }
        MOV     CL,[ESI]        { ECX = Length(src) (ECX was zero after rep)    }
        INC     ESI
        REP     MOVSB

{       copy remaining chars of s to buf        }

        MOV     ESI,EAX { restore pointer into s                }
        MOV     ECX,EDX { copy remaining bytes of s             }
        REP     MOVSB

{       calculate total chars in buf    }

        SUB     EDI,ESP         { length = bufPtr - buf         }
        MOV     ECX,[ESP+512]   { ECX = Min(length, destLength) }
{       MOV     ECX,[EBP-16]   }{ ECX = Min(length, destLength) }
        CMP     ECX,EDI
        JB      @@1
        MOV     ECX,EDI
@@1:
        MOV     EDI,EBX         { Point EDI to s                }
        MOV     ESI,ESP         { Point ESI to buf              }
        MOV     [EDI],CL        { Store length in s             }
        INC     EDI
        REP     MOVSB           { Copy length chars to s        }
        JMP     @@exit

@@smallInx:
        MOV     ECX,1
        JMP     @@cont1
@@bigInx:
        MOV     ECX,EDX
        JMP     @@cont1

@@exit:
        ADD     ESP,512+4
        POP     EDI
        POP     ESI
        POP     EBX
        RET 4
end;

function IOResult: Integer;
begin
  Result := InOutRes;
  InOutRes := 0;
end;

procedure MkDir(const S: string);
begin
  MkDir(PChar(s));
end;

procedure MkDir(P: PChar);
{$IFDEF POSIX}
var
   us: UTF8String;
{$ENDIF POSIX}
begin
  // U-OK
{$IFDEF MSWINDOWS}
  if not CreateDirectory(P, 0) then
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  us := UTF8String(P);
  if __mkdir(PAnsiChar(us), -1) <> 0 then
{$ENDIF POSIX}
    InOutError;
end;

(* ***** BEGIN LICENSE BLOCK *****
 *
 * The assembly function Move is licensed under the CodeGear license terms.
 *
 * The initial developer of the original code is Fastcode
 *
 * Portions created by the initial developer are Copyright (C) 2002-2004
 * the initial developer. All Rights Reserved.
 *
 * Contributor(s): John O'Harrow
 *
 * ***** END LICENSE BLOCK ***** *)
procedure Move(const Source; var Dest; count : Integer);
{$IFDEF PUREPASCAL}
var
  S, D: PAnsiChar;
  I: Integer;
begin
  S := PAnsiChar(@Source);
  D := PAnsiChar(@Dest);
  if S = D then Exit;
  if Cardinal(D) > Cardinal(S) then
    for I := count-1 downto 0 do
      D[I] := S[I]
  else
    for I := 0 to count-1 do
      D[I] := S[I];
end;
{$ELSE}
asm
  cmp     eax, edx
  je      @@Exit {Source = Dest}
  cmp     ecx, 32
  ja      @@LargeMove {Count > 32 or Count < 0}
  sub     ecx, 8
  jg      @@SmallMove
@@TinyMove: {0..8 Byte Move}
  jmp     dword ptr [@@JumpTable+32+ecx*4]
@@SmallMove: {9..32 Byte Move}
  fild    qword ptr [eax+ecx] {Load Last 8}
  fild    qword ptr [eax] {Load First 8}
  cmp     ecx, 8
  jle     @@Small16
  fild    qword ptr [eax+8] {Load Second 8}
  cmp     ecx, 16
  jle     @@Small24
  fild    qword ptr [eax+16] {Load Third 8}
  fistp   qword ptr [edx+16] {Save Third 8}
@@Small24:
  fistp   qword ptr [edx+8] {Save Second 8}
@@Small16:
  fistp   qword ptr [edx] {Save First 8}
  fistp   qword ptr [edx+ecx] {Save Last 8}
@@Exit:
  ret
  nop {4-Byte Align JumpTable}
  nop
@@JumpTable: {4-Byte Aligned}
  dd      @@Exit, @@M01, @@M02, @@M03, @@M04, @@M05, @@M06, @@M07, @@M08
@@LargeForwardMove: {4-Byte Aligned}
  push    edx
  fild    qword ptr [eax] {First 8}
  lea     eax, [eax+ecx-8]
  lea     ecx, [ecx+edx-8]
  fild    qword ptr [eax] {Last 8}
  push    ecx
  neg     ecx
  and     edx, -8 {8-Byte Align Writes}
  lea     ecx, [ecx+edx+8]
  pop     edx
@FwdLoop:
  fild    qword ptr [eax+ecx]
  fistp   qword ptr [edx+ecx]
  add     ecx, 8
  jl      @FwdLoop
  fistp   qword ptr [edx] {Last 8}
  pop     edx
  fistp   qword ptr [edx] {First 8}
  ret
@@LargeMove:
  jng     @@LargeDone {Count < 0}
  cmp     eax, edx
  ja      @@LargeForwardMove
  sub     edx, ecx
  cmp     eax, edx
  lea     edx, [edx+ecx]
  jna     @@LargeForwardMove
  sub     ecx, 8 {Backward Move}
  push    ecx
  fild    qword ptr [eax+ecx] {Last 8}
  fild    qword ptr [eax] {First 8}
  add     ecx, edx
  and     ecx, -8 {8-Byte Align Writes}
  sub     ecx, edx
@BwdLoop:
  fild    qword ptr [eax+ecx]
  fistp   qword ptr [edx+ecx]
  sub     ecx, 8
  jg      @BwdLoop
  pop     ecx
  fistp   qword ptr [edx] {First 8}
  fistp   qword ptr [edx+ecx] {Last 8}
@@LargeDone:
  ret
@@M01:
  movzx   ecx, [eax]
  mov     [edx], cl
  ret
@@M02:
  movzx   ecx, word ptr [eax]
  mov     [edx], cx
  ret
@@M03:
  mov     cx, [eax]
  mov     al, [eax+2]
  mov     [edx], cx
  mov     [edx+2], al
  ret
@@M04:
  mov     ecx, [eax]
  mov     [edx], ecx
  ret
@@M05:
  mov     ecx, [eax]
  mov     al, [eax+4]
  mov     [edx], ecx
  mov     [edx+4], al
  ret
@@M06:
  mov     ecx, [eax]
  mov     ax, [eax+4]
  mov     [edx], ecx
  mov     [edx+4], ax
  ret
@@M07:
  mov     ecx, [eax]
  mov     eax, [eax+3]
  mov     [edx], ecx
  mov     [edx+3], eax
  ret
@@M08:
  fild    qword ptr [eax]
  fistp   qword ptr [edx]
end;
{$ENDIF}

procedure MoveChars(const Source; var Dest; Length: Integer);
begin
  Move(Source, Dest, Length * SizeOf(Char));
end;

{$IFDEF MSWINDOWS}
function GetParamStr(P: PChar; var Param: string): PChar;
var
  i, Len: Integer;
  Start, S: PChar;
begin
  // U-OK
  while True do
  begin
    while (P[0] <> #0) and (P[0] <= ' ') do
      Inc(P);
    if (P[0] = '"') and (P[1] = '"') then Inc(P, 2) else Break;
  end;
  Len := 0;
  Start := P;
  while P[0] > ' ' do
  begin
    if P[0] = '"' then
    begin
      Inc(P);
      while (P[0] <> #0) and (P[0] <> '"') do
      begin
        Inc(Len);
        Inc(P);
      end;
      if P[0] <> #0 then
        Inc(P);
    end
    else
    begin
      Inc(Len);
      Inc(P);
    end;
  end;

  SetLength(Param, Len);

  P := Start;
  S := Pointer(Param);
  i := 0;
  while P[0] > ' ' do
  begin
    if P[0] = '"' then
    begin
      Inc(P);
      while (P[0] <> #0) and (P[0] <> '"') do
      begin
        S[i] := P^;
        Inc(P);
        Inc(i);
      end;
      if P[0] <> #0 then Inc(P);
    end
    else
    begin
      S[i] := P^;
      Inc(P);
      Inc(i);
    end;
  end;

  Result := P;
end;
{$ENDIF}

function ParamCount: Integer;
{$IFDEF MSWINDOWS}
var
  P: PChar;
  S: string;
begin
  // U-OK
  Result := 0;
  P := GetParamStr(GetCommandLine, S);
  while True do
  begin
    P := GetParamStr(P, S);
    if S = '' then Break;
    Inc(Result);
  end;
{$ENDIF MSWINDOWS}
{$IF defined(LINUX) or defined(MACOSX)}
begin
  if ArgCount > 1 then
    Result := ArgCount - 1
  else Result := 0;
{$IFEND LINUX or MACOSX}
end;

type
  PCharArray = array[0..0] of PChar;

function ParamStr(Index: Integer): string;
{$IFDEF MSWINDOWS}
var
  P: PChar;
  Buffer: array[0..260] of Char;
begin
  Result := '';
  if Index = 0 then
    SetString(Result, Buffer, GetModuleFileName(0, Buffer, Length(Buffer)))
  else
  begin
    P := GetCommandLine;
    while True do
    begin
      P := GetParamStr(P, Result);
      if (Index = 0) or (Result = '') then Break;
      Dec(Index);
    end;
  end;
{$ENDIF MSWINDOWS}
{$IF defined(LINUX) or defined(MACOSX)}
begin
  if Index < ArgCount then
    Result := PCharArray(ArgValues^)[Index]
  else
    Result := '';
{$IFEND LINUX or MACOSX}
end;

procedure       _Pos{ substr : ShortString; s : ShortString ) : Integer};
asm
{     ->EAX     Pointer to substr               }
{       EDX     Pointer to string               }
{     <-EAX     Position of substr in s or 0    }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX { Point ESI to substr           }
        MOV     EDI,EDX { Point EDI to s                }

        XOR     ECX,ECX { ECX = Length(s)               }
        MOV     CL,[EDI]
        INC     EDI             { Point EDI to first char of s  }

        PUSH    EDI             { remember s position to calculate index        }

        XOR     EDX,EDX { EDX = Length(substr)          }
        MOV     DL,[ESI]
        INC     ESI             { Point ESI to first char of substr     }

        DEC     EDX             { EDX = Length(substr) - 1              }
        JS      @@fail  { < 0 ? return 0                        }
        MOV     AL,[ESI]        { AL = first char of substr             }
        INC     ESI             { Point ESI to 2'nd char of substr      }

        SUB     ECX,EDX { #positions in s to look at    }
                        { = Length(s) - Length(substr) + 1      }
        JLE     @@fail
@@loop:
        REPNE   SCASB
        JNE     @@fail
        MOV     EBX,ECX { save outer loop counter               }
        PUSH    ESI             { save outer loop substr pointer        }
        PUSH    EDI             { save outer loop s pointer             }

        MOV     ECX,EDX
        REPE    CMPSB
        POP     EDI             { restore outer loop s pointer  }
        POP     ESI             { restore outer loop substr pointer     }
        JE      @@found
        MOV     ECX,EBX { restore outer loop counter    }
        JMP     @@loop

@@fail:
        POP     EDX             { get rid of saved s pointer    }
        XOR     EAX,EAX
        JMP     @@exit

@@found:
        POP     EDX             { restore pointer to first char of s    }
        MOV     EAX,EDI { EDI points of char after match        }
        SUB     EAX,EDX { the difference is the correct index   }
@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
end;

// Don't use var param here - var ShortString is an open string param, which passes
// the ptr in EAX and the string's declared buffer length in EDX.  Compiler codegen
// expects only two params for this call - ptr and newlength

procedure       _SetLength(s: PShortString; newLength: Byte);
begin
  Byte(s^[0]) := newLength;   // should also fill new space
end;

procedure       _SetString(s: PShortString; buffer: PAnsiChar; len: Byte);
begin
  Byte(s^[0]) := len;
  if buffer <> nil then
    Move(buffer^, s^[1], len);
end;

procedure Randomize;
{$IFDEF POSIX}
begin
  RandSeed := _time(nil);
end;
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
var
  Counter: Int64;
begin
  if QueryPerformanceCounter(Counter) then
    RandSeed := Counter
  else
    RandSeed := GetTickCount;
end;
{$ENDIF MSWINDOWS}

function Random(const ARange: Integer): Integer;
{$IF DEFINED(CPU386)}
asm
{     ->EAX     Range   }
{     <-EAX     Result  }
        PUSH    EBX
{$IFDEF PIC}
        PUSH    EAX
        CALL    GetGOT
        MOV     EBX,EAX
        POP     EAX
        MOV     ECX,[EBX].OFFSET RandSeed
        IMUL    EDX,[ECX],08088405H
        INC     EDX
        MOV     [ECX],EDX
{$ELSE}
        XOR     EBX, EBX
        IMUL    EDX,[EBX].RandSeed,08088405H
        INC     EDX
        MOV     [EBX].RandSeed,EDX
{$ENDIF}
        MUL     EDX
        MOV     EAX,EDX
        POP     EBX
end;
{$ELSEIF DEFINED(CLR)}
begin
  InitRandom;
  Result := RandomEngine.Next(ARange);
end;
{$ELSE}
  {$MESSAGE ERROR 'Random(Int):Int unimplemented'}
{$IFEND}

function Random: Extended;
{$IF DEFINED(CPU386)}
const two2neg32: double = ((1.0/$10000) / $10000);  // 2^-32
asm
{       FUNCTION _RandExt: Extended;    }

        PUSH    EBX
{$IFDEF PIC}
        CALL    GetGOT
        MOV     EBX,EAX
        MOV     ECX,[EBX].OFFSET RandSeed
        IMUL    EDX,[ECX],08088405H
        INC     EDX
        MOV     [ECX],EDX
{$ELSE}
        XOR     EBX, EBX
        IMUL    EDX,[EBX].RandSeed,08088405H
        INC     EDX
        MOV     [EBX].RandSeed,EDX
{$ENDIF}

        FLD     [EBX].two2neg32
        PUSH    0
        PUSH    EDX
        FILD    qword ptr [ESP]
        ADD     ESP,8
        FMULP   ST(1), ST(0)
        POP     EBX
end;
{$ELSEIF DEFINED(CLR)}
begin
  InitRandom;
  Result := RandomEngine.NextDouble;
end;
{$ELSE}
  {$MESSAGE ERROR 'Random:Extended unimplemented'}
{$IFEND}

procedure RmDir(const S: string);
begin
  // U-OK
  RmDir(PChar(s));
end;

procedure RmDir(P: PChar);
{$IFDEF POSIX}
var
   us: UTF8String;
{$ENDIF POSIX}
begin
  // U-OK
{$IFDEF MSWINDOWS}
  if not RemoveDirectory(P) then
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  us := UTF8String(P);
  if __rmdir(PAnsiChar(us)) <> 0 then
{$ENDIF POSIX}
    InOutError;
end;

function UpCase(ch: AnsiChar): AnsiChar;
begin
  Result := Ch;
  if Result in ['a'..'z'] then
    Dec(Result, Ord('a')-Ord('A'));
end;

function CharUpperA(lpsz: PAnsiChar): PAnsiChar; stdcall;
  external 'user32.dll' name 'CharUpperA';
function CharUpperW(lpwsz: PWideChar): PWideChar; stdcall;
  external 'user32.dll' name 'CharUpperW';
{$IFNDEF UNICODE}
function CharUpper(lpsz: PAnsiChar): PAnsiChar; stdcall;
  external 'user32.dll' name 'CharUpperA';
{$ELSE}
function CharUpper(lpwsz: PWideChar): PWideChar; stdcall;
  external 'user32.dll' name 'CharUpperW';
{$ENDIF}

function UpCase(Ch: WideChar): WideChar;
begin
  Result := Ch;
  case Ch of
    'a'..'z':
      Result := WideChar(Word(Ch) and $FFDF);
  end;
end;

procedure Set8087CW(NewCW: Word);
begin
  Default8087CW := NewCW;
  asm
        FNCLEX  // don't raise pending exceptions enabled by the new flags
{$IFDEF PIC}
        MOV     EAX,[EBX].OFFSET Default8087CW
        FLDCW   [EAX]
{$ELSE}
        FLDCW   Default8087CW
{$ENDIF}
  end;
end;

function Get8087CW: Word;
asm
        PUSH    0
        FNSTCW  [ESP].Word
        POP     EAX
end;


function Int(const X: Extended): Extended;
asm
        FLD     X
        SUB     ESP,4
        FNSTCW  [ESP].Word     // save
        FNSTCW  [ESP+2].Word   // scratch
        FWAIT
        OR      [ESP+2].Word, $0F00  // trunc toward zero, full precision
        FLDCW   [ESP+2].Word
        FRNDINT
        FWAIT
        FLDCW   [ESP].Word
        ADD     ESP,4
end;

function Frac(const X: Extended): Extended;
asm
        FLD     X
        FLD     ST(0)
        SUB     ESP,4
        FNSTCW  [ESP].Word     // save
        FNSTCW  [ESP+2].Word   // scratch
        FWAIT
        OR      [ESP+2].Word, $0F00  // trunc toward zero, full precision
        FLDCW   [ESP+2].Word
        FRNDINT
        FWAIT
        FLDCW   [ESP].Word
        ADD     ESP,4
        FSUB
end;

function Exp(const X: Extended): Extended;
asm
        {       e**x = 2**(x*log2(e))   }
        FLD     X
        FLDL2E              { y := x*log2e;      }
        FMUL
        FLD     ST(0)       { i := round(y);     }
        FRNDINT
        FSUB    ST(1), ST   { f := y - i;        }
        FXCH    ST(1)       { z := 2**f          }
        F2XM1
        FLD1
        FADD
        FSCALE              { result := z * 2**i }
        FSTP    ST(1)
end;

function Cos(const X: Extended): Extended;
asm
        FLD     X
        FCOS
        FWAIT
end;

function Sin(const X: Extended): Extended;
asm
        FLD     X
        FSIN
        FWAIT
end;

function Ln(const X: Extended): Extended;
asm
        FLD     X
        FLDLN2
        FXCH
        FYL2X
        FWAIT
end;

function ArcTan(const X: Extended): Extended;
asm
        FLD    X
        FLD1
        FPATAN
        FWAIT
end;

function Sqrt(const X: Extended): Extended;
asm
        FLD     X
        FSQRT
        FWAIT
end;

{ ----------------------------------------------------- }
{       functions & procedures that need compiler magic }
{ ----------------------------------------------------- }

procedure       _ROUND;
asm
        { ->    FST(0)  Extended argument       }
        { <-    EDX:EAX Result                  }

        SUB     ESP,8
        FISTP   qword ptr [ESP]
        FWAIT
        POP     EAX
        POP     EDX
end;

procedure       _TRUNC;
asm
       { ->    FST(0)   Extended argument       }
       { <-    EDX:EAX  Result                  }

        SUB     ESP,12
        FNSTCW  [ESP].Word          // save
        FNSTCW  [ESP+2].Word        // scratch
        FWAIT
        OR      [ESP+2].Word, $0F00  // trunc toward zero, full precision
        FLDCW   [ESP+2].Word
        FISTP   qword ptr [ESP+4]
        FWAIT
        FLDCW   [ESP].Word
        POP     ECX
        POP     EAX
        POP     EDX
end;

procedure       _AbstractError;
{$IFDEF PIC}
begin
  if Assigned(AbstractErrorProc) then
    AbstractErrorProc;
  _RunError(210);  // loses return address
end;
{$ELSE}
asm
        CMP     AbstractErrorProc, 0
        JE      @@NoAbstErrProc
        CALL    AbstractErrorProc

@@NoAbstErrProc:
        MOV     EAX,210
        JMP     _RunError
end;
{$ENDIF}

function TextOpen(var t: TTextRec): Integer; forward;

function OpenText(var t: TTextRec; Mode: Word): Integer;
begin
  if (t.Mode < fmClosed) or (t.Mode > fmInOut) then
    Result := 102
  else
  begin
    if t.Mode <> fmClosed then _Close(t);
    t.Mode := Mode;
    if (t.Name[0] = #0) and (t.OpenFunc = nil) then  // stdio
      t.OpenFunc := @TextOpen;
    Result := TTextIOFunc(t.OpenFunc)(t);
  end;
  if Result <> 0 then SetInOutRes(Result);
end;

function _ResetText(var t: TTextRec): Integer;
begin
  Result := OpenText(t, fmInput);
end;

function _RewritText(var t: TTextRec): Integer;
begin
  Result := OpenText(t, fmOutput);
end;

function _Append(var t: TTextRec): Integer;
begin
  Result := OpenText(t, fmInOut);
end;

function TextIn(var t: TTextRec): Integer;
const
  ERROR_BROKEN_PIPE = 109;
begin
  t.BufEnd := 0;
  t.BufPos := 0;
{$IFDEF POSIX}
  t.BufEnd := __read(t.Handle, t.BufPtr, t.BufSize);
  if Integer(t.BufEnd) = -1 then
  begin
    t.BufEnd := 0;
    Result := GetLastError;
  end
  else
    Result := 0;
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
  if ReadFile(t.Handle, t.BufPtr^, t.BufSize, t.BufEnd, nil) = 0 then
  begin
    Result := GetLastError;
    if Result = ERROR_BROKEN_PIPE then
      Result := 0; // NT quirk: got "broken pipe"? it's really eof
  end
  else
    Result := 0;
{$ENDIF POSIX}
end;

function FileNOPProc(var t): Integer;
begin
  Result := 0;
end;

function TextOut(var t: TTextRec): Integer;
{$IFDEF MSWINDOWS}
var
  Dummy: Cardinal;
{$ENDIF}
begin
  if t.BufPos = 0 then
    Result := 0
  else
  begin
{$IFDEF POSIX}
    if __write(t.Handle, t.BufPtr, t.BufPos) = Cardinal(-1) then
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
    if WriteFile(t.Handle, t.BufPtr^, t.BufPos, Dummy, nil) = 0 then
{$ENDIF MSWINDOWS}
      Result := GetLastError
    else
      Result := 0;
    t.BufPos := 0;
  end;
end;

function InternalClose(Handle: Integer): Boolean;
begin
{$IFDEF POSIX}
  Result := __close(Handle) = 0;
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
  Result := CloseHandle(Handle) = 1;
{$ENDIF MSWINDOWS}
end;

function TextClose(var t: TTextRec): Integer;
begin
  t.Mode := fmClosed;
  if not InternalClose(t.Handle) then
    Result := GetLastError
  else
    Result := 0;
end;

function TextOpenCleanup(var t: TTextRec): Integer;
begin
  InternalClose(t.Handle);
  t.Mode := fmClosed;
  Result := GetLastError;
end;

function TextOpen(var t: TTextRec): Integer;
{$IFDEF POSIX}
var
  Flags: Integer;
  Temp, I: Integer;
  BytesRead: Integer;
  us: UTF8String;
begin
  Result := 0;
  t.BufPos := 0;
  t.BufEnd := 0;
  case t.Mode of
    fmInput: // called by Reset
      begin
        Flags := O_RDONLY;
        t.InOutFunc := @TextIn;
      end;
    fmOutput: // called by Rewrite
      begin
        Flags := O_CREAT or O_TRUNC or O_WRONLY;
        t.InOutFunc := @TextOut;
      end;
    fmInOut:  // called by Append
      begin
        Flags := O_APPEND or O_RDWR;
        t.InOutFunc := @TextOut;
      end;
  else
    Exit;
    Flags := 0;
  end;

  t.FlushFunc := @FileNOPProc;

  if t.Name[0] = #0 then  // stdin or stdout
  begin
    if t.BufPtr = nil then  // don't overwrite bufptr provided by SetTextBuf
    begin
      t.BufPtr := @t.Buffer;
      t.BufSize := sizeof(t.Buffer);
    end;
    t.CloseFunc := @FileNOPProc;
    if t.Mode = fmOutput then
    begin
      if @t = @ErrOutput then
        t.Handle := STDERR_FILENO
      else
        t.Handle := STDOUT_FILENO;
      t.FlushFunc := @TextOut;
    end
    else
      t.Handle := STDIN_FILENO;
  end
  else
  begin
    t.CloseFunc := @TextClose;

    us := UTF8String(t.Name);
    Temp := __open(PAnsiChar(us), Flags, FileAccessRights);
    if Temp = -1 then
    begin
      t.Mode := fmClosed;
      Result := GetLastError;
      Exit;
    end;

    t.Handle := Temp;

    if t.Mode = fmInOut then      // Append mode
    begin
      t.Mode := fmOutput;

      if (t.flags and tfCRLF) <> 0 then  // DOS mode, EOF significant
      begin  // scan for EOF char in last 128 byte sector.
        Temp := _lseek(t.Handle, 0, SEEK_END);
        if Temp = -1 then
        begin
          Result := TextOpenCleanup(t);
          Exit;
        end;

        Dec(Temp, 128);
        if Temp < 0 then Temp := 0;

        if _lseek(t.Handle, Temp, SEEK_SET) = -1 then
        begin
          Result := TextOpenCleanup(t);
          Exit;
        end;

        BytesRead := __read(t.Handle, t.BufPtr, 128);
        if BytesRead = -1 then
        begin
          Result := TextOpenCleanup(t);
          Exit;
        end;

        for I := 0 to BytesRead - 1 do
        begin
          if t.Buffer[I] = AnsiChar(cEOF) then
          begin  // truncate the file here
            if _ftruncate(t.Handle, _lseek(t.Handle, I - BytesRead, SEEK_END)) = -1 then
            begin
              Result := TextOpenCleanup(t);
              Exit;
            end;
            Break;
          end;
        end;
      end;
    end;
  end;
end;
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
(*
var
  OpenMode: Integer;
  Flags, Std: ShortInt;
  Temp: Integer;
  I, BytesRead: Cardinal;
  Mode: Byte;
begin
  Result := 0;
  if (t.Mode - fmInput) > (fmInOut - fmInput) then Exit;
  Mode := t.Mode and 3;
  t.BufPos := 0;
  t.BufEnd := 0;
  t.FlushFunc := @FileNOPProc;

  if t.Name[0] = #0 then  // stdin or stdout
  begin
    t.BufPtr := @t.Buffer;
    t.BufSize := SizeOf(t.Buffer);
    t.CloseFunc := @FileNOPProc;
    if Mode = (fmOutput and 3) then
    begin
      t.InOutFunc := @TextOut;
      if @t = @ErrOutput then
        Std := STD_ERROR_HANDLE
      else
        Std := STD_OUTPUT_HANDLE;
    end
    else
    begin
      t.InOutFunc := @TextIn;
      Std := STD_INPUT_HANDLE;
    end;
    t.Handle := GetStdHandle(Std);
  end
  else
  begin
    t.CloseFunc := @TextClose;

    Flags := OPEN_EXISTING;
    if Mode = (fmInput and 3) then
    begin // called by Reset
      t.InOutFunc := @TextIn;
      OpenMode := GENERIC_READ; // open for read
    end
    else
    begin
      t.InOutFunc := @TextOut;
      if Mode = (fmInOut and 3) then  // called by Append
        OpenMode := GENERIC_READ OR GENERIC_WRITE  // open for read/write
      else
      begin  // called by Rewrite
        OpenMode := GENERIC_WRITE;      // open for write
        Flags := CREATE_ALWAYS;
      end;
    end;

    Temp := CreateFile(t.Name, OpenMode, FILE_SHARE_READ, nil, Flags, FILE_ATTRIBUTE_NORMAL, 0);
    if Temp = -1 then
    begin
      Result := TextOpenCleanup(t);
      Exit;
    end;

    t.Handle := Temp;

    if Mode = (fmInOut and 3) then
    begin
      Dec(t.Mode);  // fmInOut -> fmOutput

{;  ???  we really have to look for the first eof byte in the
; ???  last record and truncate the file there.
; Not very nice and clean...
;
; lastRecPos = Max( GetFileSize(...) - 128, 0);
}
      Temp := GetFileSize(t.Handle, 0);
      if Temp = -1 then
      begin
        Result := TextOpenCleanup(t);
        Exit;
      end;

      Dec(Temp, 128);
      if Temp < 0 then Temp := 0;

      if (SetFilePointer(t.Handle, Temp, nil, FILE_BEGIN) = -1) or
         (ReadFile(t.Handle, t.Buffer, 128, BytesRead, nil) = 0) then
      begin
        Result := TextOpenCleanup(t);
        Exit;
      end;

      for I := 0 to BytesRead do
      begin
        if t.Buffer[I] = AnsiChar(cEOF) then
        begin  // truncate the file here
          if (SetFilePointer(t.Handle, I - BytesRead, nil, FILE_END) = -1) or
            (not SetEndOfFile(t.Handle)) then
          begin
            Result := TextOpenCleanup(t);
            Exit;
          end;
          Break;
        end;
      end;
    end;
    if Mode <> (fmInput and 3) then
    begin
      case GetFileType(t.Handle) of
        0: begin  // bad file type
             TextOpenCleanup(t);
             Result := 105;
             Exit;
           end;
        2: t.FlushFunc := @TextOut;
      end;
    end;
  end;
end;
*)

asm
// -> EAX Pointer to text record

        PUSH    ESI

        MOV     ESI,EAX

        XOR     EAX,EAX
        MOV     [ESI].TTextRec.BufPos,EAX
        MOV     [ESI].TTextRec.BufEnd,EAX
        MOV     AX,[ESI].TTextRec.Mode

        SUB     EAX,fmInput
        JE      @@calledByReset

        DEC     EAX
        JE      @@calledByRewrite

        DEC     EAX
        JE      @@calledByAppend

        JMP     @@exit

@@calledByReset:

        MOV     EAX,GENERIC_READ      // open for read
        MOV     EDX,FILE_SHARE_READ
        MOV     ECX,OPEN_EXISTING

        MOV     [ESI].TTextRec.InOutFunc,offset TextIn

        JMP     @@common

@@calledByRewrite:

        MOV     EAX,GENERIC_WRITE     // open for write
        MOV     EDX,FILE_SHARE_READ
        MOV     ECX,CREATE_ALWAYS
        JMP     @@commonOut

@@calledByAppend:

        MOV     EAX,GENERIC_READ OR GENERIC_WRITE // open for read/write
        MOV     EDX,FILE_SHARE_READ
        MOV     ECX,OPEN_EXISTING

@@commonOut:

        MOV     [ESI].TTextRec.InOutFunc,offset TextOut

@@common:

        MOV     [ESI].TTextRec.CloseFunc,offset TextClose
        MOV     [ESI].TTextRec.FlushFunc,offset FileNOPProc
        {$IFNDEF UNICODE}
        CMP     byte ptr [ESI].TTextRec.Name,0
        {$ELSE}
        CMP     word ptr [ESI].TTextRec.Name,0
        {$ENDIF}
        JE      @@isCon

//  CreateFile(t.Name, EAX, EDX, Nil, ECX, FILE_ATTRIBUTE_NORMAL, 0);

        PUSH    0
        PUSH    FILE_ATTRIBUTE_NORMAL
        PUSH    ECX
        PUSH    0
        PUSH    EDX
        PUSH    EAX
        LEA     EAX,[ESI].TTextRec.Name
        PUSH    EAX
        CALL    CreateFile

        CMP     EAX,-1
        JZ      @@error

        MOV     [ESI].TTextRec.Handle,EAX
        CMP     [ESI].TTextRec.Mode,fmInOut
        JNE     @@success

        DEC     [ESI].TTextRec.Mode     // fmInOut -> fmOutput

{;  ???  we really have to look for the first eof byte in the
; ???  last record and truncate the file there.
; Not very nice and clean...
;
; lastRecPos = Max( GetFileSize(...) - 128, 0);
}
        PUSH    0
        PUSH    [ESI].TTextRec.Handle
        CALL    GetFileSize

        INC     EAX
        JZ      @@error
        SUB     EAX,129
        JNC     @@3
        XOR     EAX,EAX
@@3:
//  lseek(f.Handle, SEEK_SET, lastRecPos);

        PUSH    FILE_BEGIN
        PUSH    0
        PUSH    EAX
        PUSH    [ESI].TTextRec.Handle
        CALL    SetFilePointer

        INC     EAX
        JE      @@error

//  bytesRead = read(f.Handle, f.Buffer, 128);

        PUSH    0
        MOV     EDX,ESP
  PUSH    0
        PUSH    EDX
        PUSH    128
        LEA     EDX,[ESI].TTextRec.Buffer
        PUSH    EDX
        PUSH    [ESI].TTextRec.Handle
        CALL    ReadFile
        POP     EDX
        DEC     EAX
        JNZ     @@error

//  for (i = 0; i < bytesRead; i++)

        XOR     EAX,EAX
@@loop:
        CMP     EAX,EDX
        JAE     @@success

//    if  (f.Buffer[i] == eof)

        CMP     byte ptr [ESI].TTextRec.Buffer[EAX],eof
        JE      @@truncate
        INC     EAX
        JMP     @@loop

@@truncate:

//  lseek( f.Handle, SEEK_END, i - bytesRead );

        PUSH    FILE_END
        PUSH    0
        SUB     EAX,EDX
        PUSH    EAX
        PUSH    [ESI].TTextRec.Handle
        CALL    SetFilePointer
        INC     EAX
        JE      @@error

//  SetEndOfFile( f.Handle );

        PUSH    [ESI].TTextRec.Handle
        CALL    SetEndOfFile
        DEC     EAX
        JNE     @@error

        JMP     @@success

@@isCon:
        LEA     EAX,[ESI].TTextRec.Buffer
        MOV     [ESI].TTextRec.BufSize, TYPE TTextRec.Buffer
        MOV     [ESI].TTextRec.CloseFunc,offset FileNOPProc
        MOV     [ESI].TTextRec.BufPtr,EAX
        CMP     [ESI].TTextRec.Mode,fmOutput
        JE      @@output
        PUSH    STD_INPUT_HANDLE
        JMP     @@1
@@output:
        CMP     ESI,offset ErrOutput
        JNE     @@stdout
        PUSH    STD_ERROR_HANDLE
        JMP     @@1
@@stdout:
        PUSH    STD_OUTPUT_HANDLE
@@1:
        CALL    GetStdHandle
        CMP     EAX,-1
        JE      @@error
        MOV     [ESI].TTextRec.Handle,EAX

@@success:
  CMP     [ESI].TTextRec.Mode,fmInput
  JE      @@2
  PUSH    [ESI].TTextRec.Handle
  CALL    GetFileType
  TEST    EAX,EAX
  JE      @@badFileType
  CMP     EAX,2
  JNE     @@2
  MOV     [ESI].TTextRec.FlushFunc,offset TextOut
@@2:
  XOR     EAX,EAX
@@exit:
  POP     ESI
  RET

@@badFileType:
  PUSH    [ESI].TTextRec.Handle
  CALL    CloseHandle
  MOV     [ESI].TTextRec.Mode,fmClosed
  MOV     EAX,105
  JMP     @@exit

@@error:
  MOV     [ESI].TTextRec.Mode,fmClosed
        CALL    GetLastError
        JMP     @@exit
end;
{$ENDIF MSWINDOWS}

const
  fNameLen = 260;

function _Assign(var t: TTextRec; const s: PChar): Integer;
var
  Len: Integer;
begin
  FillChar(t, SizeOf(TFileRec), 0);
  t.BufPtr := @t.Buffer;
  t.Mode := fmClosed;
  t.Flags := tfCRLF * Byte(DefaultTextLineBreakStyle);
  t.BufSize := SizeOf(t.Buffer);
  t.OpenFunc := @TextOpen;
  {$IFDEF UNICODE}
  Len := _PWCharLen(s);
  {$ELSE}
  Len := _PCharLen(s);
  {$ENDIF}
  MoveChars(s^, t.Name, Len);
  t.Name[Len] := #0;
  Result := 0;
end;

function InternalFlush(var t: TTextRec; Func: TTextIOFunc): Integer;
begin
  case t.Mode of
    fmOutput,
    fmInOut  : Result := Func(t);
    fmInput  : Result := 0;
  else
    if (@t = @Output) or (@t = @ErrOutput) then
      Result := 0
    else
      Result := 103;
  end;
  if Result <> 0 then SetInOutRes(Result);
end;

function Flush(var t: Text): Integer;
begin
  Result := InternalFlush(TTextRec(t), TTextRec(t).InOutFunc);
end;

function _Flush(var t: TTextRec): Integer;
begin
  Result := InternalFlush(t, t.FlushFunc);
end;

type
{$IFDEF MSWINDOWS}
  TIOProc = function (hFile: Integer; Buffer: Pointer; nNumberOfBytesToWrite: Cardinal;
  var lpNumberOfBytesWritten: Cardinal; lpOverlapped: Pointer): Integer; stdcall;

function ReadFileX(hFile: Integer; Buffer: Pointer; nNumberOfBytesToRead: Cardinal;
  var lpNumberOfBytesRead: Cardinal; lpOverlapped: Pointer): Integer; stdcall;
  external kernel name 'ReadFile';
function WriteFileX(hFile: Integer; Buffer: Pointer; nNumberOfBytesToWrite: Cardinal;
  var lpNumberOfBytesWritten: Cardinal; lpOverlapped: Pointer): Integer; stdcall;
  external kernel name 'WriteFile';

{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  TIOProc = function (Handle: Integer; Buffer: Pointer; Count: Cardinal): Cardinal; cdecl;
{$ENDIF POSIX}

function BlockIO(var f: TFileRec; buffer: Pointer; recCnt: Cardinal; var recsDone: Longint;
  ModeMask: Integer; IOProc: TIOProc; ErrorNo: Integer): Cardinal;
// Note:  RecsDone ptr can be nil!
begin
  if (f.Mode and ModeMask) = ModeMask then  // fmOutput or fmInOut / fmInput or fmInOut
  begin
{$IFDEF POSIX}
    Result := IOProc(f.Handle, buffer, recCnt * f.RecSize);
    if Integer(Result) = -1 then
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
    if IOProc(f.Handle, buffer, recCnt * f.RecSize, Result, nil) = 0 then
{$ENDIF MSWINDOWS}
    begin
      SetInOutRes(GetLastError);
      Result := 0;
    end
    else
    begin
      Result := Result div f.RecSize;
      if @RecsDone <> nil then
        RecsDone := Result
      else if Result <> recCnt then
      begin
        SetInOutRes(ErrorNo);
        Result := 0;
      end
    end;
  end
  else
  begin
    SetInOutRes(103);  // file not open
    Result := 0;
  end;
end;

function _BlockRead(var f: TFileRec; buffer: Pointer; recCnt: Longint; var recsRead: Longint): Longint;
begin
  Result := BlockIO(f, buffer, recCnt, recsRead, fmInput,
    {$IFDEF MSWINDOWS} ReadFileX, {$ENDIF}
    {$IFDEF POSIX} __read, {$ENDIF}
    100);
end;

function  _BlockWrite(var f: TFileRec; buffer: Pointer; recCnt: Longint; var recsWritten: Longint): Longint;
begin
  Result := BlockIO(f, buffer, recCnt, recsWritten, fmOutput,
  {$IFDEF MSWINDOWS} WriteFileX, {$ENDIF}
  {$IFDEF POSIX} __write, {$ENDIF}
  101);
end;

function _Close(var t: TTextRec): Integer;
begin
  Result := 0;
  if (t.Mode >= fmInput) and (t.Mode <= fmInOut) then
  begin
    if (t.Mode and fmOutput) = fmOutput then  // fmOutput or fmInOut
      Result := TTextIOFunc(t.InOutFunc)(t);
    if Result = 0 then
      Result := TTextIOFunc(t.CloseFunc)(t);
    if Result <> 0 then
      SetInOutRes(Result);
  end
  else
  if @t <> @Input then
    SetInOutRes(103);
end;

procedure       _PStrCat;
asm
{     ->EAX = Pointer to destination string     }
{       EDX = Pointer to source string  }

        PUSH    ESI
        PUSH    EDI

{       load dest len into EAX  }

        MOV     EDI,EAX
        XOR     EAX,EAX
        MOV     AL,[EDI]

{       load source address in ESI, source len in ECX   }

        MOV     ESI,EDX
        XOR     ECX,ECX
        MOV     CL,[ESI]
        INC     ESI

{       calculate final length in DL and store it in the destination    }

        MOV     DL,AL
        ADD     DL,CL
        JC      @@trunc

@@cont:
        MOV     [EDI],DL

{       calculate final dest address    }

        INC     EDI
        ADD     EDI,EAX

{       do the copy     }

        REP     MOVSB

{       done    }

        POP     EDI
        POP     ESI
        RET

@@trunc:
        INC     DL      {       DL = #chars to truncate                 }
        SUB     CL,DL   {       CL = source len - #chars to truncate    }
        MOV     DL,255  {       DL = maximum length                     }
        JMP     @@cont
end;

procedure       _PStrNCat;
asm
{     ->EAX = Pointer to destination string                     }
{       EDX = Pointer to source string                          }
{       CL  = max length of result (allocated size of dest - 1) }

        PUSH    ESI
        PUSH    EDI

{       load dest len into EAX  }

        MOV     EDI,EAX
        XOR     EAX,EAX
        MOV     AL,[EDI]

{       load source address in ESI, source len in EDX   }

        MOV     ESI,EDX
        XOR     EDX,EDX
        MOV     DL,[ESI]
        INC     ESI

{       calculate final length in AL and store it in the destination    }

        ADD     AL,DL
        JC      @@trunc
        CMP     AL,CL
        JA      @@trunc

@@cont:
        MOV     ECX,EDX
        MOV     DL,[EDI]
        MOV     [EDI],AL

{       calculate final dest address    }

        INC     EDI
        ADD     EDI,EDX

{       do the copy     }

        REP     MOVSB

@@done:
        POP     EDI
        POP     ESI
        RET

@@trunc:
{       CL = maxlen     }

        MOV     AL,CL   { AL = final length = maxlen            }
        SUB     CL,[EDI]        { CL = length to copy = maxlen - destlen        }
        JBE     @@done
        MOV     DL,CL
        JMP     @@cont
end;

procedure       _PStrCpy(Dest: PShortString; Source: PShortString);
begin
  Move(Source^, Dest^, Byte(Source^[0])+1);
end;

procedure       _PStrNCpy(Dest: PShortString; Source: PShortString; MaxLen: Byte);
begin
  if MaxLen > Byte(Source^[0]) then
    MaxLen := Byte(Source^[0]);
  Byte(Dest^[0]) := MaxLen;
  Move(Source^[1], Dest^[1], MaxLen);
end;

procedure       _PStrCmp;
asm
{     ->EAX = Pointer to left string    }
{       EDX = Pointer to right string   }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX
        MOV     EDI,EDX

        XOR     EAX,EAX
        XOR     EDX,EDX
        MOV     AL,[ESI]
        MOV     DL,[EDI]
        INC     ESI
        INC     EDI

        SUB     EAX,EDX { eax = len1 - len2 }
        JA      @@skip1
        ADD     EDX,EAX { edx = len2 + (len1 - len2) = len1     }

@@skip1:
        PUSH    EDX
        SHR     EDX,2
        JE      @@cmpRest
@@longLoop:
        MOV     ECX,[ESI]
        MOV     EBX,[EDI]
        CMP     ECX,EBX
        JNE     @@misMatch
        DEC     EDX
        JE      @@cmpRestP4
        MOV     ECX,[ESI+4]
        MOV     EBX,[EDI+4]
        CMP     ECX,EBX
        JNE     @@misMatch
        ADD     ESI,8
        ADD     EDI,8
        DEC     EDX
        JNE     @@longLoop
        JMP     @@cmpRest
@@cmpRestP4:
        ADD     ESI,4
        ADD     EDI,4
@@cmpRest:
        POP     EDX
        AND     EDX,3
        JE      @@equal

        MOV     CL,[ESI]
        CMP     CL,[EDI]
        JNE     @@exit
        DEC     EDX
        JE      @@equal
        MOV     CL,[ESI+1]
        CMP     CL,[EDI+1]
        JNE     @@exit
        DEC     EDX
        JE      @@equal
        MOV     CL,[ESI+2]
        CMP     CL,[EDI+2]
        JNE     @@exit

@@equal:
        ADD     EAX,EAX
        JMP     @@exit

@@misMatch:
        POP     EDX
        CMP     CL,BL
        JNE     @@exit
        CMP     CH,BH
        JNE     @@exit
        SHR     ECX,16
        SHR     EBX,16
        CMP     CL,BL
        JNE     @@exit
        CMP     CH,BH

@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
end;

procedure       _AStrCmp;
asm
{     ->EAX = Pointer to left string    }
{       EDX = Pointer to right string   }
{       ECX = Number of chars to compare}

        PUSH    EBX
        PUSH    ESI
        PUSH    ECX
        MOV     ESI,ECX
        SHR     ESI,2
        JE      @@cmpRest

@@longLoop:
        MOV     ECX,[EAX]
        MOV     EBX,[EDX]
        CMP     ECX,EBX
        JNE     @@misMatch
        DEC     ESI
        JE      @@cmpRestP4
        MOV     ECX,[EAX+4]
        MOV     EBX,[EDX+4]
        CMP     ECX,EBX
        JNE     @@misMatch
        ADD     EAX,8
        ADD     EDX,8
        DEC     ESI
        JNE     @@longLoop
        JMP     @@cmpRest
@@cmpRestp4:
        ADD     EAX,4
        ADD     EDX,4
@@cmpRest:
        POP     ESI
        AND     ESI,3
        JE      @@exit

        MOV     CL,[EAX]
        CMP     CL,[EDX]
        JNE     @@exit
        DEC     ESI
        JE      @@equal
        MOV     CL,[EAX+1]
        CMP     CL,[EDX+1]
        JNE     @@exit
        DEC     ESI
        JE      @@equal
        MOV     CL,[EAX+2]
        CMP     CL,[EDX+2]
        JNE     @@exit

@@equal:
        XOR     EAX,EAX
        JMP     @@exit

@@misMatch:
        POP     ESI
        CMP     CL,BL
        JNE     @@exit
        CMP     CH,BH
        JNE     @@exit
        SHR     ECX,16
        SHR     EBX,16
        CMP     CL,BL
        JNE     @@exit
        CMP     CH,BH

@@exit:
        POP     ESI
        POP     EBX
end;

procedure       _WStrLCmp;
asm
{     ->EAX = Pointer to left wide string    }
{       EDX = Pointer to right wide string   }
{       ECX = Number of chars to compare}

        PUSH    EBX
        PUSH    ESI
        PUSH    ECX
        MOV     ESI,ECX
        SHR     ESI,1
        JE      @@cmpRest

@@longLoop:
        MOV     ECX,[EAX]
        MOV     EBX,[EDX]
        CMP     ECX,EBX
        JNE     @@misMatch
        DEC     ESI
        JE      @@cmpRestP4
        MOV     ECX,[EAX+4]
        MOV     EBX,[EDX+4]
        CMP     ECX,EBX
        JNE     @@misMatch
        ADD     EAX,8
        ADD     EDX,8
        DEC     ESI
        JNE     @@longLoop
        JMP     @@cmpRest
@@cmpRestp4:
        ADD     EAX,4
        ADD     EDX,4
@@cmpRest:
        POP     ESI
        AND     ESI,1
        JE      @@exit

        MOV     CX,[EAX]
        CMP     CX,[EDX]
        JNE     @@exit

@@equal:
        XOR     EAX,EAX
        JMP     @@exit

@@misMatch:
        POP     ESI
        CMP     CX,BX
        JNE     @@exit
        SHR     ECX,16
        SHR     EBX,16
        CMP     CX,BX

@@exit:
        POP     ESI
        POP     EBX
end;

function _EofFile(var f: TFileRec): Boolean;
begin
  Result := _FilePos(f) >= _FileSize(f);
end;

function _EofText(var t: TTextRec): Boolean;
asm
// -> EAX Pointer to text record
// <- AL  Boolean result
        CMP     [EAX].TTextRec.Mode,fmInput
        JNE     @@readChar
        MOV     EDX,[EAX].TTextRec.BufPos
        CMP     EDX,[EAX].TTextRec.BufEnd
        JAE     @@readChar
        ADD     EDX,[EAX].TTextRec.BufPtr
        TEST    [EAX].TTextRec.Flags,tfCRLF
        JZ      @@FalseExit
        MOV     CL,[EDX]
        CMP     CL,cEof
        JNZ     @@FalseExit

@@eof:
        MOV     AL,1
        JMP     @@exit

@@readChar:
        PUSH    EAX
        CALL    _ReadChar
        POP     EDX
        CMP     AH,cEof
        JE      @@eof
        DEC     [EDX].TTextRec.BufPos
@@FalseExit:
        XOR     EAX,EAX
@@exit:
end;

function _Eoln(var t: TTextRec): Boolean;
asm
// -> EAX Pointer to text record
// <- AL  Boolean result

        CMP     [EAX].TTextRec.Mode,fmInput
        JNE     @@readChar
        MOV     EDX,[EAX].TTextRec.BufPos
        CMP     EDX,[EAX].TTextRec.BufEnd
        JAE     @@readChar
        ADD     EDX,[EAX].TTextRec.BufPtr
        TEST    [EAX].TTextRec.Flags,tfCRLF
        MOV     AL,0
        MOV     CL,[EDX]
        JZ      @@testLF
        CMP     CL,cCR
        JE      @@eol
        CMP     CL,cEOF
        JE      @@eof
        JMP     @@exit

@@testLF:
        CMP     CL,cLF
        JE      @@eol
        CMP     CL,cEOF
        JE      @@eof
        JMP     @@exit

@@readChar:
        PUSH    EAX
        CALL    _ReadChar
        POP     EDX
        CMP     AH,cEOF
        JE      @@eof
        DEC     [EDX].TTextRec.BufPos
        XOR     ECX,ECX
        XCHG    ECX,EAX
        TEST    [EDX].TTextRec.Mode,tfCRLF
        JE     	@@testLF
        CMP     CL,cCR
        JE      @@eol
        JMP     @@exit

@@eol:
@@eof:
        MOV     AL,1
@@exit:
end;

procedure _Erase(var f: TFileRec);
{$IFDEF POSIX}
var
   us: UTF8String;
{$ENDIF POSIX}
begin
  if (f.Mode < fmClosed) or (f.Mode > fmInOut) then
    SetInOutRes(102)  // file not assigned
  else begin
{$IFDEF POSIX}
    us := UTF8String(f.Name);
    if __remove(PAnsiChar(us)) < 0 then
       SetInOutRes(GetLastError);
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
    if not DeleteFile(f.Name) then
      SetInOutRes(GetLastError);
{$ENDIF MSWINDOWS}
  end;
end;

{$IFDEF MSWINDOWS}
// Floating-point divide reverse routine
// ST(1) = ST(0) / ST(1), pop ST

procedure _FSafeDivideR;
asm
  FXCH
  JMP _FSafeDivide
end;

// Floating-point divide routine
// ST(1) = ST(1) / ST(0), pop ST

procedure _FSafeDivide;
type
  Z = packed record  // helper type to make parameter references more readable
    Dividend: Extended;   // (TBYTE PTR [ESP])
    Pad: Word;
    Divisor: Extended;    // (TBYTE PTR [ESP+12])
  end;
asm
  CMP TestFDIV,0    //Check FDIV indicator
  JLE @@FDivideChecked  //Jump if flawed or don't know
  FDIV        //Known to be ok, so just do FDIV
  RET

// FDIV constants
@@FDIVRiscTable: DB 0,1,0,0,4,0,0,7,0,0,10,0,0,13,0,0;

@@FDIVScale1:    DD $3F700000            // 0.9375
@@FDIVScale2:    DD $3F880000            // 1.0625
@@FDIV1SHL63:    DD $5F000000            // 1 SHL 63

@@TestDividend:  DD $C0000000,$4150017E  // 4195835.0
@@TestDivisor:   DD $80000000,$4147FFFF  // 3145727.0
@@TestOne:       DD $00000000,$3FF00000  // 1.0

// Flawed FDIV detection
@@FDivideDetect:
  MOV TestFDIV,1      //Indicate correct FDIV
  PUSH  EAX
  SUB ESP,12
  FSTP  TBYTE PTR [ESP]           //Save off ST
  FLD QWORD PTR @@TestDividend  //Ok if x - (x / y) * y < 1.0
  FDIV  QWORD PTR @@TestDivisor
  FMUL  QWORD PTR @@TestDivisor
  FSUBR QWORD PTR @@TestDividend
  FCOMP QWORD PTR @@TestOne
  FSTSW AX
  SHR EAX,7
  AND EAX,002H    //Zero if FDIV is flawed
  DEC EAX
  MOV TestFDIV,AL   //1 means Ok, -1 means flawed
  FLD TBYTE PTR [ESP]   //Restore ST
  ADD ESP,12
  POP EAX
  JMP _FSafeDivide

@@FDivideChecked:
  JE  @@FDivideDetect   //Do detection if TestFDIV = 0

@@1:  PUSH  EAX
  SUB ESP,24
  FSTP  [ESP].Z.Divisor     //Store Divisor and Dividend
  FSTP  [ESP].Z.Dividend
  FLD [ESP].Z.Dividend
  FLD [ESP].Z.Divisor
@@2:  MOV EAX,DWORD PTR [ESP+4].Z.Divisor   //Is Divisor a denormal?
  ADD EAX,EAX
  JNC @@20      //Yes, @@20
  XOR EAX,0E000000H   //If these three bits are not all
  TEST  EAX,0E000000H   //ones, FDIV will work
  JZ  @@10      //Jump if all ones
@@3:  FDIV        //Do FDIV and exit
  ADD ESP,24
  POP EAX
  RET

@@10: SHR EAX,28      //If the four bits following the MSB
          //of the mantissa have a decimal
          //of 1, 4, 7, 10, or 13, FDIV may
  CMP byte ptr @@FDIVRiscTable[EAX],0 //not work correctly
  JZ  @@3     //Do FDIV if not 1, 4, 7, 10, or 13
  MOV EAX,DWORD PTR [ESP+8].Z.Divisor //Get Divisor exponent
  AND EAX,7FFFH
  JZ  @@3     //Ok to FDIV if denormal
  CMP EAX,7FFFH
  JE  @@3     //Ok to FDIV if NAN or INF
  MOV EAX,DWORD PTR [ESP+8].Z.Dividend //Get Dividend exponent
  AND EAX,7FFFH
  CMP EAX,1     //Small number?
  JE  @@11      //Yes, @@11
  FMUL  DWORD PTR @@FDIVScale1  //Scale by 15/16
  FXCH
  FMUL  DWORD PTR @@FDIVScale1
  FXCH
  JMP @@3     //FDIV is now safe

@@11: FMUL  DWORD PTR @@FDIVScale2    //Scale by 17/16
  FXCH
  FMUL  DWORD PTR @@FDIVScale2
  FXCH
  JMP @@3     //FDIV is now safe

@@20: MOV EAX,DWORD PTR [ESP].Z.Divisor     //Is entire Divisor zero?
  OR  EAX,DWORD PTR [ESP+4].Z.Divisor
  JZ  @@3               //Yes, ok to FDIV
  MOV EAX,DWORD PTR [ESP+8].Z.Divisor   //Get Divisor exponent
  AND EAX,7FFFH             //Non-zero exponent is invalid
  JNZ @@3               //Ok to FDIV if invalid
  MOV EAX,DWORD PTR [ESP+8].Z.Dividend  //Get Dividend exponent
  AND EAX,7FFFH             //Denormal?
  JZ  @@21                //Yes, @@21
  CMP EAX,7FFFH             //NAN or INF?
  JE  @@3               //Yes, ok to FDIV
  MOV EAX,DWORD PTR [ESP+4].Z.Dividend  //If MSB of mantissa is zero,
  ADD EAX,EAX               //the number is invalid
  JNC @@3               //Ok to FDIV if invalid
  JMP @@22
@@21: MOV EAX,DWORD PTR [ESP+4].Z.Dividend  //If MSB of mantissa is zero,
  ADD EAX,EAX               //the number is invalid
  JC  @@3               //Ok to FDIV if invalid
@@22: FXCH                  //Scale stored Divisor image by
  FSTP  ST(0)               //1 SHL 63 and restart
  FLD ST(0)
  FMUL  DWORD PTR @@FDIV1SHL63
  FSTP  [ESP].Z.Divisor
  FLD [ESP].Z.Dividend
  FXCH
  JMP @@2
end;
{$ENDIF}

function _FilePos(var f: TFileRec): Longint;
begin
  if (f.Mode > fmClosed) and (f.Mode <= fmInOut) then
  begin
{$IFDEF POSIX}
{MACOSXTODO: lseek on os/x returns a 64bit value }
    Result := _lseek(f.Handle, 0, SEEK_CUR);
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
    Result := SetFilePointer(f.Handle, 0, nil, FILE_CURRENT);
{$ENDIF MSWINDOWS}
    if Result = -1 then
      InOutError
    else
      Result := Cardinal(Result) div f.RecSize;
  end
  else
  begin
    SetInOutRes(103);
    Result := -1;
  end;
end;

function _FileSize(var f: TFileRec): Longint;
{$IFDEF MSWINDOWS}
begin
  Result := -1;
  if (f.Mode > fmClosed) and (f.Mode <= fmInOut) then
  begin
    Result := GetFileSize(f.Handle, 0);
    if Result = -1 then
      InOutError
    else
      Result := Cardinal(Result) div f.RecSize;
  end
  else
    SetInOutRes(103);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
var
  stat: TStatStruct;
begin
  Result := -1;
  if (f.Mode > fmClosed) and (f.Mode <= fmInOut) then
  begin
    if _fxstat(STAT_VER_LINUX, f.Handle, stat) <> 0 then
      InOutError
    else
      Result := stat.st_size div f.RecSize;
  end
  else
    SetInOutRes(103);
{$ENDIF POSIX}
end;

(* ***** BEGIN LICENSE BLOCK *****
 *
 * The assembly procedure FillChar is licensed under the CodeGear license terms.
 *
 * The initial developer of the original code is Fastcode
 *
 * Portions created by the initial developer are Copyright (C) 2002-2004
 * the initial developer. All Rights Reserved.
 *
 * Contributor(s): John O'Harrow
 *
 * ***** END LICENSE BLOCK ***** *)
procedure _FillChar(var Dest; count: Integer; Value: Char);
{$IFDEF PUREPASCAL}
var
  I: Integer;
  P: PChar;
begin
  P := PChar(@Dest);
  for I := count-1 downto 0 do
    P[I] := Value;
end;
{$ELSE}
asm                                  // Size = 153 Bytes
        CMP   EDX, 32
        MOV   CH, CL                 // Copy Value into both Bytes of CX
        JL    @@Small
        MOV   [EAX  ], CX            // Fill First 8 Bytes
        MOV   [EAX+2], CX
        MOV   [EAX+4], CX
        MOV   [EAX+6], CX
        SUB   EDX, 16
        FLD   QWORD PTR [EAX]
        FST   QWORD PTR [EAX+EDX]    // Fill Last 16 Bytes
        FST   QWORD PTR [EAX+EDX+8]
        MOV   ECX, EAX
        AND   ECX, 7                 // 8-Byte Align Writes
        SUB   ECX, 8
        SUB   EAX, ECX
        ADD   EDX, ECX
        ADD   EAX, EDX
        NEG   EDX
@@Loop:
        FST   QWORD PTR [EAX+EDX]    // Fill 16 Bytes per Loop
        FST   QWORD PTR [EAX+EDX+8]
        ADD   EDX, 16
        JL    @@Loop
        FFREE ST(0)
        FINCSTP
        RET
        NOP
        NOP
        NOP
@@Small:
        TEST  EDX, EDX
        JLE   @@Done
        MOV   [EAX+EDX-1], CL        // Fill Last Byte
        AND   EDX, -2                // No. of Words to Fill
        NEG   EDX
        LEA   EDX, [@@SmallFill + 60 + EDX * 2]
        JMP   EDX
        NOP                          // Align Jump Destinations
        NOP
@@SmallFill:
        MOV   [EAX+28], CX
        MOV   [EAX+26], CX
        MOV   [EAX+24], CX
        MOV   [EAX+22], CX
        MOV   [EAX+20], CX
        MOV   [EAX+18], CX
        MOV   [EAX+16], CX
        MOV   [EAX+14], CX
        MOV   [EAX+12], CX
        MOV   [EAX+10], CX
        MOV   [EAX+ 8], CX
        MOV   [EAX+ 6], CX
        MOV   [EAX+ 4], CX
        MOV   [EAX+ 2], CX
        MOV   [EAX   ], CX
        RET                          // DO NOT REMOVE - This is for Alignment
@@Done:
end;
{$ENDIF}

procedure       Mark;
begin
  Error(reInvalidPtr);
end;

function _ReadRec(var f: TFileRec; Buffer: Pointer): Integer;
{$IFDEF POSIX}
begin
  if (f.Mode and fmInput) = fmInput then  // fmInput or fmInOut
  begin
  Result := __read(f.Handle, Buffer, f.RecSize);
  if Result = -1 then
    InOutError
  else if Cardinal(Result) <> f.RecSize then
    SetInOutRes(100);
  end
  else
  begin
  SetInOutRes(103);  // file not open for input
  Result := 0;
  end;
end;
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
asm
// -> EAX Pointer to file variable
//  EDX Pointer to buffer

        PUSH    EBX
        XOR     ECX,ECX
        MOV     EBX,EAX
        MOV     CX,[EAX].TFileRec.Mode   // File must be open
        SUB     ECX,fmInput
        JE      @@skip
        SUB     ECX,fmInOut-fmInput
        JNE     @@fileNotOpen
@@skip:

//  ReadFile(f.Handle, buffer, f.RecSize, @result, Nil);

        PUSH    0     // space for OS result
        MOV     EAX,ESP

        PUSH    0     // pass lpOverlapped
        PUSH    EAX     // pass @result

        PUSH    [EBX].TFileRec.RecSize    // pass nNumberOfBytesToRead

        PUSH    EDX     // pass lpBuffer
        PUSH    [EBX].TFileRec.Handle   // pass hFile
        CALL    ReadFile
        POP     EDX     // pop result
        DEC     EAX     // check EAX = TRUE
        JNZ     @@error

        CMP     EDX,[EBX].TFileRec.RecSize  // result = f.RecSize ?
        JE      @@exit

@@readError:
        MOV EAX,100
        JMP @@errExit

@@fileNotOpen:
        MOV EAX,103
        JMP @@errExit

@@error:
        CALL  GetLastError
@@errExit:
        CALL  SetInOutRes
@@exit:
        POP EBX
end;
{$ENDIF MSWINDOWS}


// If the file is Input std variable, try to open it
// Otherwise, runtime error.
function TryOpenForInput(var t: TTextRec): Boolean;
begin
  if @t = @Input then
  begin
    t.Flags := tfCRLF * Byte(DefaultTextLineBreakStyle);
    _ResetText(t);
  end;

  Result := t.Mode = fmInput;
  if not Result then
    SetInOutRes(104);
end;

function _ReadChar(var t: TTextRec): AnsiChar;
asm
// -> EAX Pointer to text record
// <- AL  Character read. (may be a pseudo cEOF in DOS mode)
// <- AH  cEOF = End of File, else 0
//    For eof, #$1A is returned in AL and in AH.
//    For errors, InOutRes is set and #$1A is returned.

        CMP     [EAX].TTextRec.Mode, fmInput
        JE      @@checkBuf
        PUSH    EAX
        CALL    TryOpenForInput
        TEST    AL,AL
        POP     EAX
        JZ      @@eofexit

@@checkBuf:
        MOV     EDX,[EAX].TTextRec.BufPos
        CMP     EDX,[EAX].TTextRec.BufEnd
        JAE     @@fillBuf
@@cont:
        TEST    [EAX].TTextRec.Flags,tfCRLF
        MOV     ECX,[EAX].TTextRec.BufPtr
        MOV     CL,[ECX+EDX]
        JZ      @@cont2
        CMP     CL,cEof                       // Check for EOF char in DOS mode
        JE      @@eofexit
@@cont2:
        INC     EDX
        MOV     [EAX].TTextRec.BufPos,EDX
        XOR     EAX,EAX
        JMP     @@exit

@@fillBuf:
        PUSH    EAX
        CALL    [EAX].TTextRec.InOutFunc
        TEST    EAX,EAX
        JNE     @@error
        POP     EAX
        MOV     EDX,[EAX].TTextRec.BufPos
        CMP     EDX,[EAX].TTextRec.BufEnd
        JB      @@cont

//  We didn't get characters. Must be eof then.

@@eof:
        TEST    [EAX].TTextRec.Flags,tfCRLF
        JZ      @@eofexit
//  In DOS CRLF compatibility mode, synthesize an EOF char
//  Store one eof in the buffer and increment BufEnd
        MOV     ECX,[EAX].TTextRec.BufPtr
        MOV     byte ptr [ECX+EDX],cEof
        INC     [EAX].TTextRec.BufEnd
        JMP     @@eofexit

@@error:
        CALL    SetInOutRes
        POP     EAX
@@eofexit:
        MOV     CL,cEof
        MOV     AH,CL
@@exit:
        MOV     AL,CL
end;

function _ValLongL(const s: AnsiString; var code: Integer): Longint;
{$IFNDEF UNICODE}
asm
        JMP     _ValLong
end;
{$ELSE}
begin
  Result := _ValLong(string(s), code);
end;
{$ENDIF}

function _ReadLong(var t: TTextRec): Longint;
asm
// -> EAX Pointer to text record
// <- EAX Result

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        SUB     ESP,36      // var numbuf: String[32];
        PUSH     0           // String Length
        PUSH     -1          // Refcount (-1 = string constant)
        PUSH     $00010000   // elemSize = 1, codePage = CP_ACP

        MOV     ESI,EAX
        CALL    _SeekEof
        DEC     AL
        JZ      @@eof

        LEA     EDI,[ESP+skew]     // EDI -> numBuf[0]
        MOV     BL,32
@@loop:
        MOV     EAX,ESI
        CALL    _ReadChar
        CMP     AL,' '
        JBE     @@endNum
        STOSB
        DEC     BL
        JNZ     @@loop
@@convert:
        MOV     byte ptr [EDI],0
        LEA     EAX,[ESP+skew]     // EAX -> numBuf
        MOV      ECX,EDI
        SUB      ECX,EAX
        MOV      [EAX-Skew].StrRec.length,ECX
        PUSH    EDX     // allocate code result
        MOV     EDX,ESP     // pass pointer to code
        CALL    _ValLongL    // convert
        POP     EDX     // pop code result into EDX
        TEST    EDX,EDX
        JZ      @@exit
        MOV     EAX,106
        CALL    SetInOutRes
        JMP     @@exit

@@endNum:
        CMP     AH,cEof
        JE      @@convert
        DEC     [ESI].TTextRec.BufPos
        JMP     @@convert

@@eof:
        XOR     EAX,EAX
@@exit:
        ADD     ESP,36 + 4 + 4 + 2 + 2 // length, refCnt, elemSize, codePage
        POP     EDI
        POP     ESI
        POP     EBX
end;

function ReadLine(var t: TTextRec; buf: Pointer; maxLen: Longint): Pointer;
asm
// -> EAX Pointer to text record
//  EDX Pointer to buffer
//  ECX Maximum count of chars to read
// <- ECX Actual count of chars in buffer
// <- EAX Pointer to text record

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    ECX
        MOV     ESI,ECX
        MOV     EDI,EDX

        CMP     [EAX].TTextRec.Mode,fmInput
        JE      @@start
        PUSH    EAX
        CALL    TryOpenForInput
        TEST    AL,AL
        POP     EAX
        JZ      @@exit

@@start:
        MOV     EBX,EAX

        TEST    ESI,ESI
        JLE     @@exit

        MOV     EDX,[EBX].TTextRec.BufPos
        MOV     ECX,[EBX].TTextRec.BufEnd
        SUB     ECX,EDX
        ADD     EDX,[EBX].TTextRec.BufPtr

@@loop:
        DEC     ECX
        JL      @@readChar
        MOV     AL,[EDX]
        INC     EDX
@@cont:
        CMP     AL,cLF
        JE      @@lf

        CMP     AL,cCR
        JE      @@cr

        CMP     AL,cEOF
        JE      @@eof

@@store:
        STOSB
        DEC     ESI
        JG      @@loop
        JMP     @@finish

@@eof:
        TEST    [EBX].TTextRec.Flags,tfCRLF
        JZ      @@store
        JMP     @@finish

@@cr:
        MOV     AL,[EDX]
        CMP     AL,cLF
        JNE     @@loop
@@lf:
        DEC     EDX
@@finish:
        SUB     EDX,[EBX].TTextRec.BufPtr
        MOV     [EBX].TTextRec.BufPos,EDX
        JMP     @@exit

@@readChar:
        MOV     [EBX].TTextRec.BufPos,EDX
        MOV     EAX,EBX
        CALL    _ReadChar
        MOV     EDX,[EBX].TTextRec.BufPos
        MOV     ECX,[EBX].TTextRec.BufEnd
        SUB     ECX,EDX
        ADD     EDX,[EBX].TTextRec.BufPtr
        TEST    AH,AH    //eof
        JZ      @@cont

@@exit:
        MOV     EAX,EBX
        POP     ECX
        SUB     ECX,ESI
        POP     EDI
        POP     ESI
        POP     EBX
end;

procedure _ReadString(var t: TTextRec; s: PShortString; maxLen: Longint);
asm
// -> EAX Pointer to text record
//  EDX Pointer to string
//  ECX Maximum length of string

        PUSH     EDX
        INC      EDX
        CALL     ReadLine
        POP      EDX
        MOV      [EDX],CL
end;

procedure _ReadCString(var t: TTextRec; s: PAnsiChar; maxLen: Longint);
asm
// -> EAX Pointer to text record
//  EDX Pointer to string
//  ECX Maximum length of string

        PUSH    EDX
        CALL    ReadLine
        POP     EDX
        MOV     byte ptr [EDX+ECX],0
end;

procedure _ReadLString(var t: TTextRec; var s: AnsiString; CodePage: Word);
asm
        { ->    EAX     pointer to Text         }
        {       EDX     pointer to AnsiString   }
        {       ECX     destination codepage    }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EBX,EAX
        MOV     ESI,EDX
        MOV     EDI,ECX

        MOV     EAX,EDX
        CALL    _LStrClr

        SUB     ESP,256

        MOV     EAX,EBX
        MOV     EDX,ESP
        MOV     ECX,255
        CALL    _ReadString

        MOV     EAX,ESI
        MOV     EDX,ESP
        MOV     ECX,EDI
        CALL    _LStrFromString

        CMP     byte ptr [ESP],255
        JNE     @@exit
@@loop:

        MOV     EAX,EBX
        MOV     EDX,ESP
        MOV     ECX,255
        CALL    _ReadString

        MOV     EDX,ESP
        PUSH    0
        MOV     EAX,ESP
        MOV     ECX,EDI
        CALL    _LStrFromString

        MOV     EAX,ESI
        MOV     EDX,[ESP]
        CALL    _LStrCat

        MOV     EAX,ESP
        CALL    _LStrClr
        POP     EAX

        CMP     byte ptr [ESP],255
        JE      @@loop

@@exit:
        ADD     ESP,256
        POP     EDI
        POP     ESI
        POP     EBX
end;


function IsValidMultibyteChar(const Src: PAnsiChar; SrcBytes: Integer): Boolean;
{$IFDEF MSWINDOWS}
const
  ERROR_NO_UNICODE_TRANSLATION = 1113;   // Win32 GetLastError when result = 0
  MB_ERR_INVALID_CHARS         = 8;
var
  Dest: WideChar;
begin
  Result := MultiByteToWideChar(DefaultSystemCodePage, MB_ERR_INVALID_CHARS, Src, SrcBytes, @Dest, 1) <> 0;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
begin
  Result := mblen(Src, SrcBytes) <> -1;
{$ENDIF POSIX}
end;


function _ReadWChar(var t: TTextRec): WideChar;
var
  scratch: array [0..7] of AnsiChar;
  wc: WideChar;
  i: Integer;
begin
  i := 0;
  while i < High(scratch) do
  begin
    scratch[i] := _ReadChar(t);
    Inc(i);
    scratch[i] := #0;
    if IsValidMultibyteChar(scratch, i) then
    begin
      WCharFromChar(@wc, 1, scratch, i, DefaultSystemCodePage);
      Result := wc;
      Exit;
    end;
  end;
  SetInOutRes(106);  // Invalid Input
  Result := #0;
//  Result := _ReadChar(t);
end;


procedure _ReadWCString(var t: TTextRec; s: PWideChar; maxBytes: Longint);
var
  i, maxLen: Integer;
  wc: WideChar;
begin
  if s = nil then Exit;
  i := 0;
  maxLen := maxBytes div sizeof(WideChar);
  while i < maxLen do
  begin
    wc := _ReadWChar(t);
    case Integer(wc) of
      cEOF: if _EOFText(t) then Break;
      cLF : begin
              Dec(t.BufPos);
              Break;
            end;
      cCR : if Byte(t.BufPtr[t.BufPos]) = cLF then
            begin
              Dec(t.BufPos);
              Break;
            end;
    end;
    s[i] := wc;
    Inc(i);
  end;
  s[i] := #0;
end;

procedure _ReadWString(var t: TTextRec; var s: WideString);
var
  Temp: AnsiString;
begin
  _ReadLString(t, Temp, DefaultSystemCodePage);
  s := WideString(Temp);
end;

function _ValExtL(const s: AnsiString; var code: Integer): Extended;
{$IFNDEF UNICODE}
asm
        JMP      _ValExt
end;
{$ELSE}
begin
  Val(string(s), Result, code);
end;
{$ENDIF}

function _ReadExt(var t: TTextRec): Extended;
asm
// -> EAX Pointer to text record
// <- FST(0)  Result

        PUSH     EBX
        PUSH     ESI
        PUSH     EDI
        SUB      ESP,68      // var numbuf: array[0..64] of char;
        PUSH     0           // String Length
        PUSH     -1          // Refcount (-1 = string constant)
        PUSH     $00010000   // elemSize = 1, codePage = CP_ACP

        MOV      ESI,EAX
        CALL     _SeekEof
        DEC      AL
        JZ       @@eof

        LEA      EDI,[ESP+Skew]     // EDI -> numBuf[0]
        MOV      BL,64
@@loop:
        MOV      EAX,ESI
        CALL     _ReadChar
        CMP      AL,' '
        JBE      @@endNum
        STOSB
        DEC      BL
        JNZ      @@loop
@@convert:
        MOV      byte ptr [EDI],0
        LEA      EAX,[ESP+Skew]     // EAX -> numBuf
        MOV      ECX,EDI
        SUB      ECX,EAX
        MOV      [EAX-Skew].StrRec.length,ECX
        PUSH     EDX     // allocate code result
        MOV      EDX,ESP     // pass pointer to code
        CALL     _ValExtL     // convert
        POP      EDX     // pop code result into EDX
        TEST     EDX,EDX
        JZ       @@exit
        MOV      EAX,106
        CALL     SetInOutRes
        JMP      @@exit

@@endNum:
        CMP      AH,cEOF
        JE       @@convert
        DEC      [ESI].TTextRec.BufPos
        JMP      @@convert

@@eof:
        FLDZ
@@exit:
        ADD      ESP,68 + 4 + 4 + 2 + 2 // length, refcount, elemsize, codepage
        POP      EDI
        POP      ESI
        POP      EBX
end;

procedure _ReadLn(var t: TTextRec);
asm
// -> EAX Pointer to text record

        PUSH     EBX
        MOV      EBX,EAX
@@loop:
        MOV      EAX,EBX
        CALL     _ReadChar

        CMP      AL,cLF            // accept LF as end of line
        JE       @@exit
        CMP      AH,cEOF
        JE       @@eof
        CMP      AL,cCR
        JNE      @@loop

        MOV      EAX,EBX
        CALL     _ReadChar

        CMP      AL,cLF            // accept CR+LF as end of line
        JE       @@exit
        CMP      AH,cEOF           // accept CR+EOF as end of line
        JE       @@eof
        DEC      [EBX].TTextRec.BufPos
        JMP      @@loop            // else CR+ anything else is not a line break.

@@exit:
@@eof:
        POP      EBX
end;

procedure _Rename(var f: TFileRec; newName: PChar);
var
  I: Integer;
  oldName: string;
{$IFDEF POSIX}
  usOldName: UTF8String;
  usNewName: UTF8String;
{$ENDIF POSIX}
begin
  if f.Mode = fmClosed then
  begin
    if newName = nil then newName := '';
    oldName := f.Name;
{$IFDEF POSIX}
    usNewName := UTF8String(newName);
    usOldName := UTF8String(oldName);
    if __rename(PAnsiChar(usOldName), PAnsiChar(usNewName)) = 0 then
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
    if MoveFile(PChar(oldName), newName) then
{$ENDIF MSWINDOWS}
    begin
      I := 0;
      while (newName[I] <> #0) and (I < High(f.Name)) do
      begin
        f.Name[I] := newName[I];
        Inc(I);
      end
    end
    else
      SetInOutRes(GetLastError);
  end
  else
    SetInOutRes(102);
end;

procedure       Release;
begin
  Error(reInvalidPtr);
end;

function _CloseFile(var f: TFileRec): Integer;
begin
  f.Mode := fmClosed;
  Result := 0;
  if not InternalClose(f.Handle) then
  begin
    InOutError;
    Result := 1;
  end;
end;

function OpenFile(var f: TFileRec; recSiz: Longint; mode: Longint): Integer;
{$IFDEF POSIX}
var
   Flags: Integer;
   uName: UTF8String;
begin
  Result := 0;
  if (f.Mode >= fmClosed) and (f.Mode <= fmInOut) then
  begin
    if f.Mode <> fmClosed then // not yet closed: close it
    begin
      Result := TFileIOFunc(f.CloseFunc)(f);
      if Result <> 0 then
        SetInOutRes(Result);
    end;

    if recSiz <= 0 then
      SetInOutRes(106);

    f.RecSize := recSiz;
    f.InOutFunc := @FileNopProc;

    if f.Name[0] <> #0 then
    begin
      f.CloseFunc := @_CloseFile;
      case mode of
        1: begin
             Flags := O_APPEND or O_WRONLY;
             f.Mode := fmOutput;
           end;
        2: begin
             Flags := O_RDWR;
             f.Mode := fmInOut;
           end;
        3: begin
             Flags := O_CREAT or O_TRUNC or O_RDWR;
             f.Mode := fmInOut;
           end;
      else
        Flags := O_RDONLY;
        f.Mode := fmInput;
      end;

      uName := UTF8String(f.Name);
      f.Handle := __open(PAnsiChar(uName), Flags, FileAccessRights);
    end
    else  // stdin or stdout
    begin
      f.CloseFunc := @FileNopProc;
      if mode = 3 then
        f.Handle := STDOUT_FILENO
      else
        f.Handle := STDIN_FILENO;
    end;

    if f.Handle = -1 then
    begin
      f.Mode := fmClosed;
      InOutError;
    end;
  end
  else
    SetInOutRes(102);
end;
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
const
  ShareTab: array [0..7] of Integer =
    (FILE_SHARE_READ OR FILE_SHARE_WRITE,  // OF_SHARE_COMPAT     0x00000000
     0,                                    // OF_SHARE_EXCLUSIVE  0x00000010
     FILE_SHARE_READ,                      // OF_SHARE_DENY_WRITE 0x00000020
     FILE_SHARE_WRITE,                     // OF_SHARE_DENY_READ  0x00000030
     FILE_SHARE_READ OR FILE_SHARE_WRITE,  // OF_SHARE_DENY_NONE  0x00000040
     0,0,0);
asm
//->  EAX Pointer to file record
//  EDX Record size
//  ECX File mode

        PUSH     EBX
        PUSH     ESI
        PUSH     EDI

        MOV      ESI,EDX
        MOV      EDI,ECX
        XOR      EDX,EDX
        MOV      EBX,EAX

  MOV      DX,[EAX].TFileRec.Mode
        SUB      EDX,fmClosed
        JE       @@alreadyClosed
        CMP      EDX,fmInOut-fmClosed
        JA       @@notAssignedError

//  not yet closed: close it. File parameter is still in EAX

        CALL     [EBX].TFileRec.CloseFunc
        TEST     EAX,EAX
        JE       @@alreadyClosed
        CALL     SetInOutRes

@@alreadyClosed:

        MOV     [EBX].TFileRec.Mode,fmInOut
        MOV     [EBX].TFileRec.RecSize,ESI
        MOV     [EBX].TFileRec.CloseFunc,offset _CloseFile
        MOV     [EBX].TFileRec.InOutFunc,offset FileNopProc

        {$IFNDEF UNICODE}
        CMP     byte ptr [EBX].TFileRec.Name,0
        {$ELSE}
        CMP     word ptr [EBX].TFileRec.Name,0
        {$ENDIF}
        JE      @@isCon

        MOV     EAX,GENERIC_READ OR GENERIC_WRITE
        MOV     DL,FileMode
        AND     EDX,070H
        SHR     EDX,4-2
        MOV     EDX,dword ptr [shareTab+EDX]
        MOV     ECX,CREATE_ALWAYS

        SUB     EDI,3
        JE      @@calledByRewrite

        MOV     ECX,OPEN_EXISTING
        INC     EDI
        JE      @@skip

        MOV     EAX,GENERIC_WRITE
        INC     EDI
        MOV     [EBX].TFileRec.Mode,fmOutput
        JE      @@skip

        MOV     EAX,GENERIC_READ
        MOV     [EBX].TFileRec.Mode,fmInput

@@skip:
@@calledByRewrite:

//  CreateFile(t.FileName, EAX, EDX, Nil, ECX, FILE_ATTRIBUTE_NORMAL, 0);

        PUSH     0
        PUSH     FILE_ATTRIBUTE_NORMAL
        PUSH     ECX
        PUSH     0
        PUSH     EDX
        PUSH     EAX
        LEA      EAX,[EBX].TFileRec.Name
        PUSH     EAX
        CALL     CreateFile
@@checkHandle:
        CMP      EAX,-1
        JZ       @@error

        MOV      [EBX].TFileRec.Handle,EAX
        JMP      @@exit

@@isCon:
        MOV      [EBX].TFileRec.CloseFunc,offset FileNopProc
        CMP      EDI,3
        JE       @@output
        PUSH     STD_INPUT_HANDLE
        JMP      @@1
@@output:
        PUSH     STD_OUTPUT_HANDLE
@@1:
        CALL     GetStdHandle
        JMP      @@checkHandle

@@notAssignedError:
        MOV      EAX,102
        JMP      @@errExit

@@error:
        MOV      [EBX].TFileRec.Mode,fmClosed
        CALL     GetLastError
@@errExit:
        CALL     SetInOutRes

@@exit:
        POP      EDI
        POP      ESI
        POP      EBX
end;
{$ENDIF MSWINDOWS}

function _ResetFile(var f: TFileRec; recSize: Longint): Integer;
var
  m: Byte;
begin
  m := FileMode and 3;
  if m > 2 then m := 2;
  Result := OpenFile(f, recSize, m);
end;

function _RewritFile(var f: TFileRec; recSize: Longint): Integer;
begin
  Result := OpenFile(f, recSize, 3);
end;

procedure _Seek(var f: TFileRec; recNum: Cardinal);
{$IFDEF POSIX}
begin
  if (f.Mode >= fmInput) and (f.Mode <= fmInOut) then
  begin
{MACOSXTODO: lseek on os/x returns a 64bit value }
    if _lseek(f.Handle, f.RecSize * recNum, SEEK_SET) = -1 then
      InOutError;
  end
  else
    SetInOutRes(103);
end;
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
asm
// -> EAX Pointer to file variable
//  EDX Record number

        MOV      ECX,EAX
        MOVZX    EAX,[EAX].TFileRec.Mode // check whether file is open
        SUB      EAX,fmInput
        CMP      EAX,fmInOut-fmInput
        JA       @@fileNotOpen

//  SetFilePointer(f.Handle, recNum*f.RecSize, FILE_BEGIN
        PUSH     FILE_BEGIN    // pass dwMoveMethod
        MOV      EAX,[ECX].TFileRec.RecSize
        MUL      EDX
        PUSH     0           // pass lpDistanceToMoveHigh
        PUSH     EAX           // pass lDistanceToMove
        PUSH     [ECX].TFileRec.Handle   // pass hFile
        CALL     SetFilePointer          // get current position
        INC      EAX
        JZ       InOutError
        JMP      @@exit

@@fileNotOpen:
        MOV      EAX,103
        JMP      SetInOutRes

@@exit:
end;
{$ENDIF MSWINDOWS}

function _SeekEof(var t: TTextRec): Boolean;
asm
// -> EAX Pointer to text record
// <- AL  Boolean result

        PUSH     EBX
        MOV      EBX,EAX
@@loop:
        MOV      EAX,EBX
        CALL     _ReadChar
        CMP      AL,' '
        JA       @@endloop
        CMP      AH,cEOF
        JE       @@eof
        JMP      @@loop
@@eof:
        MOV      AL,1
        JMP      @@exit

@@endloop:
        DEC      [EBX].TTextRec.BufPos
        XOR      AL,AL
@@exit:
        POP      EBX
end;

function _SeekEoln(var t: TTextRec): Boolean;
asm
// -> EAX Pointer to text record
// <- AL  Boolean result

        PUSH     EBX
        MOV      EBX,EAX
@@loop:
        MOV      EAX,EBX
        CALL     _ReadChar
        CMP      AL,' '
        JA       @@falseExit
        CMP      AH,cEOF
        JE       @@eof
        CMP      AL,cLF
        JE       @@trueExit
        CMP      AL,cCR
        JNE      @@loop

@@trueExit:
        MOV      AL,1
        JMP      @@exitloop

@@falseExit:
        XOR      AL,AL
@@exitloop:
        DEC      [EBX].TTextRec.BufPos
        JMP      @@exit

@@eof:
        MOV      AL,1
@@exit:
        POP EBX
end;

procedure _SetTextBuf(var t: TTextRec; p: Pointer; size: Longint);
begin
  if size < 0 then
    Error(reRangeError);
  t.BufPtr := P;
  t.BufSize := size;
  t.BufPos := 0;
  t.BufEnd := 0;
end;

procedure _StrLong(val, width: Longint; s: PShortString);
{$IFDEF PUREPASCAL}
var
  I: Integer;
  sign: Longint;
  a: array [0..19] of AnsiChar;
  P: PAnsiChar;
begin
  // U-OK
  sign := val;
  val := Abs(val);
  I := 0;
  repeat
//    a[I] := Chr((val mod 10) + Ord('0'));
    a[I] := AnsiChar((val mod 10) + Ord('0'));
    Inc(I);
    val := val div 10;
  until val = 0;

  if sign < 0 then
  begin
    a[I] := '-';
    Inc(I);
  end;

  if width < I then
    width := I;
  if width > 255 then
    width := 255;
//  s^[0] := Chr(width);
  s^[0] := AnsiChar(width);
  P := @S^[1];
  while width > I do
  begin
    P^ := ' ';
    Inc(P);
    Dec(width);
  end;
  repeat
    Dec(I);
    P^ := a[I];
    Inc(P);
  until I <= 0;
end;
{$ELSE}
asm
{       PROCEDURE _StrLong( val: Longint; width: Longint; VAR s: ShortString );
      ->EAX     Value
        EDX     Width
        ECX     Pointer to string       }

        PUSH    EBX             { VAR i: Longint;               }
        PUSH    ESI             { VAR sign : Longint;           }
        PUSH    EDI
        PUSH    EDX             { store width on the stack      }
        SUB     ESP,20          { VAR a: array [0..19] of Char; }

        MOV     EDI,ECX

        MOV     ESI,EAX         { sign := val                   }

        CDQ                     { val := Abs(val);  canned sequence }
        XOR     EAX,EDX
        SUB     EAX,EDX

        MOV     ECX,10
        XOR     EBX,EBX         { i := 0;                       }

@@repeat1:                      { repeat                        }
        XOR     EDX,EDX         {   a[i] := Chr( val MOD 10 + Ord('0') );}

        DIV     ECX             {   val := val DIV 10;          }

        ADD     EDX,'0'
        MOV     [ESP+EBX],DL
        INC     EBX             {   i := i + 1;                 }
        TEST    EAX,EAX         { until val = 0;                }
        JNZ     @@repeat1

        TEST    ESI,ESI
        JGE     @@2
        MOV     byte ptr [ESP+EBX],'-'
        INC     EBX
@@2:
        MOV     [EDI],BL        { s^++ := Chr(i);               }
        INC     EDI

        MOV     ECX,[ESP+20]    { spaceCnt := width - i;        }
        CMP     ECX,255
        JLE     @@3
        MOV     ECX,255
@@3:
        SUB     ECX,EBX
        JLE     @@repeat2       { for k := 1 to spaceCnt do s^++ := ' ';        }
        ADD     [EDI-1],CL
        MOV     AL,' '
        REP     STOSB

@@repeat2:                      { repeat                        }
        MOV     AL,[ESP+EBX-1]  {   s^ := a[i-1];               }
        MOV     [EDI],AL
        INC     EDI             {   s := s + 1                  }
        DEC     EBX             {   i := i - 1;                 }
        JNZ     @@repeat2       { until i = 0;                  }

        ADD     ESP,20+4
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$ENDIF}

procedure  _Str0Long(val: Longint; s: PShortString);
begin
  _StrLong(val, 0, s);
end;

procedure _Truncate(var f: TFileRec);
{$IFDEF POSIX}
begin
  if (f.Mode and fmOutput) = fmOutput then  // fmOutput or fmInOut
  begin
    if _ftruncate(f.Handle, _lseek(f.Handle, 0, SEEK_CUR)) = -1 then
      InOutError;
  end
  else
    SetInOutRes(103);
end;
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
asm
// -> EAX Pointer to text or file variable

       MOVZX   EDX,[EAX].TFileRec.Mode   // check whether file is open
       SUB     EDX,fmInput
       CMP     EDX,fmInOut-fmInput
       JA      @@fileNotOpen

       PUSH    [EAX].TFileRec.Handle
       CALL    SetEndOfFile
       DEC     EAX
       JZ      @@exit
       JMP     InOutError

@@fileNotOpen:
       MOV     EAX,103
       JMP     SetInOutRes

@@exit:
end;
{$ENDIF MSWINDOWS}

function _ValLong(const s: string; var code: Integer): Longint;
{$IFDEF PUREPASCAL}
var
  I, Len: Integer;
  Negative, Hex: Boolean;
begin
  // U-OK
  I := 1;
  code := -1;
  Result := 0;
  Negative := False;
  Hex := False;
  Len := Length(s);
  while (I <= Len) and (s[I] = ' ') do Inc(I);
  if I > Len then Exit;
  case s[I] of
    '$',
    'x',
    'X': begin
           Hex := True;
           Inc(I);
         end;
    '0': begin
          Hex := (Len > I) and (UpCase(s[I+1]) = 'X');
    if Hex then Inc(I,2);
         end;
    '-': begin
          Negative := True;
          Inc(I);
         end;
    '+': Inc(I);
  end;
  if Hex then
    while I <= Len do
    begin
      if Result > (High(Result) div 16) then
      begin
        code := I;
        Exit;
      end;
      case s[I] of
        '0'..'9': Result := Result * 16 + Ord(s[I]) - Ord('0');
        'a'..'f': Result := Result * 16 + Ord(s[I]) - Ord('a') + 10;
        'A'..'F': Result := Result * 16 + Ord(s[I]) - Ord('A') + 10;
      else
        code := I;
        Exit;
      end;
    end
  else
    while I <= Len do
    begin
      if Result > (High(Result) div 10) then
      begin
        code := I;
        Exit;
      end;
      Result := Result * 10 + Ord(s[I]) - Ord('0');
      Inc(I);
    end;
  if Negative then
    Result := -Result;
  code := 0;
end;
{$ELSE}
asm
{       FUNCTION _ValLong( s: string; VAR code: Integer ) : Longint;        }
{     ->EAX     Pointer to string       }
{       EDX     Pointer to code result  }
{     <-EAX     Result                  }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX
        PUSH    EAX             { save for the error case       }

        TEST    EAX,EAX
        JE      @@empty

        XOR     EAX,EAX
        XOR     EBX,EBX
        MOV     EDI,07FFFFFFFH / 10     { limit }

@@blankLoop:
        {$IFNDEF UNICODE}
        MOV     BL,[ESI]
        INC     ESI
        XOR     BH,BH
        {$ELSE}
        MOV     BX,[ESI]
        ADD     ESI, 2
        {$ENDIF}
        CMP     BX,' '
        JE      @@blankLoop

@@endBlanks:
        MOV     CH,0
        CMP     BX,'-'
        JE      @@minus
        CMP     BX,'+'
        JE      @@plus

@@checkDollar:
        CMP     BX,'$'
        JE      @@dollar

        CMP     BX, 'x'
        JE      @@dollar
        CMP     BX, 'X'
        JE      @@dollar
        CMP     BX, '0'
        JNE     @@firstDigit
        {$IFNDEF UNICODE}
        MOV     BL, [ESI]
        INC     ESI
        XOR     BH,BH
        {$ELSE}
        MOV     BX, [ESI]
        ADD     ESI, 2
        {$ENDIF}
        CMP     BX, 'x'
        JE      @@dollar
        CMP     BX, 'X'
        JE      @@dollar
        TEST    BX, BX
        JE      @@endDigits
        JMP     @@digLoop

@@firstDigit:
        TEST    BX,BX
        JE      @@error

@@digLoop:
        SUB     BX,'0'
        CMP     BX,9
        JA      @@error
        CMP     EAX,EDI         { value > limit ?       }
        JA      @@overFlow
        LEA     EAX,[EAX+EAX*4]
        ADD     EAX,EAX
        ADD     EAX,EBX         { fortunately, we can't have a carry    }
        {$IFNDEF UNICODE}
        MOV     BL,[ESI]
        INC     ESI
        XOR     BH,BH
        {$ELSE}
        MOV     BX,[ESI]
        ADD     ESI, 2
        {$ENDIF}
        TEST    BX,BX
        JNE     @@digLoop

@@endDigits:
        DEC     CH
        JE      @@negate
        TEST    EAX,EAX
        JGE     @@successExit
        JMP     @@overFlow

@@empty:
        {$IFNDEF UNICODE}
        INC     ESI
        {$ELSE}
        ADD     ESI, 2
        {$ENDIF}
        JMP     @@error

@@negate:
        NEG     EAX
        JLE     @@successExit
        JS      @@successExit           { to handle 2**31 correctly, where the negate overflows }

@@error:
@@overFlow:
        POP     EBX
        SUB     ESI,EBX
        JMP     @@exit

@@minus:
        INC     CH
@@plus:
        {$IFNDEF UNICODE}
        MOV     BL,[ESI]
        INC     ESI
        XOR     BH,BH
        {$ELSE}
        MOV     BX,[ESI]
        ADD     ESI, 2
        {$ENDIF}
        JMP     @@checkDollar

@@dollar:
        MOV     EDI,0FFFFFFFH
        {$IFNDEF UNICODE}
        MOV     BL,[ESI]
        INC     ESI
        XOR     BH,BH
        {$ELSE}
        MOV     BX,[ESI]
        ADD     ESI, 2
        {$ENDIF}
        TEST    BX,BX
        JZ      @@empty

@@hDigLoop:
        CMP     BX,'a'
        JB      @@upper
        SUB     BX,'a' - 'A'
@@upper:
        SUB     BX,'0'
        CMP     BX,9
        JBE     @@digOk
        SUB     BX,'A' - '0'
        CMP     BX,5
        JA      @@error
        ADD     BX,10
@@digOk:
        CMP     EAX,EDI
        JA      @@overFlow
        SHL     EAX,4
        ADD     EAX,EBX
        {$IFNDEF UNICODE}
        MOV     BL,[ESI]
        INC     ESI
        XOR     BH,BH
        {$ELSE}
        MOV     BX,[ESI]
        ADD     ESI, 2
        {$ENDIF}
        TEST    BX,BX
        JNE     @@hDigLoop

        DEC     CH
        JNE     @@successExit
        NEG     EAX

@@successExit:
        POP     ECX                     { saved copy of string pointer  }
        XOR     ESI,ESI         { signal no error to caller     }

@@exit:
        {$IFDEF UNICODE}
        SHR     ESI, 1
        {$ENDIF}
        MOV     [EDX],ESI
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$ENDIF}

function _WriteRec(var f: TFileRec; buffer: Pointer): Pointer;
{$IFDEF POSIX}
var
  Dummy: Integer;
begin
  _BlockWrite(f, Buffer, 1, Dummy);
  Result := @F;
end;
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
asm
// -> EAX Pointer to file variable
//  EDX Pointer to buffer
// <- EAX Pointer to file variable
        PUSH    EBX

        MOV     EBX,EAX

        MOVZX   EAX,[EAX].TFileRec.Mode
        SUB     EAX,fmOutput
        CMP     EAX,fmInOut-fmOutput  // File must be fmInOut or fmOutput
        JA      @@fileNotOpen

//  WriteFile(f.Handle, buffer, f.RecSize, @result, Nil);

        PUSH    0                       // space for OS result
        MOV     EAX,ESP

        PUSH    0                       // pass lpOverlapped
        PUSH    EAX                     // pass @result
        PUSH    [EBX].TFileRec.RecSize  // pass nNumberOfBytesToRead
        PUSH    EDX                     // pass lpBuffer
        PUSH    [EBX].TFileRec.Handle   // pass hFile
        CALL    WriteFile
        POP     EDX                     // pop result
        DEC     EAX                     // check EAX = TRUE
        JNZ     @@error

        CMP     EDX,[EBX].TFileRec.RecSize  // result = f.RecSize ?
        JE      @@exit

@@writeError:
        MOV     EAX,101
        JMP     @@errExit

@@fileNotOpen:
        MOV     EAX,5
        JMP     @@errExit

@@error:
        CALL    GetLastError
@@errExit:
        CALL    SetInOutRes
@@exit:
        MOV     EAX,EBX
        POP     EBX
end;
{$ENDIF MSWINDOWS}

// If the file is Output or ErrOutput std variable, try to open it
// Otherwise, runtime error.
function TryOpenForOutput(var t: TTextRec): Boolean;
begin
  if (@t = @Output) or (@t = @ErrOutput) then
  begin
    t.Flags := tfCRLF * Byte(DefaultTextLineBreakStyle);
    _RewritText(t);
  end;

  Result := t.Mode = fmOutput;
  if not Result then
    SetInOutRes(105);
end;

function _WriteBytes(var t: TTextRec; const b; cnt : Longint): Pointer;
{$IFDEF PUREPASCAL}
var
  P: PAnsiChar;
  RemainingBytes: Longint;
  Temp: Integer;
begin
  // U-OK
  Result := @t;
  if (t.Mode <> fmOutput) and not TryOpenForOutput(t) then Exit;

  P := t.BufPtr + t.BufPos;
  RemainingBytes := t.BufSize - t.BufPos;
  while RemainingBytes <= cnt do
  begin
    Inc(t.BufPos, RemainingBytes);
    Dec(cnt, RemainingBytes);
    Move(B, P^, RemainingBytes);
    Temp := TTextIOFunc(t.InOutFunc)(t);
    if Temp <> 0 then
    begin
      SetInOutRes(Temp);
      Exit;
    end;
    P := t.BufPtr + t.BufPos;
    RemainingBytes := t.BufSize - t.BufPos;
  end;
  Inc(t.BufPos, cnt);
  Move(B, P^, cnt);
end;
{$ELSE}
asm
// -> EAX Pointer to file record
//  EDX Pointer to buffer
//  ECX Number of bytes to write
// <- EAX Pointer to file record

        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EDX

        CMP     [EAX].TTextRec.Mode,fmOutput
        JE      @@loop
{$IFDEF ALIGN_STACK}
        PUSH    EAX
        PUSH    EAX
{$ENDIF ALIGN_STACK}
        PUSH    EAX
        PUSH    EDX
        PUSH    ECX
        CALL    TryOpenForOutput
        TEST    AL,AL
        POP     ECX
        POP     EDX
        POP     EAX
{$IFDEF ALIGN_STACK}
        POP     EAX
        POP     EAX
{$ENDIF ALIGN_STACK}
        JE      @@exit

@@loop:
        MOV     EDI,[EAX].TTextRec.BufPtr
        ADD     EDI,[EAX].TTextRec.BufPos

//  remainingBytes = t.bufSize - t.bufPos

        MOV     EDX,[EAX].TTextRec.BufSize
        SUB     EDX,[EAX].TTextRec.BufPos

//  if (remainingBytes <= cnt)

        CMP     EDX,ECX
        JG      @@1

//  t.BufPos += remainingBytes, cnt -= remainingBytes

        ADD     [EAX].TTextRec.BufPos,EDX
        SUB     ECX,EDX

//  copy remainingBytes, advancing ESI

{$IFDEF ALIGN_STACK}
        SUB	ESP, 12
{$ENDIF ALIGN_STACK}
        PUSH    EAX
        PUSH    ECX
        MOV     ECX,EDX
        REP     MOVSB

        CALL    [EAX].TTextRec.InOutFunc
        TEST    EAX,EAX
        JNZ     @@error

        POP     ECX
        POP     EAX
{$IFDEF ALIGN_STACK}
        ADD	ESP, 12
{$ENDIF ALIGN_STACK}
        JMP     @@loop

@@error:
        CALL    SetInOutRes
        POP     ECX
        POP     EAX
        JMP     @@exit
@@1:
        ADD     [EAX].TTextRec.BufPos,ECX
        REP     MOVSB

@@exit:
        POP     EDI
        POP     ESI
end;
{$ENDIF}

function _WriteSpaces(var t: TTextRec; cnt: Longint): Pointer;
{$IFDEF PUREPASCAL}
const
  s64Spaces = '                                                                ';
begin
  Result := @t;
  while cnt > 64 do
  begin
    _WriteBytes(t, s64Spaces, 64);
    if InOutRes <> 0 then Exit;
    Dec(cnt, 64);
  end;
  if cnt > 0 then
    _WriteBytes(t, s64Spaces, cnt);
end;
{$ELSE}
const
  spCnt = 64;
asm
// -> EAX Pointer to text record
//  EDX Number of spaces (<= 0: None)

        MOV     ECX,EDX
@@loop:
{$IFDEF PIC}
        LEA     EDX, [EBX] + offset @@spBuf
{$ELSE}
        MOV     EDX,offset @@spBuf
{$ENDIF}

        CMP     ECX,spCnt
        JLE     @@1

        SUB     ECX,spCnt
        PUSH    EAX
        PUSH    ECX
        MOV     ECX,spCnt
        CALL    _WriteBytes
        CALL    SysInit.@GetTLS
        CMP     [EAX].InOutRes,0
        JNE     @@error
        POP     ECX
        POP     EAX
        JMP     @@loop

@@error:
        POP ECX
        POP EAX
        JMP     @@exit

@@spBuf:  // 64 spaces
        DB '                                                                ';
@@1:
        TEST  ECX,ECX
        JG  _WriteBytes
@@exit:
end;
{$ENDIF}

function _Write0Char(var t: TTextRec; c: AnsiChar): Pointer;
{$IFDEF PUREPASCAL}
var
  Temp: Integer;
begin
  Result := @t;
  if not TryOpenForOutput(t) then Exit;

  if t.BufPos >= t.BufSize then
  begin
    Temp := TTextIOFunc(t.InOutFunc)(t);
    if Temp <> 0 then
    begin
      SetInOutRes(Temp);
      Exit;
    end;
  end;

  t.BufPtr[t.BufPos] := c;
  Inc(t.BufPos);
end;
{$ELSE}
asm
// -> EAX Pointer to text record
//  DL  Character

        CMP     [EAX].TTextRec.Mode,fmOutput
        JE      @@loop
        PUSH    EAX
        PUSH    EDX
        CALL    TryOpenForOutput
        TEST    AL,AL
        POP     EDX
        POP     EAX
        JNE     @@loop
        JMP     @@exit

@@flush:
        PUSH    EAX
        PUSH    EDX
        CALL    [EAX].TTextRec.InOutFunc
        TEST    EAX,EAX
        JNZ     @@error
        POP     EDX
        POP     EAX
        JMP     @@loop

@@error:
        CALL    SetInOutRes
        POP     EDX
        POP     EAX
        JMP     @@exit

@@loop:
        MOV     ECX,[EAX].TTextRec.BufPos
        CMP     ECX,[EAX].TTextRec.BufSize
        JGE     @@flush

        ADD     ECX,[EAX].TTextRec.BufPtr
        MOV     [ECX],DL

        INC     [EAX].TTextRec.BufPos
@@exit:
end;
{$ENDIF}

function _WriteChar(var t: TTextRec; c: AnsiChar; width: Integer): Pointer;
begin
  _WriteSpaces(t, width-1);
  Result := _WriteBytes(t, c, 1);
end;

function _WriteBool(var t: TTextRec; val: Boolean; width: Longint): Pointer;
const
  BoolStrs: array [Boolean] of ShortString = ('FALSE', 'TRUE');
begin
  Result := _WriteString(t, BoolStrs[val], width);
end;

function _Write0Bool(var t: TTextRec; val: Boolean): Pointer;
begin
  Result := _WriteBool(t, val, 0);
end;

function _WriteLong(var t: TTextRec; val, width: Longint): Pointer;
var
  S: string[31];
begin
  Str(val:0, S);
  Result := _WriteString(t, S, width);
end;

function _Write0Long(var t: TTextRec; val: Longint): Pointer;
begin
  Result := _WriteLong(t, val, 0);
end;

function _Write0String(var t: TTextRec; const s: ShortString): Pointer;
begin
  Result := _WriteBytes(t, S[1], Byte(S[0]));
end;

function _WriteString(var t: TTextRec; const s: ShortString; width: Longint): Pointer;
begin
  _WriteSpaces(t, Width - Byte(S[0]));
  Result := _WriteBytes(t, S[1], Byte(S[0]));
end;

function _Write0CString(var t: TTextRec; s: PAnsiChar): Pointer;
begin
  Result := _WriteCString(t, s, 0);
end;

function _WriteCString(var t: TTextRec; s: PAnsiChar; width: Longint): Pointer;
var
  len: Longint;
begin
  {$IFDEF POSIX}
  if Assigned(s) then
    len := _strlen(s)
  else
    len := 0;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  len := _strlenA(s);
  {$ENDIF}
  _WriteSpaces(t, width - len);
  Result := _WriteBytes(t, s^, len);
end;

procedure       _Write2Ext;
asm
{       PROCEDURE _Write2Ext( VAR t: Text; val: Extended; width, prec: Longint);
      ->EAX     Pointer to file record
        [ESP+4] Extended value
        EDX     Field width
        ECX     precision (<0: scientific, >= 0: fixed point)   }

        FLD     tbyte ptr [ESP+4]       { load value    }
        SUB     ESP,256         { VAR s: String;        }

        PUSH    EAX
        PUSH    EDX

{       Str( val, width, prec, s );     }

        SUB     ESP,12
        FSTP    tbyte ptr [ESP] { pass value            }
        MOV     EAX,EDX         { pass field width              }
        MOV     EDX,ECX         { pass precision                }
        LEA     ECX,[ESP+8+12]  { pass destination string       }
        CALL    _Str2Ext

{       Write( t, s, width );   }

        POP     ECX                     { pass width    }
        POP     EAX                     { pass text     }
        MOV     EDX,ESP         { pass string   }
        CALL    _WriteString

        ADD     ESP,256
        RET     12
end;

procedure       _Write1Ext;
asm
{       PROCEDURE _Write1Ext( VAR t: Text; val: Extended; width: Longint);
  ->    EAX     Pointer to file record
        [ESP+4] Extended value
        EDX     Field width             }

        OR      ECX,-1
        JMP     _Write2Ext
end;

procedure       _Write0Ext;
asm
{       PROCEDURE _Write0Ext( VAR t: Text; val: Extended);
      ->EAX     Pointer to file record
        [ESP+4] Extended value  }

        MOV     EDX,23  { field width   }
        OR      ECX,-1
        JMP     _Write2Ext
end;

function _WriteLn(var t: TTextRec): Pointer;
var
  Buf: array [0..1] of AnsiChar;
begin
  if (t.flags and tfCRLF) <> 0 then
  begin
    Buf[0] := AnsiChar(#13);
    Buf[1] := AnsiChar(#10);
    Result := _WriteBytes(t, Buf, 2);
  end
  else
  begin
    Buf[0] := AnsiChar(#10);
    Result := _WriteBytes(t, Buf, 1);
  end;
  _Flush(t);
end;

procedure       __CToPasStr(Dest: PShortString; const Source: PAnsiChar);
begin
  __CLenToPasStr(Dest, Source, 255);
end;

procedure       __CLenToPasStr(Dest: PShortString; const Source: PAnsiChar; MaxLen: Integer);
{$IFDEF PUREPASCAL}
var
  I: Integer;
begin
  I := 0;
  if Source <> nil then
  begin
    if MaxLen > 255 then MaxLen := 255;
    while (Source[I] <> #0) and (I <= MaxLen) do
    begin
      Dest^[I+1] := Source[I];
      Inc(I);
    end;
    if I > 0 then Dec(I);
  end;
  Byte(Dest^[0]) := I;
end;
{$ELSE}
asm
{     ->EAX     Pointer to destination  }
{       EDX     Pointer to source       }
{       ECX     cnt                     }

        PUSH    EBX
        PUSH    EAX             { save destination      }

        TEST    EDX,EDX
        JZ      @@nilStr
        CMP     ECX,255
        JBE     @@loop
        MOV     ECX,255
@@loop:
        MOV     BL,[EDX]        { ch = *src++;          }
        INC     EDX
        TEST    BL,BL   { if (ch == 0) break    }
        JE      @@endLoop
        INC     EAX             { *++dest = ch;         }
        MOV     [EAX],BL
        DEC     ECX             { while (--cnt != 0)    }
        JNZ     @@loop

@@endLoop:
        POP     EDX
        SUB     EAX,EDX

@@setLength:
        MOV     [EDX],AL
        POP     EBX
        RET

@@nilStr:
        POP     EDX
        XOR     EAX,EAX
        JMP     @@setLength
end;
{$ENDIF}

procedure       __ArrayToPasStr(Dest: PShortString; const Source: PAnsiChar; Len: Integer);
begin
  if Len > 255 then Len := 255;
  Byte(Dest^[0]) := Len;
  Move(Source^, Dest^[1], Len);
end;

procedure       __PasToCStr(const Source: PShortString; const Dest: PAnsiChar);
begin
  Move(Source^[1], Dest^, Byte(Source^[0]));
  Dest[Byte(Source^[0])] := #0;
end;

procedure       _SetElem;
asm
        {       PROCEDURE _SetElem( VAR d: SET; elem, size: Byte);      }
        {       EAX     =       dest address                            }
        {       DL      =       element number                          }
        {       CL      =       size of set                                     }

        PUSH    EBX
        PUSH    EDI

        MOV     EDI,EAX

        XOR     EBX,EBX { zero extend set size into ebx }
        MOV     BL,CL
        MOV     ECX,EBX { and use it for the fill       }

        XOR     EAX,EAX { for zero fill                 }
        REP     STOSB

        SUB     EDI,EBX { point edi at beginning of set again   }

        INC     EAX             { eax is still zero - make it 1 }
        MOV     CL,DL
        ROL     AL,CL   { generate a mask               }
        SHR     ECX,3   { generate the index            }
        CMP     ECX,EBX { if index >= siz then exit     }
        JAE     @@exit
        OR      [EDI+ECX],AL{ set bit                   }

@@exit:
        POP     EDI
        POP     EBX
end;

procedure       _SetRange;
asm
{       PROCEDURE _SetRange( lo, hi, size: Byte; VAR d: SET );  }
{ ->AL  low limit of range      }
{       DL      high limit of range     }
{       ECX     Pointer to set          }
{       AH      size of set             }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        XOR     EBX,EBX { EBX = set size                }
        MOV     BL,AH
        MOVZX   ESI,AL  { ESI = low zero extended       }
        MOVZX   EDX,DL  { EDX = high zero extended      }
        MOV     EDI,ECX

{       clear the set                                   }

        MOV     ECX,EBX
        XOR     EAX,EAX
        REP     STOSB

{       prepare for setting the bits                    }

        SUB     EDI,EBX { point EDI at start of set     }
        SHL     EBX,3   { EBX = highest bit in set + 1  }
        CMP     EDX,EBX
        JB      @@inrange
        LEA     EDX,[EBX-1]     { ECX = highest bit in set      }

@@inrange:
        CMP     ESI,EDX { if lo > hi then exit;         }
        JA      @@exit

        DEC     EAX     { loMask = 0xff << (lo & 7)             }
        MOV     ECX,ESI
        AND     CL,07H
        SHL     AL,CL

        SHR     ESI,3   { loIndex = lo >> 3;            }

        MOV     CL,DL   { hiMask = 0xff >> (7 - (hi & 7));      }
        NOT     CL
        AND     CL,07
        SHR     AH,CL

        SHR     EDX,3   { hiIndex = hi >> 3;            }

        ADD     EDI,ESI { point EDI to set[loIndex]     }
        MOV     ECX,EDX
        SUB     ECX,ESI { if ((inxDiff = (hiIndex - loIndex)) == 0)     }
        JNE     @@else

        AND     AL,AH   { set[loIndex] = hiMask & loMask;       }
        MOV     [EDI],AL
        JMP     @@exit

@@else:
        STOSB           { set[loIndex++] = loMask;      }
        DEC     ECX
        MOV     AL,0FFH { while (loIndex < hiIndex)     }
        REP     STOSB   {   set[loIndex++] = 0xff;      }
        MOV     [EDI],AH        { set[hiIndex] = hiMask;        }

@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
end;

procedure       _SetEq;
asm
{       FUNCTION _SetEq( CONST l, r: Set; size: Byte): ConditionCode;   }
{       EAX     =       left operand    }
{       EDX     =       right operand   }
{       CL      =       size of set     }

        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX
        MOV     EDI,EDX

        AND     ECX,0FFH

@@Loop:
        DEC     ECX
        JZ      @@ByteCheck
        MOVZX   EAX,WORD PTR [ESI+ECX-1]
        MOVZX   EDX,WORD PTR [EDI+ECX-1]
        CMP     EAX,EDX
        JNE     @@Leave
        DEC     ECX
        JNZ     @@Loop
@@Leave:

        POP     EDI
        POP     ESI
        RET

@@ByteCheck:
        MOV     AL,[ESI+ECX]
        MOV     DL,[EDI+ECX]
        CMP     AL,DL
        JNE     @@Leave
        OR      ECX,ECX // set zero flag

        POP     EDI
        POP     ESI
        RET
end;

procedure       _SetLe;
asm
{       FUNCTION _SetLe( CONST l, r: Set; size: Byte): ConditionCode;   }
{       EAX     =       left operand            }
{       EDX     =       right operand           }
{       CL      =       size of set (>0 && <= 32)       }

@@loop:
        MOV     CH,[EDX]
        NOT     CH
        AND     CH,[EAX]
        JNE     @@exit
        INC     EDX
        INC     EAX
        DEC     CL
        JNZ     @@loop
@@exit:
end;

procedure       _SetIntersect;
asm
{       PROCEDURE _SetIntersect( VAR dest: Set; CONST src: Set; size: Byte);}
{       EAX     =       destination operand             }
{       EDX     =       source operand                  }
{       CL      =       size of set (0 < size <= 32)    }

@@loop:
        MOV     CH,[EDX]
        INC     EDX
        AND     [EAX],CH
        INC     EAX
        DEC     CL
        JNZ     @@loop
end;

procedure       _SetIntersect3;
asm
{       PROCEDURE _SetIntersect3( VAR dest: Set; CONST src: Set; size: Longint; src2: Set);}
{       EAX     =       destination operand             }
{       EDX     =       source operand                  }
{       ECX     =       size of set (0 < size <= 32)    }
{ [ESP+4] = 2nd source operand    }

        PUSH    EBX
        PUSH    ESI
        MOV     ESI,[ESP+8+4]
@@loop:
        MOV     BL,[EDX+ECX-1]
        AND     BL,[ESI+ECX-1]
        MOV     [EAX+ECX-1],BL
        DEC     ECX
        JNZ     @@loop

        POP     ESI
        POP     EBX
end;

procedure       _SetUnion;
asm
{       PROCEDURE _SetUnion( VAR dest: Set; CONST src: Set; size: Byte);        }
{       EAX     =       destination operand             }
{       EDX     =       source operand                  }
{       CL      =       size of set (0 < size <= 32)    }

@@loop:
        MOV     CH,[EDX]
        INC     EDX
        OR      [EAX],CH
        INC     EAX
        DEC     CL
        JNZ     @@loop
end;

procedure       _SetUnion3;
asm
{       PROCEDURE _SetUnion3( VAR dest: Set; CONST src: Set; size: Longint; src2: Set);}
{       EAX     =       destination operand             }
{       EDX     =       source operand                  }
{       ECX     =       size of set (0 < size <= 32)    }
{ [ESP+4] = 2nd source operand    }

      PUSH  EBX
      PUSH  ESI
      MOV   ESI,[ESP+8+4]
@@loop:
      MOV   BL,[EDX+ECX-1]
      OR    BL,[ESI+ECX-1]
      MOV   [EAX+ECX-1],BL
      DEC   ECX
      JNZ   @@loop

      POP   ESI
      POP   EBX
end;

procedure       _SetSub;
asm
{       PROCEDURE _SetSub( VAR dest: Set; CONST src: Set; size: Byte);  }
{       EAX     =       destination operand             }
{       EDX     =       source operand                  }
{       CL      =       size of set (0 < size <= 32)    }

@@loop:
        MOV     CH,[EDX]
        NOT     CH
        INC     EDX
        AND     [EAX],CH
        INC     EAX
        DEC     CL
        JNZ     @@loop
end;

procedure       _SetSub3;
asm
{       PROCEDURE _SetSub3( VAR dest: Set; CONST src: Set; size: Longint; src2: Set);}
{       EAX     =       destination operand             }
{       EDX     =       source operand                  }
{       ECX     =       size of set (0 < size <= 32)    }
{ [ESP+4] = 2nd source operand    }

        PUSH    EBX
        PUSH    ESI
        MOV     ESI,[ESP+8+4]
@@loop:
        MOV     BL,[ESI+ECX-1]
        NOT     BL
        AND     BL,[EDX+ECX-1]
        MOV     [EAX+ECX-1],BL
        DEC     ECX
        JNZ     @@loop

        POP     ESI
        POP     EBX
end;

procedure       _SetExpand;
asm
{       PROCEDURE _SetExpand( CONST src: Set; VAR dest: Set; lo, hi: Byte);     }
{     ->EAX     Pointer to source (packed set)          }
{       EDX     Pointer to destination (expanded set)   }
{       CH      high byte of source                     }
{       CL      low byte of source                      }

{       algorithm:              }
{       clear low bytes         }
{       copy high-low+1 bytes   }
{       clear 31-high bytes     }

        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX
        MOV     EDI,EDX

        MOV     EDX,ECX { save low, high in dl, dh      }
        XOR     ECX,ECX
        XOR     EAX,EAX

        MOV     CL,DL   { clear low bytes               }
        REP     STOSB

        MOV     CL,DH   { copy high - low bytes }
        SUB     CL,DL
        REP     MOVSB

        MOV     CL,32   { copy 32 - high bytes  }
        SUB     CL,DH
        REP     STOSB

        POP     EDI
        POP     ESI
end;

procedure _EmitDigits;
const
  tenE17: Double = 1e17;
  tenE18: Double = 1e18;
asm
// -> FST(0)  Value, 0 <= value < 10.0
//  EAX Count of digits to generate
//  EDX Pointer to digit buffer

        PUSH    EBX
{$IFDEF PIC}
        PUSH    EAX
        CALL    GetGOT
        MOV     EBX,EAX
        POP     EAX
{$ELSE}
        XOR     EBX,EBX
{$ENDIF}
        PUSH    EDI
        MOV     EDI,EDX
        MOV     ECX,EAX

        SUB     ESP,10      // VAR bcdBuf: array [0..9] of Byte
        MOV     byte ptr [EDI],'0'  // digBuf[0] := '0'//
        FMUL    qword ptr [EBX] + offset tenE17        // val := Round(val*1e17);
        FRNDINT

        FCOM    qword ptr [EBX] + offset tenE18        // if val >= 1e18 then
        FSTSW   AX
        SAHF
        JB      @@1

        FSUB    qword ptr [EBX] + offset tenE18  //   val := val - 1e18;
        MOV     byte ptr [EDI],'1'  //   digBuf[0] := '1';
@@1:
        FBSTP   tbyte ptr [ESP]   // store packed bcd digits in bcdBuf

        MOV     EDX,8
        INC     EDI

@@2:
        WAIT
        MOV     AL,[ESP+EDX]    // unpack 18 bcd digits in 9 bytes
        MOV     AH,AL     // into 9 words = 18 bytes
        SHR     AL,4
        AND     AH,0FH
        ADD     AX,'00'
        STOSW
        DEC     EDX
        JNS     @@2

        SUB     ECX,18      // we need at least digCnt digits
        JL      @@3     // we have generated 18

        MOV     AL,'0'      // if this is not enough, append zeroes
        REP     STOSB
        JMP     @@4     // in this case, we don't need to round

@@3:
        ADD     EDI,ECX     // point EDI to the round digit
        CMP     byte ptr [EDI],'5'
        JL      @@4
@@5:
        DEC     EDI
        INC     byte ptr [EDI]
        CMP     byte ptr [EDI],'9'
        JLE     @@4
        MOV     byte ptr [EDI],'0'
        JMP     @@5

@@4:
        ADD     ESP,10
        POP     EDI
        POP     EBX
end;

procedure _ScaleExt;
asm
// -> FST(0)  Value
// <- EAX exponent (base 10)
//  FST(0)  Value / 10**eax
// PIC safe - uses EBX, but only call is to _POW10, which fixes up EBX itself

        PUSH    EBX
        SUB     ESP,12

        XOR     EBX,EBX

@@normLoop:       // loop necessary for denormals

        FLD     ST(0)
        FSTP    tbyte ptr [ESP]
        MOV     AX,[ESP+8]
        TEST    AX,AX
        JE      @@testZero
@@cont:
        SUB     AX,3FFFH
        MOV     DX,4D10H  // log10(2) * 2**16
        IMUL    DX
        MOVSX   EAX,DX    // exp10 = exp2 * log10(2)
        NEG     EAX
        JE      @@exit
        SUB     EBX,EAX
        CALL    _Pow10
        JMP     @@normLoop

@@testZero:
        CMP     dword ptr [ESP+4],0
        JNE     @@cont
        CMP     dword ptr [ESP+0],0
        JNE     @@cont

@@exit:
        ADD     ESP,12
        MOV     EAX,EBX
        POP     EBX
end;

const
  Ten: Double = 10.0;
  NanStr: String[3] = 'Nan';
  PlusInfStr: String[4] = '+Inf';
  MinInfStr: String[4] = '-Inf';

procedure _Str2Ext;//( val: Extended; width, precision: Longint; var s: String );
const
  MAXDIGS = 256;
asm
// -> [ESP+4] Extended value
//  EAX Width
//  EDX Precision
//  ECX Pointer to string

      FLD     tbyte ptr [ESP+4]

      PUSH    EBX
      PUSH    ESI
      PUSH    EDI
      MOV     EBX,EAX
      MOV     ESI,EDX
      PUSH    ECX     // save string pointer
      SUB     ESP,MAXDIGS             // VAR digBuf: array [0..MAXDIGS-1] of Char

//  limit width to 255

      CMP     EBX,255     // if width > 255 then width := 255;
      JLE     @@1
      MOV     EBX,255
@@1:

//  save sign bit in bit 0 of EDI, take absolute value of val, check for
//  Nan and infinity.

      FLD     ST(0)
      FSTP    tbyte ptr [ESP]
      XOR     EAX,EAX
      MOV     AX,word ptr [ESP+8]
      MOV     EDI,EAX
      SHR     EDI,15
      AND     AX,7FFFH
      CMP     AX,7FFFH
      JE      @@nanInf
      FABS

//  if precision < 0 then do scientific else do fixed;

      TEST    ESI,ESI
      JGE     @@fixed

//  the following call finds a decimal exponent and a reduced
//  mantissa such that val = mant * 10**exp

      CALL    _ScaleExt   // val is FST(0), exp is EAX

//  for scientific notation, we have width - 8 significant digits
//  however, we can not have less than 2 or more than 18 digits.

@@scientific:

      MOV     ESI,EBX     // digCnt := width - 8;
      SUB     ESI,8
      CMP     ESI,2     // if digCnt < 2 then digCnt := 2
      JGE     @@2
      MOV     ESI,2
      JMP     @@3
@@2:
      CMP     ESI,18      // else if digCnt > 18 then digCnt := 18;
      JLE     @@3
      MOV     ESI,18
@@3:

//  _EmitDigits( val, digCnt, digBuf )

      MOV     EDX,ESP     // pass digBuf
      PUSH    EAX     // save exponent
      MOV     EAX,ESI     // pass digCnt
      CALL    _EmitDigits   // convert val to ASCII

      MOV     EDX,EDI     // save sign in EDX
      MOV     EDI,[ESP+MAXDIGS+4] // load result string pointer

      MOV     [EDI],BL    // length of result string := width
      INC     EDI

      MOV     AL,' '      // prepare for leading blanks and sign

      MOV     ECX,EBX     // blankCnt := width - digCnt - 8
      SUB     ECX,ESI
      SUB     ECX,8
      JLE     @@4

      REP     STOSB     // emit blankCnt blanks
@@4:
      SUB     [EDI-1],CL    // if blankCnt < 0, adjust length

      TEST    DL,DL     // emit the sign (' ' or '-')
      JE      @@5
      MOV     AL,'-'
@@5:
      STOSB

      POP     EAX
      MOV     ECX,ESI     // emit digCnt digits
      MOV     ESI,ESP     // point ESI to digBuf

      CMP     byte ptr [ESI],'0'
      JE      @@5a      // if rounding overflowed, adjust exponent and ESI
      INC     EAX
      DEC     ESI
@@5a:
      INC     ESI

      MOVSB   // emit one digit
      MOV     byte ptr [EDI],'.'  // emit dot
      INC     EDI     // adjust dest pointer
      DEC     ECX     // adjust count

      REP     MOVSB

      MOV     byte ptr [EDI],'E'

      MOV     CL,'+'      // emit sign of exponent ('+' or '-')
      TEST    EAX,EAX
      JGE     @@6
      MOV     CL,'-'
      NEG     EAX
@@6:
      MOV     [EDI+1],CL

      XOR     EDX,EDX     // emit exponent
      MOV     CX,10
      DIV     CX
      ADD     DL,'0'
      MOV     [EDI+5],DL

      XOR     EDX,EDX
      DIV     CX
      ADD     DL,'0'
      MOV     [EDI+4],DL

      XOR     EDX,EDX
      DIV     CX
      ADD     DL,'0'
      MOV     [EDI+3],DL

      ADD     AL,'0'
      MOV     [EDI+2],AL

      JMP     @@exit

@@fixed:

//  FST(0)  = value >= 0.0
//  EBX = width
//  ESI = precision
//  EDI = sign

      CMP     ESI,MAXDIGS-40    // else if precision > MAXDIGS-40 then precision := MAXDIGS-40;
      JLE     @@6a
      MOV     ESI,MAXDIGS-40
@@6a:
{$IFDEF PIC}
      PUSH    EAX
      CALL    GetGOT
      FCOM    qword ptr [EAX] + offset Ten
      POP     EAX
{$ELSE}
      FCOM    qword ptr ten
{$ENDIF}
      FSTSW   AX
      SAHF
      MOV     EAX,0
      JB      @@7

      CALL    _ScaleExt   // val is FST(0), exp is EAX

      CMP     EAX,35      // if val is too large, use scientific
      JG      @@scientific

@@7:
//  FST(0)  = scaled value, 0.0 <= value < 10.0
//  EAX = exponent, 0 <= exponent

//  intDigCnt := exponent + 1;

      INC     EAX

//  _EmitDigits( value, intDigCnt + precision, digBuf );

      MOV     EDX,ESP
      PUSH    EAX
      ADD     EAX,ESI
      CALL    _EmitDigits
      POP     EAX

//  Now we need to check whether rounding to the right number of
//  digits overflowed, and if so, adjust things accordingly

      MOV     EDX,ESI     // put precision in EDX
      MOV     ESI,ESP     // point EDI to digBuf
      CMP     byte ptr [ESI],'0'
      JE      @@8
      INC     EAX
      DEC     ESI
@@8:
      INC     ESI

      MOV     ECX,EAX     // numWidth := sign + intDigCnt;
      ADD     ECX,EDI

      TEST    EDX,EDX     // if precision > 0 then
      JE      @@9
      INC     ECX     //   numWidth := numWidth + 1 + precision
      ADD     ECX,EDX

      CMP     EBX,ECX     // if width <= numWidth
      JG      @@9
      MOV     EBX,ECX     //   width := numWidth
@@9:
      PUSH    EAX
      PUSH    EDI

      MOV     EDI,[ESP+MAXDIGS+2*4] // point EDI to dest string

      MOV     [EDI],BL    // store final length in dest string
      INC     EDI

      SUB     EBX,ECX     // width := width - numWidth
      MOV     ECX,EBX
      JLE     @@10

      MOV     AL,' '      // emit width blanks
      REP     STOSB
@@10:
      SUB     [EDI-1],CL    // if blankCnt < 0, adjust length
      POP     EAX
      POP     ECX

      TEST    EAX,EAX
      JE      @@11

      MOV     byte ptr [EDI],'-'
      INC     EDI

@@11:
      REP     MOVSB     // copy intDigCnt digits

      TEST    EDX,EDX     // if precision > 0 then
      JE      @@12

      MOV     byte ptr [EDI],'.'  //   emit '.'
      INC     EDI
      MOV     ECX,EDX     //   emit precision digits
      REP     MOVSB

@@12:

@@exit:
      ADD     ESP,MAXDIGS
      POP     ECX
      POP     EDI
      POP     ESI
      POP     EBX
      RET     12

@@nanInf:
//  here: EBX = width, ECX = string pointer, EDI = sign, [ESP] = value

{$IFDEF PIC}
      CALL    GetGOT
{$ELSE}
      XOR     EAX,EAX
{$ENDIF}
      FSTP    ST(0)
      CMP     dword ptr [ESP+4],80000000H
      LEA     ESI,[EAX] + offset nanStr
      JNE     @@13
      DEC     EDI
      LEA     ESI,[EAX] + offset plusInfStr
      JNZ     @@13
      LEA     ESI,[EAX] + offset minInfStr
@@13:
      MOV     EDI,ECX
      MOV     ECX,EBX
      MOV     [EDI],CL
      INC     EDI
      SUB     CL,[ESI]
      JBE     @@14
      MOV     AL,' '
      REP     STOSB
@@14:
      SUB     [EDI-1],CL
      MOV     CL,[ESI]
      INC     ESI
      REP     MOVSB

      JMP     @@exit
end;

procedure _Str0Ext;
asm
// -> [ESP+4] Extended value
//  EAX Pointer to string

      MOV     ECX,EAX     // pass string
      MOV     EAX,23      // pass default field width
      OR      EDX,-1      // pass precision -1
      JMP     _Str2Ext
end;

procedure _Str1Ext;//( val: Extended; width: Longint; var s: String );
asm
// -> [ESP+4] Extended value
//  EAX Field width
//  EDX Pointer to string

      MOV     ECX,EDX
      OR      EDX,-1      // pass precision -1
      JMP     _Str2Ext
end;

//function _ValExt( s: string; VAR code: Integer ) : Extended;
procedure _ValExt;
asm
// -> EAX Pointer to string
//  EDX Pointer to code result
// <- FST(0)  Result

      PUSH    EBX
{$IFDEF PIC}
      PUSH    EAX
      CALL    GetGOT
      MOV     EBX,EAX
      POP     EAX
{$ELSE}
      XOR     EBX,EBX
{$ENDIF}
      PUSH    ESI
      PUSH    EDI

      PUSH    EBX     // SaveGOT = ESP+8
      MOV     ESI,EAX
      PUSH    EAX     // save for the error case

      FLDZ
      XOR     EAX,EAX
      XOR     EBX,EBX
      XOR     EDI,EDI

      PUSH    EBX     // temp to get digs to fpu

      TEST    ESI,ESI
      JE      @@empty

@@blankLoop:
      {$IFNDEF UNICODE}
      MOV     BL,[ESI]
      INC     ESI
      XOR     BH,BH
      {$ELSE}
      MOV     BX,[ESI]
      ADD     ESI, 2
      {$ENDIF}
      CMP     BX,' '
      JE      @@blankLoop

@@endBlanks:
      MOV     CH,0
      CMP     BX,'-'
      JE      @@minus
      CMP     BX,'+'
      JE      @@plus
      JMP     @@firstDigit

@@minus:
      INC     CH
@@plus:
      {$IFNDEF UNICODE}
      MOV     BL,[ESI]
      INC     ESI
      XOR     BH,BH
      {$ELSE}
      MOV     BX,[ESI]
      ADD     ESI, 2
      {$ENDIF}

@@firstDigit:
      TEST    BX,BX
      JE      @@error

      MOV     EDI,[ESP+8]     // SaveGOT

@@digLoop:
      SUB     BX,'0'
      CMP     BX,9
      JA      @@dotExp
      FMUL    qword ptr [EDI] + offset Ten
      MOV     dword ptr [ESP],EBX
      FIADD   dword ptr [ESP]
      {$IFNDEF UNICODE}
      MOV     BL,[ESI]
      INC     ESI
      XOR     BH,BH
      {$ELSE}
      MOV     BX,[ESI]
      ADD     ESI, 2
      {$ENDIF}
      TEST    BX,BX
      JNE     @@digLoop
      JMP     @@prefinish

@@dotExp:
      CMP     BX,'.' - '0'
      JNE     @@exp
      {$IFNDEF UNICODE}
      MOV     BL,[ESI]
      INC     ESI
      XOR     BH,BH
      {$ELSE}
      MOV     BX,[ESI]
      ADD     ESI, 2
      {$ENDIF}
      TEST    BX,BX
      JE      @@prefinish

//  EDI = SaveGot
@@fracDigLoop:
      SUB     BX,'0'
      CMP     BX,9
      JA      @@exp
      FMUL    qword ptr [EDI] + offset Ten
      MOV     dword ptr [ESP],EBX
      FIADD   dword ptr [ESP]
      DEC     EAX
      {$IFNDEF UNICODE}
      MOV     BL,[ESI]
      INC     ESI
      XOR     BH,BH
      {$ELSE}
      MOV     BX,[ESI]
      ADD     ESI, 2
      {$ENDIF}
      TEST    BX,BX
      JNE     @@fracDigLoop

@@prefinish:
      XOR     EDI,EDI
      JMP     @@finish

@@exp:
      CMP     BX,'E' - '0'
      JE      @@foundExp
      CMP     BX,'e' - '0'
      JNE     @@error
@@foundExp:
      {$IFNDEF UNICODE}
      MOV     BL,[ESI]
      INC     ESI
      XOR     BH,BH
      {$ELSE}
      MOV     BX,[ESI]
      ADD     ESI, 2
      {$ENDIF}
      MOV     AH,0
      CMP     BX,'-'
      JE      @@minusExp
      CMP     BX,'+'
      JE      @@plusExp
      JMP     @@firstExpDigit
@@minusExp:
      INC     AH
@@plusExp:
      {$IFNDEF UNICODE}
      MOV     BL,[ESI]
      INC     ESI
      XOR     BH,BH
      {$ELSE}
      MOV     BX,[ESI]
      ADD     ESI, 2
      {$ENDIF}
@@firstExpDigit:
      SUB     BX,'0'
      CMP     BX,9
      JA      @@error
      MOV     EDI,EBX
      {$IFNDEF UNICODE}
      MOV     BL,[ESI]
      INC     ESI
      XOR     BH,BH
      {$ELSE}
      MOV     BX,[ESI]
      ADD     ESI, 2
      {$ENDIF}
      TEST    BX,BX
      JZ      @@endExp
@@expDigLoop:
      SUB     BX,'0'
      CMP     BX,9
      JA      @@error
      LEA     EDI,[EDI+EDI*4]
      ADD     EDI,EDI
      ADD     EDI,EBX
      {$IFNDEF UNICODE}
      MOV     BL,[ESI]
      INC     ESI
      XOR     BH,BH
      {$ELSE}
      MOV     BX,[ESI]
      ADD     ESI, 2
      {$ENDIF}
      TEST    BX,BX
      JNZ     @@expDigLoop
@@endExp:
      DEC     AH
      JNZ     @@expPositive
      NEG     EDI
@@expPositive:
      MOVSX   EAX,AL

@@finish:
      ADD     EAX,EDI
      PUSH    EDX
      PUSH    ECX
      CALL    _Pow10
      POP     ECX
      POP     EDX

      DEC     CH
      JE      @@negate

@@successExit:

      ADD     ESP,12   // pop temp and saved copy of string pointer

      XOR     ESI,ESI   // signal no error to caller

@@exit:
{$IFDEF UNICODE}
      SHR     ESI,1
{$ENDIF}
      MOV     [EDX],ESI
      POP     EDI
      POP     ESI
      POP     EBX
      RET

@@negate:
      FCHS
      JMP     @@successExit

@@empty:
      {$IFNDEF UNICODE}
      INC     ESI
      {$ELSE}
      ADD     ESI,2
      {$ENDIF}

@@error:
      POP     EAX
      POP     EBX
      SUB     ESI,EBX
      ADD     ESP,4
      JMP     @@exit
end;

procedure FPower10;
asm
  JMP  _Pow10
end;

//function _Pow10(val: Extended; Power: Integer): Extended;
procedure _Pow10;
asm
// -> FST(0)  val
// -> EAX Power
// <- FST(0)  val * 10**Power

//  This routine generates 10**power with no more than two
//  floating point multiplications. Up to 10**31, no multiplications
//  are needed.

  PUSH  EBX
{$IFDEF PIC}
  PUSH  EAX
  CALL  GetGOT
  MOV   EBX,EAX
  POP   EAX
{$ELSE}
  XOR   EBX,EBX
{$ENDIF}
  TEST  EAX,EAX
  JL  @@neg
  JE  @@exit
  CMP EAX,5120
  JGE @@inf
  MOV EDX,EAX
  AND EDX,01FH
  LEA EDX,[EDX+EDX*4]
  FLD tbyte ptr @@tab0[EBX+EDX*2]

  FMULP

  SHR EAX,5
  JE  @@exit

  MOV EDX,EAX
  AND EDX,0FH
  JE  @@skip2ndMul
  LEA EDX,[EDX+EDX*4]
  FLD tbyte ptr @@tab1-10[EBX+EDX*2]
  FMULP

@@skip2ndMul:

  SHR EAX,4
  JE  @@exit
  LEA EAX,[EAX+EAX*4]
  FLD tbyte ptr @@tab2-10[EBX+EAX*2]
  FMULP
  JMP   @@exit

@@neg:
  NEG EAX
  CMP EAX,5120
  JGE @@zero
  MOV EDX,EAX
  AND EDX,01FH
  LEA EDX,[EDX+EDX*4]
  FLD tbyte ptr @@tab0[EBX+EDX*2]
  FDIVP

  SHR EAX,5
  JE  @@exit

  MOV EDX,EAX
  AND EDX,0FH
  JE  @@skip2ndDiv
  LEA EDX,[EDX+EDX*4]
  FLD tbyte ptr @@tab1-10[EBX+EDX*2]
  FDIVP

@@skip2ndDiv:

  SHR EAX,4
  JE  @@exit
  LEA EAX,[EAX+EAX*4]
  FLD tbyte ptr @@tab2-10[EBX+EAX*2]
  FDIVP

  JMP   @@exit

@@inf:
  FSTP ST(0)
  FLD tbyte ptr @@infval[EBX]
  JMP   @@exit

@@zero:
  FSTP ST(0)
  FLDZ

@@exit:
  POP EBX
  RET

@@infval:  DW  $0000,$0000,$0000,$8000,$7FFF
@@tab0:    DW  $0000,$0000,$0000,$8000,$3FFF  // 10**0
           DW  $0000,$0000,$0000,$A000,$4002    // 10**1
           DW  $0000,$0000,$0000,$C800,$4005    // 10**2
           DW  $0000,$0000,$0000,$FA00,$4008        // 10**3
           DW  $0000,$0000,$0000,$9C40,$400C        // 10**4
           DW  $0000,$0000,$0000,$C350,$400F        // 10**5
           DW  $0000,$0000,$0000,$F424,$4012        // 10**6
           DW  $0000,$0000,$8000,$9896,$4016        // 10**7
           DW  $0000,$0000,$2000,$BEBC,$4019        // 10**8
           DW  $0000,$0000,$2800,$EE6B,$401C        // 10**9
           DW  $0000,$0000,$F900,$9502,$4020        // 10**10
           DW  $0000,$0000,$B740,$BA43,$4023        // 10**11
           DW  $0000,$0000,$A510,$E8D4,$4026        // 10**12
           DW  $0000,$0000,$E72A,$9184,$402A        // 10**13
           DW  $0000,$8000,$20F4,$B5E6,$402D        // 10**14
           DW  $0000,$A000,$A931,$E35F,$4030        // 10**15
           DW  $0000,$0400,$C9BF,$8E1B,$4034        // 10**16
           DW  $0000,$C500,$BC2E,$B1A2,$4037        // 10**17
           DW  $0000,$7640,$6B3A,$DE0B,$403A        // 10**18
           DW  $0000,$89E8,$2304,$8AC7,$403E        // 10**19
           DW  $0000,$AC62,$EBC5,$AD78,$4041        // 10**20
           DW  $8000,$177A,$26B7,$D8D7,$4044        // 10**21
           DW  $9000,$6EAC,$7832,$8786,$4048        // 10**22
           DW  $B400,$0A57,$163F,$A968,$404B        // 10**23
           DW  $A100,$CCED,$1BCE,$D3C2,$404E        // 10**24
           DW  $84A0,$4014,$5161,$8459,$4052        // 10**25
           DW  $A5C8,$9019,$A5B9,$A56F,$4055        // 10**26
           DW  $0F3A,$F420,$8F27,$CECB,$4058        // 10**27
           DW  $0984,$F894,$3978,$813F,$405C        // 10**28
           DW  $0BE5,$36B9,$07D7,$A18F,$405F        // 10**29
           DW  $4EDF,$0467,$C9CD,$C9F2,$4062        // 10**30
           DW  $2296,$4581,$7C40,$FC6F,$4065        // 10**31

@@tab1:    DW  $B59E,$2B70,$ADA8,$9DC5,$4069        // 10**32
           DW  $A6D5,$FFCF,$1F49,$C278,$40D3        // 10**64
           DW  $14A3,$C59B,$AB16,$EFB3,$413D        // 10**96
           DW  $8CE0,$80E9,$47C9,$93BA,$41A8        // 10**128
           DW  $17AA,$7FE6,$A12B,$B616,$4212        // 10**160
           DW  $556B,$3927,$F78D,$E070,$427C        // 10**192
           DW  $C930,$E33C,$96FF,$8A52,$42E7        // 10**224
           DW  $DE8E,$9DF9,$EBFB,$AA7E,$4351        // 10**256
           DW  $2F8C,$5C6A,$FC19,$D226,$43BB        // 10**288
           DW  $E376,$F2CC,$2F29,$8184,$4426        // 10**320
           DW  $0AD2,$DB90,$2700,$9FA4,$4490        // 10**352
           DW  $AA17,$AEF8,$E310,$C4C5,$44FA        // 10**384
           DW  $9C59,$E9B0,$9C07,$F28A,$4564        // 10**416
           DW  $F3D4,$EBF7,$4AE1,$957A,$45CF        // 10**448
           DW  $A262,$0795,$D8DC,$B83E,$4639        // 10**480

@@tab2:    DW  $91C7,$A60E,$A0AE,$E319,$46A3        // 10**512
           DW  $0C17,$8175,$7586,$C976,$4D48        // 10**1024
           DW  $A7E4,$3993,$353B,$B2B8,$53ED        // 10**1536
           DW  $5DE5,$C53D,$3B5D,$9E8B,$5A92        // 10**2048
           DW  $F0A6,$20A1,$54C0,$8CA5,$6137        // 10**2560
           DW  $5A8B,$D88B,$5D25,$F989,$67DB        // 10**3072
           DW  $F3F8,$BF27,$C8A2,$DD5D,$6E80        // 10**3584
           DW  $979B,$8A20,$5202,$C460,$7525        // 10**4096
           DW  $59F0,$6ED5,$1162,$AE35,$7BCA        // 10**4608
end;

const
  RealBias = 129;
  ExtBias  = $3FFF;

procedure _Real2Ext;//( val : Real ) : Extended;
asm
// -> EAX Pointer to value
// <- FST(0)  Result

//  the REAL data type has the following format:
//  8 bit exponent (bias 129), 39 bit fraction, 1 bit sign

  MOV DH,[EAX+5]  // isolate the sign bit
  AND DH,80H
  MOV DL,[EAX]  // fetch exponent
  TEST  DL,DL   // exponent zero means number is zero
  JE  @@zero

  ADD DX,ExtBias-RealBias // adjust exponent bias

  PUSH    EDX   // the exponent is at the highest address

  MOV EDX,[EAX+2] // load high fraction part, set hidden bit
  OR  EDX,80000000H
  PUSH  EDX   // push high fraction part

  MOV DL,[EAX+1]  // load remaining low byte of fraction
  SHL EDX,24    // clear low 24 bits
  PUSH  EDX

  FLD tbyte ptr [ESP] // pop result onto chip
  ADD ESP,12

  RET

@@zero:
  FLDZ
  RET
end;

procedure _Ext2Real;//( val : Extended ) : Real;
asm
// -> FST(0)  Value
//  EAX Pointer to result

  PUSH  EBX

  SUB ESP,12
  FSTP  tbyte ptr [ESP]

  POP EBX     // EBX is low half of fraction
  POP EDX     // EDX is high half of fraction
  POP ECX     // CX is exponent and sign

  SHR EBX,24      // set carry to last bit shifted out
  ADC BL,0      // if bit was 1, round up
  ADC EDX,0
  ADC CX,0
  JO  @@overflow

  ADD EDX,EDX     // shift fraction 1 bit left
  ADD CX,CX     // shift sign bit into carry
  RCR EDX,1     // attach sign bit to fraction
  SHR CX,1      // restore exponent, deleting sign

  SUB CX,ExtBias-RealBias // adjust exponent
  JLE @@underflow
  TEST  CH,CH     // CX must be in 1..255
  JG  @@overflow

  MOV [EAX],CL
  MOV [EAX+1],BL
  MOV [EAX+2],EDX

  POP EBX
  RET

@@underflow:
  XOR ECX,ECX
  MOV [EAX],ECX
  MOV [EAX+4],CX
  POP EBX
  RET

@@overflow:
  POP EBX
  MOV AL,8
  JMP Error
end;

const
    ovtInstanceSize = -8;   { Offset of instance size in OBJECTs    }
    ovtVmtPtrOffs   = -4;

procedure       _ObjSetup;
asm
{       FUNCTION _ObjSetup( self: ^OBJECT; vmt: ^VMT): ^OBJECT; }
{     ->EAX     Pointer to self (possibly nil)  }
{       EDX     Pointer to vmt  (possibly nil)  }
{     <-EAX     Pointer to self                 }
{       EDX     <> 0: an object was allocated   }
{       Z-Flag  Set: failure, Cleared: Success  }

        CMP     EDX,1   { is vmt = 0, indicating a call         }
        JAE     @@skip1 { from a constructor?                   }
        RET                     { return immediately with Z-flag cleared        }

@@skip1:
        PUSH    ECX
        TEST    EAX,EAX { is self already allocated?            }
        JNE     @@noAlloc
        MOV     EAX,[EDX].ovtInstanceSize
        TEST    EAX,EAX
        JE      @@zeroSize
        PUSH    EDX
{$IFDEF ALIGN_STACK}
        SUB	ESP, 4
{$ENDIF ALIGN_STACK}
        CALL    _GetMem
{$IFDEF ALIGN_STACK}
        ADD	ESP, 4
{$ENDIF ALIGN_STACK}
        POP     EDX
        TEST    EAX,EAX
        JZ      @@fail

        {       Zero fill the memory }
        PUSH    EDI
        MOV     ECX,[EDX].ovtInstanceSize
        MOV     EDI,EAX
        PUSH    EAX
        XOR     EAX,EAX
        SHR     ECX,2
        REP     STOSD
        MOV     ECX,[EDX].ovtInstanceSize
        AND     ECX,3
        REP     STOSB
        POP     EAX
        POP     EDI

        MOV     ECX,[EDX].ovtVmtPtrOffs
        TEST    ECX,ECX
        JL      @@skip
        MOV     [EAX+ECX],EDX   { store vmt in object at this offset    }
@@skip:
        TEST    EAX,EAX { clear zero flag                               }
        POP     ECX
        RET

@@fail:
        XOR     EDX,EDX
        POP     ECX
        RET

@@zeroSize:
        XOR     EDX,EDX
        CMP     EAX,1   { clear zero flag - we were successful (kind of) }
        POP     ECX
        RET

@@noAlloc:
        MOV     ECX,[EDX].ovtVmtPtrOffs
        TEST    ECX,ECX
        JL      @@exit
        MOV     [EAX+ECX],EDX   { store vmt in object at this offset    }
@@exit:
        XOR     EDX,EDX { clear allocated flag                  }
        TEST    EAX,EAX { clear zero flag                               }
        POP     ECX
end;

procedure       _ObjCopy;
asm
{       PROCEDURE _ObjCopy( dest, src: ^OBJECT; vmtPtrOff: Longint);    }
{     ->EAX     Pointer to destination          }
{       EDX     Pointer to source               }
{       ECX     Offset of vmt in those objects. }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EDX
        MOV     EDI,EAX

        LEA     EAX,[EDI+ECX]   { remember pointer to dest vmt pointer  }
        MOV     EDX,[EAX]       { fetch dest vmt pointer        }

        MOV     EBX,[EDX].ovtInstanceSize

        MOV     ECX,EBX { copy size DIV 4 dwords        }
        SHR     ECX,2
        REP     MOVSD

        MOV     ECX,EBX { copy size MOD 4 bytes }
        AND     ECX,3
        REP     MOVSB

        MOV     [EAX],EDX       { restore dest vmt              }

        POP     EDI
        POP     ESI
        POP     EBX
end;

procedure       _Fail;
asm
{       FUNCTION _Fail( self: ^OBJECT; allocFlag:Longint): ^OBJECT;     }
{     ->EAX     Pointer to self (possibly nil)  }
{       EDX     <> 0: Object must be deallocated        }
{     <-EAX     Nil                                     }

        TEST    EDX,EDX
        JE      @@exit  { if no object was allocated, return    }
        CALL    _FreeMem
@@exit:
        XOR     EAX,EAX
end;

procedure       _FpuInit;
asm
        FNINIT
        FWAIT
{$IFDEF PIC}
        CALL    GetGOT
        MOV     EAX,[EAX].OFFSET Default8087CW
        FLDCW   [EAX]
{$ELSE}
        FLDCW   Default8087CW
{$ENDIF}
end;

procedure       _BoundErr;
asm
        MOV     AL,reRangeError
        JMP     Error
end;

procedure       _IntOver;
asm
        MOV     AL,reIntOverflow
        JMP     Error
end;

function TObject.ClassType: TClass;
begin
  Pointer(Result) := PPointer(Self)^;
end;

class function TObject.ClassName: string;
begin
  Result := UTF8ToString(PShortString(PPointer(Integer(Self) + vmtClassName)^)^);
end;

class function TObject.ClassNameIs(const Name: string): Boolean;
{$IFDEF MSWINDOWS}
var
  LClassName: string;
begin
  LClassName := ClassName;
  Result := CompareString(UTF8CompareLocale, NORM_IGNORECASE, PChar(LClassName),
    Length(LClassName), PChar(Name), Length(Name)) = CSTR_EQUAL;
end;
{$ELSE}
{$IFDEF PUREPASCAL_not_fixed_yet}
var
  Temp: ShortString;
  I: Byte;
begin
  Result := False;
  Temp := ClassName;
  for I := 0 to Byte(Temp[0]) do
    if Temp[I] <> Name[I] then Exit;
  Result := True;
end;
{$ELSE}
asm
        PUSH    EBX
        XOR     EBX,EBX
        OR      EDX,EDX
        JE      @@exit
        MOV     EAX,[EAX].vmtClassName
        XOR     ECX,ECX
        MOV     CL,[EAX]
        CMP     ECX,[EDX-4]
        JNE     @@exit
        DEC     EDX
@@loop:
        MOV     BH,[EAX+ECX]
        XOR     BH,[EDX+ECX]
        AND     BH,0DFH
        JNE     @@exit
        DEC     ECX
        JNE     @@loop
        INC     EBX
@@exit:
        MOV     AL,BL
        POP     EBX
end;
{$ENDIF}
{$ENDIF}

class function TObject.ClassParent: TClass;
{$IFDEF PUREPASCAL}
begin
  Pointer(Result) := PPointer(Integer(Self) + vmtParent)^;
  if Result <> nil then
    Pointer(Result) := PPointer(Result)^;
end;
{$ELSE}
asm
        MOV     EAX,[EAX].vmtParent
        TEST    EAX,EAX
        JE      @@exit
        MOV     EAX,[EAX]
@@exit:
end;
{$ENDIF}

class function TObject.NewInstance: TObject;
begin
  Result := InitInstance(_GetMem(InstanceSize));
end;

procedure TObject.FreeInstance;
begin
  CleanupInstance;
  _FreeMem(Self);
end;

class function TObject.InstanceSize: Longint;
begin
  Result := PInteger(Integer(Self) + vmtInstanceSize)^;
end;

constructor TObject.Create;
begin
end;

destructor TObject.Destroy;
begin
end;

procedure TObject.Free;
begin
  if Self <> nil then
    Destroy;
end;

class function TObject.InitInstance(Instance: Pointer): TObject;
{$IFDEF PUREPASCAL}
var
  IntfTable: PInterfaceTable;
  ClassPtr: TClass;
  I: Integer;
begin
  FillChar(Instance^, InstanceSize, 0);
  PInteger(Instance)^ := Integer(Self);
  ClassPtr := Self;
  while ClassPtr <> nil do
  begin
    IntfTable := ClassPtr.GetInterfaceTable;
    if IntfTable <> nil then
      for I := 0 to IntfTable.EntryCount-1 do
    with IntfTable.Entries[I] do
    begin
      if VTable <> nil then
        PInteger(@PAnsiChar(Instance)[IOffset])^ := Integer(VTable);
    end;
    ClassPtr := ClassPtr.ClassParent;
  end;
  Result := Instance;
end;
{$ELSE}
asm
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EBX,EAX
        MOV     EDI,EDX
        STOSD
        MOV     ECX,[EBX].vmtInstanceSize
        XOR     EAX,EAX
        PUSH    ECX
        SHR     ECX,2
        DEC     ECX
        REP     STOSD
        POP     ECX
        AND     ECX,3
        REP     STOSB
        MOV     EAX,EDX
        MOV     EDX,ESP
@@0:    MOV     ECX,[EBX].vmtIntfTable
        TEST    ECX,ECX
        JE      @@1
        PUSH    ECX
@@1:    MOV     EBX,[EBX].vmtParent
        TEST    EBX,EBX
        JE      @@2
        MOV     EBX,[EBX]
        JMP     @@0
@@2:    CMP     ESP,EDX
        JE      @@5
@@3:    POP     EBX
        MOV     ECX,[EBX].TInterfaceTable.EntryCount
        ADD     EBX,4
@@4:    MOV     ESI,[EBX].TInterfaceEntry.VTable
        TEST    ESI,ESI
        JE      @@4a
        MOV     EDI,[EBX].TInterfaceEntry.IOffset
        MOV     [EAX+EDI],ESI
@@4a:   ADD     EBX,TYPE TInterfaceEntry
        DEC     ECX
        JNE     @@4
        CMP     ESP,EDX
        JNE     @@3
@@5:    POP     EDI
        POP     ESI
        POP     EBX
end;
{$ENDIF}

procedure TObject.CleanupInstance;
{$IFDEF PUREPASCAL}
var
  ClassPtr: TClass;
  InitTable: Pointer;
begin
  ClassPtr := ClassType;
  InitTable := PPointer(Integer(ClassPtr) + vmtInitTable)^;
  while (ClassPtr <> nil) and (InitTable <> nil) do
  begin
    _FinalizeRecord(Self, InitTable);
    ClassPtr := ClassPtr.ClassParent;
    if ClassPtr <> nil then
      InitTable := PPointer(Integer(ClassPtr) + vmtInitTable)^;
  end;
end;
{$ELSE}
asm
        PUSH    EBX
        PUSH    ESI
        MOV     EBX,EAX
        MOV     ESI,EAX
@@loop:
        MOV     ESI,[ESI]
        MOV     EDX,[ESI].vmtInitTable
        MOV     ESI,[ESI].vmtParent
        TEST    EDX,EDX
        JE      @@skip
        CALL    _FinalizeRecord
        MOV     EAX,EBX
@@skip:
        TEST    ESI,ESI
        JNE     @@loop

        MOV     EAX,EBX
        CALL    TMonitor.Destroy;

        POP     ESI
        POP     EBX
end;
{$ENDIF}

function InvokeImplGetter(Self: TObject; ImplGetter: Cardinal): IInterface;
{$IFDEF PUREPASCAL}
var
  M: function: IInterface of object;
begin
  TMethod(M).Data := Self;
  case ImplGetter of
    $FF000000..$FFFFFFFF:  // Field
        Result := IInterface(Pointer(Cardinal(Self) + (ImplGetter and $00FFFFFF)));
    $FE000000..$FEFFFFFF:  // virtual method
      begin
        // sign extend vmt slot offset = smallint cast
        TMethod(M).Code := PPointer(Integer(Self) + SmallInt(ImplGetter))^;
        Result := M;
      end;
  else // static method
    TMethod(M).Code := Pointer(ImplGetter);
    Result := M;
  end;
end;
{$ELSE}
asm
        XCHG    EDX,ECX
        CMP     ECX,$FF000000
        JAE     @@isField
        CMP     ECX,$FE000000
        JB      @@isStaticMethod

        {       the GetProc is a virtual method }
        MOVSX   ECX,CX                  { sign extend slot offs }
        ADD     ECX,[EAX]               { vmt   + slotoffs      }
        JMP     dword ptr [ECX]         { call vmt[slot]        }

@@isStaticMethod:
        JMP     ECX

@@isField:
        AND     ECX,$00FFFFFF
        ADD     ECX,EAX
        MOV     EAX,EDX
        MOV     EDX,[ECX]
        JMP     _IntfCopy
end;
{$ENDIF}

function TObject.Equals(Obj: TObject): Boolean;
begin
  Result := Obj = Self;
end;

function TObject.GetHashCode: Integer;
begin
  Result := Integer(Self);
end;

function TObject.GetInterface(const IID: TGUID; out Obj): Boolean;
var
  InterfaceEntry: PInterfaceEntry;
begin
  Pointer(Obj) := nil;
  InterfaceEntry := GetInterfaceEntry(IID);
  if InterfaceEntry <> nil then
  begin
    if InterfaceEntry^.IOffset <> 0 then
    begin
      Pointer(Obj) := Pointer(Integer(Self) + InterfaceEntry^.IOffset);
      if Pointer(Obj) <> nil then IInterface(Obj)._AddRef;
    end
    else
      IInterface(Obj) := InvokeImplGetter(Self, InterfaceEntry^.ImplGetter);
  end else if (Int64(ObjCastGUID.D1) = Int64(IID.D1)) and
   (Int64(ObjCastGUID.D4) = Int64(IID.D4)) then
    Pointer(Obj) := Self;
  Result := Pointer(Obj) <> nil;
end;

class function TObject.GetInterfaceEntry(const IID: TGUID): PInterfaceEntry;
{$IFDEF PUREPASCAL}
var
  ClassPtr: TClass;
  IntfTable: PInterfaceTable;
  I: Integer;
begin
  ClassPtr := Self;
  while ClassPtr <> nil do
  begin
    IntfTable := ClassPtr.GetInterfaceTable;
    if IntfTable <> nil then
      for I := 0 to IntfTable.EntryCount-1 do
      begin
        Result := @IntfTable.Entries[I];
//        if Result^.IID = IID then Exit;
        if (Int64(Result^.IID.D1) = Int64(IID.D1)) and
           (Int64(Result^.IID.D4) = Int64(IID.D4)) then Exit;
      end;
    ClassPtr := ClassPtr.ClassParent;
  end;
  Result := nil;
end;
{$ELSE}
asm
        PUSH    EBX
        PUSH    ESI
        MOV     EBX,EAX
@@1:    MOV     EAX,[EBX].vmtIntfTable
        TEST    EAX,EAX
        JE      @@4
        MOV     ECX,[EAX].TInterfaceTable.EntryCount
        ADD     EAX,4
@@2:    MOV     ESI,[EDX].Integer[0]
        CMP     ESI,[EAX].TInterfaceEntry.IID.Integer[0]
        JNE     @@3
        MOV     ESI,[EDX].Integer[4]
        CMP     ESI,[EAX].TInterfaceEntry.IID.Integer[4]
        JNE     @@3
        MOV     ESI,[EDX].Integer[8]
        CMP     ESI,[EAX].TInterfaceEntry.IID.Integer[8]
        JNE     @@3
        MOV     ESI,[EDX].Integer[12]
        CMP     ESI,[EAX].TInterfaceEntry.IID.Integer[12]
        JE      @@5
@@3:    ADD     EAX,type TInterfaceEntry
        DEC     ECX
        JNE     @@2
@@4:    MOV     EBX,[EBX].vmtParent
        TEST    EBX,EBX
        JE      @@4a
        MOV     EBX,[EBX]
        JMP     @@1
@@4a:   XOR     EAX,EAX
@@5:    POP     ESI
        POP     EBX
end;
{$ENDIF}

class function TObject.GetInterfaceTable: PInterfaceTable;
begin
  Result := PPointer(Integer(Self) + vmtIntfTable)^;
end;

type
  PClassData = ^TClassData;
  TClassData = record
    ClassType: TClass;
    ParentInfo: Pointer;
    PropCount: SmallInt;
    UnitName: ShortString;
  end;

class function TObject.UnitName: string;
var
  LClassInfo: Pointer;
begin
  LClassInfo := ClassInfo;
  if LClassInfo <> nil then
    Result := UTF8ToString(PClassData(Integer(LClassInfo) + 2 + PByte(Integer(LClassInfo) + 1)^).UnitName)
  else
    Result := '';
end;

function _IsClass(Child: TObject; Parent: TClass): Boolean;
begin
  Result := (Child <> nil) and Child.InheritsFrom(Parent);
end;

function _AsClass(Child: TObject; Parent: TClass): TObject;
{$IFDEF PUREPASCAL}
begin
  Result := Child;
  if not (Child is Parent) then
    Error(reInvalidCast);   // loses return address
end;
{$ELSE}
asm
        { ->    EAX     left operand (class)    }
        {       EDX VMT of right operand        }
        { <-    EAX      if left is derived from right, else runtime error      }
        TEST    EAX,EAX
        JE      @@exit
        MOV     ECX,EAX
@@loop:
        MOV     ECX,[ECX]
        CMP     ECX,EDX
        JE      @@exit
        MOV     ECX,[ECX].vmtParent
        TEST    ECX,ECX
        JNE     @@loop

        {       do runtime error        }
        MOV     AL,reInvalidCast
        JMP     Error

@@exit:
end;
{$ENDIF}

function _IntfAsClass(const Intf: IInterface; Parent: TClass): TObject;
asm
        PUSH    EDX
        PUSH    0
        MOV     EDX,EAX
        LEA     ECX,ObjCastGUID
        MOV     EAX,ESP
        CALL    _IntfCast
        POP     EAX
        POP     EDX
        JMP    _AsClass
end;

function _SafeIntfAsClass(const Intf: IInterface; Parent: TClass): TObject;
begin
  if (Intf <> nil) and (Intf.QueryInterface(ObjCastGUID, Result) = S_OK) and (Result is Parent) then
    Exit;
  Result := nil;
end;

function _IntfIsClass(const Intf: IInterface; Parent: TClass): Boolean;
begin
  Result := _SafeIntfAsClass(Intf, Parent) <> nil;
end;

function _GetHelperDelegate(Instance: TObject; HelperClass: TClass): TObject;
begin
  Result := TClassHelperBaseClass(HelperClass)._Create(Instance);
end;

function _GetHelperIntf(Instance: TObject; HelperClass: TClass): IInterface;
var
  IntfTable: PInterfaceTable;
  P: PInterfaceEntry;
begin
  IntfTable := HelperClass.GetInterfaceTable;
  if IntfTable <> nil then
  begin
    if IntfTable.EntryCount > 0 then
    begin
      P := @IntfTable.Entries[0];
      if Instance.GetInterfaceEntry(P.IID) <> nil then
      begin
        Result := TClassHelperBase(Instance);
        Exit;
      end;
    end;
  end;
  Result := TClassHelperBase(_GetHelperDelegate(Instance, HelperClass));
end;

procedure FindDynaMethod;
{       function        FindDynaMethod(vmt: TClass; selector: Smallint) : Pointer;       }
asm
        { ->    EAX     vmt of class            }
        {       SI      dynamic method index    }
        { <-    ESI pointer to routine  }
        {       ZF = 0 if found         }
        {       trashes: EAX, ECX               }

        PUSH    EDI
        XCHG    EAX,ESI
        JMP     @@haveVMT
@@outerLoop:
        MOV     ESI,[ESI]
@@haveVMT:
        MOV     EDI,[ESI].vmtDynamicTable
        TEST    EDI,EDI
        JE      @@parent
        MOVZX   ECX,word ptr [EDI]
        PUSH    ECX
        ADD     EDI,2
        REPNE   SCASW
        JE      @@found
        POP     ECX
@@parent:
        MOV     ESI,[ESI].vmtParent
        TEST    ESI,ESI
        JNE     @@outerLoop
        JMP     @@exit

@@found:
        POP     EAX
        ADD     EAX,EAX
        SUB     EAX,ECX         { this will always clear the Z-flag ! }
        MOV     ESI,[EDI+EAX*2-4]

@@exit:
        POP     EDI
end;

function GetDynaMethod(vmt: TClass; selector: Smallint): Pointer;
asm
  PUSH ESI

  MOV ESI,EDX
  CALL FindDynaMethod
  JZ @@notFound
  MOV EAX,ESI
  JMP @@exit

@@notFound:
  XOR EAX,EAX

@@exit:
  POP ESI
end;

procedure       _CallDynaInst;
asm
        PUSH    EAX
        PUSH    ECX
        MOV     EAX,[EAX]
        CALL    FindDynaMethod
        POP     ECX
        POP     EAX
        JE      @@Abstract
        JMP     ESI
@@Abstract:
        POP     ECX
        JMP     _AbstractError
end;


procedure       _CallDynaClass;
asm
        PUSH    EAX
        PUSH    ECX
        CALL    FindDynaMethod
        POP     ECX
        POP     EAX
        JE      @@Abstract
        JMP     ESI
@@Abstract:
        POP     ECX
        JMP     _AbstractError
end;


procedure       _FindDynaInst;
asm
        PUSH    ESI
        MOV     ESI,EDX
        MOV     EAX,[EAX]
        CALL    FindDynaMethod
        MOV     EAX,ESI
        POP     ESI
        JNE     @@exit
        POP     ECX
        JMP     _AbstractError
@@exit:
end;


procedure       _FindDynaClass;
asm
        PUSH    ESI
        MOV     ESI,EDX
        CALL    FindDynaMethod
        MOV     EAX,ESI
        POP     ESI
        JNE     @@exit
        POP     ECX
        JMP     _AbstractError
@@exit:
end;

class function TObject.InheritsFrom(AClass: TClass): Boolean;
{$IFDEF PUREPASCAL}
var
  ClassPtr: TClass;
begin
  ClassPtr := Self;
  while (ClassPtr <> nil) and (ClassPtr <> AClass) do
    ClassPtr := PPointer(Integer(ClassPtr) + vmtParent)^;
  Result := ClassPtr = AClass;
end;
{$ELSE}
asm
        { ->    EAX     Pointer to our class    }
        {       EDX     Pointer to AClass               }
        { <-    AL      Boolean result          }
        JMP     @@haveVMT
@@loop:
        MOV     EAX,[EAX]
@@haveVMT:
        CMP     EAX,EDX
        JE      @@success
        MOV     EAX,[EAX].vmtParent
        TEST    EAX,EAX
        JNE     @@loop
        JMP     @@exit
@@success:
        MOV     AL,1
@@exit:
end;
{$ENDIF}


class function TObject.ClassInfo: Pointer;
begin
  Result := PPointer(Integer(Self) + vmtTypeInfo)^;
end;

function TObject.SafeCallException(ExceptObject: TObject;
  ExceptAddr: Pointer): HResult;
begin
  Result := HResult($8000FFFF); { E_UNEXPECTED }
end;

function TObject.ToString: string;
begin
  Result := ClassName;
end;

procedure TObject.DefaultHandler(var Message);
begin
end;


procedure TObject.AfterConstruction;
begin
end;

procedure TObject.BeforeDestruction;
begin
end;

procedure TObject.Dispatch(var Message);
asm
    PUSH    ESI
    MOV     SI,[EDX]
    OR      SI,SI
    JE      @@default
    CMP     SI,0C000H
    JAE     @@default
    PUSH    EAX
    MOV     EAX,[EAX]
    CALL    FindDynaMethod
    POP     EAX
    JE      @@default
    MOV     ECX,ESI
    POP     ESI
    JMP     ECX

@@default:
    POP     ESI
    MOV     ECX,[EAX]
    JMP     DWORD PTR [ECX] + VMTOFFSET TObject.DefaultHandler
end;

function UTF8Compare(const Str1, Str2: ShortString): Boolean;
{$IFDEF MSWINDOWS}
var
  Len1, Len2: Integer;
  LStr1, LStr2: array[0..255] of WideChar;
begin
  Len1 := MultiByteToWideChar(CP_UTF8, 0, @Str1[1], Length(Str1), LStr1, Length(LStr1));
  Len2 := MultiByteToWideChar(CP_UTF8, 0, @Str2[1], Length(Str2), LStr2, Length(LStr2));
  Result := CompareStringW(UTF8CompareLocale, NORM_IGNORECASE, LStr1, Len1, LStr2, Len2) = CSTR_EQUAL;
{$ENDIF MSWINDOWS}
{$IF defined(LINUX) or defined(MACOSX)}
begin
   { MACOSXTODO: UTF8 support }
  Error(reMacNotImplemented);  // not implemented yet
{$IFEND LINUX or MACOSX}
end;

function UTF8ShortStringToString(const Str: ShortString): string;
begin
  Result := UTF8ToString(Str);
end;

class function TObject.MethodAddress(const Name: ShortString): Pointer;
asm
        { ->    EAX     Pointer to class        }
        {       EDX     Pointer to name }
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        XOR     ECX,ECX
        XOR     EDI,EDI
        MOV     BL,[EDX]
        JMP     @@haveVMT
@@outer:                                { upper 16 bits of ECX are 0 !  }
        MOV     EAX,[EAX]
@@haveVMT:
        MOV     ESI,[EAX].vmtMethodTable
        TEST    ESI,ESI
        JE      @@parent
        MOV     DI,[ESI]                { EDI := method count           }
        TEST    EDI,EDI
        JZ      @@parent
        ADD     ESI,2
@@inner:                                { upper 16 bits of ECX are 0 !  }
        MOV     CL,[ESI+6]              { compare length of strings     }
        CMP     CL,BL
        JE      @@cmpChar
@@cont:                                 { upper 16 bits of ECX are 0 !  }
        MOV     CX,[ESI]                { fetch length of method desc   }
        ADD     ESI,ECX                 { point ESI to next method      }
        DEC     EDI
        JNZ     @@inner
@@parent:
        MOV     EAX,[EAX].vmtParent     { fetch parent vmt              }
        TEST    EAX,EAX
        JNE     @@outer
        JMP     @@exit                  { return NIL                    }

@@notEqual:
        MOV     BL,[EDX]                { restore BL to length of name  }
        JMP     @@cont

@@utf8Cmp:
        PUSH    EAX
        PUSH    EDX
        LEA     EAX,[ESI+6]
        CALL    UTF8Compare
        XOR     ECX,ECX
        TEST    AL,AL
        POP     EDX
        POP     EAX
        JZ      @@notEqual
        JMP     @@foundIt

@@cmpChar:                              { upper 16 bits of ECX are 0 !  }
        MOV     CH,0                    { upper 24 bits of ECX are 0 !  }
@@cmpCharLoop:
        MOV     BL,[ESI+ECX+6]          { case insensitive string cmp   }
        TEST    BL,$80
        JNZ     @@utf8Cmp
        XOR     BL,[EDX+ECX+0]          { last char is compared first   }
        TEST    BL,$80
        JNZ     @@utf8Cmp
        AND     BL,$DF
        JNE     @@notEqual
        DEC     ECX                     { ECX serves as counter         }
        JNZ     @@cmpCharLoop

@@foundIt:
        { found it }
        MOV     EAX,[ESI+2]

@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
end;

class function TObject.MethodAddress(const Name: string): Pointer;
begin
{$IFDEF UNICODE}
  Result := MethodAddress(UTF8EncodeToShortString(Name));
{$ELSE}
  Result := MethodAddress(ShortString(Name));
{$ENDIF}
end;

class function TObject.MethodName(Address: Pointer): string;
asm
        { ->    EAX     Pointer to class        }
        {       EDX     Address         }
        {       ECX Pointer to result   }
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EDI,ECX
        XOR     EBX,EBX
        XOR     ECX,ECX
        JMP     @@haveVMT
@@outer:
        MOV     EAX,[EAX]
@@haveVMT:
        MOV     ESI,[EAX].vmtMethodTable { fetch pointer to method table }
        TEST    ESI,ESI
        JE      @@parent
        MOV     CX,[ESI]
        TEST    ECX,ECX
        JZ      @@parent
        ADD     ESI,2
@@inner:
        CMP     EDX,[ESI+2]
        JE      @@found
        MOV     BX,[ESI]
        ADD     ESI,EBX
        DEC     ECX
        JNZ     @@inner
@@parent:
        MOV     EAX,[EAX].vmtParent
        TEST    EAX,EAX
        JNE     @@outer
        LEA     ESI,@@emptyStr
        JMP     @@exit

@@emptyStr:
        DB      0

@@found:
        ADD     ESI,6
@@exit:
        MOV     EAX,ESI
        MOV     EDX,EDI
        POP     EDI
        POP     ESI
        POP     EBX
        JMP     UTF8ShortStringToString
end;


function TObject.FieldAddress(const Name: ShortString): Pointer;
asm
  { ->    EAX     Pointer to instance     }
        {       EDX     Pointer to name }
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        XOR     ECX,ECX
        XOR     EDI,EDI
        MOV     BL,[EDX]

        PUSH    EAX                     { save instance pointer         }

@@outer:
        MOV     EAX,[EAX]               { fetch class pointer           }
        MOV     ESI,[EAX].vmtFieldTable
        TEST    ESI,ESI
        JE      @@parent
        MOV     DI,[ESI]                { fetch count of fields         }
        TEST    EDI,EDI
        JZ      @@parent                { fieldExTab ref only           }
        ADD     ESI,6                   { count:U2 + classTab:P         }
@@inner:
        MOV     CL,[ESI+6]              { compare string lengths        }
        CMP     CL,BL
        JE      @@cmpChar
@@cont:
        LEA     ESI,[ESI+ECX+7] { point ESI to next field       }
        DEC     EDI
        JNZ     @@inner
@@parent:
        MOV     EAX,[EAX].vmtParent     { fetch parent VMT              }
        TEST    EAX,EAX
        JNE     @@outer
        POP     EDX                     { forget instance, return Nil   }
        JMP     @@exit

@@notEqual:
        MOV     BL,[EDX]                { restore BL to length of name  }
        MOV     CL,[ESI+6]              { ECX := length of field name   }
        JMP     @@cont

@@utf8Cmp:
        PUSH    EAX
        PUSH    EDX
        LEA     EAX,[ESI+6]
        CALL    UTF8Compare
        XOR     ECX,ECX
        TEST    AL,AL
        POP     EDX
        POP     EAX
        JZ      @@notEqual
        JMP     @@foundIt

@@cmpChar:
        MOV     BL,[ESI+ECX+6]  { case insensitive string cmp   }
        TEST    BL,$80
        JNZ     @@utf8Cmp
        XOR     BL,[EDX+ECX+0]  { starting with last char       }
        TEST    BL,$80
        JNZ     @@utf8Cmp
        AND     BL,$DF
        JNE     @@notEqual
        DEC     ECX                     { ECX serves as counter         }
        JNZ     @@cmpChar

@@foundIt:
        { found it }
        MOV     EAX,[ESI]           { result is field offset plus ...   }
        POP     EDX
        ADD     EAX,EDX         { instance pointer              }

@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
end;

function TObject.FieldAddress(const Name: string): Pointer;
begin
{$IFDEF UNICODE}
  Result := FieldAddress(UTF8EncodeToShortString(Name));
{$ELSE}
  Result := FieldAddress(ShortString(Name));
{$ENDIF}
end;

function _ClassCreate(AClass: TClass; Alloc: Boolean): TObject;
asm
        { ->    EAX = pointer to VMT      }
        { <-    EAX = pointer to instance }
        PUSH    EDX
        PUSH    ECX
        PUSH    EBX
        TEST    DL,DL
        JL      @@noAlloc
        CALL    DWORD PTR [EAX] + VMTOFFSET TObject.NewInstance
@@noAlloc:
{$IFNDEF PC_MAPPED_EXCEPTIONS}
        XOR     EDX,EDX
        LEA     ECX,[ESP+16]
        MOV     EBX,FS:[EDX]
        MOV     [ECX].TExcFrame.next,EBX
        MOV     [ECX].TExcFrame.hEBP,EBP
        MOV     [ECX].TExcFrame.desc,offset @desc
        MOV     [ECX].TexcFrame.ConstructedObject,EAX   { trick: remember copy to instance }
        MOV     FS:[EDX],ECX
{$ENDIF PC_MAPPED_EXCEPTIONS}
        POP     EBX
        POP     ECX
        POP     EDX
        RET

{$IFNDEF PC_MAPPED_EXCEPTIONS}
@desc:
        JMP     _HandleAnyException

  {       destroy the object                                                      }

        MOV     EAX,[ESP+8+9*4]
        MOV     EAX,[EAX].TExcFrame.ConstructedObject
        TEST    EAX,EAX
        JE      @@skip
        MOV     ECX,[EAX]
        MOV     DL,$81
        PUSH    EAX
        CALL    DWORD PTR [ECX] + VMTOFFSET TObject.Destroy
        POP     EAX
        CALL    _ClassDestroy
@@skip:
  {       reraise the exception   }
        CALL    _RaiseAgain
{$ENDIF}
end;

procedure _ClassDestroy(Instance: TObject);
begin
  Instance.FreeInstance;
end;


function _AfterConstruction(Instance: TObject): TObject;
begin
  try
    Instance.AfterConstruction;
  	Result := Instance;
  except
    _BeforeDestruction(Instance, 1);
    raise;
  end;
end;

function _BeforeDestruction(Instance: TObject; OuterMost: ShortInt): TObject;
// Must preserve DL on return!
{$IFDEF PUREPASCAL}
begin
  Result := Instance;
  if OuterMost > 0 then Exit;
  Instance.BeforeDestruction;
end;
{$ELSE}
asm
       { ->  EAX  = pointer to instance }
       {      DL  = dealloc flag        }

        TEST    DL,DL
        JG      @@outerMost
        RET
@@outerMost:
        PUSH    EAX
        PUSH    EDX
        MOV     EDX,[EAX]
        CALL    DWORD PTR [EDX] + VMTOFFSET TObject.BeforeDestruction
        POP     EDX
        POP     EAX
end;
{$ENDIF}

{ TMonitor }

class procedure TMonitor.CheckMonitorSupport;
begin
  if MonitorSupport = nil then
    Error(reNoMonitorSupport);
end;

function TMonitor.CheckOwningThread: Cardinal;
begin
  Result := FOwningThread;
  if Result <> GetCurrentThreadId then
    Error(reMonitorNotLocked)
end;

class function TMonitor.Create: PMonitor;
begin
  New(Result);
  FillChar(Result^, SizeOf(Result^), 0);
end;

class procedure TMonitor.Destroy(AObject: TObject);
var
  MonitorFld: PPMonitor;
  Monitor: PMonitor;
begin
  MonitorFld := GetFieldAddress(AObject);
  if MonitorFld^ <> nil then
  begin
    Monitor := MonitorFld^;
    MonitorFld^ := nil;
    Monitor.Destroy;
  end;
end;

procedure TMonitor.Destroy;
begin
  if (MonitorSupport <> nil) and (FLockEvent <> nil) then
    MonitorSupport.FreeSyncObject(FLockEvent);
  Dispose(@Self);
end;

class procedure TMonitor.Enter(AObject: TObject);
begin
  CheckMonitorSupport;
  GetMonitor(AObject).Enter(INFINITE);
end;

class function TMonitor.Enter(AObject: TObject; Timeout: Cardinal): Boolean;
begin
  CheckMonitorSupport;
  Result := GetMonitor(AObject).Enter(Timeout);
end;

function TMonitor.DequeueWaiter: PWaitingThread;
begin
  Result := FWaitQueue;
  if (Result = nil) or (Result.Next = Result) then
  begin
    FWaitQueue := nil;
    System.Exit;
  end else
  begin
    Result := FWaitQueue.Next;
    FWaitQueue.Next := FWaitQueue.Next.Next;
  end;
end;

function TMonitor.Enter(Timeout: Cardinal): Boolean;
{$IFDEF MACOSX}
begin
  Error(reMacNotImplemented);  // not implemented yet, because of call to GetTickCount
end;
{$ELSE !MACOSX}
label
  TryAgain;
var
  Done: Boolean;
  LockCount: Integer;
  StartCount, EndCount: Cardinal;
  SpinCount: Integer;
begin
  SpinCount := FSpinCount;
// Return here if signaled and lock wasn't acquired
TryAgain:
  Result := TryEnter;
  if not Result and (Timeout <> 0) then
  begin
    Done := False;
    // Get the spin count
    if SpinCount > 0 then
    begin
      StartCount := GetTickCount;
      while SpinCount > 0 do
      begin
        if (Timeout <> INFINITE) and ((GetTickCount - StartCount) >= Timeout) then
        begin
          Result := False;
          System.Exit;
        end;
        // if there are already waiters, don't bother spinning
        if FLockCount > 1 then
          Break;
        // Try to get the lock
        if FLockCount = 0 then
          if InterlockedCompareExchange(FLockCount, 1, 0) = 0 then
          begin
            FOwningThread := GetCurrentThreadId;
            FRecursionCount := 1;
            Result := True;
            System.Exit;
          end;
        asm
          PAUSE // Just do nothing here...
        end;
        Dec(SpinCount);
        // Keep trying until the spin count expires
      end;
      // Adjust the timeout in case the spin-lock expired above.
      if Timeout <> INFINITE then
      begin
        EndCount := GetTickCount;
        if EndCount - StartCount >= Timeout then
        begin
          Result := False;
          System.Exit;
        end;
        Dec(Timeout, EndCount - StartCount);
      end;
    end;
    // Before we can block, we add our count to the lock
    while True do
    begin
      LockCount := FLockCount;
      if LockCount = 0 then
        goto TryAgain;
      if InterlockedCompareExchange(FLockCount, LockCount + 2, LockCount) = LockCount then
        Break;
    end;
    while True do
    begin
      StartCount := GetTickCount;
      // We're not the owner, so blocking is needed
      // GetEvent does a "safe" allocation of the Event
      Result := MonitorSupport.WaitAndOrSignalObject(nil, GetEvent, Timeout) = WAIT_OBJECT_0;
      if Timeout <> INFINITE then
      begin
        EndCount := GetTickCount;
        if EndCount - StartCount < Timeout then
          Dec(Timeout, EndCount - StartCount)
        else
          Timeout := 0;
      end;
      if Result then
      begin
        // Event was signaled, so try to acquire the lock since this could be a spurious condition
        while True do
        begin
          LockCount := FLockCount;
          if LockCount and 1 <> 0 then
            Break;
          if InterlockedCompareExchange(FLockCount, (LockCount - 2) or 1, LockCount) = LockCount then
          begin
            Done := True;
            Break;
          end;
        end;
      end else
      begin
        // We timed out, remove our presence from the lock count
        repeat
          LockCount := FLockCount;
        until InterlockedCompareExchange(FLockCount, LockCount - 2, LockCount) = LockCount;
        Done := True;
      end;
      if Done then
        Break;
    end;
    if Result then
    begin
      FOwningThread := GetCurrentThreadId;
      FRecursionCount := 1;
    end;
  end;
end;
{$ENDIF !MACOSX}

procedure TMonitor.Exit;
var
  LockCount: Integer;
begin
  CheckOwningThread;
  Dec(FRecursionCount);
  if FRecursionCount = 0 then
  begin
    FOwningThread := 0;
    while True do
    begin
      LockCount := FLockCount;
      if InterlockedCompareExchange(FLockCount, LockCount - 1, LockCount) = LockCount then
      begin
        // if LockCount is <> 0 after we dropped our lock, there were waiters, so signal them
        if LockCount and not 1 <> 0 then
          MonitorSupport.WaitAndOrSignalObject(GetEvent, nil, 0);
        Break;
      end;
    end;
  end;
end;

class procedure TMonitor.Exit(AObject: TObject);
begin
  CheckMonitorSupport;
  GetMonitor(AObject).Exit;
end;

function TMonitor.GetEvent: Pointer;
{$IFDEF MACOSX}
begin
  Error(reMacNotImplemented);  // not implemented yet, because of the call to Sleep below
end;
{$ELSE !MACOSX}
var
  SleepTime: Integer;
  Event: Pointer;
begin
  SleepTime := 1;
  Result := FLockEvent;
  if Result = nil then
    while True do
    begin
      Event := MonitorSupport.NewSyncObject;
      Result := InterlockedCompareExchangePointer(FLockEvent, Event, nil);
      if Result = nil then
        // We won!  Nobody else was trying to allocate the Event.
        Result := Event
      else if Event <> nil then
        // Oh Well. We tried. Close the handle if someone got to it first.
        MonitorSupport.FreeSyncObject(Event);
      // Check if we actually were able to allocate the event without fail
      if Result <> nil then
        System.Exit;
      // We failed to allocate the event, so wait a bit to see if one becomes available
      Sleep(SleepTime);
      // Don't let it run-away, so return to a reasonable value and keep trying
      if SleepTime > 512 then
        SleepTime := 1
      else
        // Next time wait a little longer
        SleepTime := SleepTime shl 1;
    end;
end;
{$ENDIF !MACOSX}

class function TMonitor.GetFieldAddress(AObject: TObject): PPMonitor;
begin
  Result := PPMonitor(Integer(AObject) + AObject.InstanceSize - hfFieldSize + hfMonitorOffset);
end;

class function TMonitor.GetMonitor(AObject: TObject): PMonitor;
var
  MonitorFld: PPMonitor;
  Monitor: PMonitor;
begin
  MonitorFld := GetFieldAddress(AObject);
  Result := MonitorFld^;
  if Result = nil then
  begin
    Monitor := TMonitor.Create;
    Result := InterlockedCompareExchangePointer(Pointer(MonitorFld^), Monitor, nil);
    if Result = nil then
      Result := Monitor
    else
      Dispose(Monitor);
  end;
end;

procedure TMonitor.Pulse;
var
  WaitingThread: PWaitingThread;
begin
  CheckOwningThread;
  WaitingThread := DequeueWaiter;
  if WaitingThread <> nil then
    MonitorSupport.WaitAndOrSignalObject(WaitingThread.WaitEvent, nil, 0);
end;

class procedure TMonitor.Pulse(AObject: TObject);
begin
  CheckMonitorSupport;
  GetMonitor(AObject).Pulse;
end;

procedure TMonitor.PulseAll;
var
  WaitingThread: PWaitingThread;
begin
  CheckOwningThread;
  WaitingThread := DequeueWaiter;
  while WaitingThread <> nil do
  begin
    MonitorSupport.WaitAndOrSignalObject(WaitingThread.WaitEvent, nil, 0);
    WaitingThread := DequeueWaiter;
  end;
end;

class procedure TMonitor.PulseAll(AObject: TObject);
begin
  CheckMonitorSupport;
  GetMonitor(AObject).PulseAll;
end;

procedure TMonitor.QueueWaiter(var WaitingThread: TWaitingThread);
begin
  if FWaitQueue = nil then
  begin
    FWaitQueue := @WaitingThread;
    WaitingThread.Next := @WaitingThread;
  end else
  begin
    WaitingThread.Next := FWaitQueue.Next;
    FWaitQueue.Next := @WaitingThread;
    FWaitQueue := @WaitingThread;
  end;
end;

procedure TMonitor.RemoveWaiter(var WaitingThread: TWaitingThread);
var
  Last, Walker: PWaitingThread;
begin
  if FWaitQueue <> nil then
  begin
    Last := FWaitQueue.Next;
    Walker := Last.Next;
    while Walker <> FWaitQueue do
    begin
      if Walker = @WaitingThread then
      begin
        Last.Next := Walker.Next;
        Break;
      end;
      Last := Walker;
      Walker := Walker.Next;
    end;
    if (Walker = FWaitQueue) and (Walker = @WaitingThread) then
      if Walker.Next = Walker then
        FWaitQueue := nil
      else
        FWaitQueue := Last;
  end;
end;

class procedure TMonitor.SetSpinCount(AObject: TObject; ASpinCount: Integer);
var
  Monitor: PMonitor;
begin
  if CPUCount > 1 then
  begin
    Monitor := GetMonitor(AObject);
    asm
         MOV EAX, Monitor
         LEA EAX, [EAX].TMonitor.FSpinCount
         MOV ECX, ASpinCount
         LOCK XCHG [EAX], ECX
    end;
  end;
end;

class function TMonitor.TryEnter(AObject: TObject): Boolean;
begin
  CheckMonitorSupport;
  Result := GetMonitor(AObject).TryEnter;
end;

function TMonitor.TryEnter: Boolean;
begin
  if FOwningThread = GetCurrentThreadId then  // check for recursion
  begin
    // Only the owning thread can increment this value so no need to guard it
    Inc(FRecursionCount);
    Result := True;
  // check to see if we can gain ownership
  end else if InterlockedCompareExchange(FLockCount, 1, 0) = 0 then
  begin
    //  Yep, got it.  Now claim ownership
    FOwningThread := GetCurrentThreadId;
    FRecursionCount := 1;
    Result := True;
  end else
    Result := False;
end;

function TMonitor.Wait(Timeout: Cardinal): Boolean;
var
  RecursionCount: Integer;
  WaitingThread: TWaitingThread;
begin
  WaitingThread.Next := nil;
  WaitingThread.Thread := CheckOwningThread;
  // This event should probably be cached someplace.
  // Probably not on the instance since this is a per-thread-per-instance resource
  WaitingThread.WaitEvent := MonitorSupport.NewWaitObject;
  try
    // Save the current recursion count for later
    RecursionCount := FRecursionCount;
    // Add the current thread to the waiting queue
    QueueWaiter(WaitingThread);
    // Set it back to almost released so the next Exit call actually drops the lock
    FRecursionCount := 1;
    // Now complete the exit and signal any waiters
    Self.Exit;
    // Get in line for someone to do a Pulse or PulseAll
    Result := MonitorSupport.WaitAndOrSignalObject(nil, WaitingThread.WaitEvent, Timeout) = WAIT_OBJECT_0;
    // Got to get the lock back and block waiting for it.
    Enter(INFINITE);
    // Remove any dangling waiters from the list
    RemoveWaiter(WaitingThread);
    // Lets restore the recursion to return to the proper nesting level
    FRecursionCount := RecursionCount;
  finally
    MonitorSupport.FreeWaitObject(WaitingThread.WaitEvent);
  end;
end;

class function TMonitor.Wait(AObject: TObject; Timeout: Cardinal): Boolean;
begin
  CheckMonitorSupport;
  Result := GetMonitor(AObject).Wait(Timeout);
end;

function MonitorEnter(AObject: TObject; Timeout: Cardinal = INFINITE): Boolean;
begin
  Result := TMonitor.Enter(AObject, Timeout);
end;

function MonitorTryEnter(AObject: TObject): Boolean;
begin
  Result := TMonitor.TryEnter(AObject);
end;

procedure MonitorExit(AObject: TObject);
begin
  TMonitor.Exit(AObject);
end;

function MonitorWait(AObject: TObject; Timeout: Cardinal): Boolean;
begin
  Result := TMonitor.Wait(AObject, Timeout);
end;

procedure MonitorPulse(AObject: TObject);
begin
  TMonitor.Pulse(AObject);
end;

procedure MonitorPulseAll(AObject: TObject);
begin
  TMonitor.PulseAll(AObject);
end;

procedure MemoryBarrier;
asm
      PUSH EAX
      XCHG [ESP],EAX
      POP  EAX
end;

procedure YieldProcessor;
asm
  PAUSE
end;

{
  The following NotifyXXXX routines are used to "raise" special exceptions
  as a signaling mechanism to an interested debugger.  If the debugger sets
  the DebugHook flag to 1 or 2, then all exception processing is tracked by
  raising these special exceptions.  The debugger *MUST* respond to the
  debug event with DBG_CONTINUE so that normal processing will occur.
}
{$IF defined(LINUX) or defined(MACOSX)}
const
  excRaise      = 0; { an exception is being raised by the user (could be a reraise) }
  excCatch      = 1; { an exception is about to be caught }
  excFinally    = 2; { a finally block is about to be executed because of an exception }
  excUnhandled  = 3; { no user exception handler was found (the app will die) }

procedure _DbgExcNotify(
  NotificationKind: Integer;
  ExceptionObject: Pointer;
  ExceptionName: PShortString;
  ExceptionLocation: Pointer;
  HandlerAddr: Pointer); cdecl; export;
begin
{$IFDEF DEBUG}
  {
    This code is just for debugging the exception handling system.  The debugger
    needs _DbgExcNotify, however to place breakpoints in, so the function itself
    cannot be removed.
  }
  asm
    PUSH EAX
    PUSH EDX
  end;
  if Assigned(ExcNotificationProc) then
    ExcNotificationProc(NotificationKind, ExceptionObject, ExceptionName, ExceptionLocation, HandlerAddr);
  asm
    POP EDX
    POP EAX
  end;
{$ENDIF DEBUG}
end;

{
  The following functions are used by the debugger for the evaluator.  If you
  change them IN ANY WAY, the debugger will cease to function correctly.
}
procedure _DbgEvalMarker;
begin
end;

procedure _DbgEvalExcept(E: TObject);
begin
end;

procedure _DbgEvalEnd;
begin
end;

{
  This function is used by the debugger to provide a soft landing spot
  when evaluating a function call that may raise an unhandled exception.
  The return address of _DbgEvalMarker is pushed onto the stack so that
  the unwinder will transfer control to the except block.
}
procedure _DbgEvalFrame;
begin
  try
    _DbgEvalMarker;
  except on E: TObject do
    _DbgEvalExcept(E);
  end;
  _DbgEvalEnd;
end;

{
  These export names need to match the names that will be generated into
  the .symtab section, so that the debugger can find them if stabs
  debug information is being generated.
}
exports
  _DbgExcNotify   name  '@DbgExcNotify',
  _DbgEvalFrame   name  '@DbgEvalFrame',
  _DbgEvalMarker  name  '@DbgEvalMarker',
  _DbgEvalExcept  name  '@DbgEvalExcept',
  _DbgEvalEnd     name  '@DbgEvalEnd';
{$IFEND LINUX or MACOSX}

{ tell the debugger that the next raise is a re-raise of the current non-Delphi
  exception }
procedure       NotifyReRaise;
asm
{$IF defined(LINUX) or defined(MACOSX)}
{     ->EAX     Pointer to exception object }
{       EDX     location of exception       }
        PUSH    0                   { handler addr }
        PUSH    EDX                 { location of exception }
        MOV     ECX, [EAX]
        PUSH    [ECX].vmtClassName  { exception name }
        PUSH    EAX                 { exception object }
        PUSH    excRaise            { notification kind }
        CALL    _DbgExcNotify
        ADD     ESP, 20
{$ELSE !(LINUX or MACOSX)}
        CMP     BYTE PTR DebugHook,1
        JBE     @@1
        PUSH    0
        PUSH    0
        PUSH    cContinuable
        PUSH    cDelphiReRaise
        CALL    RaiseExceptionProc
@@1:
{$IFEND !(LINUX or MACOSX)}
end;

{ tell the debugger about the raise of a non-Delphi exception }
{$IFDEF MSWINDOWS}
procedure       NotifyNonDelphiException;
asm
        CMP     BYTE PTR DebugHook,0
        JE      @@1
        PUSH    EAX
        PUSH    EAX
        PUSH    EDX
        PUSH    ESP
        PUSH    2
        PUSH    cContinuable
        PUSH    cNonDelphiException
        CALL    RaiseExceptionProc
        ADD     ESP,8
        POP     EAX
@@1:
end;
{$ENDIF}

{ Tell the debugger where the handler for the current exception is located }
procedure       NotifyExcept;
asm
{$IF defined(LINUX) or defined(MACOSX)}
{     ->EAX     Pointer to exception object }
{       EDX     handler addr                }
        PUSH    EAX
        MOV     EAX, [EAX].TRaisedException.ExceptObject

        PUSH    EDX                 { handler addr }
        PUSH    0                   { location of exception }
        MOV     ECX, [EAX]
        PUSH    [ECX].vmtClassName  { exception name }
        PUSH    EAX                 { exception object }
        PUSH    excCatch            { notification kind }
        CALL    _DbgExcNotify
        ADD     ESP, 20

        POP     EAX
{$ELSE !(LINUX or MACOSX)}
        PUSH    ESP
        PUSH    1
        PUSH    cContinuable
        PUSH    cDelphiExcept           { our magic exception code }
        CALL    RaiseExceptionProc
        ADD     ESP,4
        POP     EAX
{$IFEND !(LINUX or MACOSX)}
end;

procedure       NotifyOnExcept;
asm
{$IF defined(LINUX) or defined(MACOSX)}
{     ->EAX     Pointer to exception object }
{       EDX     handler addr                }
        PUSH    EDX                 { handler addr }
        PUSH    0                   { location of exception }
        MOV     ECX, [EAX]
        PUSH    [ECX].vmtClassName  { exception name }
        PUSH    EAX                 { exception object }
        PUSH    excCatch            { notification kind }
        CALL    _DbgExcNotify
        ADD     ESP, 20
{$ELSE !(LINUX or MACOSX)}
        CMP     BYTE PTR DebugHook,1
        JBE     @@1
        PUSH    EAX
        PUSH    [EBX].TExcDescEntry.handler
        JMP     NotifyExcept
@@1:
{$IFEND !(LINUX or MACOSX)}
end;

{$IFDEF MSWINDOWS}
procedure       NotifyAnyExcept;
asm
        CMP     BYTE PTR DebugHook,1
        JBE     @@1
        PUSH    EAX
        PUSH    EBX
        JMP     NotifyExcept
@@1:
end;

procedure       CheckJmp;
asm
        TEST    ECX,ECX
        JE      @@3
        MOV     EAX,[ECX + 1]
        CMP     BYTE PTR [ECX],0E9H { near jmp }
        JE      @@1
        CMP     BYTE PTR [ECX],0EBH { short jmp }
        JNE     @@3
        MOVSX   EAX,AL
        INC     ECX
        INC     ECX
        JMP     @@2
@@1:
        ADD     ECX,5
@@2:
        ADD     ECX,EAX
@@3:
end;
{$ENDIF MSWINDOWS}

{ Notify debugger of a finally during an exception unwind }
procedure       NotifyExceptFinally;
asm
{$IF defined(LINUX) or defined(MACOSX)}
{     ->EAX     Pointer to exception object }
{       EDX     handler addr                }
        PUSH    EDX                 { handler addr }
        PUSH    0                   { location of exception }
        PUSH    0                   { exception name }
        PUSH    0                   { exception object }
        PUSH    excFinally          { notification kind }
        CALL    _DbgExcNotify
        ADD     ESP, 20
{$ELSE !(LINUX or MACOSX)}
        CMP     BYTE PTR DebugHook,1
        JBE     @@1
        PUSH    EAX
        PUSH    EDX
        PUSH    ECX
        CALL    CheckJmp
        PUSH    ECX
        PUSH    ESP                     { pass pointer to arguments }
        PUSH    1                       { there is 1 argument }
        PUSH    cContinuable            { continuable execution }
        PUSH    cDelphiFinally          { our magic exception code }
        CALL    RaiseExceptionProc
        POP     ECX
        POP     ECX
        POP     EDX
        POP     EAX
@@1:
{$IFEND !(LINUX or MACOSX)}
end;


{ Tell the debugger that the current exception is handled and cleaned up.
  Also indicate where execution is about to resume. }
{$IFDEF MSWINDOWS}
procedure       NotifyTerminate;
asm
        CMP     BYTE PTR DebugHook,1
        JBE     @@1
        PUSH    EDX
        PUSH    ESP
        PUSH    1
        PUSH    cContinuable
        PUSH    cDelphiTerminate        { our magic exception code }
        CALL    RaiseExceptionProc
        POP     EDX
@@1:
end;
{$ENDIF MSWINDOWS}

{ Tell the debugger that there was no handler found for the current exception
  and we are about to go to the default handler }
procedure       NotifyUnhandled;
asm
{$IF defined(LINUX) or defined(MACOSX)}
{     ->EAX     Pointer to exception object }
{       EDX     location of exception       }
        PUSH    EAX
        MOV     EAX, [EAX].TRaisedException.ExceptObject

        PUSH    0                   { handler addr }
        PUSH    EDX                 { location of exception }
        MOV     ECX, [EAX]
        PUSH    [ECX].vmtClassName  { exception name }
        PUSH    EAX                 { exception object }
        PUSH    excUnhandled        { notification kind }
        CALL    _DbgExcNotify
        ADD     ESP, 20

        POP     EAX
{$ELSE !(LINUX or MACOSX)}
        PUSH    EAX
        PUSH    EDX
        CMP     BYTE PTR DebugHook,1
        JBE     @@1
        PUSH    ESP
        PUSH    2
        PUSH    cContinuable
        PUSH    cDelphiUnhandled
        CALL    RaiseExceptionProc
@@1:
        POP     EDX
        POP     EAX
{$IFEND !(LINUX or MACOSX)}
end;

{$IFDEF PC_MAPPED_EXCEPTIONS}
//  MaybeCooptException
//    If a Delphi exception is thrown from C++, a TRaisedException object
//    will not be allocated yet on this side.  We need to keep things sane,
//    so we have to intercept such exceptions from the C++ side, and convert
//    them so that they appear to have been thrown from this RTL.  If we
//    throw a Delphi exception, then we set the private_2 member of
//    _Unwind_Exception to 0.  If C++ throws it, it sets it to the address
//    of the throw point.  We use this to distinguish the two cases, and
//    adjust data structures as appropriate.  On entry to this function,
//    EDX is the private_2 member, as set from SysRaiseException, and
//    EAX is the exception object in question.
//
procedure MaybeCooptException;
asm
        // If this exception is from C++, then private_2 will be a
        // throw address.  If not, then it will be zero.  private_1
        // will be either the exception object itself, or a TRaisedException.
        OR      EDX, EDX            // From C++?
        JZ      @@ExcAllocated

        // We've decided that the exception is from C++, but it is a
        // Delphi exception object.  We will coopt the exception now
        // by installing a TRaisedException into the unwinder exception,
        // and setting private_2 to 0.  Then the exception will look
        // like it was truly thrown from this RTL.
        CALL    AllocateException

@@ExcAllocated:
end;

function LinkException(Exc: PRaisedException): PRaisedException;
asm
        PUSH  EDX     // preserve EDX because of HandleOnException
        PUSH    EAX
{$IFDEF ALIGN_STACK}
        SUB	ESP, 4
{$ENDIF ALIGN_STACK}
        CALL    SysInit.@GetTLS
{$IFDEF ALIGN_STACK}
        ADD	ESP, 4
{$ENDIF ALIGN_STACK}
        POP     EDX
        MOV   ECX, [EAX].ExceptionList
        MOV     [EDX].TRaisedException.Prev, ECX
        MOV     [EAX].ExceptionList, EDX
        MOV     EAX, EDX
        POP   EDX
end;

function UnlinkException: PRaisedException;
asm
        CALL    SysInit.@GetTLS
        MOV     EDX, [EAX].ExceptionList
        MOV     EDX, [EDX].TRaisedException.Prev
        MOV     [EAX].ExceptionList, EDX
end;
{$ENDIF PC_MAPPED_EXCEPTIONS}

{$IFDEF MSWINDOWS}
procedure _HandleFinallyInternal; forward;
{$ENDIF MSWINDOWS}

procedure       _HandleAnyException;
asm
{$IFDEF PC_MAPPED_EXCEPTIONS}
        CMP     ECX, UW_EXC_CLASS_BORLANDCPP    // C++ exception?
        JNE     @@handleIt                      // nope, handle it
        // C++ exceptions aren't wanted here.  Retoss them as is
        CALL    SysRaiseCPPException

@@handleIt:
        PUSH    EAX
        PUSH    EDX
        CALL    UnblockOSExceptions
        POP     EDX
        POP     EAX

        // If the exception is a Delphi exception thrown from C++, coopt it.
        CALL    MaybeCooptException

        OR      [EAX].TRaisedException.Flags, excIsBeingHandled
        CALL    LinkException
        MOV     ESI, EBX
        MOV     EDX, [ESP]
        CALL    NotifyExcept
        MOV     EBX, ESI
{$ENDIF PC_MAPPED_EXCEPTIONS}
{$IFNDEF  PC_MAPPED_EXCEPTIONS}
        { ->    [ESP+ 4] excPtr: PExceptionRecord       }
        {       [ESP+ 8] errPtr: PExcFrame              }
        {       [ESP+12] ctxPtr: Pointer                }
        {       [ESP+16] dspPtr: Pointer                }
        { <-    EAX return value - always one   }

        MOV     EAX,[ESP+4]
        TEST    [EAX].TExceptionRecord.ExceptionFlags,cUnwindInProgress
        JNE     @@exit

        CMP     [EAX].TExceptionRecord.ExceptionCode,cDelphiException
        MOV     EDX,[EAX].TExceptionRecord.ExceptObject
        MOV     ECX,[EAX].TExceptionRecord.ExceptAddr
        JE      @@DelphiException
        CLD
        CALL    _FpuInit
        MOV     EDX,ExceptObjProc
        TEST    EDX,EDX
        JE      @@exit
        CALL    EDX
        TEST    EAX,EAX
        JE      @@exit
        MOV     EDX,[ESP+12]
        MOV     ECX,[ESP+4]
        CMP     [ECX].TExceptionRecord.ExceptionCode,cCppException
        JE      @@CppException
        CALL    NotifyNonDelphiException
{$IFDEF MSWINDOWS}
        CMP     BYTE PTR JITEnable,0
        JBE     @@CppException
        CMP     BYTE PTR DebugHook,0
        JA      @@CppException                     // Do not JIT if debugging
        LEA     ECX,[ESP+4]
        PUSH    EAX
        PUSH    ECX
        CALL    UnhandledExceptionFilter
        CMP     EAX,EXCEPTION_CONTINUE_SEARCH
        POP     EAX
        JE      @@exit
        MOV     EDX,EAX
        MOV     EAX,[ESP+4]
        MOV     ECX,[EAX].TExceptionRecord.ExceptionAddress
        JMP     @@GoUnwind
{$ENDIF MSWINDOWS}
@@CppException:
        MOV     EDX,EAX
        MOV     EAX,[ESP+4]
        MOV     ECX,[EAX].TExceptionRecord.ExceptionAddress

@@DelphiException:
{$IFDEF MSWINDOWS}
        CMP     BYTE PTR JITEnable,1
        JBE     @@GoUnwind
        CMP     BYTE PTR DebugHook,0                { Do not JIT if debugging }
        JA      @@GoUnwind
        PUSH    EAX
        LEA     EAX,[ESP+8]
        PUSH    EDX
        PUSH    ECX
        PUSH    EAX
        CALL    UnhandledExceptionFilter
        CMP     EAX,EXCEPTION_CONTINUE_SEARCH
        POP     ECX
        POP     EDX
        POP     EAX
        JE      @@exit
{$ENDIF MSWINDOWS}

@@GoUnwind:
        OR      [EAX].TExceptionRecord.ExceptionFlags,cUnwinding

        PUSH    EBX
        XOR     EBX,EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        MOV     EBX,FS:[EBX]
        PUSH    EBX                     { Save pointer to topmost frame }
        PUSH    EAX                     { Save OS exception pointer     }
        PUSH    EDX                     { Save exception object         }
        PUSH    ECX                     { Save exception address        }

        MOV     EDX,[ESP+8+8*4]

        PUSH    0
        PUSH    EAX
        PUSH    offset @@returnAddress
        PUSH    EDX
        CALL    RtlUnwindProc
@@returnAddress:

        MOV     EDI,[ESP+8+8*4]

        {       Make the RaiseList entry on the stack }

        CALL    SysInit.@GetTLS
        PUSH    [EAX].RaiseListPtr
        MOV     [EAX].RaiseListPtr,ESP

        MOV     EBP,[EDI].TExcFrame.hEBP
        MOV     EBX,[EDI].TExcFrame.desc
        MOV     [EDI].TExcFrame.desc,offset @@exceptFinally

        ADD     EBX,TExcDesc.instructions
        CALL    NotifyAnyExcept
        JMP     EBX

@@exceptFinally:
        JMP     _HandleFinallyInternal

@@destroyExcept:
        {       we come here if an exception handler has thrown yet another exception }
        {       we need to destroy the exception object and pop the raise list. }

        CALL    SysInit.@GetTLS
        MOV     ECX,[EAX].RaiseListPtr
        MOV     EDX,[ECX].TRaiseFrame.NextRaise
        MOV     [EAX].RaiseListPtr,EDX

        MOV     EAX,[ECX].TRaiseFrame.ExceptObject
        JMP     TObject.Free

@@exit:
        MOV     EAX,1
{$ENDIF !PC_MAPPED_EXCEPTIONS}  { not PC_MAPPED_EXCEPTIONS }
end;

{$IFDEF PC_MAPPED_EXCEPTIONS}
{
  Common code between the Win32 and PC mapped exception handling
  scheme.  This function takes a pointer to an object, and an exception
  'on' descriptor table and finds the matching handler descriptor.

  For support of Linux, we assume that EBX has been loaded with the GOT
  that pertains to the code which is handling the exception currently.
  If this function is being called from code which is not PIC, then
  EBX should be zero on entry.
}
procedure FindOnExceptionDescEntry;
asm
        { ->    EAX raised object: Pointer                }
        {       EDX descriptor table: ^TExcDesc           }
        {       EBX GOT of user code, or 0 if not an SO   }
        { <-    EAX matching descriptor: ^TExcDescEntry   }
        PUSH    EBP
        MOV     EBP, ESP
        SUB     ESP, 8                          { Room for vtable temp, and adjustor }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV [EBP - 8], EBX      { Store the potential GOT }
        MOV EAX, [EAX]          { load vtable of exception object }
        MOV     EBX,[EDX].TExcDesc.cnt
        LEA     ESI,[EDX].TExcDesc.excTab       { point ECX to exc descriptor table }
        MOV     [EBP - 4], EAX                  { temp for vtable of exception object }

@@innerLoop:
        MOV     EAX,[ESI].TExcDescEntry.vTable
        TEST    EAX,EAX                         { catch all clause?                     }
        JE      @@found                         { yes: This is the handler              }
        ADD     EAX, [EBP - 8]                  { add in the adjustor (could be 0) }
        MOV     EDI,[EBP - 4]                   { load vtable of exception object       }
        JMP     @@haveVMT

@@vtLoop:
        MOV     EDI,[EDI]
@@haveVMT:
        MOV     EAX,[EAX]
        CMP     EAX,EDI
        JE      @@found

        MOV     ECX,[EAX].vmtInstanceSize
        CMP     ECX,[EDI].vmtInstanceSize
        JNE     @@parent

        MOV     EAX,[EAX].vmtClassName
        MOV     EDX,[EDI].vmtClassName

        XOR     ECX,ECX
        MOV     CL,[EAX]
        CMP     CL,[EDX]
        JNE     @@parent

        INC     EAX
        INC     EDX
        CALL    _AStrCmp
        JE      @@found

@@parent:
        MOV     EDI,[EDI].vmtParent             { load vtable of parent         }
        MOV     EAX,[ESI].TExcDescEntry.vTable
        ADD     EAX, [EBP - 8]                  { add in the adjustor (could be 0) }
        TEST    EDI,EDI
        JNE     @@vtLoop

        ADD     ESI,8
        DEC     EBX
        JNZ     @@innerLoop

        { Didn't find a handler. }
        XOR     ESI, ESI

@@found:
        MOV     EAX, ESI
@@done:
        POP     EDI
        POP     ESI
        POP     EBX
        MOV     ESP, EBP
        POP     EBP
end;
{$ENDIF PC_MAPPED_EXCEPTIONS}

{$IFDEF PC_MAPPED_EXCEPTIONS}
procedure       _HandleOnExceptionPIC;
asm
        { ->    EAX obj : Exception object }
        {       [RA]  desc: ^TExcDesc }
        { <-    Doesn't return }

        // Mark the exception as being handled
        OR      [EAX].TRaisedException.Flags, excIsBeingHandled

        MOV     ESI, EBX                      // Save the GOT
        MOV     EDX, [ESP]                    // Get the addr of the TExcDesc
        PUSH    EAX                           // Save the object
        MOV     EAX, [EAX].TRaisedException.ExceptObject
        CALL    FindOnExceptionDescEntry
        OR      EAX, EAX
        JE      @@NotForMe

        MOV     EBX, ESI                      // Set back to user's GOT
        MOV     EDX, EAX
        POP     EAX                           // Get the object back
        POP     ECX                           // Ditch the return addr

        CALL    LinkException

        // Get the Pascal object itself.
        MOV     EAX, [EAX].TRaisedException.ExceptObject

        MOV     EDX, [EDX].TExcDescEntry.handler
        ADD     EDX, EBX                      // adjust for GOT
        CALL    NotifyOnExcept

        MOV     EBX, ESI                      // Make sure of user's GOT
        JMP     EDX                           // Back to the user code
        // never returns
@@NotForMe:
        POP     EAX                           // Get the exception object

        // Mark that we're reraising this exception, so that the
        // compiler generated exception handler for the 'except on' clause
        // will not get confused
        OR      [EAX].TRaisedException.Flags, excIsBeingReRaised

        JMP     SysRaiseException             // Should be using resume here
end;
{$ENDIF PC_MAPPED_EXCEPTIONS}

procedure       _HandleOnException;
{$IFDEF PC_MAPPED_EXCEPTIONS}
asm
        { ->    EAX obj : Exception object }
        {       [RA]  desc: ^TExcDesc }
        { <-    Doesn't return }

        CMP     ECX, UW_EXC_CLASS_BORLANDCPP    // C++ exception?
        JNE     @@handleIt                      // nope, handle it
{$IFDEF ALIGN_STACK}
        SUB	ESP, 12
{$ENDIF ALIGN_STACK}
        // C++ exceptions aren't wanted here.  Retoss them as is
        CALL    SysRaiseCPPException

@@handleIt:
{$IFDEF ALIGN_STACK}
        SUB	ESP, 12
{$ENDIF ALIGN_STACK}
        // If the exception is a Delphi exception thrown from C++, coopt it.
        CALL    MaybeCooptException

        // Mark the exception as being handled
        OR      [EAX].TRaisedException.Flags, excIsBeingHandled

{$IFDEF ALIGN_STACK}
        MOV     EDX, [ESP + 12]               // Get the addr of the TExcDesc
{$ELSE !ALIGN_STACK}
        MOV     EDX, [ESP]                    // Get the addr of the TExcDesc
{$ENDIF !ALIGN_STACK}
        // STACK: 16 - no alignment required by FindOnExceptionDescEntry
        PUSH    EAX                           // Save the object
        PUSH    EBX                           // Save EBX
        XOR     EBX, EBX                      // No GOT
        MOV     EAX, [EAX].TRaisedException.ExceptObject
        CALL    FindOnExceptionDescEntry
        POP     EBX                           // Restore EBX
        OR      EAX, EAX                      // Is the exception for me?
        JE      @@NotForMe

        MOV     EDX, EAX
        POP     EAX                           // Get the object back
{$IFDEF ALIGN_STACK}
        ADD	ESP, 16			      // Ditch the alignment _and_ return addr
{$ELSE !ALIGN_STACK}
        POP     ECX                           // Ditch the return addr
{$ENDIF !ALIGN_STACK}

        CALL    LinkException

        // Get the Pascal object itself.
        MOV     EAX, [EAX].TRaisedException.ExceptObject

        MOV     EDX, [EDX].TExcDescEntry.handler
        CALL    NotifyOnExcept                // Tell the debugger about it

        JMP     EDX                           // Back to the user code
        // never returns
@@NotForMe:
        POP     EAX                           // Get the exception object

        // Mark that we're reraising this exception, so that the
        // compiler generated exception handler for the 'except on' clause
        // will not get confused
        OR      [EAX].TRaisedException.Flags, excIsBeingReRaised
        JMP     SysRaiseException             // Should be using resume here
end;
{$ENDIF PC_MAPPED_EXCEPTIONS}
{$IFNDEF  PC_MAPPED_EXCEPTIONS}
asm
        { ->    [ESP+ 4] excPtr: PExceptionRecord       }
        {       [ESP+ 8] errPtr: PExcFrame              }
        {       [ESP+12] ctxPtr: Pointer                }
        {       [ESP+16] dspPtr: Pointer                }
        { <-    EAX return value - always one   }

        MOV     EAX,[ESP+4]
        TEST    [EAX].TExceptionRecord.ExceptionFlags,cUnwindInProgress
        JNE     @@exit

        CMP     [EAX].TExceptionRecord.ExceptionCode,cDelphiException
        JE      @@DelphiException
        CLD
        CALL    _FpuInit
        MOV     EDX,ExceptClsProc
        TEST    EDX,EDX
        JE      @@exit
        CALL    EDX
        TEST    EAX,EAX
        JNE     @@common
        JMP     @@exit

@@DelphiException:
        MOV     EAX,[EAX].TExceptionRecord.ExceptObject
        MOV     EAX,[EAX]                       { load vtable of exception object       }

@@common:

        MOV     EDX,[ESP+8]

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        MOV     ECX,[EDX].TExcFrame.desc
        MOV     EBX,[ECX].TExcDesc.cnt
        LEA     ESI,[ECX].TExcDesc.excTab       { point ECX to exc descriptor table }
        MOV     EBP,EAX                         { load vtable of exception object }

@@innerLoop:
        MOV     EAX,[ESI].TExcDescEntry.vTable
        TEST    EAX,EAX                         { catch all clause?                     }
        JE      @@doHandler                     { yes: go execute handler               }
        MOV     EDI,EBP                         { load vtable of exception object       }
        JMP     @@haveVMT

@@vtLoop:
        MOV     EDI,[EDI]
@@haveVMT:
        MOV     EAX,[EAX]
        CMP     EAX,EDI
        JE      @@doHandler

        MOV     ECX,[EAX].vmtInstanceSize
        CMP     ECX,[EDI].vmtInstanceSize
        JNE     @@parent

        MOV     EAX,[EAX].vmtClassName
        MOV     EDX,[EDI].vmtClassName

        XOR     ECX,ECX
        MOV     CL,[EAX]
        CMP     CL,[EDX]
        JNE     @@parent

        INC     EAX
        INC     EDX
        CALL    _AStrCmp
        JE      @@doHandler

@@parent:
        MOV     EDI,[EDI].vmtParent             { load vtable of parent         }
        MOV     EAX,[ESI].TExcDescEntry.vTable
        TEST    EDI,EDI
        JNE     @@vtLoop

        ADD     ESI,8
        DEC     EBX
        JNZ     @@innerLoop

        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
        JMP     @@exit

@@doHandler:
        MOV     EAX,[ESP+4+4*4]
        CMP     [EAX].TExceptionRecord.ExceptionCode,cDelphiException
        MOV     EDX,[EAX].TExceptionRecord.ExceptObject
        MOV     ECX,[EAX].TExceptionRecord.ExceptAddr
        JE      @@haveObject
        CALL    ExceptObjProc
        MOV     EDX,[ESP+12+4*4]
        CALL    NotifyNonDelphiException
{$IFDEF MSWINDOWS}
        CMP     BYTE PTR JITEnable,0
        JBE     @@NoJIT
        CMP     BYTE PTR DebugHook,0
        JA      @@noJIT                 { Do not JIT if debugging }
        LEA     ECX,[ESP+4]
        PUSH    EAX
        PUSH    ECX
        CALL    UnhandledExceptionFilter
        CMP     EAX,EXCEPTION_CONTINUE_SEARCH
        POP     EAX
        JE      @@exit
{$ENDIF MSWINDOWS}
@@noJIT:
        MOV     EDX,EAX
        MOV     EAX,[ESP+4+4*4]
        MOV     ECX,[EAX].TExceptionRecord.ExceptionAddress
        JMP     @@GoUnwind

@@haveObject:
{$IFDEF MSWINDOWS}
        CMP     BYTE PTR JITEnable,1
        JBE     @@GoUnwind
        CMP     BYTE PTR DebugHook,0
        JA      @@GoUnwind
        PUSH    EAX
        LEA     EAX,[ESP+8]
        PUSH    EDX
        PUSH    ECX
        PUSH    EAX
        CALL    UnhandledExceptionFilter
        CMP     EAX,EXCEPTION_CONTINUE_SEARCH
        POP     ECX
        POP     EDX
        POP     EAX
        JE      @@exit
{$ENDIF MSWINDOWS}

@@GoUnwind:
        XOR     EBX,EBX
        MOV     EBX,FS:[EBX]
        PUSH    EBX                     { Save topmost frame     }
        PUSH    EAX                     { Save exception record  }
        PUSH    EDX                     { Save exception object  }
        PUSH    ECX                     { Save exception address }

        MOV     EDX,[ESP+8+8*4]
        OR      [EAX].TExceptionRecord.ExceptionFlags,cUnwinding

        PUSH    ESI                     { Save handler entry     }

        PUSH    0
        PUSH    EAX
        PUSH    offset @@returnAddress
        PUSH    EDX
        CALL    RtlUnwindProc
@@returnAddress:

        POP     EBX                     { Restore handler entry  }

        MOV     EDI,[ESP+8+8*4]

        {       Make the RaiseList entry on the stack }

        CALL    SysInit.@GetTLS
        PUSH    [EAX].RaiseListPtr
        MOV     [EAX].RaiseListPtr,ESP

        MOV     EBP,[EDI].TExcFrame.hEBP
        MOV     [EDI].TExcFrame.desc,offset @@exceptFinally
        MOV     EAX,[ESP].TRaiseFrame.ExceptObject
        CALL    NotifyOnExcept
        JMP     [EBX].TExcDescEntry.handler

@@exceptFinally:
        JMP     _HandleFinallyInternal

@@destroyExcept:
        {       we come here if an exception handler has thrown yet another exception }
        {       we need to destroy the exception object and pop the raise list. }

        CALL    SysInit.@GetTLS
        MOV     ECX,[EAX].RaiseListPtr
        MOV     EDX,[ECX].TRaiseFrame.NextRaise
        MOV     [EAX].RaiseListPtr,EDX

        MOV     EAX,[ECX].TRaiseFrame.ExceptObject
        JMP     TObject.Free
@@exit:
        MOV     EAX,1
end;
{$ENDIF !PC_MAPPED_EXCEPTIONS}

procedure       _HandleFinally;
asm
{$IFDEF PC_MAPPED_EXCEPTIONS}
{$IFDEF PIC}
        MOV     ESI, EBX
{$ENDIF PIC}
        CMP     ECX, UW_EXC_CLASS_BORLANDCPP    // C++ exception?
        JNE     @@handleIt                      // nope, handle it
        // unwinding a C++ exception.  We handle that specially.
        PUSH    EAX
        PUSH    EDX
        PUSH    ECX
        MOV     EDX, [ESP+12]
        CALL    EDX
        POP     ECX
        POP     EDX
        POP     EAX
        CALL    SysRaiseCPPException

@@handleIt:
{$IFDEF ALIGN_STACK}
        SUB	ESP, 4 { RA, XX, and 2 PUSHes to come = 16}
{$ENDIF ALIGN_STACK}
        PUSH    EAX
        PUSH    EDX
        CALL    UnblockOSExceptions
        POP     EDX
        POP     EAX
{$IFDEF ALIGN_STACK}
        SUB	ESP, 8 { RA, prev XX, XX XX = 16 }
{$ENDIF ALIGN_STACK}

        // If the exception is a Delphi exception thrown from C++, coopt it.
        CALL    MaybeCooptException

{$IFDEF ALIGN_STACK}
        MOV     EDX, [ESP + 12] { get the return address; stack still aligned }
{$ELSE !ALIGN_STACK}
        MOV     EDX, [ESP]
{$ENDIF !ALIGN_STACK}
        CALL    NotifyExceptFinally
{$IFDEF ALIGN_STACK}
        { stack is still aligned with some scratch space - use it }
        MOV	[ESP], EAX
{$ELSE !ALIGN_STACK}
        PUSH    EAX
{$ENDIF !ALIGN_STACK}
{$IFDEF PIC}
        MOV     EBX, ESI
{$ENDIF PIC}
        {
          Mark the current exception with the EBP of the handler.  If
          an exception is raised from the finally block, then this
          exception will be orphaned.  We will catch this later, when
          we clean up the next except block to complete execution.
          See DoneExcept.
        }
        MOV [EAX].TRaisedException.HandlerEBP, EBP
        CALL    EDX
        POP     EAX
{$IFDEF ALIGN_STACK}
        { We have to make it look like we've arrived here and setup
          a basic EBP frame, in order for the unwind that we will now
          cause to succeed properly.  We popped the saved EAX, now
          we have to get rid of stuff up to the original return address. }
        ADD	ESP, 8
{$ENDIF ALIGN_STACK}
        {
          We executed the finally handler without adverse reactions.
          It's safe to clear the marker now.
        }
        MOV [EAX].TRaisedException.HandlerEBP, $FFFFFFFF
        PUSH    EBP
        MOV     EBP, ESP
{$IFDEF ALIGN_STACK}
        SUB	ESP, 8
{$ENDIF ALIGN_STACK}
        CALL    SysRaiseException             // Should be using resume here
{$ENDIF PC_MAPPED_EXCEPTIONS}
{$IFDEF MSWINDOWS}
        { ->    [ESP+ 4] excPtr: PExceptionRecord       }
        {       [ESP+ 8] errPtr: PExcFrame              }
        {       [ESP+12] ctxPtr: Pointer                }
        {       [ESP+16] dspPtr: Pointer                }
        { <-    EAX return value - always one   }

        MOV     EAX,[ESP+4]
        TEST    [EAX].TExceptionRecord.ExceptionFlags,cUnwindInProgress
        JE      @@exit

        PUSH    EBX
        XOR     EBX,EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        {       Make exception frame    }

        PUSH    EBP
        PUSH    offset @@exceptFinally
        PUSH    dword ptr FS:[EBX]
        MOV     FS:[EBX],ESP

        MOV     EBX,FS:[EBX]
        MOV     EDX,[EAX].TExceptionRecord.ExceptObject
        MOV     ECX,[EAX].TExceptionRecord.ExceptAddr
        PUSH    EBX                     { Save pointer to topmost frame }
        PUSH    EAX                     { Save OS exception pointer     }
        PUSH    EDX                     { Save exception object         }
        PUSH    ECX                     { Save exception address        }

        MOV     EDI,[ESP+8+11*4]        { Load errPtr:PExcFrame         }

        {       Make the RaiseList entry on the stack   }

        CALL    SysInit.@GetTLS
        PUSH    [EAX].RaiseListPtr
        MOV     [EAX].RaiseListPtr,ESP

        MOV     ECX,[EDI].TExcFrame.desc
        MOV     EBP,[EDI].TExcFrame.hEBP
        MOV     [EDI].TExcFrame.desc,offset @@exceptFinally
        ADD     ECX,TExcDesc.instructions
        CALL    NotifyExceptFinally
        CALL    ECX

        CALL    SysInit.@GetTLS
        MOV     ECX,[EAX].RaiseListPtr
        MOV     EDX,[ECX].TRaiseFrame.NextRaise
        MOV     [EAX].RaiseListPtr,EDX
        ADD     ESP,5*4                 { Remove local RaiseList        }

        {       Remove exception frame  }

        XOR     EAX,EAX
        POP     EDX
        POP     ECX
        POP     ECX
        MOV     FS:[EAX],EDX

        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
        JMP     @@exit

@@exceptFinally:
        JMP     _HandleFinallyInternal

@@destroyExcept:
        {       we come here if an finalization handler has thrown yet  }
        {       another exception we need to destroy the exception      }
        {       object and pop the raise list.                          }

        CALL    SysInit.@GetTLS
        MOV     ECX,[EAX].RaiseListPtr
        MOV     EDX,[ECX].TRaiseFrame.NextRaise
        MOV     [EAX].RaiseListPtr,EDX

        MOV     EAX,[ECX].TRaiseFrame.ExceptObject
        JMP     TObject.Free

@@exit:
        MOV     EAX,1
{$ENDIF MSWINDOWS}
end;

{$IFDEF MSWINDOWS}
procedure       _HandleFinallyInternal;
asm
        { ->    [ESP+ 4] excPtr: PExceptionRecord       }
        {       [ESP+ 8] errPtr: PExcFrame              }
        {       [ESP+12] ctxPtr: Pointer                }
        {       [ESP+16] dspPtr: Pointer                }
        { <-    EAX return value - always one           }

        MOV     EAX,[ESP+4]
        MOV     EDX,[ESP+8]
        TEST    [EAX].TExceptionRecord.ExceptionFlags,cUnwindInProgress
        JE      @@exit
        MOV     ECX,[EDX].TExcFrame.desc
        MOV     [EDX].TExcFrame.desc,offset @@exit

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        MOV     EBP,[EDX].TExcFrame.hEBP
        ADD     ECX,TExcDesc.instructions
        CALL    NotifyExceptFinally
        CALL    ECX

        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX

@@exit:
        MOV     EAX,1
end;
{$ENDIF MSWINDOWS}


procedure       _HandleAutoException;
{$IF defined(LINUX) or defined(MACOSX)}
{$IFDEF PC_MAPPED_EXCEPTIONS}
asm
        // EAX = TObject reference, or nil
        // [ESP] = ret addr

        PUSH    EAX
        PUSH    EDX
        CALL    UnblockOSExceptions
        POP     EDX
        POP     EAX

        // If the exception is a Delphi exception thrown from C++, coopt it.
        CALL    MaybeCooptException

        CALL    LinkException
//
//  The compiler wants the stack to look like this:
//  ESP+4->  HRESULT
//  ESP+0->  ret addr
//
//  Make it so.
//
        POP     EDX
        PUSH    8000FFFFH
        PUSH    EDX

        OR      EAX, EAX    // Was this a method call?
        JE      @@Done

        PUSH    EAX
        CALL    CurrentException
        MOV     EDX, [EAX].TRaisedException.ExceptObject
        MOV     ECX, [EAX].TRaisedException.ExceptionAddr;
        POP     EAX
        MOV     EAX, [EAX]
        CALL    DWORD PTR [EAX] + VMTOFFSET TObject.SafeCallException;
        MOV     [ESP+4], EAX
@@Done:
        CALL    _DoneExcept
end;
{$ELSE !PC_MAPPED_EXCEPTIONS}
begin
  Error(reSafeCallError);  //!!
end;
{$ENDIF !PC_MAPPED_EXCEPTIONS}
{$IFEND LINUX or MACOSX}
{$IFDEF MSWINDOWS}
asm
  { ->    [ESP+ 4] excPtr: PExceptionRecord       }
  {       [ESP+ 8] errPtr: PExcFrame              }
  {       [ESP+12] ctxPtr: Pointer                }
  {       [ESP+16] dspPtr: Pointer                }
  { <-    EAX return value - always one           }

        MOV     EAX,[ESP+4]
        TEST    [EAX].TExceptionRecord.ExceptionFlags,cUnwindInProgress
        JNE     @@exit

        CMP     [EAX].TExceptionRecord.ExceptionCode,cDelphiException
        CLD
        CALL    _FpuInit
        JE      @@DelphiException
        CMP     BYTE PTR JITEnable,0
        JBE     @@DelphiException
        CMP     BYTE PTR DebugHook,0
        JA      @@DelphiException

@@DoUnhandled:
        LEA     EAX,[ESP+4]
        PUSH    EAX
        CALL    UnhandledExceptionFilter
        CMP     EAX,EXCEPTION_CONTINUE_SEARCH
        JE      @@exit
        MOV     EAX,[ESP+4]
        JMP     @@GoUnwind

@@DelphiException:
        CMP     BYTE PTR JITEnable,1
        JBE     @@GoUnwind
        CMP     BYTE PTR DebugHook,0
        JA      @@GoUnwind
        JMP     @@DoUnhandled

@@GoUnwind:
        OR      [EAX].TExceptionRecord.ExceptionFlags,cUnwinding

        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        MOV     EDX,[ESP+8+3*4]

        PUSH    0
        PUSH    EAX
        PUSH    offset @@returnAddress
        PUSH    EDX
        CALL    RtlUnwindProc

@@returnAddress:
        POP     EBP
        POP     EDI
        POP     ESI
        MOV     EAX,[ESP+4]
        MOV     EBX,8000FFFFH
        CMP     [EAX].TExceptionRecord.ExceptionCode,cDelphiException
        JNE     @@done

        MOV     EDX,[EAX].TExceptionRecord.ExceptObject
        MOV     ECX,[EAX].TExceptionRecord.ExceptAddr
        MOV     EAX,[ESP+8]
        MOV     EAX,[EAX].TExcFrame.SelfOfMethod
        TEST    EAX,EAX
        JZ      @@freeException
        MOV     EBX,[EAX]
        CALL    DWORD PTR [EBX] + VMTOFFSET TObject.SafeCallException
        MOV     EBX,EAX
@@freeException:
        MOV     EAX,[ESP+4]
        MOV     EAX,[EAX].TExceptionRecord.ExceptObject
        CALL    TObject.Free
@@done:
        XOR     EAX,EAX
        MOV     ESP,[ESP+8]
        POP     ECX
        MOV     FS:[EAX],ECX
        POP     EDX
        POP     EBP
        LEA     EDX,[EDX].TExcDesc.instructions
        POP     ECX
        JMP     EDX
@@exit:
        MOV     EAX,1
end;
{$ENDIF MSWINDOWS}

{$IFDEF PC_MAPPED_EXCEPTIONS}
procedure       _RaiseAtExcept;
asm
        { ->    EAX     Pointer to exception object     }
        { ->    EDX     Purported addr of exception     }
        { Be careful: EBX is not set up in PIC mode. }
        { Outward bound calls must go through an exported fn, like SysRaiseException }
        OR      EAX, EAX
        JNE     @@GoAhead
        MOV     EAX, 216
        CALL    _RunError

@@GoAhead:
{$IFDEF ALIGN_STACK}
        SUB	ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    BlockOSExceptions
{$IFDEF ALIGN_STACK}
        ADD	ESP, 12
{$ENDIF ALIGN_STACK}
        PUSH    EBP
        MOV     EBP, ESP
{$IFDEF ALIGN_STACK}
        SUB	ESP, 8
{$ENDIF ALIGN_STACK}
        CALL    NotifyReRaise
        CALL    AllocateException
        CALL    SysRaiseException
        {
          This can only return if there was a terrible error.  In this event,
          we have to bail out.
        }
{$IFDEF ALIGN_STACK}
        ADD	ESP, 8
{$ENDIF ALIGN_STACK}
        JMP     _Run0Error
end;
{$ENDIF PC_MAPPED_EXCEPTIONS}

procedure       _RaiseExcept;
asm
{$IFDEF PC_MAPPED_EXCEPTIONS}
        { ->    EAX     Pointer to exception object     }
        MOV     EDX, [ESP]
        JMP     _RaiseAtExcept
{$ENDIF PC_MAPPED_EXCEPTIONS}
{$IFDEF MSWINDOWS}
  { When making changes to the way Delphi Exceptions are raised, }
  { please realize that the C++ Exception handling code reraises }
  { some exceptions as Delphi Exceptions.  Of course we want to  }
  { keep exception raising compatible between Delphi and C++, so }
  { when you make changes here, consult with the relevant C++    }
  { exception handling engineer. The C++ code is in xx.cpp, in   }
  { the RTL sources, in function tossAnException.                }

  { ->    EAX     Pointer to exception object     }
  {       [ESP]   Error address           }

        OR      EAX, EAX
        JNE     @@GoAhead
        MOV     EAX, 216
        CALL    _RunError
@@GoAhead:
        POP     EDX

        PUSH    ESP
        PUSH    EBP
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        PUSH    EAX                             { pass class argument           }
        PUSH    EDX                             { pass address argument         }

        MOV     EAX,ESP                         { Need these values later }
        PUSH    ESP                             { pass pointer to arguments             }
        PUSH    7                               { there are seven arguments               }
        PUSH    cNonContinuable                 { we can't continue execution   }
        PUSH    cDelphiException                { our magic exception code              }
        PUSH    EDX                             { pass the user's return address        }
        MOV     EDX,RaiseExceptObjProc          { has this been hooked? }
        TEST    EDX,EDX
        JZ      @@2

        PUSH    [EAX + 6 * 4]
        PUSH    [EAX + 5 * 4]
        PUSH    [EAX + 4 * 4]
        PUSH    [EAX + 3 * 4]
        PUSH    [EAX + 2 * 4]
        PUSH    [EAX + 1 * 4]                   { object }
        PUSH    [EAX + 0 * 4]                   { address }
        PUSH    7                               { how many of the above }
        PUSH    [EAX + 0 * 4]                   { the address goes here again }
        PUSH    EAX
        PUSH    EDX
        CALL    RaiseList
        MOV     ECX,EAX
        POP     EDX
        POP     EAX
        TEST    ECX,ECX
        JZ      @@1
        MOV     ECX,[ECX].TRaiseFrame.ExceptionRecord
@@1:    PUSH    ECX
        PUSH    cNonContinuable
        PUSH    cDelphiException
        MOV     EAX,ESP
        CALL    EDX
        ADD     ESP,12 * 4                      { Cleanup 12 DWORDS from the stack }
@@2:
        JMP     RaiseExceptionProc
{$ENDIF MSWINDOWS}
end;


{$IFDEF PC_MAPPED_EXCEPTIONS}
{
  Used in the PC mapping exception implementation to handle exceptions in constructors.
}
procedure       _ClassHandleException;
asm
  {
  EAX = Ptr to TRaisedException
  EDX = self
  ECX = top flag
  }
        PUSH     ECX
        CALL     LinkException
        MOV      EAX,EDX
        POP      EDX
        TEST     DL, DL
        JE       _RaiseAgain
        MOV      ECX,[EAX]
        MOV      DL,$81
        PUSH     EAX
        CALL     DWORD PTR [ECX] + VMTOFFSET TObject.Destroy
        POP      EAX
        CALL     _ClassDestroy
        JMP      _RaiseAgain
end;
{$ENDIF PC_MAPPED_EXCEPTIONS}

procedure       _RaiseAgain;
asm
{$IFDEF PC_MAPPED_EXCEPTIONS}
        CALL    CurrentException
// The following notifies the debugger of a reraise of exceptions.  This will
// be supported in a later release, but is disabled for now.
//        PUSH    EAX
//        MOV     EDX, [EAX].TRaisedException.ExceptionAddr
//        MOV     EAX, [EAX].TRaisedException.ExceptObject
//        CALL    NotifyReRaise                   { Tell the debugger }
//        POP     EAX
        TEST    [EAX].TRaisedException.Flags, excIsBeingHandled
        JZ      @@DoIt
        OR      [EAX].TRaisedException.Flags, excIsBeingReRaised
@@DoIt:
        PUSH    EAX
        CALL    UnlinkException
        POP     EAX
        MOV     EDX, [ESP]                      { Get the user's addr }
        JMP     SysRaiseException
{$ENDIF PC_MAPPED_EXCEPTIONS}
{$IFDEF MSWINDOWS}
        { ->    [ESP        ] return address to user program }
        {       [ESP+ 4     ] raise list entry (4 dwords)    }
        {       [ESP+ 4+ 4*4] saved topmost frame            }
        {       [ESP+ 4+ 5*4] saved registers (4 dwords)     }
        {       [ESP+ 4+ 9*4] return address to OS           }
        { ->    [ESP+ 4+10*4] excPtr: PExceptionRecord       }
        {       [ESP+ 8+10*4] errPtr: PExcFrame              }

        { Point the error handler of the exception frame to something harmless }

        MOV     EAX,[ESP+8+10*4]
        MOV     [EAX].TExcFrame.desc,offset @@exit

        { Pop the RaiseList }

        CALL    SysInit.@GetTLS
        MOV     EDX,[EAX].RaiseListPtr
        MOV     ECX,[EDX].TRaiseFrame.NextRaise
        MOV     [EAX].RaiseListPtr,ECX

        { Destroy any objects created for non-delphi exceptions }

        MOV     EAX,[EDX].TRaiseFrame.ExceptionRecord
        AND     [EAX].TExceptionRecord.ExceptionFlags,NOT cUnwinding
        CMP     [EAX].TExceptionRecord.ExceptionCode,cDelphiException
        JE      @@delphiException
        MOV     EAX,[EDX].TRaiseFrame.ExceptObject
        CALL    TObject.Free
        CALL    NotifyReRaise

@@delphiException:

        XOR     EAX,EAX
        ADD     ESP,5*4
        MOV     EDX,FS:[EAX]
        POP     ECX
        MOV     EDX,[EDX].TExcFrame.next
        MOV     [ECX].TExcFrame.next,EDX

        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
@@exit:
        MOV     EAX,1
{$ENDIF MSWINDOWS}
end;

{$IFDEF PC_MAPPED_EXCEPTIONS}
{
  This is implemented slow and dumb.  The theory is that it is rare
  to throw an exception past an except handler, and that the penalty
  can be particularly high here.  Partly it's done the dumb way for
  the sake of maintainability.  It could be inlined.
}
procedure       _DestroyException;
var
  Exc: PRaisedException;
  RefCount: Integer;
  ExcObj: Pointer;
  ExcAddr: Pointer;
begin
  asm
    CMP     ECX, UW_EXC_CLASS_BORLANDCPP
    JNE     @@notCPP
    CALL    SysRaiseCPPException
@@notCPP:
    MOV     Exc, EAX
  end;

  if (Exc^.Flags and excIsBeingReRaised) = 0 then
  begin
    RefCount := Exc^.RefCount;
    ExcObj := Exc^.ExceptObject;
    ExcAddr := Exc^.ExceptionAddr;
    Exc^.RefCount := 1;
    FreeException;
    _DoneExcept;
    Exc := AllocateException(ExcObj, ExcAddr);
    Exc^.RefCount := RefCount;
  end;

  Exc^.Flags := Exc^.Flags and not (excIsBeingReRaised or excIsBeingHandled);

  SysRaiseException(Exc);
end;

procedure CleanupException;
asm
        CALL    FreeException
        OR      EAX, EAX
        JE      @@Done
        CALL    TObject.Free
@@Done:
end;
{$ENDIF PC_MAPPED_EXCEPTIONS}

procedure       _DoneExcept;
asm
{$IFDEF PC_MAPPED_EXCEPTIONS}
        CALL    FreeException
        OR      EAX, EAX
        JE      @@Done
        CALL    TObject.Free
@@Done:
        CALL    UnlinkException
        {
          Take a peek at the next exception object on the stack.
          If its EBP marker is at an address lower than our current
          EBP, then we know that it was orphaned when an exception was
          thrown from within the execution of a finally block.  We clean
          it up now, so that we won't leak exception records/objects.
        }
        CALL    CurrentException
        OR      EAX, EAX
        JE      @@Done2
        CMP     [EAX].TRaisedException.HandlerEBP, EBP
        JA      @@Done2
        CALL    FreeException
        OR      EAX, EAX
        JE      @@Done2
        CALL    TObject.Free
@@Done2:
{$ENDIF PC_MAPPED_EXCEPTIONS}
{$IFDEF MSWINDOWS}
        { ->    [ESP+ 4+10*4] excPtr: PExceptionRecord       }
        {       [ESP+ 8+10*4] errPtr: PExcFrame              }

        { Pop the RaiseList }

        CALL    SysInit.@GetTLS
        MOV     EDX,[EAX].RaiseListPtr
        MOV     ECX,[EDX].TRaiseFrame.NextRaise
        MOV     [EAX].RaiseListPtr,ECX

        { Destroy exception object }

        MOV     EAX,[EDX].TRaiseFrame.ExceptObject
        CALL    TObject.Free

        POP     EDX
        MOV     ESP,[ESP+8+9*4]
        XOR     EAX,EAX
        POP     ECX
        MOV     FS:[EAX],ECX
        POP     EAX
        POP     EBP
        CALL    NotifyTerminate
        JMP     EDX
{$ENDIF MSWINDOWS}
end;

{$IFNDEF PC_MAPPED_EXCEPTIONS}
procedure   _TryFinallyExit;
asm
{$IFDEF MSWINDOWS}
        XOR     EDX,EDX
        MOV     ECX,[ESP+4].TExcFrame.desc
        MOV     EAX,[ESP+4].TExcFrame.next
        ADD     ECX,TExcDesc.instructions
        MOV     FS:[EDX],EAX
        CALL    ECX
@@1:    RET     12
{$ENDIF MSWINDOWS}
end;
{$ENDIF !PC_MAPPED_EXCEPTIONS}

{$IFNDEF PC_MAPPED_EXCEPTIONS}
procedure       MapToRunError(P: PExceptionRecord); stdcall;
const
  STATUS_ACCESS_VIOLATION         = $C0000005;
  STATUS_ARRAY_BOUNDS_EXCEEDED    = $C000008C;
  STATUS_FLOAT_DENORMAL_OPERAND   = $C000008D;
  STATUS_FLOAT_DIVIDE_BY_ZERO     = $C000008E;
  STATUS_FLOAT_INEXACT_RESULT     = $C000008F;
  STATUS_FLOAT_INVALID_OPERATION  = $C0000090;
  STATUS_FLOAT_OVERFLOW           = $C0000091;
  STATUS_FLOAT_STACK_CHECK        = $C0000092;
  STATUS_FLOAT_UNDERFLOW          = $C0000093;
  STATUS_INTEGER_DIVIDE_BY_ZERO   = $C0000094;
  STATUS_INTEGER_OVERFLOW         = $C0000095;
  STATUS_PRIVILEGED_INSTRUCTION   = $C0000096;
  STATUS_STACK_OVERFLOW           = $C00000FD;
  STATUS_CONTROL_C_EXIT           = $C000013A;
var
  ErrCode: Byte;
begin
  case P.ExceptionCode of
    STATUS_INTEGER_DIVIDE_BY_ZERO:  ErrCode := 200; { reDivByZero }
    STATUS_ARRAY_BOUNDS_EXCEEDED:   ErrCode := 201; { reRangeError }
    STATUS_FLOAT_OVERFLOW:          ErrCode := 205; { reOverflow }
    STATUS_FLOAT_INEXACT_RESULT,
    STATUS_FLOAT_INVALID_OPERATION,
    STATUS_FLOAT_STACK_CHECK:       ErrCode := 207; { reInvalidOp }
    STATUS_FLOAT_DIVIDE_BY_ZERO:    ErrCode := 200; { reZeroDivide }
    STATUS_INTEGER_OVERFLOW:        ErrCode := 215; { reIntOverflow}
    STATUS_FLOAT_UNDERFLOW,
    STATUS_FLOAT_DENORMAL_OPERAND:  ErrCode := 206; { reUnderflow }
    STATUS_ACCESS_VIOLATION:        ErrCode := 216; { reAccessViolation }
    STATUS_PRIVILEGED_INSTRUCTION:  ErrCode := 218; { rePrivInstruction }
    STATUS_CONTROL_C_EXIT:          ErrCode := 217; { reControlBreak }
    STATUS_STACK_OVERFLOW:          ErrCode := 202; { reStackOverflow }
  else                              ErrCode := 255;
  end;
  RunErrorAt(ErrCode, P.ExceptionAddress);
end;

procedure       _ExceptionHandler;
asm
        MOV     EAX,[ESP+4]

        TEST    [EAX].TExceptionRecord.ExceptionFlags,cUnwindInProgress
        JNE     @@exit
{$IFDEF MSWINDOWS}
        CMP     BYTE PTR DebugHook,0
        JA      @@ExecuteHandler
        LEA     EAX,[ESP+4]
        PUSH    EAX
        CALL    UnhandledExceptionFilter
        CMP     EAX,EXCEPTION_CONTINUE_SEARCH
        JNE     @@ExecuteHandler
        JMP     @@exit
{$ENDIF MSWINDOWS}

@@ExecuteHandler:
        MOV     EAX,[ESP+4]
        CLD
        CALL    _FpuInit
        MOV     EDX,[ESP+8]

        PUSH    0
        PUSH    EAX
        PUSH    offset @@returnAddress
        PUSH    EDX
        CALL    RtlUnwindProc

@@returnAddress:
        MOV     EBX,[ESP+4]
        CMP     [EBX].TExceptionRecord.ExceptionCode,cDelphiException
        MOV     EDX,[EBX].TExceptionRecord.ExceptAddr
        MOV     EAX,[EBX].TExceptionRecord.ExceptObject
        JE      @@DelphiException2

        MOV     EDX,ExceptObjProc
        TEST    EDX,EDX
        JE      MapToRunError
        MOV     EAX,EBX
        CALL    EDX
        TEST    EAX,EAX
        JE      MapToRunError
        MOV     EDX,[EBX].TExceptionRecord.ExceptionAddress

@@DelphiException2:

        CALL    NotifyUnhandled
        MOV     ECX,ExceptProc
        TEST    ECX,ECX
        JE      @@noExceptProc
        CALL    ECX             { call ExceptProc(ExceptObject, ExceptAddr) }

@@noExceptProc:
        MOV     ECX,[ESP+4]
        MOV     EAX,217
        MOV     EDX,[ECX].TExceptionRecord.ExceptAddr
        MOV     [ESP],EDX
        JMP     _RunError

@@exit:
        XOR     EAX,EAX
end;

procedure       SetExceptionHandler(Context: PInitContext);
asm
        { ->    EAX   PInitContext
        { ->    [EBP-type(TExcFrame)] TExcFrame local (returned in EAX) }

        PUSH    EAX               { Save off Context pointer }
        XOR     EDX,EDX           { using [EDX] saves some space over [0] }
        LEA     EAX,[EBP-type(TExcFrame)]
        MOV     ECX,FS:[EDX]      { ECX := head of chain                  }
        MOV     FS:[EDX],EAX      { head of chain := @exRegRec            }

        MOV     [EAX].TExcFrame.next,ECX
{$IFDEF PIC}
        LEA     EDX,[EBX]._ExceptionHandler
        MOV     [EAX].TExcFrame.desc,EDX
{$ELSE}
        MOV     [EAX].TExcFrame.desc,offset _ExceptionHandler
{$ENDIF}
        MOV     [EAX].TExcFrame.hEBP,EBP
        POP     ECX               { Restore Context pointer }
        MOV     [ECX].TInitContext.ExcFrame,EAX
end;

procedure       UnsetExceptionHandler(Context: PInitContext);
asm
        { ->    EAX   PInitContext }

        MOV     EAX,[EAX].TInitContext.ExcFrame
        XOR     EDX,EDX
        TEST    EAX,EAX
        JZ      @@exit

        MOV     ECX,FS:[EDX]    { ECX := head of chain          }
        CMP     EAX,ECX         { simple case: our record is first      }
        JNE     @@search
        MOV     EAX,[EAX]       { head of chain := exRegRec.next        }
        MOV     FS:[EDX],EAX
        JMP     @@exit

@@loop:
        MOV     ECX,[ECX]
@@search:
        CMP     ECX,-1          { at end of list?                       }
        JE      @@exit          { yes - didn't find it          }
        CMP     [ECX],EAX       { is it the next one on the list?       }
        JNE     @@loop          { no - look at next one on list }
@@unlink:                       { yes - unlink our record               }
        MOV     EAX,[EAX]       { get next record on list               }
        MOV     [ECX],EAX       { unlink our record                     }
@@exit:
end;
{$ENDIF !PC_MAPPED_EXCEPTIONS} // not PC_MAPPED_EXCEPTIONS

var
  InitContext: TInitContext;
  DLLThreadContext: TInitContext;

type
  TProc = procedure;

{$IFDEF LINUX}
procedure CallProc(Proc: Pointer; GOT: Cardinal);
asm
        PUSH    EBX
        MOV     EBX,EDX
        ADD     EAX,EBX
        CALL    EAX
        POP     EBX
end;
{$ENDIF}

procedure FinalizeUnits;
var
  Count: Integer;
  Table: PUnitEntryTable;
  P: Pointer;
begin
  if InitContext.InitTable = nil then
    exit;
  Count := InitContext.InitCount;
  Table := InitContext.InitTable^.UnitInfo;
{$IFDEF LINUX}
  Inc(Cardinal(Table), InitContext.Module^.GOT);
{$ENDIF}
  try
    while Count > 0 do
    begin
      Dec(Count);
      InitContext.InitCount := Count;
      P := Table^[Count].FInit;
      if Assigned(P) and Assigned(Pointer(P^)) then
      begin
{$IFDEF LINUX}
        CallProc(P, InitContext.Module^.GOT);
{$ENDIF}
{$IFDEF MACOSX}

        TProc(P)();
{$ENDIF}
{$IFDEF MSWINDOWS}
        TProc(P)();
{$ENDIF}
      end;
    end;
  except
    FinalizeUnits;  { try to finalize the others }
    raise;
  end;
end;

const
  errCaption: array[0..5] of AnsiChar = ('E', 'r', 'r', 'o', 'r', #0);

{***********************************************************}
{$IFDEF TRIAL_EDITION}
{
	This code is used as part of the timeout test for
	applications built with trial editions of the product.  It provides
    the current local time in a format native to the platform in question.

    The linker will generate a checksum of _InitUnitPrep that it will
    place into linked units.  The code generated for _InitUnitPrep must
    not contain fixups actually in the image, as this could alter the
    code at load time, invalidating the checksum.  Take great care to
    make sure that this code is entirely position independent on all
    platforms and circumstances to avoid a serious problem!
}
{$IFDEF MSWINDOWS}
type
  TSystemTime = record
    wYear: Word;
    wMonth: Word;
    wDayOfWeek: Word;
    wDay: Word;
    wHour: Word;
    wMinute: Word;
    wSecond: Word;
    wMilliseconds: Word;
  end;
  TFileTime = record
        LowTime: Integer;
        HighTime: Integer;
  end;


procedure GetLocalTime(var lpSystemTime: TSystemTime); stdcall; external 'kernel32.dll' name 'GetLocalTime';
procedure SystemTimeToFileTime(const lpSystemTime: TSystemTime; var Dest: TFileTime); stdcall; external 'kernel32.dll' name 'SystemTimeToFileTime';

function _InitUnitPrep: Int64;
var
  SystemTime: TSystemTime;
  FileTime: TFileTime;
  Days: Int64;
begin
  GetLocalTime(SystemTime);
  SystemTimeToFileTime(SystemTime, FileTime);

    // used to hack the result to force a failure for testing:
  Days := 1000000000 div 100;
  Days := Days * 3600;
  Days := Days * 24;
  Days := Days * 31;
  Days := 0;

  Result := Int64(FileTime) + Days;
//  Dec(InitContext.InitTable^.UnitCount);
end;
{$ENDIF}
{$IFDEF LINUX}

function _InitUnitPrep: Integer;
var
  Days: Integer;
begin
  Days := 0;    // used to hack the result to force a failure for testing
    Result := _time(nil) + Days;
end;
{$ENDIF}

resourcestring
{$IFDEF LINUX}
  SExpiredMsg =
  'This module was compiled with a trial version of Kylix.'+#10+
  'The trial period has expired.'+#10;
{$ENDIF}
{$IFDEF MACOSX}
  SExpiredMsg =
  'This module was compiled with a trial version of Delphi.'+#10+
  'The trial period has expired.'+#10;
{$ENDIF}
{$IFDEF MSWINDOWS}
  SExpiredMsg =
  'This module was compiled with a trial version of Delphi.'+#13+#10+
  'The trial period has expired.'+#13+#10;
{$ENDIF}
var
  ExpiredMsg: String;

procedure _Expired;
{$IFDEF MSWINDOWS}
var
  Dummy: Cardinal;
begin
  // U-OK
  ExpiredMsg := LoadResString(@SExpiredMsg);
  if IsConsole then
    WriteFile(GetStdHandle(STD_ERROR_HANDLE), PChar(ExpiredMsg), Length(ExpiredMsg), Dummy, nil)
  else
    MessageBox(0, PChar(ExpiredMsg), errCaption, 0);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
begin
  ExpiredMsg := LoadResString(@SExpiredMsg);
  __write(2, PChar(ExpiredMsg), Length(ExpiredMsg));
{$ENDIF POSIX}
  Halt(232);
end;

{$ENDIF}	//TRIAL_EDITION


procedure InitUnits;
var
  Count, I: Integer;
  Table: PUnitEntryTable;
  P: Pointer;
begin
  if InitContext.InitTable = nil then
    exit;
  Count := InitContext.InitTable^.UnitCount;
  I := 0;
  Table := InitContext.InitTable^.UnitInfo;
{$IFDEF LINUX}
  Inc(Cardinal(Table), InitContext.Module^.GOT);
{$ENDIF}
  try
    while I < Count do
    begin
      P := Table^[I].Init;
      Inc(I);
      InitContext.InitCount := I;
      if Assigned(P) and Assigned(Pointer(P^)) then
      begin
{$IFDEF LINUX}
        CallProc(P, InitContext.Module^.GOT);
{$ENDIF}
{$IFDEF MACOSX}

        TProc(P)();
{$ENDIF}
{$IFDEF MSWINDOWS}
        TProc(P)();
{$ENDIF}
      end;
    end;
  except
    FinalizeUnits;
    raise;
  end;
end;

procedure _PackageLoad(const Table : PackageInfo; Module: PLibModule);
var
  SavedContext: TInitContext;
begin
  SavedContext := InitContext;
  InitContext.DLLInitState := 0;
  InitContext.InitTable := Table;
  InitContext.InitCount := 0;
  InitContext.Module := Module;
  InitContext.OuterContext := @SavedContext;
  try
    InitUnits;
  finally
    InitContext := SavedContext;
  end;
end;


procedure _PackageUnload(const Table : PackageInfo; Module: PLibModule);
var
  SavedContext: TInitContext;
begin
  SavedContext := InitContext;
  InitContext.DLLInitState := 0;
  InitContext.InitTable := Table;
  InitContext.InitCount := Table^.UnitCount;
  InitContext.Module := Module;
  InitContext.OuterContext := @SavedContext;
  try
    FinalizeUnits;
  finally
    InitContext := SavedContext;
  end;
end;

{$IF defined(LINUX) or defined(MACOSX)}
procedure       _StartExe(InitTable: PackageInfo; Module: PLibModule; Argc: Integer; Argv: Pointer);
begin
  ArgCount := Argc;
  ArgValues := Argv;
{$IFEND LINUX or MACOSX}
{$IFDEF MSWINDOWS}
procedure       _StartExe(InitTable: PackageInfo; Module: PLibModule);
begin
  RaiseExceptionProc := @RaiseException;
  RTLUnwindProc := @RTLUnwind;
{$ENDIF MSWINDOWS}
  InitContext.InitTable := InitTable;
  InitContext.InitCount := 0;
  InitContext.Module := Module;
  MainInstance := Module.Instance;
{$IFNDEF PC_MAPPED_EXCEPTIONS}
  SetExceptionHandler(@InitContext);
{$ENDIF !PC_MAPPED_EXCEPTIONS}
  IsLibrary := False;
  InitUnits;
end;

{$IFDEF MSWINDOWS}
procedure       _StartLib;
asm
        { ->    EAX InitTable   }
        {       EDX Module      }
        {       ECX InitTLS     }
        {       [ESP+4] DllProc }
        {       [EBP+8] HInst   }
        {       [EBP+12] Reason }
        {       [EBP-(typeTExcFrame)] TExcFrame local }
        {       [EBP-(type TExcFrame)-(type TInitContext)] TInitContext local }

        { Push some desperately needed registers }

        PUSH    ECX
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        { Setup EBX to point to InitContext or DLLThreadContext based on Reason }

        MOV     EBX,offset InitContext
        CMP     DWORD PTR [EBP+12],2    // DLL_THEAD_ATTACH
        JL      @@notDLLThread
        MOV     EBX,offset DLLThreadContext

        { Save the current init context into the stackframe of our caller }

@@notDLLThread:
        MOV     ESI,EBX
        LEA     EDI,[EBP - (type TExcFrame) - (type TInitContext)]
        MOV     ECX,(type TInitContext)/4
        REP     MOVSD

        { Setup the current InitContext }

        POP     [EBX].TInitContext.DLLSaveEDI
        POP     [EBX].TInitContext.DLLSaveESI
        POP     [EBX].TInitContext.DLLSaveEBX
        MOV     [EBX].TInitContext.DLLSaveEBP,EBP
        MOV     [EBX].TInitContext.InitTable,EAX
        MOV     [EBX].TInitContext.Module,EDX
        LEA     ECX,[EBP - (type TExcFrame) - (type TInitContext)]
        MOV     [EBX].TInitContext.OuterContext,ECX

        { Get and save the current thread ID }

        CALL    GetCurrentThreadID
        MOV     [EBX].TInitContext.ThreadID,EAX
        MOV     EAX,[EBX].TInitContext.InitTable

        { Setup InitCount for FinalizeUnits call }

        XOR     ECX,ECX
        CMP     DWORD PTR [EBP+12],0    // Reason = DLL_PROCESS_DETACH?
        JNE     @@notShutDown
        MOV     ECX,[EAX].PackageInfoTable.UnitCount
@@notShutDown:
        MOV     [EBX].TInitContext.InitCount,ECX

        { Setup exception handler }

        MOV     EAX, offset RaiseException
        MOV     RaiseExceptionProc, EAX
        MOV     EAX, offset RTLUnwind
        MOV     RTLUnwindProc, EAX

        MOV     EAX,EBX                 // Pass address of current context
        CALL    SetExceptionHandler

        MOV     EAX,[EBP+12]
        INC     EAX
        MOV     [EBX].TInitContext.DLLInitState,AL
        DEC     EAX

        { Init any needed TLS }

        POP     ECX
        MOV     EDX,[ECX]
        MOV     [EBX].TInitContext.ExitProcessTLS,EDX
        JE      @@skipTLSproc
        CMP     AL,3                    // DLL_THREAD_DETACH
        JGE     @@skipTLSproc           // call ExitThreadTLS proc after DLLProc
        CALL    dword ptr [ECX+EAX*4]   // Call TlsProc[Reason]

@@skipTLSproc:

        { Call any DllProc }

        PUSH    ECX                     // TlsProc
        MOV     ECX,[ESP+8]             // DLLProc
        TEST    ECX,ECX
        JE      @@noDllProc
        MOV     EAX,[EBP+12]            // Reason
        MOV     EDX,[EBP+16]            // Reserved
        CALL    ECX

@@noDllProc:

        POP     ECX
        MOV     EAX, [EBP+12]
        CMP     AL,3                    // DLL_THREAD_DETACH
        JL      @@afterDLLproc          // don't free TLS on process shutdown
        CALL    dword ptr [ECX+EAX*4]   // Call TlsProc[Reason]

@@afterDLLProc:

        { Set IsLibrary if there was no exe yet }

        CMP     MainInstance,0
        JNE     @@haveExe
        MOV     IsLibrary,1
        FNSTCW  Default8087CW           // save host exe's FPU preferences

@@haveExe:

        MOV     EAX,[EBP+12]
        DEC     EAX
        JNE     _Halt0
        CALL    InitUnits
        RET     4
end;
{$ENDIF  MSWINDOWS}
{$IF defined(LINUX) or defined(MACOSX)}
procedure       _StartLib(Context: PInitContext; Module: PLibModule; DLLProc: TDLLProcEx);
var
  TempSwap: TInitContext;
begin
  // Context's register save fields are already initialized.
  // Save the current InitContext and activate the new Context by swapping them
  TempSwap := InitContext;
  InitContext := PInitContext(Context)^;
  PInitContext(Context)^ := TempSwap;

  InitContext.Module := Module;
  InitContext.OuterContext := Context;

  // DLLInitState is initialized by SysInit to 0 for shutdown, 1 for startup
  // Inc DLLInitState to distinguish from package init:
  // 0 for package, 1 for DLL shutdown, 2 for DLL startup

  Inc(InitContext.DLLInitState);

  if InitContext.DLLInitState = 1 then
  begin
    InitContext.InitTable := Module.InitTable;
    if Assigned(InitContext.InitTable) then
      InitContext.InitCount := InitContext.InitTable.UnitCount  // shutdown
  end
  else
  begin
    Module.InitTable := InitContext.InitTable;  // save for shutdown
    InitContext.InitCount := 0;  // startup
  end;

  if Assigned(DLLProc) then
    DLLProc(InitContext.DLLInitState-1,0);

  if MainInstance = 0 then        { Set IsLibrary if there was no exe yet }
  begin
    IsLibrary := True;
    Default8087CW := Get8087CW;
  end;

  if InitContext.DLLInitState = 1 then
    _Halt0
  else
    InitUnits;
end;
{$IFEND LINUX or MACOSX}

function LoadResStringA(ResStringRec: PResStringRec): AnsiString;
begin
  Result := AnsiString(LoadResString(ResStringRec));
end;

function LoadResStringW(ResStringRec: PResStringRec): WideString;
begin
  Result := WideString(LoadResString(ResStringRec));
end;

function LoadResStringU(ResStringRec: PResStringRec): UnicodeString;
begin
  Result := UnicodeString(LoadResString(ResStringRec));
end;

procedure _InitResStrings;
asm
        { ->    EAX     Pointer to init table               }
        {                 record                            }
        {                   cnt: Integer;                   }
        {                   tab: array [1..cnt] record      }
        {                      variableAddress: Pointer;    }
        {                      resStringAddress: Pointer;   }
        {                      stringKind: (LString, WString, UString) as Int32; }
        {                   end;                            }
        {                 end;                              }
        { EBX = caller's GOT for PIC callers, 0 for non-PIC }

{$IFDEF MSWINDOWS}
        PUSH    EBX
        XOR     EBX,EBX
{$ENDIF}
        PUSH    EDI
        PUSH    ESI
        MOV     EDI,[EBX+EAX]     // EDI := initTable.cnt
        LEA     ESI,[EBX+EAX+4]   // ESI := @initTable.tab
@@loop:
        MOV     EAX,[ESI+4]       // EAX := initTable.tab[i].resStringAddress
        ADD     EAX,EBX
        MOV     EDX,[ESI]         // EDX := initTable.tab[i].variableAddress
        ADD     EDX,EBX
        MOV     ECX,[ESI+8]       // ECX := initTable.tab[i].stringKind

        // Handle appropriate string kind.
        TEST    ECX,ECX
        JZ      @@lstring
        DEC     ECX
        JZ      @@wstring
        DEC     ECX
        JZ      @@ustring
        INT     3

@@lstring:
        CALL    LoadResStringA
        JMP     @@doneLoad

@@wstring:
        CALL    LoadResStringW
        JMP     @@doneLoad

@@ustring:
        CALL    LoadResStringU

@@doneLoad:
        ADD     ESI,12
        DEC     EDI
        JNZ     @@loop

        POP     ESI
        POP     EDI
{$IFDEF MSWINDOWS}
        POP     EBX
{$ENDIF}
end;

procedure _InitResStringImports;
asm
        { ->    EAX     Pointer to init table               }
        {                 record                            }
        {                   cnt: Integer;                   }
        {                   tab: array [1..cnt] record      }
        {                      variableAddress: Pointer;    }
        {                      resStringAddress: ^Pointer; *** note indirection  }
        {                      stringKind: (LString, WString, UString) as Int32; }
        {                   end;                            }
        {                 end;                              }
        { EBX = caller's GOT for PIC callers, 0 for non-PIC }

{$IFDEF MSWINDOWS}
        PUSH    EBX
        XOR     EBX,EBX
{$ENDIF}
        PUSH    EDI
        PUSH    ESI
        MOV     EDI,[EBX+EAX]     // EDI := initTable.cnt
        LEA     ESI,[EBX+EAX+4]   // ESI := @initTable.tab
@@loop:
        MOV     EAX,[ESI+4]       // EAX := initTable.tab[i].resStringAddress
        MOV     EAX,[EBX+EAX]     // EAX := EAX^ (to do indirection)
        MOV     EDX,[ESI]         // EDX := initTable.tab[i].variableAddress
        ADD     EDX,EBX
        MOV     ECX,[ESI+8]       // ECX := initTable.tab[i].stringKind

        // Handle appropriate string kind.
        TEST    ECX,ECX
        JZ      @@lstring
        DEC     ECX
        JZ      @@wstring
        DEC     ECX
        JZ      @@ustring
        INT     3

@@lstring:
        CALL    LoadResStringA
        JMP     @@doneLoad

@@wstring:
        CALL    LoadResStringW
        JMP     @@doneLoad

@@ustring:
        CALL    LoadResStringU

@@doneLoad:
        ADD     ESI,12
        DEC     EDI
        JNZ     @@loop

        POP     ESI
        POP     EDI
{$IFDEF MSWINDOWS}
        POP     EBX
{$ENDIF}
end;

procedure _InitImports;
asm
        { ->    EAX     Pointer to init table               }
        {                 record                            }
        {                   cnt: Integer;                   }
        {                   tab: array [1..cnt] record      }
        {                      variableAddress: Pointer;    }
        {                      sourceAddress: ^Pointer;     }
        {                      sourceOffset: Longint;       }
        {                   end;                            }
        {                 end;                              }
        { ->    EDX     Linux only, this points to          }
        {               SysInit.ModuleIsCpp                 }
        { EBX = caller's GOT for PIC callers, 0 for non-PIC }

{$IFDEF MSWINDOWS}
        PUSH    EBX
        XOR     EBX,EBX
{$ENDIF MSWINDOWS}
        PUSH    EDI
        PUSH    ESI
        MOV     EDI,[EBX+EAX]
        LEA     ESI,[EBX+EAX+4]
{$IFDEF LINUX}
        {
            The C++ linker may have already fixed these things up to valid
            addresses.  In this case, we don't want to do this pass.  If this
            module's init tab was linked with ilink, then SysInit.ModuleIsCpp
            will be set, and we'll bail out.
        }
        CMP     BYTE PTR[EDX+EBX], 0  { SysInit.ModuleIsCpp }
        JNE     @@exit
{$ENDIF LINUX}
@@loop:
        MOV     EAX,[ESI+4]     { load address of import    }
        MOV     EDX,[ESI]       { load address of variable  }
        MOV     EAX,[EBX+EAX]   { load contents of import   }
        ADD     EAX,[ESI+8]     { calc address of variable  }
        MOV     [EBX+EDX],EAX   { store result              }
        ADD     ESI,12
        DEC     EDI
        JNZ     @@loop

@@exit:

        POP     ESI
        POP     EDI
{$IFDEF MSWINDOWS}
        POP     EBX
{$ENDIF MSWINDOWS}
end;

{$IFDEF MSWINDOWS}
procedure _InitWideStrings;
asm
     { ->    EAX     Pointer to init table               }
     {                 record                            }
     {                   cnt: Integer;                   }
     {                   tab: array [1..cnt] record      }
     {                      variableAddress: Pointer;    }
     {                      stringAddress: ^Pointer;     }
     {                   end;                            }
     {                 end;                              }

    PUSH    EBX
    PUSH    ESI
    MOV     EBX,[EAX]
    LEA     ESI,[EAX+4]
@@loop:
    MOV     EDX,[ESI+4]     { load address of string    }
    MOV     EAX,[ESI]       { load address of variable  }
    CALL    _WStrAsg
    ADD     ESI,8
    DEC     EBX
    JNZ     @@loop

    POP     ESI
    POP     EBX
end;
{$ENDIF MSWINDOWS}

var
  runErrMsg: array[0..29] of AnsiChar = (
    'R', 'u', 'n', 't', 'i', 'm', 'e', ' ', // 0..7
    'e', 'r', 'r', 'o', 'r', ' ', ' ', ' ', // 8..15
    ' ', ' ', 'a', 't', ' ', '0', '0', '0', // 16..23
    '0', '0', '0', '0', '0', #0);           // 24..29

procedure MakeErrorMessage;
const
  dig: AnsiString = '0123456789ABCDEF';
var
  digit: Byte;
  Temp: Integer;
  Addr: Cardinal;
begin
  digit := 16;
  Temp := ExitCode;
  repeat
    runErrMsg[digit] := AnsiChar(Ord('0') + (Temp mod 10));
    Temp := Temp div 10;
    Dec(digit);
  until Temp = 0;
  digit := 28;
  Addr := Cardinal(ErrorAddr);
  repeat
    runErrMsg[digit] := dig[1 + (Addr and $F)];
    Addr := Addr div 16;
    Dec(digit);
  until Addr = 0;
end;


procedure       ExitDll(Context: PInitContext);
asm
        { ->    EAX  PInitContext }

        { Restore the InitContext }
        MOV     EDI,EAX
        MOV     EBX,[EDI].TInitContext.DLLSaveEBX
        MOV     EBP,[EDI].TInitContext.DLLSaveEBP
        PUSH    [EDI].TInitContext.DLLSaveESI
        PUSH    [EDI].TInitContext.DLLSaveEDI

        MOV     ESI,[EDI].TInitContext.OuterContext
        MOV     ECX,(type TInitContext)/4
        REP     MOVSD
        POP     EDI
        POP     ESI

{$IFDEF MSWINDOWS}
        // Linux: See notes in legacy versions of this file.
        { Return False if ExitCode <> 0, and set ExitCode to 0 }
        XOR     EAX,EAX
        XCHG    EAX, ExitCode
        NEG     EAX
        SBB     EAX,EAX
        INC     EAX
{$ENDIF MSWINDOWS}

        LEAVE
{$IFDEF MSWINDOWS}
        RET     12
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
        RET
{$ENDIF LINUX}
end;

procedure WriteErrorMessage;
{$IFDEF MSWINDOWS}
var
  Dummy: Cardinal;
begin
  if IsConsole then
  begin
    with TTextRec(Output) do
    begin
      if (Mode = fmOutput) and (BufPos > 0) then
        TTextIOFunc(InOutFunc)(TTextRec(Output));  // flush out text buffer
    end;
    // Leave #0 off end of runErrMsg
    WriteFile(GetStdHandle(STD_OUTPUT_HANDLE), runErrMsg, Sizeof(runErrMsg) - 1, Dummy, nil);
    WriteFile(GetStdHandle(STD_OUTPUT_HANDLE), sLineBreak, 2, Dummy, nil);
  end
  else if not NoErrMsg then
    MessageBoxA(0, runErrMsg, errCaption, 0);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
var
  c: AnsiChar;
begin
  with TTextRec(Output) do
  begin
    if (Mode = fmOutput) and (BufPos > 0) then
      TTextIOFunc(InOutFunc)(TTextRec(Output));  // flush out text buffer
  end;
   __write(STDERR_FILENO, @runErrMsg, Sizeof(runErrMsg)-1);
   c := sLineBreak;
   __write(STDERR_FILENO, @c, 1);
{$ENDIF POSIX}
end;

var
  RTLInitFailed: Boolean = False;

procedure _Halt0;
var
  P: procedure;
begin
{$IF defined(LINUX) or defined(MACOSX)}
  if (ExitCode <> 0) and CoreDumpEnabled then
    __raise(SIGABRT);

  if (InitContext.DLLInitState = 2) and (ExitCode <> 0) then
    RTLInitFailed := True;

  if (InitContext.DLLInitState = 1) and RTLInitFailed then
    // RTL failed to initialized in library startup.  Units have already been
    // finalized, don't finalize them again.
    ExitDll(@InitContext);
{$IFEND LINUX or MACOSX}

  { If there was some kind of runtime error, alert the user }

  if ErrorAddr <> nil then
  begin
    MakeErrorMessage;
    WriteErrorMessage;
    ErrorAddr := nil;
  end;

  { For DLL_THREAD_ATTACH or DLL_THREAD_DETACH, just cleanup and exit }
// MACOSXTODO: double check thread attach/detach for POSIX systems.
{$IFDEF MSWINDOWS}
  if Assigned(DLLThreadContext.ExcFrame) and
    (GetCurrentThreadId = DLLThreadContext.ThreadID) then
  begin
{$IFNDEF PC_MAPPED_EXCEPTIONS}
    UnsetExceptionHandler(@DLLThreadContext);
{$ENDIF !PC_MAPPED_EXCEPTIONS}
    ExitDll(@DLLThreadContext);
  end;
{$ENDIF MSWINDOWS}

  if InitContext.DLLInitState = 0 then
    while ExitProc <> nil do
    begin
      @P := ExitProc;
      ExitProc := nil;
      P;
    end;

  { This loop exists because we might be nested in PackageLoad calls when }
  { Halt got called. We need to unwind these contexts.                    }

  while True do
  begin

    { If we are a library, and we are starting up fine, there are no units to finalize }

    if (InitContext.DLLInitState = 2) and (ExitCode = 0) then
      InitContext.InitCount := 0;

    { Undo any unit initializations accomplished so far }

    FinalizeUnits;

    if (InitContext.DLLInitState <= 1) or (ExitCode <> 0) then
    begin
      if InitContext.Module <> nil then
        with InitContext do
        begin
          UnregisterModule(Module);
{$IFDEF PC_MAPPED_EXCEPTIONS}
          SysUnregisterIPLookup(Module.CodeSegStart);
{$ENDIF PC_MAPPED_EXCEPTIONS}
          if (Module.ResInstance <> Module.Instance) and (Module.ResInstance <> 0) then
            FreeLibrary(Module.ResInstance);
        end;
    end;

{$IFNDEF PC_MAPPED_EXCEPTIONS}
    UnsetExceptionHandler(@InitContext);
{$ENDIF PC_MAPPED_EXCEPTIONS}

{$IFDEF MSWINDOWS}
    if InitContext.DllInitState = 1 then
      InitContext.ExitProcessTLS;
{$ENDIF MSWINDOWS}

    if InitContext.DllInitState <> 0 then
      ExitDll(@InitContext);

    if InitContext.OuterContext = nil then
    begin
      {
        If an ExitProcessProc is set, we call it.  Note that at this
        point the RTL is completely shutdown.  The only thing this is used
        for right now is the proper semantic handling of signals under Linux.
      }
      if Assigned(ExitProcessProc) then
        ExitProcessProc;
      ExitProcess(ExitCode);
    end;

    InitContext := InitContext.OuterContext^
  end;
end;

procedure _Halt;
begin
  ExitCode := Code;
  _Halt0;
end;


procedure _Run0Error;
{$IFDEF PUREPASCAL}
begin
  _RunError(0);   // loses return address
end;
{$ELSE}
asm
        XOR     EAX,EAX
        JMP     _RunError
end;
{$ENDIF}


procedure _RunError(errorCode: Byte);
{$IFDEF PUREPASCAL}
begin
  ErrorAddr := Pointer(-1);  // no return address available
  Halt(errorCode);
end;
{$ELSE}
asm
{$IFDEF PIC}
        PUSH    EAX
        CALL    GetGOT
        MOV     EBX, EAX
        POP     EAX
        MOV     ECX, [EBX].ErrorAddr
        POP     [ECX]
{$ELSE}
        POP     ErrorAddr
{$ENDIF}
        JMP     _Halt
end;
{$ENDIF}

{$IFDEF PC_MAPPED_EXCEPTIONS}
procedure _UnhandledException;
type TExceptProc = procedure (Obj: TObject; Addr: Pointer);
begin
  if Assigned(ExceptProc) then
    TExceptProc(ExceptProc)(ExceptObject, ExceptAddr)
  else
    RunErrorAt(230, ExceptAddr);
end;
{$ENDIF PC_MAPPED_EXCEPTIONS}

procedure _Assert(const Message, Filename: String; LineNumber: Integer);
{$IFDEF PUREPASCAL}
begin
  if Assigned(AssertErrorProc) then
    AssertErrorProc(Message, Filename, LineNumber, Pointer(-1))
  else
    Error(reAssertionFailed);  // loses return address
end;
{$ELSE !PUREPASCAL}
asm
        PUSH    EBX
{$IFDEF PIC}
        PUSH    EAX
        PUSH    ECX
        CALL    GetGOT
        MOV     EBX, EAX
        MOV     EAX, [EBX].AssertErrorProc
        CMP     [EAX], 0
        POP     ECX
        POP     EAX
{$ELSE !PIC}
        CMP     AssertErrorProc,0
{$ENDIF !PIC}
        JNZ     @@1
        MOV     AL,reAssertionFailed
        CALL    Error
        JMP     @@exit

@@1:    PUSH    [ESP+4].Pointer
{$IFDEF PIC}
        MOV     EBX, [EBX].AssertErrorProc
        CALL    [EBX]
{$ELSE !PIC}
        CALL    AssertErrorProc
{$ENDIF !PIC}
@@exit:
        POP     EBX
end;
{$ENDIF !PUREPASCAL}

type
  PThreadRec = ^TThreadRec;
  TThreadRec = record
    {
      WARNING: Don't change these fields without also changing them in
      the C++ RTL : winrtl/source/vcl/crtlvcl.cpp
    }
    Func: TThreadFunc;
    Parameter: Pointer;
  end;

{$IFDEF MSWINDOWS}
function ThreadWrapper(Parameter: Pointer): Integer; stdcall;
{$ELSE}
function ThreadWrapper(Parameter: Pointer): Pointer; cdecl;
{$ENDIF}
asm
{$IFDEF PC_MAPPED_EXCEPTIONS}
        { Mark the top of the stack with a signature }
        PUSH    UNWINDFI_TOPOFSTACK
{$ENDIF PC_MAPPEDEXCEPTIONS}
        CALL    _FpuInit
        PUSH    EBP
{$IFNDEF PC_MAPPED_EXCEPTIONS}
        XOR     ECX,ECX
        PUSH    offset _ExceptionHandler
        MOV     EDX,FS:[ECX]
        PUSH    EDX
        MOV     FS:[ECX],ESP
{$ENDIF !PC_MAPPED_EXCEPTIONS}
{$IFDEF PC_MAPPED_EXCEPTIONS}
    // The signal handling code in SysUtils depends on being able to
    // discriminate between Delphi threads and foreign threads in order
    // to choose the disposition of certain signals.  It does this by
    // testing a TLS index.  However, we allocate TLS in a lazy fashion,
    // so this test can fail unless we've already allocated the TLS segment.
    // So we force the allocation of the TLS index value by touching a TLS
    // value here.  So don't remove this silly call to AreOSExceptionsBlocked.
        CALL    AreOSExceptionsBlocked
{$ENDIF PC_MAPPED_EXCEPTIONS}
        MOV     EAX,Parameter

        MOV     ECX,[EAX].TThreadRec.Parameter
        MOV     EDX,[EAX].TThreadRec.Func
        PUSH    ECX
        PUSH    EDX
        CALL    _FreeMem
        POP     EDX
        POP     EAX
        CALL    EDX

{$IFNDEF PC_MAPPED_EXCEPTIONS}
        XOR     EDX,EDX
        POP     ECX
        MOV     FS:[EDX],ECX
        POP     ECX
{$ENDIF !PC_MAPPED_EXCEPTIONS}
        POP     EBP
{$IFDEF PC_MAPPED_EXCEPTIONS}
        { Ditch our TOS marker }
        ADD     ESP, 4
{$ENDIF PC_MAPPED_EXCEPTIONS}
end;


{$IFDEF MSWINDOWS}
function BeginThread(SecurityAttributes: Pointer; StackSize: LongWord;
  ThreadFunc: TThreadFunc; Parameter: Pointer; CreationFlags: LongWord;
  var ThreadId: LongWord): Integer;
var
  P: PThreadRec;
begin
  if Assigned(SystemThreadFuncProc) then
    P := PThreadRec(SystemThreadFuncProc(ThreadFunc, Parameter))
  else
  begin
    New(P);
    P.Func := ThreadFunc;
    P.Parameter := Parameter;
  end;
  IsMultiThread := TRUE;
  Result := CreateThread(SecurityAttributes, StackSize, @ThreadWrapper, P,
    CreationFlags, ThreadID);
end;


procedure EndThread(ExitCode: Integer);
begin
  if Assigned(SystemThreadEndProc) then
    SystemThreadEndProc(ExitCode);
  ExitThread(ExitCode);
end;
{$ENDIF}

{$IFDEF POSIX}
function BeginThread(Attribute: PThreadAttr;
                     ThreadFunc: TThreadFunc;
                     Parameter: Pointer;
                     var ThreadId: Cardinal): Integer;
var
  P: PThreadRec;
begin
  if Assigned(BeginThreadProc) then
    Result := BeginThreadProc(Attribute, ThreadFunc, Parameter, ThreadId)
  else
  begin
    New(P);
    P.Func := ThreadFunc;
    P.Parameter := Parameter;
    IsMultiThread := True;
    Result := _pthread_create(ThreadID, Attribute, @ThreadWrapper, P);
  end;
end;

procedure EndThread(ExitCode: Integer);
begin
  if Assigned(EndThreadProc) then
    EndThreadProc(ExitCode);
  // No "else" required since EndThreadProc does not (!!should not!!) return.
  _pthread_detach(GetCurrentThreadID);
  _pthread_exit(ExitCode);
end;
{$ENDIF POSIX}

procedure _LStrClr(var S);
{$IFDEF PUREPASCAL}
var
  P: PStrRec;
begin
  if Pointer(S) <> nil then
  begin
    P := Pointer(Integer(S) - Sizeof(StrRec));
    Pointer(S) := nil;
    if P.refCnt > 0 then
      if InterlockedDecrement(P.refCnt) = 0 then
        FreeMem(P);
  end;
end;
{$ELSE}
asm
        { ->    EAX pointer to str      }

        MOV     EDX,[EAX]                       { fetch str                     }
        TEST    EDX,EDX                         { if nil, nothing to do         }
        JE      @@done
        MOV     dword ptr [EAX],0               { clear str                     }
        MOV     ECX,[EDX-skew].StrRec.refCnt    { fetch refCnt                  }
        DEC     ECX                             { if < 0: literal str           }
        JL      @@done
   LOCK DEC     [EDX-skew].StrRec.refCnt        { threadsafe dec refCount       }
        JNE     @@done
        PUSH    EAX
        LEA     EAX,[EDX-skew].StrRec.codePage    { if refCnt now zero, deallocate}
        CALL    _FreeMem
        POP     EAX
@@done:
end;
{$ENDIF}

procedure       _LStrArrayClr(var StrArray; cnt: longint);
{$IFDEF PUREPASCAL}
var
  P: Pointer;
begin
  P := @StrArray;
  while cnt > 0 do
  begin
    _LStrClr(P^);
    Dec(cnt);
    Inc(Integer(P), sizeof(Pointer));
  end;
end;
{$ELSE}
asm
        { ->    EAX pointer to str      }
        {       EDX cnt         }

        PUSH    EBX
        PUSH    ESI
        MOV     EBX,EAX
        MOV     ESI,EDX

@@loop:
        MOV     EDX,[EBX]                       { fetch str                     }
        TEST    EDX,EDX                         { if nil, nothing to do         }
        JE      @@doneEntry
        MOV     dword ptr [EBX],0               { clear str                     }
        MOV     ECX,[EDX-skew].StrRec.refCnt    { fetch refCnt                  }
        DEC     ECX                             { if < 0: literal str           }
        JL      @@doneEntry
   LOCK DEC     [EDX-skew].StrRec.refCnt        { threadsafe dec refCount       }
        JNE     @@doneEntry
        LEA     EAX,[EDX-skew].StrRec.codePage  { if refCnt now zero, deallocate}
        CALL    _FreeMem
@@doneEntry:
        ADD     EBX,4
        DEC     ESI
        JNE     @@loop

        POP     ESI
        POP     EBX
end;
{$ENDIF}

{ 99.03.11
  This function is used when assigning to global variables.

  Literals are copied to prevent a situation where a dynamically
  allocated DLL or package assigns a literal to a variable and then
  is unloaded -- thereby causing the string memory (in the code
  segment of the DLL) to be removed -- and therefore leaving the
  global variable pointing to invalid memory.
}
procedure _LStrAsg(var dest; const source);
{$IFDEF PUREPASCAL}
var
  S, D: Pointer;
  P: PStrRec;
  Temp: Longint;
begin
  S := Pointer(source);
  if S <> nil then
  begin
    P := PStrRec(Integer(S) - sizeof(StrRec));
    if P.refCnt < 0 then   // make copy of string literal
    begin
      Temp := P.length;
      S := _NewAnsiString(Temp, P.codePage);
      Move(Pointer(source)^, S^, Temp);
      P := PStrRec(Integer(S) - sizeof(StrRec));
    end;
    InterlockedIncrement(P.refCnt);
  end;

  D := Pointer(dest);
  Pointer(dest) := S;
  if D <> nil then
  begin
    P := PStrRec(Integer(D) - sizeof(StrRec));
    if P.refCnt > 0 then
      if InterlockedDecrement(P.refCnt) = 0 then
        FreeMem(P);
  end;
end;
{$ELSE}
asm
        { ->    EAX pointer to dest   str      }
        { ->    EDX pointer to source str      }

                TEST    EDX,EDX                           { have a source? }
                JE      @@2                               { no -> jump     }

                CMP     [EDX-Skew].StrRec.elemSize,1
                JE      @@isAnsi
                MOV     ECX,DefaultSystemCodePage
                JMP     _LStrFromUStr

@@isAnsi:
                MOV     ECX,[EDX-skew].StrRec.refCnt
                INC     ECX
                JG      @@1                               { literal string -> jump not taken }

                PUSH    EAX
                PUSH    EDX
                MOV     EAX,[EDX-skew].StrRec.length
                MOVZX   EDX,[EDX-skew].StrRec.codePage
                CALL    _NewAnsiString
                MOV     EDX,EAX
                POP     EAX
                PUSH    EDX
                MOV     ECX,[EAX-skew].StrRec.length
                CALL    Move
                POP     EDX
                POP     EAX
                JMP     @@2

@@1:
           LOCK INC     [EDX-skew].StrRec.refCnt

@@2:            XCHG    EDX,[EAX]
                TEST    EDX,EDX
                JE      @@3
                MOV     ECX,[EDX-skew].StrRec.refCnt
                DEC     ECX
                JL      @@3
           LOCK DEC     [EDX-skew].StrRec.refCnt
                JNE     @@3
                LEA     EAX,[EDX-skew].StrRec.codePage
                CALL    _FreeMem
@@3:
end;
{$ENDIF}

procedure _LStrLAsg(var dest; const source);
{$IFDEF PUREPASCAL}
var
  P: Pointer;
begin
  P := Pointer(source);
  _LStrAddRef(P);
  P := Pointer(dest);
  Pointer(dest) := Pointer(source);
  _LStrClr(P);
end;
{$ELSE}
asm
{ ->    EAX     pointer to dest }
{       EDX     source          }

                TEST    EDX,EDX
                JE      @@sourceDone

                CMP     [EDX-Skew].StrRec.elemSize,1
                JE      @@isAnsi
                MOV     ECX,DefaultSystemCodePage
                JMP     _LStrFromUStr

@@isAnsi:
                { bump up the ref count of the source }

                MOV     ECX,[EDX-skew].StrRec.refCnt
                INC     ECX
                JLE     @@sourceDone                    { literal assignment -> jump taken }
           LOCK INC     [EDX-skew].StrRec.refCnt
@@sourceDone:

                { we need to release whatever the dest is pointing to   }

                XCHG    EDX,[EAX]                       { fetch str                    }
                TEST    EDX,EDX                         { if nil, nothing to do        }
                JE      @@done
                MOV     ECX,[EDX-skew].StrRec.refCnt    { fetch refCnt                 }
                DEC     ECX                             { if < 0: literal str          }
                JL      @@done
           LOCK DEC     [EDX-skew].StrRec.refCnt        { threadsafe dec refCount      }
                JNE     @@done
                LEA     EAX,[EDX-skew].StrRec.codePage  { if refCnt now zero, deallocate}
                CALL    _FreeMem
@@done:
end;
{$ENDIF}

function _NewAnsiString(length: Longint; CodePage: Word): Pointer;
{$IFDEF PUREPASCAL}
var
  P: PStrRec;
begin
  Result := nil;
  if length <= 0 then Exit;
  // Alloc an extra null for strings with even length.  This has no actual cost
  // since the allocator will round up the request to an even size anyway.
  // All widestring allocations have even length, and need a double null terminator.
  GetMem(P, length + sizeof(StrRec) + 1 + ((length + 1) and 1));
  Result := Pointer(Integer(P) + sizeof(StrRec));
  P.length := length;
  P.refcnt := 1;
  P.codePage := Word(DefaultSystemCodePage);
  P.elemSize := 1;
  PWideChar(Result)[length div 2] := #0;  // length guaranteed >= 2
end;
{$ELSE}
asm
  { ->    EAX     length                  }
  { <-    EAX pointer to new string       }

          TEST    EAX,EAX
          JLE     @@lengthLEZero
          PUSH    EAX
          ADD     EAX,rOff+2                       // one or two nulls (Ansi/Wide)
          JO      @@overflow
          AND     EAX, not 1                   // round up to even length
          PUSH    EDX
          PUSH    EAX
          CALL    _GetMem
          POP     EDX                              // actual allocated length (>= 2)
          POP     ECX
          MOV     word ptr [EAX+EDX-2],0           // double null terminator
          ADD     EAX,rOff
          POP     EDX                              // requested string length
          MOV     [EAX-skew].StrRec.length,EDX
          MOV     [EAX-skew].StrRec.refCnt,1
          TEST    ECX,ECX
          JNE     @@NotDefault
          MOV     ECX,DefaultSystemCodePage
@@NotDefault:
          MOV     EDX,ECX
          MOV     word ptr [EAX-skew].StrRec.codePage,DX
          MOV     word ptr [EAX-skew].StrRec.elemSize,1
          RET

@@overflow:
          JMP     _IntOver

@@lengthLEZero:
          XOR     EAX,EAX
end;
{$ENDIF}

procedure _LStrFromPCharLen(var Dest: AnsiString; Source: PAnsiChar; Length: Integer; CodePage: Word);
asm
  { ->    EAX     pointer to dest }
  {       EDX source              }
  {       ECX length              }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     EBX,EAX
        MOV     ESI,EDX
        MOV     EDI,ECX

        { allocate new string }

        MOV     EAX,EDI
        MOVZX   EDX,CodePage

        CALL    _NewAnsiString
        MOV     ECX,EDI
        MOV     EDI,EAX

        TEST    ESI,ESI
        JE      @@noMove

        MOV     EDX,EAX
        MOV     EAX,ESI
        CALL    Move

        { assign the result to dest }

@@noMove:
        MOV     EAX,EBX
        CALL    _LStrClr
        MOV     [EBX],EDI

        POP     EDI
        POP     ESI
        POP     EBX
end;

{$IFDEF POSIX}
procedure LocaleConversionError;
begin
  Error(reCodesetConversion);
end;

type
  TCharacterSizeProc = function(P: Pointer; MaxLen: Integer): Integer;

function CharacterSizeWideChar(P: Pointer; MaxLen: Integer): Integer;
begin
  Result := SizeOf(WideChar);
end;

function CharacterSizeLocaleChar(P: Pointer; MaxLen: Integer): Integer;
begin
  Assert(Assigned(P));
  Result := mblen(P, MaxLen);
  if Result <= 0 then
  begin
    mblen(nil, 0);
    Result := SizeOf(Char);
  end;
end;

function BufConvert(var Dest;   DestBytes: Integer;
                    const Source; SrcBytes: Integer;
                    context: _Ticonv_t;
                    DestCharSize: Integer;
                    SourceCharSize: TCharacterSizeProc): Integer;
const
  E2BIG = 7;
  EINVAL = 22;
  EILSEQ = 84;
const
  UnknownCharIndicator = '?';
var
  SrcBytesLeft, DestBytesLeft, Zero: Integer;
  s, d, pNil: Pointer;
  LastError: Integer;
  cs: Integer;
begin
  Result := -1;

  // Make copies of parameters. iconv modifies param pointers.
  DestBytesLeft := DestBytes;
  SrcBytesLeft := SrcBytes;
  s := Pointer(Source);
  d := Pointer(Dest);

  while True do
  begin
    Result := iconv(context, s, SrcBytesLeft, d, DestBytesLeft);
    if Result <> -1 then
      Break
    else
    begin
      LastError := GetLastError;
      if (LastError = E2BIG) and (SrcBytesLeft > 0) and (DestBytesLeft > 0) then
        Continue;

      if (LastError <> EINVAL) and (LastError <> EILSEQ) then
        LocaleConversionError;
      pNil := nil;
      Zero := 0;
      iconv(context, pNil, Zero, pNil, Zero); // Reset state of context

      // Invalid input character in conversion stream.
      // Skip input character and write '?' to output stream.
      // The glibc iconv() implementation also returns EILSEQ
      // for a valid input character that cannot be converted
      // into the requested codeset.
      cs := SourceCharSize(s, SrcBytesLeft);
      Inc(Cardinal(s), cs);
      Dec(SrcBytesLeft, cs);

      Assert(DestCharSize in [1, 2]);
      case DestCharSize of
        1:
          begin
            PAnsiChar(d)^ := UnknownCharIndicator;
            Inc(PAnsiChar(d));
            Dec(DestBytesLeft, SizeOf(AnsiChar));
          end;

        2:
          begin
            PWideChar(d)^ := UnknownCharIndicator;
            Inc(PWideChar(d));
            Dec(DestBytesLeft, SizeOf(WideChar));
          end;
      end;
    end;
  end;

  if Result <> -1 then
    Result := DestBytes - DestBytesLeft;
end;
{$ENDIF POSIX}

{$IFDEF POSIX}
{
 CharFromWChar is used now in a dual mode.  In one mode, it calculates the length
 of the buffer you need to allocate for the destination.  In another, it actually
 does the conversion.  Now, Windows provides a nice API for doing both of these
 steps:  WideCharToMultiByte.  The POSIX APIs (iconv) don't appear to provide an
 easy way to do this.  Since we've moved to unicode strings for internal string
 representation, this API gets a lot of use when writing things to stdout, etc.
 In the interests of getting hello world up and running, I've hacked the implementation
 here to just take every other character, and be done with it.  Allen will have to
 come back and address this.
}
{$DEFINE HACK_POSIX_UNICODE}
{$ENDIF}

function CharFromWChar(CharDest: PAnsiChar; DestBytes: Integer; const WCharSource: PWideChar; SrcChars: Integer; CodePage: Integer): Integer;
{$IFDEF POSIX}
var
  IconvContext: _Ticonv_t;
{$IFDEF HACK_POSIX_UNICODE}
  P: PAnsiChar;
{$ENDIF HACK_POSIX_UNICODE}
{$ENDIF POSIX}
begin
{$IFDEF POSIX}
{$IFDEF HACK_POSIX_UNICODE}
   if CharDest = nil then
   begin
      // someone is asking for the length
      Result := SrcChars;
   end
   else
   begin
      if (SrcChars > DestBytes) then
         SrcChars := DestBytes;
         P := PAnsiChar(WCharSource);
      while SrcChars > 0 do begin
         CharDest^ := P^;
         Dec(SrcChars);
         Inc(CharDest);
         Inc(P, 2);
      end;
   end;
{$ELSE !HACK_POSIX_UNICODE}
  if (DestBytes <> 0) and (SrcChars <> 0) then
  begin
    IconvContext := iconv_open(nl_langinfo(_NL_CTYPE_CODESET_NAME), 'UNICODELITTLE');
    if IconvContext = _Ticonv_t(-1) then
      LocaleConversionError;
    try
      Result := BufConvert(CharDest, DestBytes, WCharSource, SrcChars * SizeOf(WideChar),
         IconvContext, 1, CharacterSizeWideChar);
    finally
      iconv_close(IconvContext);
    end;
  end
  else
    Result := 0;
{$ENDIF !HACK_POSIX_UNICODE}
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
  if CodePage = 0 then
    CodePage := DefaultSystemCodePage;
  Result := WideCharToMultiByte(CodePage, 0, WCharSource, SrcChars, CharDest,
    DestBytes, nil, nil);
{$ENDIF MSWINDOWS}
end;

function CharFromWChar(CharDest: PAnsiChar; DestBytes: Integer; const WCharSource: PWideChar; SrcChars: Integer): Integer;
begin
  Result := CharFromWChar(CharDest, DestBytes, WCharSource, SrcChars, DefaultSystemCodePage);
end;

function WCharFromChar(WCharDest: PWideChar; DestChars: Integer; const CharSource: PAnsiChar; SrcBytes: Integer; CodePage: Integer): Integer;
{$IFDEF POSIX}
var
  IconvContext: _Ticonv_t;
{$ENDIF POSIX}
begin
{$IFDEF POSIX}
  if (DestChars <> 0) and (SrcBytes <> 0) then
  begin
     // MACOSXTODO: fix nl_langinfo
    IconvContext := iconv_open('UNICODELITTLE', nl_langinfo(_NL_CTYPE_CODESET_NAME));
    if IconvContext = _Ticonv_t(-1) then
      LocaleConversionError;
    try
      Result := BufConvert(WCharDest, DestChars * SizeOf(WideChar), CharSource, SrcBytes,
        IconvContext, 2, CharacterSizeLocaleChar);
    finally
      iconv_close(IconvContext);
    end;
    if Result <> -1 then
      Result := Result div SizeOf(WideChar);
  end
  else
    Result := 0;
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
  Result := MultiByteToWideChar(CodePage, 0, CharSource, SrcBytes, WCharDest,
    DestChars);
{$ENDIF}
end;

procedure EnsureAnsiString(var S: UnicodeString);
begin
  _LStrFromUStr(AnsiString(Pointer(S)), S, DefaultSystemCodePage);
end;

procedure EnsureUnicodeString(var S: AnsiString);
begin
  _UStrFromLStr(UnicodeString(Pointer(S)), S);
end;

function _InternalUStrFromLStr(var Dest: UnicodeString; const Source: AnsiString): Pointer;
begin
  _UStrFromLStr(Dest, Source);
  Result := Pointer(Dest);
end;

function _EnsureUnicodeString(var S: UnicodeString): Pointer;
begin
  Result := Pointer(S);
  if (Result <> nil) and (PWord(Integer(Result) - 10)^ <> 2) then    // StrRec.elemSize
    Result := _InternalUStrFromLStr(S, AnsiString(Pointer(S)));
end;

function _InternalLStrFromUStr(var Dest: AnsiString; const Source: UnicodeString; CodePage: Word): Pointer;
begin
  _LStrFromUStr(Dest, Source, CodePage);
  Result := Pointer(Dest);
end;

function _EnsureAnsiString(var S: AnsiString; CodePage: Word): Pointer;
begin
  Result := Pointer(S);
  if (Result <> nil) and (PWord(Integer(Result) - 10)^ <> 1) then    // StrRec.elemSize
    Result := _InternalLStrFromUStr(S, UnicodeString(Pointer(S)), CodePage);
end;

function StringElementSize(const S: UnicodeString): Word; overload;
begin
  if S <> '' then
    Result := PWord(Integer(S) - 10)^                            // StrRec.elemSize
  else
    Result := SizeOf(WideChar);
end;

function StringElementSize(const S: RawByteString): Word; overload;
begin
  if S <> '' then
    Result := PWord(Integer(S) - 10)^                           // StrRec.elemSize
  else
    Result := SizeOf(AnsiChar);
end;

function StringCodePage(const S: UnicodeString): Word; overload;
begin
  if S <> '' then
    Result := PWord(Integer(S) - 12)^                          // StrRec.codePage
  else
    Result := Word(DefaultUnicodeCodePage);
end;

function StringCodePage(const S: RawByteString): Word; overload;
begin
  if S <> '' then
    Result := PWord(Integer(S) - 12)^                          // StrRec.codePage
  else
    Result := Word(DefaultSystemCodePage);
end;

function StringRefCount(const S: UnicodeString): Longint;
begin
  Result := Longint(S);
  if Result <> 0 then                // PStrRec should be used here, but
    Result := PLongint(Result - 8)^; // a private symbol can't be inlined
end;

function StringRefCount(const S: RawByteString): Longint;
begin
  Result := Longint(S);
  if Result <> 0 then                // PStrRec should be used here, but
    Result := PLongint(Result - 8)^; // a private symbol can't be inlined
end;

procedure _LStrFromPWCharLen(var Dest: AnsiString; Source: PWideChar; Length: Integer; CodePage: Word);
var
  DestLen: Integer;
begin
  if Length <= 0 then
  begin
    _LStrClr(Dest);
    Exit;
  end;

  if CodePage = 0 then
    CodePage := DefaultSystemCodePage;

  DestLen := CharFromWChar(nil, 0, Source, Length, CodePage);
  SetLength(Dest, DestLen);
  if DestLen > 0 then
  begin
    CharFromWChar(Pointer(Dest), DestLen, Source, Length, CodePage);
    PStrRec(Integer(Pointer(Dest)) - skew).codePage := CodePage;
  end
  else
    _LStrClr(Dest);
end;

procedure SetAnsiString(Dest: PAnsiString; Source: PWideChar; Length: Integer; CodePage: Word);
begin
  _LStrFromPWCharLen(Dest^, Source, Length, CodePage);
end;

procedure _LStrFromChar(var Dest: AnsiString; Source: AnsiChar; CodePage: Word);
asm
    PUSH    EDX
    MOV     EDX,ESP
    PUSH    ECX
    MOV     ECX,1
    CALL    _LStrFromPCharLen
    POP     EDX
end;


procedure _LStrFromWChar(var Dest: AnsiString; Source: WideChar; CodePage: Word);
asm
    PUSH    EDX
    MOV     EDX,ESP
    PUSH    ECX
    MOV     ECX,1
    CALL    _LStrFromPWCharLen
    POP     EDX
end;


procedure _LStrFromPChar(var Dest: AnsiString; Source: PAnsiChar; CodePage: Word);
asm
        XCHG    ECX,[ESP]
        PUSH    ECX
        XOR     ECX,ECX
        TEST    EDX,EDX
        JE      @@5
        PUSH    EDX
@@0:    CMP     CL,[EDX+0]
        JE      @@4
        CMP     CL,[EDX+1]
        JE      @@3
        CMP     CL,[EDX+2]
        JE      @@2
        CMP     CL,[EDX+3]
        JE      @@1
        ADD     EDX,4
        JMP     @@0
@@1:    INC     EDX
@@2:    INC     EDX
@@3:    INC     EDX
@@4:    MOV     ECX,EDX
        POP     EDX
        SUB     ECX,EDX
@@5:    JMP     _LStrFromPCharLen
end;


procedure _LStrFromPWChar(var Dest: AnsiString; Source: PWideChar; CodePage: Word);
asm
        XCHG    ECX,[ESP]
        PUSH    ECX
        XOR     ECX,ECX
        TEST    EDX,EDX
        JE      @@5
        PUSH    EDX
@@0:    CMP     CX,[EDX+0]
        JE      @@4
        CMP     CX,[EDX+2]
        JE      @@3
        CMP     CX,[EDX+4]
        JE      @@2
        CMP     CX,[EDX+6]
        JE      @@1
        ADD     EDX,8
        JMP     @@0
@@1:    ADD     EDX,2
@@2:    ADD     EDX,2
@@3:    ADD     EDX,2
@@4:    MOV     ECX,EDX
        POP     EDX
        SUB     ECX,EDX
        SHR     ECX,1
@@5:    JMP     _LStrFromPWCharLen
end;


procedure _LStrFromString(var Dest: AnsiString; const Source: ShortString; CodePage: Word);
asm
        XCHG    ECX,[ESP]
        PUSH    ECX
        XOR     ECX,ECX
        MOV     CL,[EDX]
        INC     EDX
        JMP     _LStrFromPCharLen
end;

procedure _LStrFromArray(var Dest: AnsiString; Source: PAnsiChar; Length: Integer; CodePage: Word);
asm
        PUSH    EDI
        PUSH    EAX
        PUSH    ECX
        MOV     EDI,EDX
        XOR     EAX,EAX
        REPNE   SCASB
        JNE     @@1
        NOT     ECX
@@1:    POP     EAX
        ADD     ECX,EAX
        POP     EAX
        POP     EDI
        POP     EBP
        JMP     _LStrFromPCharLen
end;


procedure _LStrFromWArray(var Dest: AnsiString; Source: PWideChar; Length: Integer; CodePage: Word);
asm
        PUSH    EDI
        PUSH    EAX
        PUSH    ECX
        MOV     EDI,EDX
        XOR     EAX,EAX
        REPNE   SCASW
        JNE     @@1
        NOT     ECX
@@1:    POP     EAX
        ADD     ECX,EAX
        POP     EAX
        POP     EDI
        POP     EBP
        JMP     _LStrFromPWCharLen
end;


procedure _LStrFromWStr(var Dest: AnsiString; const Source: WideString; CodePage: Word);
asm
        { ->    EAX pointer to dest              }
        {       EDX pointer to WideString data   }
        {       ECX CodePage                     }

        XCHG    ECX,[ESP]
        PUSH    ECX
        XOR     ECX,ECX
        TEST    EDX,EDX
        JE      @@1
        MOV     ECX,[EDX-4]
        SHR     ECX,1
@@1:    JMP     _LStrFromPWCharLen
end;


procedure _LStrToString{(var Dest: ShortString; const Source: AnsiString; MaxLen: Integer)};
asm
        { ->    EAX pointer to result   }
        {       EDX AnsiString s        }
        {       ECX length of result    }

        PUSH    EBX
        TEST    EDX,EDX
        JE      @@empty
        MOV     EBX,[EDX-skew].StrRec.length
        TEST    EBX,EBX
        JE      @@empty

        CMP     ECX,EBX
        JL      @@truncate
        MOV     ECX,EBX
@@truncate:
        MOV     [EAX],CL
        INC     EAX

        XCHG    EAX,EDX
        CALL    Move

        JMP     @@exit

@@empty:
        MOV     byte ptr [EAX],0

@@exit:
        POP     EBX
end;

function _LStrLen(const S: AnsiString): Longint;
begin
  Result := Longint(S);
  if Result <> 0 then                // PStrRec should be used here, but
    Result := PLongint(Result - 4)^; // a private symbol can't be inlined
end;

function _PCharLen(P: PAnsiChar): Longint;
asm
        TEST    EAX,EAX
        JE      @@5
        PUSH    EAX
        XOR     ECX,ECX
@@0:    CMP     CL,[EAX+0]
        JE      @@4
        CMP     CL,[EAX+1]
        JE      @@3
        CMP     CL,[EAX+2]
        JE      @@2
        CMP     CL,[EAX+3]
        JE      @@1
        ADD     EAX,4
        JMP     @@0
@@1:    INC     EAX
@@2:    INC     EAX
@@3:    INC     EAX
@@4:    POP     ECX
        SUB     EAX,ECX
@@5:
end;

procedure       _LStrCat{var dest: AnsiString; source: AnsiString};
asm
        { ->    EAX     pointer to dest }
        {       EDX source              }

        TEST    EDX,EDX
        JE      @@exit

        MOV     ECX,[EAX]
        TEST    ECX,ECX
        JE      _LStrAsg

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EBX,EAX
        MOV     ESI,EDX
        MOV     EDI,ECX
        CMP     ESI,ECX
        JE      @@appendSelf

        CMP     [ECX-skew].StrRec.elemSize,1
        JE      @@destIsAnsi
        XOR     EDX,EDX
        CMP     [ESI-skew].StrRec.elemSize,1
        JNE     @@sourceNotAnsi
        MOVZX   EDX,[ESI-skew].StrRec.codePage

@@sourceNotAnsi:
        CALL    _EnsureAnsiString
        MOV     EDI,EAX
        MOV     ECX,EAX

@@destIsAnsi:
        PUSH    0
        CMP     [ESI-skew].StrRec.elemSize,1
        JE      @@sourceIsAnsi

        MOV     EAX,ESI
        MOV     [ESP],ESI
        CALL    _LStrAddRef
        MOV     EAX,ESP
        MOVZX   EDX,[EDI-skew].StrRec.codePage
        CALL    _EnsureAnsiString
        MOV     ESI,[ESP]
        MOV     ECX,EDI

@@sourceIsAnsi:
        MOV     EDI,[ECX-skew].StrRec.length
        MOV     EDX,[ESI-skew].StrRec.length
        ADD     EDX,EDI
        JO      @@lengthOverflow
        MOV     EAX,EBX
        MOVZX   ECX,[ECX-skew].StrRec.codePage
        CALL    _LStrSetLength
        MOV     EAX,ESI
        MOV     ECX,[ESI-skew].StrRec.length

@@noTemp:
        MOV     EDX,[EBX]
        ADD     EDX,EDI
        CALL    Move
        MOV     EAX,ESP                      // Need to clear out the temp we may have created above
        MOV     EDX,[EAX]
        TEST    EDX,EDX
        JE      @@tempEmpty

        CALL    _LStrClr

@@tempEmpty:
        POP     EAX
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@appendSelf:
        CMP     [ECX-skew].StrRec.elemSize,1
        JE      @@selfIsAnsi
        MOV     EAX,EBX
        XOR     EDX,EDX
        CALL    _EnsureAnsiString
        MOV     ECX,EAX
        MOV     EAX,EBX

@@selfIsAnsi:
        MOV     EDI,[ECX-skew].StrRec.length
        MOV     EDX,EDI
        SHL     EDX,1
        JO      @@lengthOverflow
        MOVZX   ECX,[ECX-skew].StrRec.codePage
        CALL    _LStrSetLength
        MOV     EAX,[EBX]
        MOV     ECX,EDI
        PUSH    0
        JMP     @@noTemp

@@lengthOverflow:
        JMP     _IntOver

@@exit:
end;


procedure       _LStrCat3{var dest:AnsiString; source1: AnsiString; source2: AnsiString};
asm
        {     ->EAX = Pointer to dest   }
        {       EDX = source1           }
        {       ECX = source2           }

        TEST    EDX,EDX
        JE      @@assignSource2

        TEST    ECX,ECX
        JE      _LStrAsg

        CMP     EDX,[EAX]
        JE      @@appendToDest

        CMP     ECX,[EAX]
        JE      @@theHardWay

        PUSH    EAX
        PUSH    ECX
        CALL    _LStrAsg

        POP     EDX
        POP     EAX
        JMP     _LStrCat

@@theHardWay: // s(*EAX,ECX) := source1(EDX) + s(ECX)

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     EBX,EDX
        MOV     ESI,ECX
        PUSH    EAX

        PUSH    0               // Need temps - source1
        PUSH    0               // source2
        CMP     [ESI-Skew].StrRec.elemSize,1
        JE      @@source2IsAnsi

        MOV     [ESP],ESI
        MOV     EAX,ESI
        CALL    _LStrAddRef
        XOR     EDX,EDX           //  Just use default
        CMP     [EBX-skew].StrRec.elemSize,1
        JNE     @@defCodePage
        MOVZX   EDX,[EBX-skew].StrRec.codePage
@@defCodePage:
        MOV     EAX,ESP
        CALL    EnsureAnsiString  //  Convert it to Ansi
        MOV     ESI,[ESP]

@@source2IsAnsi:
        CMP     [EBX-Skew].StrRec.elemSize,1
        JE      @@source1IsAnsi

        MOV     [ESP+4],EBX
        MOV     EAX,EBX
        CALL    _LStrAddRef
        MOVZX   EDX,[ESI-skew].StrRec.codePage // use whatever codepage source2 is
        LEA     EAX,[ESP+4]
        CALL    _EnsureAnsiString
        MOV     EBX,[ESP+4]

@@source1IsAnsi:
        MOV     EAX,[EBX-skew].StrRec.length
        ADD     EAX,[ESI-skew].StrRec.length
        JO      @@overflow
        MOVZX   EDX,[ESI-skew].StrRec.codePage // use source2's codepage
        CALL    _NewAnsiString

        MOV     EDI,EAX

        MOV     EDX,EDI
        MOV     EAX,EBX
        MOV     ECX,[EBX-skew].StrRec.length
        CALL    Move

        MOV     EDX,EDI
        MOV     EAX,ESI
        MOV     ECX,[ESI-skew].StrRec.length
        ADD     EDX,[EBX-skew].StrRec.length
        CALL    Move

        MOV     EAX,[ESP]
        OR      EAX,[ESP+4]
        JZ      @@noclear

        MOV     EAX,ESP
        MOV     EDX,2
        CALL    _LStrArrayClr   // Clean up the temps

@@noclear:
        POP     EAX             // Remove the temps from the stack
        POP     EAX
        POP     EAX
        MOV     EDX,EDI
        TEST    EDI,EDI
        JE      @@skip
        DEC     [EDI-skew].StrRec.refCnt    // EDI = local temp str - pass this reference to the caller
@@skip:
        CALL    _LStrAsg

        POP     EDI
        POP     ESI
        POP     EBX

        JMP     @@exit

@@assignSource2:
        MOV     EDX,ECX
        JMP     _LStrAsg

@@appendToDest:
        MOV     EDX,ECX
        JMP     _LStrCat

@@overflow:
        JMP     _IntOver

@@exit:
end;


procedure       _LStrCatN{var dest:AnsiString; argCnt: Integer; ...};
asm
        {     ->EAX = Pointer to dest   }
        {       EDX = number of args (>= 3)     }
        {       [EBP+8], [EBP+12], ... crgCnt AnsiString arguments, reverse order }

        PUSH    EBP
        MOV     EBP,ESP
        PUSH    0                  // Need a place to preserve the first arg's codepage
        PUSH    0                  // Place to save the current string param index
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EDX
        PUSH    EAX
        MOV     EBX,EDX

        XOR     EDI,EDI
        PUSH    0                 // Potential temp
        MOV     ECX,[EBP+EDX*4+4] // first arg is furthest out
        TEST    ECX,ECX
        JZ      @@0
        CMP     [ECX-skew].StrRec.elemSize,1
        JE      @@argIsAnsi

        MOV     ESI,EAX
        MOV     EAX,[EBP+EDX*4+4]
        CALL    _LStrAddRef
        MOV     EDX,EBX
        LEA     EAX,[EBP+EDX*4+4]
        MOV     EDX,[EBP-4]
        CALL    _EnsureAnsiString
        MOV     ECX,EAX
        MOV     EDX,EBX
        MOV     [ESP],EAX
        MOV     EAX,ESI
        JMP     @@noCodePage

@@argIsAnsi:
        MOVZX   ESI,[ECX-skew].StrRec.codePage
        MOV     [EBP-4],ESI      // Save the first arg's code page in case we need to copy
@@noCodePage:
        CMP     [EAX],ECX          // is dest = first arg?
        JNE     @@firstNotDest
        MOV     EDI,ECX            // EDI nonzero -> potential appendstr case
        MOV     EAX,[ECX-skew].StrRec.length
        DEC     EDX
        JMP     @@loop1
@@0:
        XOR     EAX,EAX
        JMP     @@2

@@firstNotDest:
        XOR     EAX,EAX
        JMP     @@skipPush

@@loop1:
        PUSH    0

@@skipPush:
        MOV     ECX,[EBP+EDX*4+4]
        TEST    ECX,ECX
        JE      @@2
        CMP     [ECX-skew].StrRec.elemSize,1
        JE      @@nStrIsAnsi

        MOV     ESI,EAX
        MOV     [EBP-8],EDX
        MOV     EAX,[EBP+EDX*4+4]
        CALL    _LStrAddRef
        MOV     EDX,[EBP-8]
        LEA     EAX,[EBP+EDX*4+4]
        MOV     EDX,[EBP-4]
        CALL    _EnsureAnsiString
        MOV     EDX,[EBP-8]
        MOV     ECX,EAX
        MOV     [ESP],EAX
        MOV     EAX,ESI

@@nStrIsAnsi:
        ADD     EAX,[ECX-skew].StrRec.length
        JO      @@overflow
        CMP     [EBP-4],0      // Have we already found a valid codepage?
        JNZ     @@1
        MOVZX   ESI,[ECX-skew].StrRec.codePage // Save the first non-blank arg we find's codepage
        MOV     [EBP-4],ESI
@@1:
        CMP     EDI,ECX          // is dest an arg besides arg1?
        JNE     @@2
        XOR     EDI,EDI          // can't appendstr - dest is multiple args
@@2:
        DEC     EDX
        JNE     @@loop1

@@append:
        TEST    EDI,EDI          // dest is 1st and only 1st arg?
        JZ      @@copy
        MOV     EDX,EAX          // length into EDX
        MOV     EAX,[EBP-7*4]    // ptr to str into EAX
        MOV     ESI,[EDI-skew].StrRec.Length  // save old size before realloc
        MOVZX   ECX,[EDI-skew].StrRec.codePage
        CALL    _LStrSetLength
        MOV     EDI,[EBP-7*4]    // append other strs to dest
        PUSH    [EDI]
        ADD     ESI,[EDI]        // ESI = end of old string
        DEC     EBX
        JMP     @@loop2

@@copy:
        MOV     EDX,[EBP-4]
        CALL    _NewAnsiString
        PUSH    EAX
        MOV     ESI,EAX

@@loop2:
        MOV     EAX,[EBP+EBX*4+4]
        TEST    EAX,EAX
        JZ      @@3
        MOV     EDX,ESI
        TEST    EAX,EAX
        JE      @@3
        MOV     ECX,[EAX-skew].StrRec.length
        ADD     ESI,ECX
        CALL    Move
@@3:
        DEC     EBX
        JNE     @@loop2

        MOV     EDX,[EBP-6*4]
        MOV     EAX,EDX
        NEG     EAX
        LEA     EAX,[EBP+EAX*4-7*4]
        CALL    _LStrArrayClr
        POP     EDX
        MOV     EAX,[EBP-7*4]
        TEST    EDI,EDI
        JNZ     @@exit

        TEST    EDX,EDX
        JE      @@skip
        DEC     [EDX-skew].StrRec.refCnt   // EDX = local temp str
@@skip:
        CALL    _LStrAsg

@@exit:
        MOV     EDX,[EBP-6*4]
        LEA     ESP,[ESP+EDX*4]
        POP     EAX
        POP     EDX
        POP     EDI
        POP     ESI
        POP     EBX
        POP     EAX                        // This cleans up the temp used for the codePage
        POP     EAX                        // This cleans up the temp used for string param index
        POP     EBP                        // Restore the frame
        POP     EAX
        LEA     ESP,[ESP+EDX*4]
        JMP     EAX // Unbalanced CALL/RET means clobbered branch prediction.
                    // Should fix codegen and have caller pop arguments, like cdecl.

@@overflow:
        JMP     _IntOver
end;

{Original code by Pierre le Riche. Licensed under the CodeGear license terms.}
procedure _LStrCmp{left: AnsiString; right: AnsiString};
asm
  {On entry:
     eax = @Left[1]
     edx = @Right[1]
   On exit:
     Result in flags:
       CF = 1 if Left < Right, CF = 0 otherwise
       ZF = 1 if Left = Right, ZF = 0 otherwise}
  {Do Left and Right point to the same string data?}
  cmp eax, edx
  je @DoneNoPop
  {Is one of the two string pointers perhaps nil?}
  test eax, edx
  jz @PossibleNilString
@BothStringsNonNil:
  {Check that the left string is Ansi}
  cmp byte ptr [eax - Skew].StrRec.elemSize, 1
  jne @LeftNotAnsi
  {Check that the right string is Ansi}
  cmp byte ptr [edx - Skew].StrRec.elemSize, 1
  jne @RightNotAnsi
  {Compare the first two characters. (There has to be a trailing #0, and at
   least one other character for non-nil strings, so this comparison is safe).
   In "random" string compares this can save significant CPU time.}
  movzx ecx, word ptr [eax]
  cmp cx, [edx]
  jne @InitialMismatch
  {Save ebx}
  push ebx
  {Set ebx = Length(Left)}
  mov ebx, [eax - 4]
  xor ecx, ecx
  {Set ebx = Length(Left) - Length(Right)}
  sub ebx, [edx - 4]
  {Save the length difference on the stack}
  push ebx
  {Set ecx = 0 if Length(Left) <= Length(Right), $ffffffff otherwise}
  adc ecx, -1
  {Set ecx = - min(length(Left), Length(Right))}
  and ecx, ebx
  sub ecx, [eax - 4]
  {Adjust the pointers to be negative offset based}
  sub eax, ecx
  sub edx, ecx
@CompareLoop:
  {Compare four bytes per cycle. (The start of string data is at least DWord
   aligned, so this is safe.)}
  mov ebx, [eax + ecx]
  xor ebx, [edx + ecx]
  jnz @Mismatch
  {Next four bytes}
  add ecx, 4
  js @CompareLoop
  {All characters match up to the compare length}
@MatchUpToLength:
  {Restore the string length difference to eax}
  pop eax
  {Set the flags according to the length difference}
  add eax, eax
  {Restore ebx and return}
  pop ebx
@DoneNoPop:
  ret
@Mismatch:
  {Find the byte index that mismatched}
  bsf ebx, ebx
  shr ebx, 3
  {Is the mismatch beyond the compare length?}
  add ecx, ebx
  jns @MatchUpToLength
  {Compare the mismatched byte, setting the flags}
  mov al, [eax + ecx]
  cmp al, [edx + ecx]
  {Pop the length difference, restore ebx and return}
  pop ebx
  pop ebx
  ret
@InitialMismatch:
  {Swap the two characters into the correct order}
  movzx edx, word ptr [edx]
  bswap ecx
  bswap edx
  {Compare them again so the flags are set correctly}
  cmp ecx, edx
  ret
@PossibleNilString:
  {There is a good probability that one of the strings are nil (but not both)}
  test eax, eax
  jz @StringNil
  test edx, edx
  jnz @BothStringsNonNil
@StringNil:
  {One of the strings are nil, so compare pointers}
  cmp eax, edx
  ret
@LeftNotAnsi:
  {Create an Ansi copy of the left string}
  push 0
  push edx
  mov edx, eax
  lea eax, [esp + 4]
  call _LStrFromUStr
  {Call this routine with the new left string and the old right string}
  pop edx
  mov eax, [esp]
  jmp @RedoCompare
@RightNotAnsi:
  {Create an Ansi copy of the right string}
  push 0
  push eax
  lea eax, [esp + 4]
  call _LStrFromUStr
  {Call this routine with the old left string and the new right string}
  pop eax
  mov edx, [esp]
@RedoCompare:
  call _LStrCmp
  {Free the temporary string, keeping the flags unchanged. FreeMem is called
   directly because thread safety is not needed and _LStrClr is expensive.}
  pop eax
  pushf
  sub eax, skew
  call _FreeMem
  popf
end;

{Original code by Pierre le Riche. Licensed under the CodeGear license terms.}
procedure _LStrEqual{const Left, Right: AnsiString};
asm
  {On entry:
     eax = @Left[1]
     edx = @Right[1]
   On exit:
     Result in flags:
       ZF = 1 if Left = Right, ZF = 0 otherwise}
  {Do Left and Right point to the same string data?}
  cmp eax, edx
  je @CompareDoneNoPop
  {Is one of the two string pointers perhaps nil?}
  test eax, edx
  jz @PossibleNilString
@BothStringsNonNil:
  {Is the left-hand string Ansi?}
  cmp byte ptr [eax - Skew].StrRec.elemSize, 1
  jne @LeftNotAnsi
  {Is the right-hand string Ansi?}
  cmp byte ptr [edx - Skew].StrRec.elemSize, 1
  jne @RightNotAnsi
  {Compare lengths}
  mov ecx, [eax - 4]
  cmp ecx, [edx - 4]
  jne @CompareDoneNoPop
  {Save ebx}
  push ebx
  {Get pointers to the 4th last bytes in the strings}
  lea edx, [edx + ecx - 4]
  lea ebx, [eax + ecx - 4]
  {Negate the loop counter}
  neg ecx
  {Compare the last four bytes. If the string length is less than four bytes
   then part of the length field is compared again - no harm done.}
  mov eax, [ebx]
  cmp eax, [edx]
  jne @CompareDonePop
@CompareLoop:
  {Next four bytes}
  add ecx, 4
  jns @Match
  {Compare four bytes per iteration}
  mov eax, [ebx + ecx]
  cmp eax, [edx + ecx]
  je @CompareLoop
@CompareDonePop:
  pop ebx
@CompareDoneNoPop:
  ret
@Match:
  {Strings match - set the zero flag}
  xor eax, eax
  pop ebx
  ret
@PossibleNilString:
  {There is a good probability that one of the strings are nil (but not both)}
  test eax, eax
  jz @FirstStringNil
  test edx, edx
  jnz @BothStringsNonNil
  {Right is nil - compare lengths of the strings}
  cmp [eax - 4], edx
  ret
@FirstStringNil:
  {Left is nil - compare lengths of the strings}
  cmp eax, [edx - 4]
  ret
@LeftNotAnsi:
  {Swap the left and right strings, so the right string is the non Ansi
   string.}
  xchg eax, edx
@RightNotAnsi:
  {Create an Ansi copy of the right string}
  push 0
  push eax
  lea eax, [esp + 4]
  call _LStrFromUStr
  {Call this routine with the old left string and the new right string}
  pop eax
  mov edx, [esp]
  call _LStrEqual
  {Free the temporary string, keeping the flags unchanged. FreeMem is called
   directly because thread safety is not needed and _LStrClr is expensive.}
  pop eax
  pushf
  sub eax, skew
  call _FreeMem
  popf
end;

function _LStrAddRef(var str): Pointer;
{$IFDEF PUREPASCAL}
var
  P: PStrRec;
begin
  if @str <> nil then
  begin
    P := Pointer(Integer(@str) - sizeof(StrRec));
    if P.refcnt >= 0 then
      InterlockedIncrement(P.refcnt);
    Result := Pointer(@str);
  end
  else
     Result := nil;
end;
{$ELSE}
asm
        { ->    EAX     str     }
        TEST    EAX,EAX
        JE      @@exit
        MOV     EDX,[EAX-skew].StrRec.refCnt
        INC     EDX
        JLE     @@exit
   LOCK INC     [EAX-skew].StrRec.refCnt
@@exit:
end;
{$ENDIF}

function PICEmptyString: PWideChar;
begin
  Result := '';
end;

function _LStrToPChar(const s: AnsiString): PAnsiChar;
{$IFDEF PUREPASCAL}
const
  EmptyString = '';
begin
  if Pointer(s) = nil then
    Result := EmptyString
  else
    Result := Pointer(s);
end;
{$ELSE}
asm
        { ->    EAX pointer to str              }
        { <-    EAX pointer to PChar    }

        TEST    EAX,EAX
        JE      @@handle0
        RET
{$IFDEF PIC}
@@handle0:
        JMP     PICEmptyString
{$ELSE}
        // refCnt=-1,length=0 in case cast back to string
        DW      $0000
        DW      $0001
        DD      $FFFFFFFF
        DD      $00000000
@@zeroByte:
        DB      0
@@handle0:
        MOV     EAX,offset @@zeroByte
{$ENDIF}
end;
{$ENDIF}

function InternalUniqueString(var str): Pointer;
asm
        { ->    EAX pointer to str              }
        { <-    EAX pointer to unique copy      }
        MOV     EDX,[EAX]
        TEST    EDX,EDX
        JE      @@exit

        CMP     [EDX-skew].StrRec.elemSize,1
        JE      @@isAnsi

        PUSH    EAX
        CALL    EnsureAnsiString
        POP     EAX
        MOV     EDX,[EAX]

@@isAnsi:
        MOV     ECX,[EDX-skew].StrRec.refCnt
        DEC     ECX
        JE      @@exit

        PUSH    EBX
        MOV     EBX,EAX
        MOV     EAX,[EDX-skew].StrRec.length
        MOVZX   EDX,[EDX-skew].StrRec.codePage
        CALL    _NewAnsiString
        MOV     EDX,EAX
        MOV     EAX,[EBX]
        MOV     [EBX],EDX
        PUSH    EAX
        MOV     ECX,[EAX-skew].StrRec.length
        CALL    Move
        POP     EAX
        MOV     ECX,[EAX-skew].StrRec.refCnt
        DEC     ECX
        JL      @@skip
   LOCK DEC     [EAX-skew].StrRec.refCnt
        JNZ     @@skip
        LEA     EAX,[EAX-skew].StrRec.codePage  { if refCnt now zero, deallocate}
        CALL    _FreeMem
@@skip:
        MOV     EDX,[EBX]
        POP     EBX
@@exit:
        MOV     EAX,EDX
end;


procedure UniqueString(var str: AnsiString);
asm
        JMP     InternalUniqueString
end;

procedure _UniqueStringA(var str: AnsiString);
asm
        JMP     InternalUniqueString
end;

procedure UniqueString(var str: WideString);
asm
{$IF defined(LINUX) or defined(MACOSX)}
        JMP     InternalUniqueString
{$IFEND}
{$IFDEF MSWINDOWS}
    // nothing to do - Windows WideStrings are always single reference
{$ENDIF}
end;

procedure _UniqueStringW(var str: WideString);
asm
{$IF defined(LINUX) or defined(MACOSX)}
        JMP     InternalUniqueString
{$IFEND}
{$IFDEF MSWINDOWS}
    // nothing to do - Windows WideStrings are always single reference
{$ENDIF}
end;

procedure       _LStrCopy{ const s : AnsiString; index, count : Integer) : AnsiString};
asm
        {     ->EAX     Source string                   }
        {       EDX     index                           }
        {       ECX     count                           }
        {       [ESP+4] Pointer to result string        }

        PUSH    EBX

        TEST    EAX,EAX
        JE      @@srcEmpty

        MOV     EBX,[EAX-skew].StrRec.length
        TEST    EBX,EBX
        JE      @@srcEmpty

{       make index 0-based and limit to 0 <= index < Length(src) }

        DEC     EDX
        JL      @@smallInx
        CMP     EDX,EBX
        JGE     @@bigInx

@@cont1:

{       limit count to satisfy 0 <= count <= Length(src) - index        }

        SUB     EBX,EDX { calculate Length(src) - index }
        TEST    ECX,ECX
        JL      @@smallCount
        CMP     ECX,EBX
        JG      @@bigCount

@@cont2:
        CMP     word ptr [EAX-skew].StrRec.elemSize,1
        JNE     @@isUnicode

        ADD     EDX,EAX
        MOVZX   EAX,[EAX-skew].StrRec.codePage
        PUSH    EAX
        MOV     EAX,[ESP+4+4+4]
        CALL    _LStrFromPCharLen
        JMP     @@exit

@@isUnicode:
        SHL     EDX,1
        ADD     EDX,EAX
        MOV     EAX,[ESP+4+4]
        PUSH    DefaultSystemCodePage
        CALL    _LStrFromPWCharLen
        JMP     @@exit

@@smallInx:
        XOR     EDX,EDX
        JMP     @@cont1
@@bigCount:
        MOV     ECX,EBX
        JMP     @@cont2
@@bigInx:
@@smallCount:
@@srcEmpty:
        MOV     EAX,[ESP+4+4]
        CALL    _LStrClr
@@exit:
        POP     EBX
        RET     4
end;


procedure       _LStrDelete{ var s : AnsiString; index, count : Integer };
asm
        {     ->EAX     Pointer to s    }
        {       EDX     index           }
        {       ECX     count           }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     EBX,EAX
        MOV     ESI,EDX
        MOV     EDI,ECX

        CALL    UniqueString

        MOV     EDX,[EBX]
        TEST    EDX,EDX         { source already empty: nothing to do   }
        JE      @@exit

        MOV     ECX,[EDX-skew].StrRec.length

{       make index 0-based, if not in [0 .. Length(s)-1] do nothing     }

        DEC     ESI
        JL      @@exit
        CMP     ESI,ECX
        JGE     @@exit

{       limit count to [0 .. Length(s) - index] }

        TEST    EDI,EDI
        JLE     @@exit
        SUB     ECX,ESI         { ECX = Length(s) - index       }
        CMP     EDI,ECX
        JLE     @@1
        MOV     EDI,ECX
@@1:

{       move length - index - count characters from s+index+count to s+index }

        SUB     ECX,EDI         { ECX = Length(s) - index - count       }
        ADD     EDX,ESI         { EDX = s+index                 }
        LEA     EAX,[EDX+EDI]   { EAX = s+index+count           }
        CALL    Move

{       set length(s) to length(s) - count      }

        MOV     EDX,[EBX]
        MOV     EAX,EBX
        MOVZX   ECX,[EDX-skew].StrRec.codePage
        MOV     EDX,[EDX-skew].StrRec.length
        SUB     EDX,EDI
        CALL    _LStrSetLength

@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
end;


procedure       _LStrInsert{ const source : AnsiString; var s : AnsiString; index : Integer };
asm
        { ->    EAX source string                       }
        {       EDX     pointer to destination string   }
        {       ECX index                               }

        TEST    EAX,EAX
        JE      @@nothingToDo

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        MOV     EBX,EAX
        MOV     ESI,EDX
        MOV     EDI,ECX

{       make index 0-based and limit to 0 <= index <= Length(s) }

        MOV     EDX,[EDX]
        PUSH    EDX
        TEST    EDX,EDX
        JE      @@sIsNull
        MOV     EDX,[EDX-skew].StrRec.length
@@sIsNull:
        DEC     EDI
        JGE     @@indexNotLow
        XOR     EDI,EDI
@@indexNotLow:
        CMP     EDI,EDX
        JLE     @@indexNotHigh
        MOV     EDI,EDX
@@indexNotHigh:

        MOV     EBP,[EBX-skew].StrRec.length

{       set length of result to length(source) + length(s)      }

        MOV     EAX,[ESI]
        TEST    EAX,EAX
        JNE     @@DestNotNull
        MOV     EAX,EBX
@@DestNotNull:
        MOVZX   ECX,[EAX-skew].StrRec.codePage
        MOV     EAX,ESI
        ADD     EDX,EBP
        JO      @@overflow
        CALL    _LStrSetLength
        POP     EAX

        CMP     EAX,EBX
        JNE     @@notInsertSelf
        MOV     EBX,[ESI]

@@notInsertSelf:

{       move length(s) - length(source) - index chars from s+index to s+index+length(source) }

        MOV     EAX,[ESI]                       { EAX = s       }
        LEA     EDX,[EDI+EBP]                   { EDX = index + length(source)  }
        MOV     ECX,[EAX-skew].StrRec.length
        SUB     ECX,EDX                         { ECX = length(s) - length(source) - index }
        ADD     EDX,EAX                         { EDX = s + index + length(source)      }
        ADD     EAX,EDI                         { EAX = s + index       }
        CALL    Move

{       copy length(source) chars from source to s+index        }

        MOV     EAX,EBX
        MOV     EDX,[ESI]
        MOV     ECX,EBP
        ADD     EDX,EDI
        CALL    Move

@@exit:
        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
@@nothingToDo:
        RET

@@overflow:
        JMP     _IntOver
end;

{
  This one needs to be visible for AnsiString support in C++.
}
procedure       _LStrPos{ const substr : AnsiString; const s : AnsiString ) : Integer};
asm
{     ->EAX     Pointer to substr               }
{       EDX     Pointer to string               }
{     <-EAX     Position of substr in s or 0    }

        TEST    EAX,EAX
        JE      @@noWork

        TEST    EDX,EDX
        JE      @@stringEmpty

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX                         { Point ESI to substr           }
        MOV     EDI,EDX                         { Point EDI to s                }

        MOV     ECX,[EDI-skew].StrRec.length    { ECX = Length(s)               }

        PUSH    EDI                             { remember s position to calculate index        }

        MOV     EDX,[ESI-skew].StrRec.length    { EDX = Length(substr)          }

        DEC     EDX                             { EDX = Length(substr) - 1              }
        JS      @@fail                          { < 0 ? return 0                        }
        MOV     AL,[ESI]                        { AL = first char of substr             }
        INC     ESI                             { Point ESI to 2'nd char of substr      }

        SUB     ECX,EDX                         { #positions in s to look at    }
                                                { = Length(s) - Length(substr) + 1      }
        JLE     @@fail
@@loop:
        REPNE   SCASB
        JNE     @@fail
        MOV     EBX,ECX                         { save outer loop counter               }
        PUSH    ESI                             { save outer loop substr pointer        }
        PUSH    EDI                             { save outer loop s pointer             }

        MOV     ECX,EDX
        REPE    CMPSB
        POP     EDI                             { restore outer loop s pointer  }
        POP     ESI                             { restore outer loop substr pointer     }
        JE      @@found
        MOV     ECX,EBX                         { restore outer loop counter    }
        JMP     @@loop

@@fail:
        POP     EDX                             { get rid of saved s pointer    }
        XOR     EAX,EAX
        JMP     @@exit

@@stringEmpty:
        XOR     EAX,EAX
        JMP     @@noWork

@@found:
        POP     EDX                             { restore pointer to first char of s    }
        MOV     EAX,EDI                         { EDI points of char after match        }
        SUB     EAX,EDX                         { the difference is the correct index   }
@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
@@noWork:
end;

(* ***** BEGIN LICENSE BLOCK *****
 *
 * The function Pos is licensed under the CodeGear license terms.
 *
 * The initial developer of the original code is Fastcode
 *
 * Portions created by the initial developer are Copyright (C) 2002-2004
 * the initial developer. All Rights Reserved.
 *
 * Contributor(s): Aleksandr Sharahov
 *
 * ***** END LICENSE BLOCK ***** *)
function Pos(const substr, str: RawByteString): Integer; overload;
asm
       push  ebx
       push  esi
       add   esp, -16
       test  edx, edx
       jz    @NotFound
       test  eax, eax
       jz    @NotFound
       mov   esi, [edx-4] //Length(Str)
       mov   ebx, [eax-4] //Length(Substr)
       cmp   esi, ebx
       jl    @NotFound
       test  ebx, ebx
       jle   @NotFound
       dec   ebx
       add   esi, edx
       add   edx, ebx
       mov   [esp+8], esi
       add   eax, ebx
       mov   [esp+4], edx
       neg   ebx
       movzx ecx, byte ptr [eax]
       mov   [esp], ebx
       jnz   @FindString

       sub   esi, 2
       mov   [esp+12], esi

@FindChar2:
       cmp   cl, [edx]
       jz    @Matched0ch
       cmp   cl, [edx+1]
       jz    @Matched1ch
       add   edx, 2
       cmp   edx, [esp+12]
       jb    @FindChar4
       cmp   edx, [esp+8]
       jb    @FindChar2
@NotFound:
       xor   eax, eax
       jmp   @Exit0ch

@FindChar4:
       cmp   cl, [edx]
       jz    @Matched0ch
       cmp   cl, [edx+1]
       jz    @Matched1ch
       cmp   cl, [edx+2]
       jz    @Matched2ch
       cmp   cl, [edx+3]
       jz    @Matched3ch
       add   edx, 4
       cmp   edx, [esp+12]
       jb    @FindChar4
       cmp   edx, [esp+8]
       jb    @FindChar2
       xor   eax, eax
       jmp   @Exit0ch

@Matched2ch:
       add   edx, 2
@Matched0ch:
       inc   edx
       mov   eax, edx
       sub   eax, [esp+4]
@Exit0ch:
       add   esp, 16
       pop   esi
       pop   ebx
       ret

@Matched3ch:
       add   edx, 2
@Matched1ch:
       add   edx, 2
       xor   eax, eax
       cmp   edx, [esp+8]
       ja    @Exit1ch
       mov   eax, edx
       sub   eax, [esp+4]
@Exit1ch:
       add   esp, 16
       pop   esi
       pop   ebx
       ret

@FindString4:
       cmp   cl, [edx]
       jz    @Test0
       cmp   cl, [edx+1]
       jz    @Test1
       cmp   cl, [edx+2]
       jz    @Test2
       cmp   cl, [edx+3]
       jz    @Test3
       add   edx, 4
       cmp   edx, [esp+12]
       jb    @FindString4
       cmp   edx, [esp+8]
       jb    @FindString2
       xor   eax, eax
       jmp   @Exit1

@FindString:
       sub   esi, 2
       mov   [esp+12], esi
@FindString2:
       cmp   cl, [edx]
       jz    @Test0
@AfterTest0:
       cmp   cl, [edx+1]
       jz    @Test1
@AfterTest1:
       add   edx, 2
       cmp   edx, [esp+12]
       jb    @FindString4
       cmp   edx, [esp+8]
       jb    @FindString2
       xor   eax, eax
       jmp   @Exit1

@Test3:
       add   edx, 2
@Test1:
       mov   esi, [esp]
@Loop1:
       movzx ebx, word ptr [esi+eax]
       cmp   bx, word ptr [esi+edx+1]
       jnz   @AfterTest1
       add   esi, 2
       jl    @Loop1
       add   edx, 2
       xor   eax, eax
       cmp   edx, [esp+8]
       ja    @Exit1
@RetCode1:
       mov   eax, edx
       sub   eax, [esp+4]
@Exit1:
       add   esp, 16
       pop   esi
       pop   ebx
       ret

@Test2:
       add   edx,2
@Test0:
       mov   esi, [esp]
@Loop0:
       movzx ebx, word ptr [esi+eax]
       cmp   bx, word ptr [esi+edx]
       jnz   @AfterTest0
       add   esi, 2
       jl    @Loop0
       inc   edx
@RetCode0:
       mov   eax, edx
       sub   eax, [esp+4]
       add   esp, 16
       pop   esi
       pop   ebx
end;

procedure       _LStrSetLength{ var str: AnsiString; newLength: Integer; CodePage: Word };
asm
        { ->    EAX     Pointer to str  }
        {       EDX new length  }
        {       ECX     codePage }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EBP
        MOV     EBX,EAX
        MOV     ESI,EDX
        MOV     EBP,ECX
        XOR     EDI,EDI

        TEST    EDX,EDX
        JLE     @@setString

        MOV     EAX,[EBX]
        TEST    EAX,EAX
        JE      @@copyString

        CMP     word ptr [EAX-skew].StrRec.elemSize,1
        JE      @@isAnsi

        PUSH    0
        MOV     EAX,ESP
        MOV     ECX,EBP
        MOV     EDX,[EBX]
        CALL    _LStrFromUStr
        POP     EDI
        MOV     EAX,EDI
        MOV     EDX,ESI
        XCHG    EDI,[EBX]

@@isAnsi:
        CMP     [EAX-skew].StrRec.refCnt,1
        JNE     @@copyString

        SUB     EAX,rOff
        ADD     EDX,rOff+1
        JO      @@overflow
        PUSH    EAX
        MOV     EAX,ESP
        CALL    _ReallocMem
        POP     EAX
        ADD     EAX,rOff
        MOV     [EBX],EAX
        MOV     [EAX-skew].StrRec.length,ESI
        MOV     BYTE PTR [EAX+ESI],0
        TEST    EDI,EDI       // Was a temp created?
        JZ      @@exit
        PUSH    EDI
        MOV     EAX,ESP
        CALL    _LStrClr
        POP     EDI
        JMP     @@exit

@@overflow:
        JMP     _IntOver

@@copyString:
        MOV     EAX,EDX
        MOV     EDX,EBP
        CALL    _NewAnsiString
        MOV     EDI,EAX

        MOV     EAX,[EBX]
        TEST    EAX,EAX
        JE      @@setString

        MOV     EDX,EDI
        MOV     ECX,[EAX-skew].StrRec.length
        CMP     ECX,ESI
        JL      @@moveString
        MOV     ECX,ESI

@@moveString:
        CALL    Move

@@setString:
        MOV     EAX,EBX
        CALL    _LStrClr
        MOV     [EBX],EDI

@@exit:
        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
end;

function StringOfChar(ch: AnsiChar; count: Integer): AnsiString; overload;
asm
        { ->    AL      c               }
        {       EDX     count           }
        {       ECX     result  }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     EBX,EAX
        MOV     ESI,EDX
        MOV     EDI,ECX

        MOV     EAX,ECX
        CALL    _LStrClr

        TEST    ESI,ESI
        JLE     @@exit

        MOV     EAX,ESI
        MOV     EDX,DefaultSystemCodePage
        CALL    _NewAnsiString

        MOV     [EDI],EAX

        MOV     EDX,ESI
        MOV     CL,BL

        CALL    _FillChar

@@exit:
        POP     EDI
        POP     ESI
        POP     EBX

end;


function _Write0LString(var t: TTextRec; const s: AnsiString): Pointer;
begin
  Result := _WriteLString(t, s, 0);
end;


function _WriteLString(var t: TTextRec; const s: AnsiString; width: Longint): Pointer;
{$IFDEF PUREPASCAL}
var
  i: Integer;
  P: PStrRec;
  converted: AnsiString;
begin
  i := Length(s);
  _WriteSpaces(t, width - i);
  if Pointer(s) <> nil then
  begin
    P := Pointer(Integer(s) - Sizeof(StrRec));
    if P.elemSize <> 1 then
    begin
       _LStrFromUStr(converted, UnicodeString(s), DefaultSystemCodePage);
       Result := _WriteBytes(t, converted, i);
    end
    else
       Result := _WriteBytes(t, s[1], i);
  end
  else
    Result := _WriteBytes(t, s[1], i);
end;
{$ELSE !PUREPASCAL}
asm
        { ->    EAX     Pointer to text record  }
        {       EDX     Pointer to AnsiString   }
        {       ECX     Field width             }
        PUSH    EBX
        PUSH    ESI

        MOV     EBX,EDX

        MOV     EDX,ECX
        XOR     ECX,ECX
        TEST    EBX,EBX
        JE      @@skip
        MOV     ECX,[EBX-skew].StrRec.length
        SUB     EDX,ECX
@@skip:
        PUSH    ECX
        CALL    _WriteSpaces
        POP     ECX

        TEST    EBX,EBX
        JE      @@empty
        CMP     [EBX-Skew].StrRec.elemSize,1
        JNE     @@unicodeString

@@empty:
        MOV     EDX,EBX
        POP     ESI
        POP     EBX
        JMP     _WriteBytes

@@unicodeString:
{$IFDEF ALIGN_STACK}
//   	SUB	ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH    0
        PUSH    ECX
        MOV     ESI,EAX
        LEA     EAX,[ESP + 4]
        MOV     EDX,EBX
        MOV     ECX,DefaultSystemCodePage
        CALL    _LStrFromUStr
        MOV     EDX,[ESP + 4]
        POP     ECX
        MOV     EAX,ESI
        CALL    _WriteBytes
        LEA     EAX,[ESP + 4]
        CALL    _LStrClr
        MOV     EAX,ESI
        POP     ESI
end;
{$ENDIF !PUREPASCAL}


function _Write0WString(var t: TTextRec; const s: WideString): Pointer;
begin
  Result := _WriteWString(t, s, 0);
end;


function _WriteWString(var t: TTextRec; const s: WideString; width: Longint): Pointer;
var
  i: Integer;
begin
  i := Length(s);
  _WriteSpaces(t, width - i);
  Result := _WriteLString(t, AnsiString(s), 0);
end;


function _Write0WCString(var t: TTextRec; s: PWideChar): Pointer;
begin
  Result := _WriteWCString(t, s, 0);
end;


function _WriteWCString(var t: TTextRec; s: PWideChar; width: Longint): Pointer;
var
  i: Integer;
begin
  i := 0;
  if (s <> nil) then
    while s[i] <> #0 do
      Inc(i);

  _WriteSpaces(t, width - i);
  Result := _WriteLString(t, AnsiString(s), 0);
end;


function _Write0WChar(var t: TTextRec; c: WideChar): Pointer;
begin
  Result := _WriteWChar(t, c, 0);
end;


function _WriteWChar(var t: TTextRec; c: WideChar; width: Integer): Pointer;
begin
  _WriteSpaces(t, width - 1);
  Result := _WriteLString(t, AnsiString(c), 0);
end;

function _WriteVariant(var T: TTextRec; const V: TVarData; Width: Integer): Pointer;
var
  S: AnsiString;
begin
  if Assigned(VarToLStrProc) then
  begin
    VarToLStrProc(S, V);
    _WriteLString(T, S, Width);
  end
  else
    Error(reVarInvalidOp);
  Result := @T;
end;

function _Write0Variant(var T: TTextRec; const V: TVarData): Pointer;
begin
  Result := _WriteVariant(T, V, 0);
end;

{$IFDEF MSWINDOWS}
procedure WStrError;
asm
        MOV     AL,reOutOfMemory
        JMP     Error
end;
{$ENDIF}

function _NewWideString(CharLength: Longint): Pointer;
{$IF defined(LINUX) or defined(MACOSX)}
begin
   { MACOSXTODO: check code page on this }
   Result := _NewAnsiString(CharLength*2, 0);
//  Result := _NewAnsiString(CharLength*2);
end;
{$IFEND}
{$IFDEF MSWINDOWS}
asm
        TEST    EAX,EAX
        JE      @@1
        PUSH    EAX
        PUSH    0
        CALL    SysAllocStringLen
        TEST    EAX,EAX
        JE      WStrError
@@1:
end;
{$ENDIF}

procedure WStrSet(var S: WideString; P: PWideChar);
{$IFDEF PUREPASCAL}
var
  Temp: Pointer;
begin
  Temp := Pointer(InterlockedExchange(Pointer(S), Pointer(P)));
  if Temp <> nil then
    _WStrClr(Temp);
end;
{$ELSE}
asm
{$IF defined(LINUX) or defined(MACOSX)}
        XCHG    [EAX],EDX
        TEST    EDX,EDX
        JZ      @@1
        PUSH    EDX
        MOV     EAX, ESP
        CALL    _WStrClr
        POP     EAX
{$IFEND}
{$IFDEF MSWINDOWS}
        XCHG    [EAX],EDX
        TEST    EDX,EDX
        JZ      @@1
        PUSH    EDX
        CALL    SysFreeString
{$ENDIF}
@@1:
end;
{$ENDIF}


procedure _WStrClr(var S);
{$IF defined(LINUX) or defined(MACOSX)}
asm
        JMP     _LStrClr;
end;
{$IFEND LINUX or MACOSX}
{$IFDEF MSWINDOWS}
asm
        { ->    EAX     Pointer to WideString  }

        MOV     EDX,[EAX]
        TEST    EDX,EDX
        JE      @@1
        MOV     DWORD PTR [EAX],0
        PUSH    EAX
        PUSH    EDX
        CALL    SysFreeString
        POP     EAX
@@1:
end;
{$ENDIF MSWINDOWS}


procedure _WStrArrayClr(var StrArray; Count: Integer);
{$IF defined(LINUX) or defined(MACOSX)}
asm
        JMP     _LStrArrayClr
end;
{$IFEND LINUX or MACOSX}
{$IFDEF MSWINDOWS}
asm
        PUSH    EBX
        PUSH    ESI
        MOV     EBX,EAX
        MOV     ESI,EDX
@@1:    MOV     EAX,[EBX]
        TEST    EAX,EAX
        JE      @@2
        MOV     DWORD PTR [EBX],0
        PUSH    EAX
        CALL    SysFreeString
@@2:    ADD     EBX,4
        DEC     ESI
        JNE     @@1
        POP     ESI
        POP     EBX
end;
{$ENDIF MSWINDOWS}


procedure _WStrAsg(var Dest: WideString; const Source: WideString);
{$IF defined(LINUX) or defined(MACOSX)}
asm
        JMP     _LStrAsg
end;
{$IFEND LINUX or MACOSX}
{$IFDEF MSWINDOWS}
asm
        { ->    EAX     Pointer to WideString }
        {       EDX     Pointer to data       }
        CMP     [EAX],EDX
        JE      @@1
        TEST    EDX,EDX
        JE      _WStrClr
        MOV     ECX,[EDX-4]
        SHR     ECX,1
        JE      _WStrClr
        PUSH    ECX
        PUSH    EDX
        PUSH    EAX
        CALL    SysReAllocStringLen
        TEST    EAX,EAX
        JE      WStrError
@@1:
end;
{$ENDIF MSWINDOWS}

procedure _WStrLAsg(var Dest: WideString; const Source: WideString);
{$IF defined(LINUX) or defined(MACOSX)}
asm
        JMP   _LStrLAsg
end;
{$IFEND LINUX or MACOSX}
{$IFDEF MSWINDOWS}
asm
        JMP   _WStrAsg
end;
{$ENDIF MSWINDOWS}

procedure InternalWStrFromPCharLen(var Dest: WideString; Source: PAnsiChar; Length: Integer; CodePage: Integer);
var
  DestLen: Integer;
  Buffer: array[0..2047] of WideChar;
begin
  if Length <= 0 then
  begin
    _WStrClr(Dest);
    Exit;
  end;
  if Length+1 < High(Buffer) then
  begin
    DestLen := WCharFromChar(Buffer, High(Buffer), Source, Length, CodePage);
    if DestLen > 0 then
    begin
      _WStrFromPWCharLen(Dest, @Buffer, DestLen);
      Exit;
    end;
  end;

  DestLen := (Length + 1);
  _WStrSetLength(Dest, DestLen);  // overallocate, trim later
  DestLen := WCharFromChar(PWideChar(Dest), DestLen, Source, Length, CodePage);
  if DestLen < 0 then DestLen := 0;
  _WStrSetLength(Dest, DestLen);
end;

procedure _WStrFromPCharLen(var Dest: WideString; Source: PAnsiChar; Length: Integer);
begin
  InternalWStrFromPCharLen(Dest, Source, Length, DefaultSystemCodePage);
end;

procedure _WStrFromPWCharLen(var Dest: WideString; Source: PWideChar; CharLength: Integer);
{$IF defined(LINUX) or defined(MACOSX)}
var
  Temp: Pointer;
begin
  Temp := Pointer(Dest);
  if CharLength > 0 then
  begin
    Pointer(Dest) := _NewWideString(CharLength);
    if Source <> nil then
      Move(Source^, Pointer(Dest)^, CharLength * sizeof(WideChar));
  end
  else
    Pointer(Dest) := nil;
  _WStrClr(Temp);
end;
{$IFEND LINUX or MACOSX}
{$IFDEF MSWINDOWS}
asm
        { ->    EAX     Pointer to WideString (dest)      }
        {       EDX     Pointer to characters (source)    }
        {       ECX     number of characters  (not bytes) }
        TEST    ECX,ECX
        JE      _WStrClr

        PUSH    EAX

        PUSH    ECX
        PUSH    EDX
        CALL    SysAllocStringLen
        TEST    EAX,EAX
        JE      WStrError

        POP     EDX
        PUSH    [EDX].PWideChar
        MOV     [EDX],EAX

        CALL    SysFreeString
end;
{$ENDIF MSWINDOWS}


procedure _WStrFromChar(var Dest: WideString; Source: AnsiChar);
asm
        PUSH    EDX
        MOV     EDX,ESP
        MOV     ECX,1
        CALL    _WStrFromPCharLen
        POP     EDX
end;


procedure _WStrFromWChar(var Dest: WideString; Source: WideChar);
asm
        { ->    EAX     Pointer to WideString (dest)   }
        {       EDX     character             (source) }
        PUSH    EDX
        MOV     EDX,ESP
        MOV     ECX,1
        CALL    _WStrFromPWCharLen
        POP     EDX
end;


procedure _WStrFromPChar(var Dest: WideString; Source: PAnsiChar);
asm
  { ->    EAX     Pointer to WideString (dest)   }
        {       EDX     Pointer to character  (source) }
        XOR     ECX,ECX
        TEST    EDX,EDX
        JE      @@5
        PUSH    EDX
@@0:    CMP     CL,[EDX+0]
        JE      @@4
        CMP     CL,[EDX+1]
        JE      @@3
        CMP     CL,[EDX+2]
        JE      @@2
        CMP     CL,[EDX+3]
        JE      @@1
        ADD     EDX,4
        JMP     @@0
@@1:    INC     EDX
@@2:    INC     EDX
@@3:    INC     EDX
@@4:    MOV     ECX,EDX
        POP     EDX
        SUB     ECX,EDX
@@5:    JMP     _WStrFromPCharLen
end;


procedure _WStrFromPWChar(var Dest: WideString; Source: PWideChar);
asm
        { ->    EAX     Pointer to WideString (dest)   }
        {       EDX     Pointer to character  (source) }
        XOR     ECX,ECX
        TEST    EDX,EDX
        JE      @@5
        PUSH    EDX
@@0:    CMP     CX,[EDX+0]
        JE      @@4
        CMP     CX,[EDX+2]
        JE      @@3
        CMP     CX,[EDX+4]
        JE      @@2
        CMP     CX,[EDX+6]
        JE      @@1
        ADD     EDX,8
        JMP     @@0
@@1:    ADD     EDX,2
@@2:    ADD     EDX,2
@@3:    ADD     EDX,2
@@4:    MOV     ECX,EDX
        POP     EDX
        SUB     ECX,EDX
        SHR     ECX,1
@@5:    JMP     _WStrFromPWCharLen
end;


procedure _WStrFromString(var Dest: WideString; const Source: ShortString);
asm
        XOR     ECX,ECX
        MOV     CL,[EDX]
        INC     EDX
        JMP     _WStrFromPCharLen
end;


procedure _WStrFromArray(var Dest: WideString; Source: PAnsiChar; Length: Integer);
asm
        PUSH    EDI
        PUSH    EAX
        PUSH    ECX
        MOV     EDI,EDX
        XOR     EAX,EAX
        REPNE   SCASB
        JNE     @@1
        NOT     ECX
@@1:    POP     EAX
        ADD     ECX,EAX
        POP     EAX
        POP     EDI
        JMP     _WStrFromPCharLen
end;


procedure _WStrFromWArray(var Dest: WideString; Source: PWideChar; Length: Integer);
asm
        PUSH    EDI
        PUSH    EAX
        PUSH    ECX
        MOV     EDI,EDX
        XOR     EAX,EAX
        REPNE   SCASW
        JNE     @@1
        NOT     ECX
@@1:    POP     EAX
        ADD     ECX,EAX
        POP     EAX
        POP     EDI
        JMP     _WStrFromPWCharLen
end;


procedure _WStrFromLStr(var Dest: WideString; const Source: AnsiString);
asm
        XOR     ECX,ECX
        TEST    EDX,EDX
        JE      @@1
        MOV     ECX,[EDX-Skew].StrRec.length
        CMP     [EDX-Skew].StrRec.elemSize,1
        JNE     @@2
// Inject the CodePage parameter onto the stack ahead of the return address
        XCHG    ECX,[ESP]
        PUSH    ECX
        MOVZX   ECX,[EDX-Skew].StrRec.codePage
        XCHG    ECX,[ESP + 4]
        JMP     InternalWStrFromPCharLen
@@1:    JMP     _WStrFromPCharLen
@@2:    JMP     _WStrFromPWCharLen
end;


procedure _WStrToString(Dest: PShortString; const Source: WideString; MaxLen: Integer);
var
  SourceLen, DestLen: Integer;
  Buffer: array[0..511] of AnsiChar;
begin
  if MaxLen > 255 then MaxLen := 255;
  SourceLen := Length(Source);
  if SourceLen >= MaxLen then SourceLen := MaxLen;
  if SourceLen = 0 then
    DestLen := 0
  else
  begin
    DestLen := CharFromWChar(Buffer, High(Buffer), PWideChar(Pointer(Source)), SourceLen);
    if DestLen < 0 then
      DestLen := 0
    else if DestLen > MaxLen then
      DestLen := MaxLen;
  end;
  Dest^[0] := AnsiChar(Chr(DestLen));
  if DestLen > 0 then Move(Buffer, Dest^[1], DestLen);
end;

function _WStrToPWChar(const S: WideString): PWideChar;
{$IFDEF PUREPASCAL}
const
  EmptyString = '';
begin
  if Pointer(S) = nil then
    Result := EmptyString
  else
    Result := Pointer(S);
end;
{$ELSE}
asm
        TEST    EAX,EAX
        JE      @@1
        RET
{$IFDEF PIC}
@@1:    JMP     PICEmptyString
{$ELSE}
        NOP
@@0:    DW      0
@@1:    MOV     EAX,OFFSET @@0
{$ENDIF}
end;
{$ENDIF}


function _WStrLen(const S: WideString): Longint; inline;
begin
  Result := Longint(S);
  if Result <> 0 then
    Result := PLongint(Result - 4)^ shr 1;
end;

function _PWCharLen(P: PWideChar): Longint;
asm
        TEST    EAX,EAX
        JE      @@5
        PUSH    EAX
        XOR     ECX,ECX
@@0:    CMP     CX,[EAX+0]
        JE      @@4
        CMP     CX,[EAX+2]
        JE      @@3
        CMP     CX,[EAX+4]
        JE      @@2
        CMP     CX,[EAX+6]
        JE      @@1
        ADD     EAX,8
        JMP     @@0
@@1:    ADD     EAX,2
@@2:    ADD     EAX,2
@@3:    ADD     EAX,2
@@4:    POP     ECX
        SUB     EAX,ECX
        SHR     EAX,1
@@5:
end;

procedure _WStrCat(var Dest: WideString; const Source: WideString);
var
  DestLen, SourceLen: Integer;
  NewStr: PWideChar;
begin
  SourceLen := Length(Source);
  if SourceLen <> 0 then
  begin
    DestLen := Length(Dest);
    NewStr := _NewWideString(DestLen + SourceLen);
    if DestLen > 0 then
      Move(Pointer(Dest)^, NewStr^, DestLen * sizeof(WideChar));
    Move(Pointer(Source)^, NewStr[DestLen], SourceLen * sizeof(WideChar));
    WStrSet(Dest, NewStr);
  end;
end;


procedure _WStrCat3(var Dest: WideString; const Source1, Source2: WideString);
var
  Source1Len, Source2Len: Integer;
  NewStr: PWideChar;
begin
  Source1Len := Length(Source1);
  Source2Len := Length(Source2);
  if (Source1Len <> 0) or (Source2Len <> 0) then
  begin
    NewStr := _NewWideString(Source1Len + Source2Len);
    Move(Pointer(Source1)^, Pointer(NewStr)^, Source1Len * sizeof(WideChar));
    Move(Pointer(Source2)^, NewStr[Source1Len], Source2Len * sizeof(WideChar));
    WStrSet(Dest, NewStr);
  end
  else
    _WStrClr(Dest);
end;


procedure _WStrCatN{var Dest: WideString; ArgCnt: Integer; ...};
asm
        {     ->EAX = Pointer to dest }
        {       EDX = number of args (>= 3) }
        {       [ESP+4], [ESP+8], ... crgCnt WideString arguments }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDX
        PUSH    EAX
        MOV     EBX,EDX

        XOR     EAX,EAX
@@loop1:
        MOV     ECX,[ESP+EDX*4+4*4]
        TEST    ECX,ECX
        JE      @@1
        ADD     EAX,[ECX-4]
@@1:
        DEC     EDX
        JNE     @@loop1

        SHR     EAX,1
        CALL    _NewWideString
        PUSH    EAX
        MOV     ESI,EAX

@@loop2:
        MOV     EAX,[ESP+EBX*4+5*4]
        MOV     EDX,ESI
        TEST    EAX,EAX
        JE      @@2
        MOV     ECX,[EAX-4]
        ADD     ESI,ECX
        CALL    Move
@@2:
        DEC     EBX
        JNE     @@loop2

        POP     EDX
        POP     EAX
        CALL    WStrSet

        POP     EDX
        POP     ESI
        POP     EBX
        POP     EAX
        LEA     ESP,[ESP+EDX*4]
        JMP     EAX
end;

procedure _WStrCmp{left: WideString; right: WideString};
asm
{     ->EAX = Pointer to left string    }
{       EDX = Pointer to right string   }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX
        MOV     EDI,EDX

        CMP     EAX,EDX
        JE      @@exit

        TEST    ESI,ESI
        JE      @@str1null

        TEST    EDI,EDI
        JE      @@str2null

        MOV     EAX,[ESI-4]
        MOV     EDX,[EDI-4]

        SUB     EAX,EDX { eax = len1 - len2 }
        JA      @@skip1 { len1 > len2 (unsigned)? }
        ADD     EDX,EAX { edx = len2 + (len1 - len2) = len1     }
                        // edx := Min(len1, len2)
@@skip1:
        PUSH    EDX
        SHR     EDX,2
        JE      @@cmpRest
@@longLoop:
        MOV     ECX,[ESI]
        MOV     EBX,[EDI]
        CMP     ECX,EBX
        JNE     @@misMatch
        DEC     EDX
        JE      @@cmpRestP4
        MOV     ECX,[ESI+4]
        MOV     EBX,[EDI+4]
        CMP     ECX,EBX
        JNE     @@misMatch
        ADD     ESI,8
        ADD     EDI,8
        DEC     EDX
        JNE     @@longLoop
        JMP     @@cmpRest
@@cmpRestP4:
        ADD     ESI,4
        ADD     EDI,4
@@cmpRest:
        POP     EDX
        AND     EDX,2
        JE      @@equal

        MOV     CX,[ESI]
        MOV     BX,[EDI]
        CMP     CX,BX
        JNE     @@exit

@@equal:
        ADD     EAX,EAX
        JMP     @@exit

@@str1null:
        MOV     EDX,[EDI-4]
        SUB     EAX,EDX
        JMP     @@exit

@@str2null:
        MOV     EAX,[ESI-4]
        SUB     EAX,EDX
        JMP     @@exit

@@misMatch:
        POP     EDX
        CMP     CX,BX
        JNE     @@exit
        SHR     ECX,16
        SHR     EBX,16
        CMP     CX,BX

@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
end;

procedure _WStrEqual{const Left, Right: WideString};
asm
        JMP     _WStrCmp
end;

function _WStrCopy(const S: WideString; Index, Count: Integer): WideString;
var
  L, N: Integer;
begin
  L := Length(S);
  if Index < 1 then Index := 0 else
  begin
    Dec(Index);
    if Index > L then Index := L;
  end;
  if Count < 0 then N := 0 else
  begin
    N := L - Index;
    if N > Count then N := Count;
  end;
  _WStrFromPWCharLen(Result, PWideChar(Pointer(S)) + Index, N);
end;


procedure _WStrDelete(var S: WideString; Index, Count: Integer);
var
  L, N: Integer;
  NewStr: PWideChar;
begin
  L := Length(S);
  if (L > 0) and (Index >= 1) and (Index <= L) and (Count > 0) then
  begin
    Dec(Index);
    N := L - Index - Count;
    if N < 0 then N := 0;
    if (Index = 0) and (N = 0) then NewStr := nil else
    begin
      NewStr := _NewWideString(Index + N);
      if Index > 0 then
        Move(Pointer(S)^, NewStr^, Index * 2);
      if N > 0 then
        Move(PWideChar(Pointer(S))[L - N], NewStr[Index], N * 2);
    end;
    WStrSet(S, NewStr);
  end;
end;


procedure _WStrInsert(const Source: WideString; var Dest: WideString; Index: Integer);
var
  SourceLen, DestLen: Integer;
  NewStr: PWideChar;
begin
  SourceLen := Length(Source);
  if SourceLen > 0 then
  begin
    DestLen := Length(Dest);
    if Index < 1 then Index := 0 else
    begin
      Dec(Index);
      if Index > DestLen then Index := DestLen;
    end;
    NewStr := _NewWideString(DestLen + SourceLen);
    if Index > 0 then
      Move(Pointer(Dest)^, NewStr^, Index * 2);
    Move(Pointer(Source)^, NewStr[Index], SourceLen * 2);
    if Index < DestLen then
      Move(PWideChar(Pointer(Dest))[Index], NewStr[Index + SourceLen],
        (DestLen - Index) * 2);
    WStrSet(Dest, NewStr);
  end;
end;


function Pos(const substr, str: WideString): Integer; overload;
asm
{     ->EAX     Pointer to substr               }
{       EDX     Pointer to string               }
{     <-EAX     Position of substr in str or 0  }

        TEST    EAX,EAX
        JE      @@noWork

        TEST    EDX,EDX
        JE      @@stringEmpty

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX                         { Point ESI to substr           }
        MOV     EDI,EDX                         { Point EDI to s                }

        MOV     ECX,[EDI-4]                     { ECX = Length(s)               }
        SHR     ECX,1

        PUSH    EDI                             { remember s position to calculate index        }

        MOV     EDX,[ESI-4]                     { EDX = Length(substr)          }
        SHR     EDX,1

        DEC     EDX                             { EDX = Length(substr) - 1              }
        JS      @@fail                          { < 0 ? return 0                        }
        MOV     AX,[ESI]                        { AL = first char of substr             }
        ADD     ESI,2                           { Point ESI to 2'nd char of substr      }

        SUB     ECX,EDX                         { #positions in s to look at    }
                                                { = Length(s) - Length(substr) + 1      }
        JLE     @@fail
@@loop:
        REPNE   SCASW
        JNE     @@fail
        MOV     EBX,ECX                         { save outer loop counter               }
        PUSH    ESI                             { save outer loop substr pointer        }
        PUSH    EDI                             { save outer loop s pointer             }

        MOV     ECX,EDX
        REPE    CMPSW
        POP     EDI                             { restore outer loop s pointer  }
        POP     ESI                             { restore outer loop substr pointer     }
        JE      @@found
        MOV     ECX,EBX                         { restore outer loop counter    }
        JMP     @@loop

@@fail:
        POP     EDX                             { get rid of saved s pointer    }
        XOR     EAX,EAX
        JMP     @@exit

@@stringEmpty:
        XOR     EAX,EAX
        JMP     @@noWork

@@found:
        POP     EDX                             { restore pointer to first char of s    }
        MOV     EAX,EDI                         { EDI points of char after match        }
        SUB     EAX,EDX                         { the difference is the correct index   }
        SHR     EAX,1
@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
@@noWork:
end;


procedure _WStrSetLength(var S: WideString; NewLength: Integer);
var
  NewStr: PWideChar;
  Count: Integer;
begin
  NewStr := nil;
  if NewLength > 0 then
  begin
    NewStr := _NewWideString(NewLength);
    Count := Length(S);
    if Count > 0 then
    begin
      if Count > NewLength then Count := NewLength;
      Move(Pointer(S)^, NewStr^, Count * 2);
    end;
  end;
  WStrSet(S, NewStr);
end;


function StringOfChar(Ch: WideChar; Count: Integer): UnicodeString; overload;
var
  P: PWideChar;
begin
  _UStrFromPWCharLen(Result, nil, Count);
  P := Pointer(Result);
  while Count > 0 do
  begin
    Dec(Count);
    P[Count] := Ch;
  end;
end;

function _WStrAddRef(var str: WideString): Pointer;
{$IF defined(LINUX) or defined(MACOSX)}
asm
        JMP     _LStrAddRef
end;
{$IFEND LINUX or MACOSX}
{$IFDEF MSWINDOWS}
asm
        MOV     EDX,[EAX]
        TEST    EDX,EDX
        JE      @@1
        PUSH    EAX
        MOV     ECX,[EDX-4]
        SHR     ECX,1
        PUSH    ECX
        PUSH    EDX
        CALL    SysAllocStringLen
        POP     EDX
        TEST    EAX,EAX
        JE      WStrError
        MOV     [EDX],EAX
@@1:
end;
{$ENDIF MSWINDOWS}

procedure _WCharToString(Dest: PShortString; const Source: WideChar; MaxLen: Integer);
var
  DestLen: Integer;
  Buffer: array[0..255] of AnsiChar;
begin
  if MaxLen > 255 then MaxLen := 255;
  DestLen := CharFromWChar(Buffer, High(Buffer), @Source, 1);
  if DestLen < 0 then
    DestLen := 0
  else if DestLen > MaxLen then
    DestLen := MaxLen;
  Dest^[0] := AnsiChar(Chr(DestLen));
  if DestLen > 0 then Move(Buffer, Dest^[1], DestLen);
end;

{ UnicodeString helper functions }

function _UStrAddRef(str: Pointer { UnicodeString }): Pointer;
asm
        JMP     _LStrAddRef
end;

procedure _UStrClr(var S);
asm
        JMP     _LStrClr
end;

procedure _UStrArrayClr(var StrArray; Count: Integer);
asm
        JMP     _LStrArrayClr
end;

procedure _UStrAsg(var Dest: UnicodeString; const Source: UnicodeString); // globals (need copy)
{$IFDEF PUREPASCAL}
var
  S, D: Pointer;
  P: PStrRec;
  Temp: Longint;
begin
  S := Pointer(source);
  if S <> nil then
  begin
    P := PStrRec(Integer(S) - sizeof(StrRec));
    if P.refCnt < 0 then   // make copy of string literal
    begin
      Temp := P.length;
      S := _NewUnicodeString(Temp);
      Move(Pointer(source)^, S^, Temp * SizeOf(WideChar));
      P := PStrRec(Integer(S) - sizeof(StrRec));
    end;
    InterlockedIncrement(P.refCnt);
  end;

  D := Pointer(dest);
  Pointer(dest) := S;
  if D <> nil then
  begin
    P := PStrRec(Integer(D) - sizeof(StrRec));
    if P.refCnt > 0 then
      if InterlockedDecrement(P.refCnt) = 0 then
        FreeMem(P);
  end;
end;
{$ELSE}
asm
        { ->    EAX pointer to dest   str      }
        { ->    EDX pointer to source str      }

                TEST    EDX,EDX                           { have a source? }
                JE      @@2                               { no -> jump     }

                CMP     [EDX-Skew].StrRec.elemSize,2
                JE      @@isUnicode
                JMP     _UStrFromLStr

@@isUnicode:
                MOV     ECX,[EDX-skew].StrRec.refCnt
                INC     ECX
                JG      @@1                               { literal string -> jump not taken }

                PUSH    EAX
                PUSH    EDX
                MOV     EAX,[EDX-skew].StrRec.length
                CALL    _NewUnicodeString
                MOV     EDX,EAX
                POP     EAX
                PUSH    EDX
                MOV     ECX,[EAX-skew].StrRec.length
                SHL     ECX,1                             { length to bytes for move }
                CALL    Move
                POP     EDX
                POP     EAX
                JMP     @@2

@@1:
           LOCK INC     [EDX-skew].StrRec.refCnt

@@2:            XCHG    EDX,[EAX]
                TEST    EDX,EDX
                JE      @@3
                MOV     ECX,[EDX-skew].StrRec.refCnt
                DEC     ECX
                JL      @@3
           LOCK DEC     [EDX-skew].StrRec.refCnt
                JNE     @@3
                LEA     EAX,[EDX-skew].StrRec.codePage
                CALL    _FreeMem
@@3:
end;
{$ENDIF}

procedure _UStrLAsg(var Dest: UnicodeString; const Source: UnicodeString); // locals
asm
{ ->    EAX     pointer to dest }
{       EDX     source          }

                TEST    EDX,EDX
                JE      @@sourceDone

                CMP     [EDX-Skew].StrRec.elemSize,2
                JE      @@isUnicode
                JMP     _UStrFromLStr

@@isUnicode:
                { bump up the ref count of the source }

                MOV     ECX,[EDX-skew].StrRec.refCnt
                INC     ECX
                JLE     @@sourceDone                    { literal assignment -> jump taken }
           LOCK INC     [EDX-skew].StrRec.refCnt
@@sourceDone:

                { we need to release whatever the dest is pointing to   }

                XCHG    EDX,[EAX]                       { fetch str                    }
                TEST    EDX,EDX                         { if nil, nothing to do        }
                JE      @@done
                MOV     ECX,[EDX-skew].StrRec.refCnt    { fetch refCnt                 }
                DEC     ECX                             { if < 0: literal str          }
                JL      @@done
           LOCK DEC     [EDX-skew].StrRec.refCnt        { threadsafe dec refCount      }
                JNE     @@done
                LEA     EAX,[EDX-skew].StrRec.codePage  { if refCnt now zero, deallocate}
                CALL    _FreeMem
@@done:
end;

type
  TEmptyStringW = packed record
    Rec: StrRec;
    Nul: Word;
  end;

const
  // Using initialized constant to be sure of alignment.
  // Not as read-only as code segment, but code entry points
  // have no alignment guarantees.
  EmptyStringW: TEmptyStringW = (
    Rec: (
      codePage: Word($FFFF);
      elemSize: 2;
      refCnt: -1;
      length: 0);
    Nul: 0);

function _UStrToPWChar(const S: UnicodeString): PWideChar;
{$IFDEF PUREPASCAL}
const
  EmptyString = '';
begin
  if Pointer(S) = nil then
    Result := EmptyString
  else
    Result := Pointer(S);
end;
{$ELSE}
asm
        { ->    EAX pointer to S            }
        { <-    EAX pointer to PWideChar    }

        TEST    EAX,EAX
        JE      @@handle0
        RET
{$IFDEF PIC}
@@handle0:
        JMP     PICEmptyString
{$ELSE}
@@handle0:
        MOV     EAX, offset EmptyStringW.Nul
{$ENDIF}
end;
{$ENDIF}

procedure InternalUStrFromPCharLen(var Dest: UnicodeString; Source: PAnsiChar; Length: Integer; CodePage: Integer);
var
  DestLen: Integer;
  Buffer: array[0..2047] of WideChar;
begin
  if Length <= 0 then
  begin
    _UStrClr(Dest);
    Exit;
  end;
  if Length+1 < High(Buffer) then
  begin
    DestLen := WCharFromChar(Buffer, High(Buffer), Source, Length, CodePage);
    if DestLen > 0 then
    begin
      _UStrFromPWCharLen(Dest, @Buffer, DestLen);
      Exit;
    end;
  end;

  DestLen := (Length + 1);
  _UStrSetLength(Dest, DestLen);  // overallocate, trim later
  DestLen := WCharFromChar(Pointer(Dest), DestLen, Source, Length, CodePage);
  if DestLen < 0 then DestLen := 0;
  _UStrSetLength(Dest, DestLen);
end;

procedure _UStrFromPCharLen(var Dest: UnicodeString; Source: PAnsiChar; Length: Integer);
begin
  InternalUStrFromPCharLen(Dest, Source, Length, DefaultSystemCodePage);
end;

procedure _UStrFromPWCharLen(var Dest: UnicodeString; Source: PWideChar; CharLength: Integer);
{$IF defined(LINUX) or defined(MACOSX)}
var
  Temp: Pointer;
begin
  Temp := Pointer(Dest);
  if CharLength > 0 then
  begin
    Pointer(Dest) := _NewUnicodeString(CharLength);
    if Source <> nil then
      Move(Source^, Pointer(Dest)^, CharLength * SizeOf(WideChar));
  end
  else
    Pointer(Dest) := nil;
  _UStrClr(Temp);
end;
{$IFEND LINUX or MACOSX}
{$IFDEF MSWINDOWS}
asm
  { ->    EAX     pointer to dest   }
  {       EDX source                }
  {       ECX length in characters  }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     EBX,EAX // EBX := addr of Dest (result) in EBX
        MOV     ESI,EDX // ESI := source
        MOV     EDI,ECX // EDI := length

        { allocate new string }

        MOV     EAX,EDI // EAX := length

        CALL    _NewUnicodeString // EAX := new string (result)
        MOV     ECX,EDI // ECX := length
        MOV     EDI,EAX // EDI := result

        TEST    ESI,ESI // nil source?
        JE      @@noMove

        MOV     EDX,EAX // EDX := result (dest for Move)
        MOV     EAX,ESI // EAX := source (source for Move)
        SHL     ECX,1   // ECX := ECX * 2 (turn length into characters)
        CALL    Move

        { assign the result to dest }

@@noMove:
        MOV     EAX,EBX
        CALL    _LStrClr
        MOV     [EBX],EDI

        POP     EDI
        POP     ESI
        POP     EBX
end;
{$ENDIF MSWINDOWS}

procedure _UStrFromChar(var Dest: UnicodeString; Source: AnsiChar);
asm
        PUSH    EDX      // char on stack
        MOV     EDX,ESP  // addr of char on stack in EDX
        MOV     ECX,1
        CALL    _UStrFromPCharLen
        POP     EDX
end;

procedure _UStrFromWChar(var Dest: UnicodeString; Source: WideChar);
asm
        PUSH    EDX
        MOV     EDX,ESP
        MOV     ECX,1
        CALL    _UStrFromPWCharLen
        POP     EDX
end;

procedure _UStrFromPChar(var Dest: UnicodeString; Source: PAnsiChar);
asm
        { ->    EAX     Pointer to WideString (dest)   }
        {       EDX     Pointer to character  (source) }
        XOR     ECX,ECX
        TEST    EDX,EDX
        JE      @@5
        PUSH    EDX
@@0:    CMP     CL,[EDX+0]
        JE      @@4
        CMP     CL,[EDX+1]
        JE      @@3
        CMP     CL,[EDX+2]
        JE      @@2
        CMP     CL,[EDX+3]
        JE      @@1
        ADD     EDX,4
        JMP     @@0
@@1:    INC     EDX
@@2:    INC     EDX
@@3:    INC     EDX
@@4:    MOV     ECX,EDX
        POP     EDX
        SUB     ECX,EDX
@@5:    JMP     _UStrFromPCharLen
end;

procedure _UStrFromPWChar(var Dest: UnicodeString; Source: PWideChar);
asm
        { ->    EAX     Pointer to WideString (dest)   }
        {       EDX     Pointer to character  (source) }
        XOR     ECX,ECX
        TEST    EDX,EDX
        JE      @@5
        PUSH    EDX
@@0:    CMP     CX,[EDX+0]
        JE      @@4
        CMP     CX,[EDX+2]
        JE      @@3
        CMP     CX,[EDX+4]
        JE      @@2
        CMP     CX,[EDX+6]
        JE      @@1
        ADD     EDX,8
        JMP     @@0
@@1:    ADD     EDX,2
@@2:    ADD     EDX,2
@@3:    ADD     EDX,2
@@4:    MOV     ECX,EDX
        POP     EDX
        SUB     ECX,EDX
        SHR     ECX,1
@@5:    JMP     _UStrFromPWCharLen
end;

procedure _UStrFromArray(var Dest: UnicodeString; Source: PAnsiChar; Length: Integer);
asm
        PUSH    EDI
        PUSH    EAX
        PUSH    ECX
        MOV     EDI,EDX
        XOR     EAX,EAX
        REPNE   SCASB      // find #0
        JNE     @@1
        NOT     ECX
@@1:    POP     EAX
        ADD     ECX,EAX
        POP     EAX
        POP     EDI
        JMP     _UStrFromPCharLen
end;

procedure _UStrFromWArray(var Dest: UnicodeString; Source: PWideChar; Length: Integer);
asm
        PUSH    EDI
        PUSH    EAX
        PUSH    ECX
        MOV     EDI,EDX
        XOR     EAX,EAX
        REPNE   SCASW     // find #$0000
        JNE     @@1
        NOT     ECX
@@1:    POP     EAX
        ADD     ECX,EAX
        POP     EAX
        POP     EDI
        JMP     _UStrFromPWCharLen
end;

procedure _UStrFromLStr(var Dest: UnicodeString; const Source: AnsiString);
asm
        XOR     ECX,ECX
        TEST    EDX,EDX
        JE      @@1
        CMP     [EDX-Skew].StrRec.elemSize,2
        JE      @@2
        MOVZX   ECX,WORD PTR [EDX-Skew].StrRec.codePage
        XCHG    ECX,[ESP]
        PUSH    ECX
        MOV     ECX,[EDX-Skew].StrRec.length
        JMP     InternalUStrFromPCharLen
@@1:    JMP     _UStrFromPCharLen
@@2:    JMP     _UStrAsg
end;

procedure _LStrFromUStr(var Dest: AnsiString; const Source: UnicodeString; CodePage: Word);
asm
        { ->    EAX pointer to dest                 }
        {       EDX pointer to UnicodeString data   }
        {       ECX destination codepage            }

        XCHG    ECX,[ESP]
        PUSH    ECX
        XOR     ECX,ECX
        TEST    EDX,EDX
        JE      @@1
        CMP     [EDX-Skew].StrRec.elemSize,1
        JE      @@2
        MOV     ECX,[EDX-4]     // length in UnicodeString is widechar count
@@1:    JMP     _LStrFromPWCharLen
@@2:    POP     ECX
        XCHG    ECX,[ESP]
        JMP     _LStrAsg
end;

procedure _UStrFromWStr(var Dest: UnicodeString; const Source: WideString);
asm
        { ->    EAX pointer to dest                 }
        {       EDX pointer to WideString data      }

        XOR     ECX,ECX
        TEST    EDX,EDX
        JZ      @@1            // nil source => zero length
        MOV     ECX,[EDX-4]
        SHR     ECX,1          // length in WideString is byte count
@@1:    JMP     _UStrFromPWCharLen
end;

procedure _WStrFromUStr(var Dest: WideString; const Source: UnicodeString);
asm
        { ->    EAX pointer to dest                 }
        {       EDX pointer to UnicodeString data   }

        XOR     ECX,ECX
        TEST    EDX,EDX
        JZ      @@1            // nil source => zero length
        MOV     ECX,[EDX-Skew].StrRec.length    // length in UnicodeString is widechar count
        CMP     [EDX-Skew].StrRec.elemSize,2
        JNE     @@2
@@1:    JMP     _WStrFromPWCharLen
// Inject the CodePage parameter onto the stack ahead of the return address
@@2:    XCHG    ECX,[ESP]
        PUSH    ECX
        MOVZX   ECX,[EDX-Skew].StrRec.codePage
        XCHG    ECX,[ESP + 4]
        JMP     InternalWStrFromPCharLen
end;

procedure _UStrToString(Dest: PShortString; const Source: UnicodeString; MaxLen: Integer);
var
  SourceLen, DestLen: Integer;
  Buffer: array[0..511] of AnsiChar;
begin
  if MaxLen > 255 then MaxLen := 255;
  SourceLen := Length(Source);
  if SourceLen >= MaxLen then SourceLen := MaxLen;
  if SourceLen = 0 then
    DestLen := 0
  else
  begin
    DestLen := CharFromWChar(Buffer, High(Buffer), PWideChar(Pointer(Source)), SourceLen);
    if DestLen < 0 then
      DestLen := 0
    else if DestLen > MaxLen then
      DestLen := MaxLen;
  end;
  Dest^[0] := AnsiChar(Chr(DestLen));
  if DestLen > 0 then Move(Buffer, Dest^[1], DestLen);
end;

procedure _UStrFromString(var Dest: UnicodeString; const Source: ShortString);
asm
        XOR     ECX,ECX
        MOV     CL,[EDX]
        INC     EDX
        JMP     _UStrFromPCharLen
end;

function _UStrLen(const S: UnicodeString): Integer;
begin
  Result := Longint(S);
  if Result <> 0 then                // PStrRec should be used here, but
    Result := PLongint(Result - 4)^; // a private symbol can't be inlined
end;

procedure _UStrSetLength(var S: UnicodeString; NewLength: Integer);
asm
        { ->    EAX     Pointer to S  }
        {       EDX     new length    }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EBX,EAX       // EBX saves @S
        MOV     ESI,EDX       // ESI saves NewLength (chars)
        XOR     EDI,EDI       // EDI := 0; EDI is Temp (result)

        TEST    EDX,EDX       // NewLength <= 0?
        JLE     @@setString   // Assign S := Temp

        MOV     EAX,[EBX]     // EAX := S
        TEST    EAX,EAX       // nil?
        JE      @@copyString  // cannot reallocate (it's nil), so copy

        CMP     [EAX-skew].StrRec.elemSize,2
        JE      @@isUnicode

        PUSH    0
        MOV     EAX,ESP
        MOV     EDX,[EBX]
        CALL    _UStrFromLStr
        POP     EDI
        MOV     EAX,EDI
        MOV     EDX,ESI
        XCHG    EDI,[EBX]

@@isUnicode:
        CMP     [EAX-skew].StrRec.refCnt,1 // !!! MT safety
        JNE     @@copyString  // not unique, so copy

        SUB     EAX,rOff      // Offset EAX "S" to start of memory block
        ADD     EDX,EDX       // Double length to get size
        JO      @@overflow
        ADD     EDX,rOff+2    // Add string rec size
        JO      @@overflow
        PUSH    EAX           // Put S on stack
        MOV     EAX,ESP       // to pass by reference
        CALL    _ReallocMem
        POP     EAX
        ADD     EAX,rOff      // Readjust
        MOV     [EBX],EAX     // Store
        MOV     [EAX-skew].StrRec.length,ESI
        MOV     WORD PTR [EAX+ESI*2],0 // Null terminate
        TEST    EDI,EDI       // Was a temp created?
        JZ      @@exit
        PUSH    EDI
        MOV     EAX,ESP
        CALL    _LStrClr
        POP     EDI
        JMP     @@exit

@@overflow:
        JMP     _IntOver

@@copyString:
        MOV     EAX,EDX       // EAX := NewLength
        CALL    _NewUnicodeString
        MOV     EDI,EAX       // EDI "Temp" := new string

        MOV     EAX,[EBX]     // EAX := S, also Source of Move
        TEST    EAX,EAX       // nil?
        JE      @@setString   // Assign straight away

        MOV     EDX,EDI       // EDX := EDI "Temp", also Dest of Move
        MOV     ECX,[EAX-skew].StrRec.length  // ECX := Length(S), also Count of Move
        CMP     ECX,ESI       // ECX "Length(S)" <> NewLength
        JL      @@moveString  // ECX smaller => jump
        MOV     ECX,ESI       // ECX := ESI

@@moveString:
        SHL     ECX,1         // Length widechars to bytes translation
        CALL    Move          // Move ECX chars from EAX to EDX

@@setString:
        MOV     EAX,EBX       // EAX := @S
        CALL    _LStrClr      // clear S
        MOV     [EBX],EDI     // S := Temp

@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
end;

procedure _UStrCat(var Dest: UnicodeString; const Source: UnicodeString);
asm
        { ->    EAX     pointer to dest }
        {       EDX source              }

        TEST    EDX,EDX       // Source empty, nop.
        JE      @@exit

        MOV     ECX,[EAX]     // ECX := Dest
        TEST    ECX,ECX       // Nil source => assignment
        JE      _UStrAsg

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EBX,EAX         // EBX := @Dest
        MOV     ESI,EDX         // ESI := Source
        CMP     ESI,ECX
        JE      @@appendSelf

        CMP     [ECX-skew].StrRec.elemSize,2
        JE      @@destIsUnicode
        CALL    _EnsureUnicodeString
        MOV     EDI,EAX
        MOV     ECX,EAX

@@destIsUnicode:
        PUSH    0
        CMP     [ESI-skew].StrRec.elemSize,2
        JE      @@sourceIsUnicode

        MOV     EDI,ECX
        MOV     EAX,ESI
        MOV     [ESP],ESI
        CALL    _UStrAddRef
        MOV     EAX,ESP
        CALL    _EnsureUnicodeString
        MOV     ESI,[ESP]
        MOV     ECX,EDI

@@sourceIsUnicode:
        MOV     EDI,[ECX-skew].StrRec.length  // EDI := Length(Dest)
        MOV     EDX,[ESI-skew].StrRec.length  // EDX := Length(Source)
        ADD     EDX,EDI         // EDX := (Length(Source) + Length(Dest)) * 2
        TEST    EDX,$C0000000
        JNZ     @@lengthOverflow

        MOV     EAX,EBX
        CALL    _UStrSetLength  // Set length of Dest
        MOV     EAX,ESI         // EAX := Source
        MOV     ECX,[ESI-skew].StrRec.length // ECX := Length(Source)

@@noTemp:
        MOV     EDX,[EBX]       // EDX := Dest
        SHL     EDI,1           // EDI to bytes (Length(Dest) * 2)
        ADD     EDX,EDI         // Offset EDX for destination of move
        SHL     ECX,1           // convert Length(Source) to bytes
        CALL    Move            // Move(Source, Dest + Length(Dest)*2, Length(Source)*2)
        MOV     EAX,ESP         // Need to clear out the temp we may have created above
        MOV     EDX,[EAX]
        TEST    EDX,EDX
        JE      @@tempEmpty

        CALL    _LStrClr

@@tempEmpty:
        POP     EAX
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@appendSelf:
        CMP     [ECX-skew].StrRec.elemSize,2
        JE      @@selfIsUnicode
        MOV     EAX,EBX
        XOR     EDX,EDX
        CALL    _EnsureUnicodeString
        MOV     ECX,EAX
        MOV     EAX,EBX

@@selfIsUnicode:
        MOV     EDI,[ECX-skew].StrRec.length
        MOV     EDX,EDI
        SHL     EDX,1
        TEST    EDX,$C0000000
        JNZ     @@lengthOverflow
        CALL    _UStrSetLength
        MOV     EAX,[EBX]
        MOV     ECX,EDI
        PUSH    0
        JMP     @@noTemp

@@lengthOverflow:
        JMP     _IntOver

@@exit:
end;

procedure _UStrCat3(var Dest: UnicodeString; const Source1, Source2: UnicodeString);
asm
        {     ->EAX = Pointer to dest   }
        {       EDX = source1           }
        {       ECX = source2           }

        TEST    EDX,EDX
        JE      @@assignSource2

        TEST    ECX,ECX
        JE      _UStrAsg

        CMP     EDX,[EAX]
        JE      @@appendToDest

        CMP     ECX,[EAX]
        JE      @@theHardWay

        PUSH    EAX
        PUSH    ECX
        CALL    _UStrAsg

        POP     EDX
        POP     EAX
        JMP     _UStrCat

@@theHardWay: // s(*EAX,ECX) := source1(EDX) + s(ECX)

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     EBX,EDX         // EBX := source1
        MOV     ESI,ECX         // ESI := source2
        PUSH    EAX             // Push(@s)

        PUSH    0               // Need temps - source1
        PUSH    0               // source2
        CMP     [ESI-Skew].StrRec.elemSize,2
        JE      @@source2IsUnicode

        MOV     [ESP],ESI
        MOV     EAX,ESI
        CALL    _UStrAddRef
        MOV     EAX,ESP
        CALL    _EnsureUnicodeString  //  Convert it to Ansi
        MOV     ESI,[ESP]

@@source2IsUnicode:
        CMP     [EBX-Skew].StrRec.elemSize,2
        JE      @@source1IsUnicode

        MOV     [ESP+4],EBX
        MOV     EAX,EBX
        CALL    _UStrAddRef
        LEA     EAX,[ESP+4]
        CALL    _EnsureUnicodeString
        MOV     EBX,[ESP+4]

@@source1IsUnicode:
        MOV     EAX,[EBX-skew].StrRec.length
        ADD     EAX,[ESI-skew].StrRec.length

        TEST    EAX,$C0000000   // either of top two bits set => overflow for size
        JNZ     @@overflow
        CALL    _NewUnicodeString   // EAX := new string ("result")

        MOV     EDI,EAX         // EDI := result

        MOV     EDX,EDI         // EDX := result
        MOV     EAX,EBX         // EAX := source1
        MOV     ECX,[EBX-skew].StrRec.length // ECX := Length(source1)
        SHL     ECX,1           // double ECX for bytes
        CALL    Move            // Move(source1, result, Length(source1)*2)

        MOV     EAX,ESI         // EAX := source2
        MOV     ECX,[ESI-skew].StrRec.length // ECX := Length(source2)
        SHL     ECX,1           // ECX => to bytes
        MOV     EDX,[EBX-skew].StrRec.length // EDX := Length(source1)
        SHL     EDX,1           // EDX => to bytes
        ADD     EDX,EDI         // EDX := result + (num bytes in source1)
        CALL    Move            // Move(source2, result+offset, Length(source2)*2)

        MOV     EAX,[ESP]       // Check if there was a temp created
        OR      EAX,[ESP+4]
        JZ      @@noTemp
        MOV     EDX,2
        MOV     EAX,ESP
        CALL    _LStrArrayClr        // Call _LStrArrayClr directly since _UStrArrayClr jumps to it

@@noTemp:
        POP     EAX             // Remove the temps from the stack
        POP     EAX
        POP     EAX             // EAX := Pop() // @s
        MOV     EDX,EDI         // EDX := result
        TEST    EDI,EDI
        JE      @@skip          // result is nil? => don't decrement
        DEC     [EDI-skew].StrRec.refCnt    // EDI = local temp str; _UStrAsg will addref, so ensure final refCnt = 1
@@skip:
        CALL    _UStrAsg

        POP     EDI
        POP     ESI
        POP     EBX

        JMP     @@exit

@@assignSource2:
        MOV     EDX,ECX
        JMP     _UStrAsg

@@appendToDest:
        MOV     EDX,ECX
        JMP     _UStrCat

@@overflow:
        JMP     _IntOver

@@exit:
end;

procedure _UStrCatN{var dest:UnicodeString; argCnt: Integer; ...};
asm
        {     ->EAX = Pointer to dest   }
        {       EDX = number of args (>= 3)     }
        {       [ESP+4], [ESP+8], ... argCnt UnicodeString arguments, reverse order }

        PUSH    EBP
        MOV     EBP,ESP
        PUSH    0                  // Place to save the current string param index
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EDX
        PUSH    EAX
        MOV     EBX,EDX

        XOR     EDI,EDI
        PUSH    0                 // Potential temp
        MOV     ECX,[EBP+EDX*4+4] // first arg is furthest out
        TEST    ECX,ECX
        JZ      @@0

        CMP     [ECX-skew].StrRec.elemSize,2
        JE      @@argIsUnicode

        MOV     ESI,EAX
        MOV     EAX,[EBP+EDX*4+4]
        CALL    _UStrAddRef
        MOV     EDX,EBX
        LEA     EAX,[EBP+EDX*4+4]
        CALL    _EnsureUnicodeString
        MOV     ECX,EAX
        MOV     EDX,EBX
        MOV     [ESP],EAX
        MOV     EAX,ESI

@@argIsUnicode:
        CMP     [EAX],ECX          // is dest = first arg?
        JNE     @@firstNotDest
        MOV     EDI,ECX            // EDI nonzero -> potential appendstr case
        MOV     EAX,[ECX-skew].StrRec.length  // EAX accumulates final length during @@loop1
        DEC     EDX
        JMP     @@loop1
@@0:
        XOR     EAX,EAX
        JMP     @@1

@@firstNotDest:
        XOR     EAX,EAX
        JMP     @@skipPush

@@loop1:
        PUSH    0

@@skipPush:
        MOV     ECX,[EBP+EDX*4+4]
        TEST    ECX,ECX
        JE      @@1

        CMP     [ECX-skew].StrRec.elemSize,2
        JE      @@nStrIsUnicode

        MOV     ESI,EAX
        MOV     [EBP-4],EDX
        MOV     EAX,[EBP+EDX*4+4]
        CALL    _UStrAddRef
        MOV     EDX,[EBP-4]
        LEA     EAX,[EBP+EDX*4+4]
        CALL    _EnsureUnicodeString
        MOV     EDX,[EBP-4]
        MOV     ECX,EAX
        MOV     [ESP],EAX
        MOV     EAX,ESI

@@nStrIsUnicode:
        ADD     EAX,[ECX-skew].StrRec.length
        TEST    EAX,$C0000000
        JNZ     @@overflow

        CMP     EDI,ECX          // is dest an arg besides arg1?
        JNE     @@1
        XOR     EDI,EDI          // can't appendstr - dest is multiple args
@@1:
        DEC     EDX
        JNE     @@loop1

@@append:
        TEST    EDI,EDI          // dest is 1st and only 1st arg?
        JZ      @@copy
        MOV     EDX,EAX          // length into EDX
        MOV     EAX,[EBP-6*4]    // ptr to str into EAX
        MOV     ESI,[EDI-skew].StrRec.Length  // save old size before realloc
        CALL    _UStrSetLength
        MOV     EDI,[EBP-6*4]    // append other strs to dest
        PUSH    [EDI]
        SHL     ESI,1            // Length to bytes for offset into string
        ADD     ESI,[EDI]        // ESI = end of old string
        DEC     EBX
        JMP     @@loop2

@@copy:
        CALL    _NewUnicodeString
        PUSH    EAX
        MOV     ESI,EAX

@@loop2:
        // Loop invariants:
        // - ESI is target of move, going through final dest
        // - EBX is arg index in stack to get arguments;
        //   last argument pushed last => lowest address => addresses decrease from first to last
        MOV     EAX,[EBP+EBX*4+4]     // EAX := argN
        TEST    EAX,EAX
        JZ      @@2
        MOV     EDX,ESI                 // EDX := dest
        TEST    EAX,EAX                 // argN nil?
        JE      @@2                     // => skip
        MOV     ECX,[EAX-skew].StrRec.length    // ECX := Length(argN)
        SHL     ECX,1                   // ECX to bytes
        ADD     ESI,ECX                 // ESI (running target of move) += ECX
        CALL    Move                    // Move(argN, dest, Length(argN) * 2)
@@2:
        DEC     EBX
        JNE     @@loop2

        MOV     EDX,[EBP-5*4]
        MOV     EAX,EDX
        NEG     EAX
        LEA     EAX,[EBP+EAX*4-6*4]
        CALL    _LStrArrayClr
        POP     EDX
        MOV     EAX,[EBP-6*4]
        TEST    EDI,EDI
        JNZ     @@exit

        TEST    EDX,EDX
        JE      @@skip
        DEC     [EDX-skew].StrRec.refCnt   // EDX = local temp str
@@skip:
        CALL    _UStrAsg

@@exit:
        MOV     EDX,[EBP-5*4]
        LEA     ESP,[ESP+EDX*4]
        POP     EAX
        POP     EDX
        POP     EDI
        POP     ESI
        POP     EBX
        POP     EAX                        // This cleans up the temp used for string param index
        POP     EBP
        POP     EAX // ret address from CALL
        LEA     ESP,[ESP+EDX*4]
        JMP     EAX // Unbalanced CALL/RET means clobbered branch prediction.
                    // Should fix codegen and have caller pop arguments, like cdecl.

@@overflow:
        JMP     _IntOver
end;

{$IFDEF PUREPASCAL}
//  StrRec = packed record
//    codePage: Word;
//    elemSize: Word;
//    refCnt: Longint;
//    length: Longint;
//  end;
function strEltSize(const s: UnicodeString): Integer; inline;
begin
   Result := PStrRec(Pointer(LongWord(s) - SizeOf(StrRec)))^.elemSize;
end;

function strRawLength(const s: UnicodeString): Longint; inline;
begin
//   Result := PStrRec(Pointer(LongWord(s) - SizeOf(StrRec)))^.length;
   Result := PLongint(Pointer(LongWord(s) - SizeOf(Longint)))^;
end;

function StrRecIsNil(const p: PStrRec): Boolean; inline;
begin
   Result := LongWord(p) = LongWord(-SizeOf(StrRec));
end;

function _UStrCmpAsymmetric(const left, right: UnicodeString): Integer;
begin
   // this case should only arise in the face of C++ code
   Error(reMacNotImplemented);
end;

function _UStrCmpInternal(const left, right: UnicodeString): Integer;
var
   LengthDelta: Integer;
   leftR: PStrRec;
   rightR: PStrRec;
   compareLength: Integer;
begin
   if Pointer(left) = Pointer(right) then
      Exit(0);

   leftR := PStrRec(LongWord(Pointer(left)) - SizeOf(StrRec));
   rightR := PStrRec(LongWord(Pointer(right)) - SizeOf(StrRec));
   if StrRecIsNil(leftR) then
   begin
//      if StrRecIsNil(rightR) then
//         Exit(0)
//      else
//         Exit(leftR^.length);
      Exit(1);
   end
   else if StrRecIsNil(rightR) then
//      Exit(-leftR^.length)
      Exit(-1)
   else
   begin
      // both strings are present
      LengthDelta := leftR^.length - rightR^.length;
      if LengthDelta <> 0 then
         Exit(LengthDelta)
      else
      begin
         // both strings same length - now the awful payload escape
         if (leftR^.elemSize <> 2) or (rightR^.elemSize <> 2) then
         begin
            // This saves us from creating a temp for the original strings,
            // generating a very slight additional cost for the asymmetric case, which
            // is the more rare case.
            Result := _UStrCmpAsymmetric(UnicodeString(Pointer(Integer(leftR) + SizeOf(StrRec))),
                                         UnicodeString(Pointer(Integer(rightR) + SizeOf(StrRec))));
         end
         else
         begin
            // plain old unicode strings
            compareLength := leftR^.length;
            if compareLength > rightR^.length then
               compareLength := rightR^.length;
            Result := memcmp(Pointer(Integer(leftR) + SizeOf(StrRec)),
                             Pointer(Integer(rightR) + SizeOf(StrRec)),
                             compareLength);
         end;
      end;
    end;
end;
{$ENDIF PUREPASCAL}
{Original code by Pierre le Riche. Licensed under the CodeGear license terms.}
procedure _UStrCmp{left, right: UnicodeString};
asm
  {On entry:
     eax = @Left[1]
     edx = @Right[1]
   On exit:
     Result in flags:
       CF = 1 if Left < Right, CF = 0 otherwise
       ZF = 1 if Left = Right, ZF = 0 otherwise}
  {Do Left and Right point to the same string data?}
  cmp eax, edx
  je @DoneNoPop
  {Is one of the two string pointers perhaps nil?}
  test eax, edx
  jz @PossibleNilString
@BothStringsNonNil:
  {Check that the left string is UTF-16}
  cmp byte ptr [eax - Skew].StrRec.elemSize, 2
  jne @LeftNotUTF16
  {Check that the right string is UTF-16}
  cmp byte ptr [edx - Skew].StrRec.elemSize, 2
  jne @RightNotUTF16
  {Compare the first two characters. (There has to be a trailing #0, and non-nil
   UnicodeStrings must contain at least one other character so this comparison
   is safe). In "random" string compares this can save significant CPU time.}
  mov ecx, [eax]
  cmp ecx, [edx]
  jne @InitialMismatch
  {Save ebx}
  push ebx
  {set ebx = Length(Left)}
  mov ebx, [eax - 4]
  xor ecx, ecx
  {set ebx = Length(Left) - Length(Right)}
  sub ebx, [edx - 4]
  {Save the length difference on the stack}
  push ebx
  {set ecx = 0 if Length(Left) < Length(Right), $ffffffff otherwise}
  adc ecx, -1
  {set ecx = - min(length(Left), Length(Right))}
  and ecx, ebx
  sub ecx, [eax - 4]
  {Two bytes per character}
  add ecx, ecx
  {Adjust the pointers to be negative offset based}
  sub eax, ecx
  sub edx, ecx
@CompareLoop:
  {Next four bytes}
  add ecx, 4
  jns @MatchUpToLength
  {Compare four bytes (two characters) per cycle. This compare may include the
   trailing #0 for uneven string lengths, in which case no harm is done.}
  mov ebx, [eax + ecx]
  cmp ebx, [edx + ecx]
  je @CompareLoop
  {Found a mismatch: Swap the two characters into the correct order}
  mov edx, [edx + ecx]
  ror ebx, 16
  ror edx, 16
  {Compare the characters again, setting the flags}
  cmp ebx, edx
  {Pop the length difference, restore ebx and return}
  pop ebx
  pop ebx
  ret
  {All characters match up to the compare length}
@MatchUpToLength:
  {Restore the string length difference to eax}
  pop eax
  {Set the flags according to the length difference}
  add eax, eax
  {Restore ebx and return}
  pop ebx
@DoneNoPop:
  ret
@InitialMismatch:
  {Swap the two characters into the correct order}
  mov edx, [edx]
  ror ecx, 16
  ror edx, 16
  {Compare them again so the flags are set correctly}
  cmp ecx, edx
  ret
@PossibleNilString:
  {There is a good probability that one of the strings are nil (but not both)}
  test eax, eax
  jz @StringNil
  test edx, edx
  jnz @BothStringsNonNil
@StringNil:
  {One of the strings are nil, so compare pointers}
  cmp eax, edx
  ret
@LeftNotUTF16:
  {Create a UTF-16 copy of the left string}
  push 0
  push edx
  mov edx, eax
  lea eax, [esp + 4]
  call _UStrFromLStr
  {Call this routine with the new left string and the old right string}
  pop edx
  mov eax, [esp]
  jmp @RedoCompare
@RightNotUTF16:
  {Create a UTF-16 copy of the right string}
  push 0
  push eax
  lea eax, [esp + 4]
  call _UStrFromLStr
  {Call this routine with the old left string and the new right string}
  pop eax
  mov edx, [esp]
@RedoCompare:
  call _UStrCmp
  {Free the temporary string, keeping the flags unchanged. FreeMem is called
   directly because thread safety is not needed and _UStrClr is expensive.}
  pop eax
  pushf
  sub eax, skew
  call _FreeMem
  popf
end;

{Original code by Pierre le Riche. Licensed under the CodeGear license terms.}
procedure _UStrEqual{const Left, Right: UnicodeString};
asm
  {On entry:
     eax = @Left[1]
     edx = @Right[1],
   On exit:
     ZF = 1 if Left = Right
     ZF = 0 if Left <> Right}
  {Same string?}
  cmp eax, edx
  je @DoneNoPop
  {Any of the two possibly nil?}
  test eax, edx
  jz @PossibleNilString
@BothStringsNonNil:
  {Check that the left string is UTF-16}
  cmp byte ptr [eax - Skew].StrRec.elemSize, 2
  jne @LeftNotUTF16
  {Check that the right string is UTF-16}
  cmp byte ptr [edx - Skew].StrRec.elemSize, 2
  jne @RightNotUTF16
  {Get the string length}
  mov ecx, [eax - skew].StrRec.length
  {Are the string lengths the same?}
  cmp ecx, [edx - skew].StrRec.length
  jne @DoneNoPop
  {Two bytes per character}
  add ecx, ecx
  {Point eax and edx to just past the last character}
  add eax, ecx
  add edx, ecx
  {Make the counter negative based}
  neg ecx
  {Save ebx}
  push ebx
@CompareLoop:
  {Compare four bytes per iteration}
  mov ebx, [eax + ecx]
  cmp ebx, [edx + ecx]
  jne @Mismatch
  {Next two characters}
  add ecx, 4
  js @CompareLoop
  {Match: Set the ZF}
  xor eax, eax
@Mismatch:
  {Restore ebx}
  pop ebx
  {Done}
  ret
@PossibleNilString:
  {There is a good probability that one of the strings are nil (but not both)}
  test eax, eax
  jz @StringNil
  test edx, edx
  jnz @BothStringsNonNil
@StringNil:
  {One of the strings are nil. Clear the ZF.}
  cmp eax, edx
@DoneNoPop:
  ret
@LeftNotUTF16:
  {Swap the left and right strings, so the right string is the non UTF-16
   string.}
  xchg eax, edx
@RightNotUTF16:
  {Create a UTF-16 copy of the right string}
  push 0
  push eax
  lea eax, [esp + 4]
  call _UStrFromLStr
  {Call this routine with the old left string and the new right string}
  pop eax
  mov edx, [esp]
  call _UStrEqual
  {Free the temporary string, keeping the flags unchanged. FreeMem is called
   directly because thread safety is not needed and _UStrClr is expensive.}
  pop eax
  pushf
  sub eax, skew
  call _FreeMem
  popf
end;

function _UStrCopy(const S: UnicodeString; Index, Count: Integer): UnicodeString;
var
  L, N: Integer;
begin
  L := Length(S);
  if Index < 1 then Index := 0 else
  begin
    Dec(Index);
    if Index > L then Index := L;
  end;
  if Count < 0 then N := 0 else
  begin
    N := L - Index;
    if N > Count then N := Count;
  end;
  if StringElementSize(S) = SizeOf(WideChar) then
    _UStrFromPWCharLen(Result, PWideChar(Pointer(S)) + Index, N)
  else
    _UStrFromPCharLen(Result, PAnsiChar(Pointer(S)) + Index, N);
end;

procedure UStrSet(var S: UnicodeString; P: PWideChar);
{$IFDEF PUREPASCAL}
var
  Temp: Pointer;
begin
  Temp := Pointer(InterlockedExchange(Pointer(S), Pointer(P)));
  if Temp <> nil then
    _UStrClr(Temp);
end;
{$ELSE}
asm
        XCHG    [EAX],EDX
        TEST    EDX,EDX
        JZ      @@1
        PUSH    EDX
        MOV     EAX,ESP
        CALL    _UStrClr
        POP     EAX
@@1:
end;
{$ENDIF}

procedure _UStrDelete(var S: UnicodeString; Index, Count: Integer);
var
  L, N: Integer;
begin
  UniqueString(S);
  L := Length(S);
  if (Index >= 1) and (Index <= L) and (Count > 0) then
  begin
    Dec(Index);
    N := L - Index - Count;
    if N < 0 then
      N := 0;
    Move(PWideChar(Pointer(S))[L - N], PWideChar(Pointer(S))[Index], N * 2);
    SetLength(S, Index + N);
  end;
end;

procedure _UStrInsert(const Source: UnicodeString; var Dest: UnicodeString; Index: Integer);
var
  SourceLen, DestLen, NewLen: Integer;
  SelfInsert: Boolean;
begin
  SourceLen := Length(Source);
  if SourceLen > 0 then
  begin
    DestLen := Length(Dest);
    if Index < 1 then Index := 0 else
    begin
      Dec(Index);
      if Index > DestLen then Index := DestLen;
    end;
    SelfInsert := (Pointer(Source) = Pointer(Dest));
    NewLen := DestLen + SourceLen;
    if NewLen < 0 then   // overflow check
      _IntOver;
    SetLength(Dest, NewLen);
    if Index < DestLen then
      Move(PWideChar(Pointer(Dest))[Index], PWideChar(Pointer(Dest))[Index + SourceLen],
        (DestLen - Index) * 2);
    if SelfInsert then
      Move(Pointer(Dest)^, PWideChar(Pointer(Dest))[Index], SourceLen * 2)
    else
      Move(Pointer(Source)^, PWideChar(Pointer(Dest))[Index], SourceLen * 2);
  end;
end;

function UnicodeStringToUCS4String(const S: UnicodeString): UCS4String;
var
  I: Integer;
  CharCount: Integer;
begin
  CharCount := 0;
  SetLength(Result, Length(S) + 1);
  I := 0;
  while I < Length(S) do
  begin
    if ((S[I + 1] >= #$D800) and (S[I + 1] <= #$DFFF)) and (I + 1 < Length(S)) then
    begin
      Result[I] := UCS4Char((Cardinal(S[I + 1]) and $000003FF) shl 10 or (Cardinal(S[I + 2]) and $000003FF) + $00010000);
      Inc(I);
    end
    else
      Result[I] := UCS4Char(S[I + 1]);
    Inc(CharCount);
    Inc(I);
  end;
  Result[CharCount] := 0;
  SetLength(Result, CharCount + 1);
end;

function UCS4StringToUnicodeString(const S: UCS4String): UnicodeString;
var
  I: Integer;
  CharCount: Integer;
begin
  SetLength(Result, Length(S) * 2 - 1); //Maximum possible number of characters
  CharCount := 0;

  I := 0;
  while I < Length(S) - 1 do
  begin
    if S[I] >= $10000 then
    begin
      Inc(CharCount);
      Result[CharCount] := WideChar((((S[I] - $00010000) shr 10) and $000003FF) or $D800);
      Inc(CharCount);
      Result[CharCount] := WideChar(((S[I] - $00010000) and $000003FF)or $DC00);
    end
    else
    begin
      Inc(CharCount);
      Result[CharCount] := WideChar(S[I]);
    end;

    Inc(I);
  end;

  SetLength(Result, CharCount);
end;


//function UTF8Encode(const WS: UnicodeString): UTF8String;
//function UTF8Decode(const S: UTF8String): UnicodeString;



procedure _ReadUString(var t: TTextRec; var s: UnicodeString);
var
  Temp: AnsiString;
begin
  // !!! FIXME
  _ReadLString(t, Temp, DefaultSystemCodePage);
  s := UnicodeString(Temp);
end;

function _Write0UString(var t: TTextRec; const s: UnicodeString): Pointer;
begin
  Result := _WriteWString(t, s, 0);
end;

function _WriteUString(var t: TTextRec; const s: UnicodeString; width: Longint): Pointer;
var
  i: Integer;
begin
  // !!! FIXME
  i := Length(s);
  _WriteSpaces(t, width - i);
  Result := _WriteLString(t, AnsiString(s), 0);
end;

procedure _InitUnicodeStrings;
asm
     { ->    EAX     Pointer to init table               }
     {                 record                            }
     {                   cnt: Integer;                   }
     {                   tab: array [1..cnt] record      }
     {                      variableAddress: Pointer;    }
     {                      stringAddress: ^Pointer;     }
     {                   end;                            }
     {                 end;                              }

    PUSH    EBX
    PUSH    ESI
    MOV     EBX,[EAX]
    LEA     ESI,[EAX+4]
@@loop:
    MOV     EDX,[ESI+4]     { load address of string    }
    MOV     EAX,[ESI]       { load address of variable  }
    CALL    _UStrAsg
    ADD     ESI,8
    DEC     EBX
    JNZ     @@loop

    POP     ESI
    POP     EBX
end;

function InternalUniqueStringU(var str): Pointer;
asm
        { ->    EAX pointer to str              }
        { <-    EAX pointer to unique copy      }
        MOV     EDX,[EAX]       // EDX := str
        TEST    EDX,EDX         // nil?
        JE      @@exit

        CMP     [EDX-skew].StrRec.elemSize,2
        JE      @@isUnicode

        PUSH    EAX
        CALL    EnsureUnicodeString
        POP     EAX
        MOV     EDX,[EAX]

@@isUnicode:
        MOV     ECX,[EDX-skew].StrRec.refCnt // ECX := str.refCnt
        DEC     ECX             // refCnt = 1?
        JE      @@exit

        PUSH    EBX
        MOV     EBX,EAX         // EBX := @str
        MOV     EAX,[EDX-skew].StrRec.length
        CALL    _NewUnicodeString
        MOV     EDX,EAX         // EDX := newStr
        MOV     EAX,[EBX]       // EAX := str
        MOV     [EBX],EDX       // @newStr^ := newStr
        PUSH    EAX             // save str
        MOV     ECX,[EAX-skew].StrRec.length
        SHL     ECX,1           // ECX := Length(str) * 2
        CALL    Move            // Move(str, newStr, Length(str) * 2)
        POP     EAX             // EAX := str
        MOV     ECX,[EAX-skew].StrRec.refCnt // ECX := str.refCnt
        DEC     ECX
        JL      @@skip          // Was already zero?
   LOCK DEC     [EAX-skew].StrRec.refCnt
        JNZ     @@skip
        LEA     EAX,[EAX-skew].StrRec.codePage  { if refCnt now zero, deallocate}
        CALL    _FreeMem
@@skip:
        MOV     EDX,[EBX]       // EDX := @str^ (= newStr)
        POP     EBX
@@exit:
        MOV     EAX,EDX         // EAX := newStr
end;

procedure UniqueString(var str: UnicodeString); overload;
asm
        JMP     InternalUniqueStringU
end;

procedure _UniqueStringU(var str: UnicodeString);
asm
        JMP     InternalUniqueStringU
end;

function _NewUnicodeString(CharLength: Longint): Pointer;
{$IFDEF PUREPASCAL}
var
  P: PStrRec;
begin
  Result := nil;
  if CharLength <= 0 then Exit;
  // Allocate a memory with record and extra wide-null terminator.
  GetMem(P, SizeOf(StrRec) + (CharLength + 1) * SizeOf(WideChar));
  Result := Pointer(Integer(P) + sizeof(StrRec));
  P.length := CharLength;
  P.refCnt := 1;
  P.elemSize := SizeOf(WideChar);
  P.codePage := Word(DefaultUnicodeCodePage);
  PWideChar(Result)[CharLength] := #0;
end;
{$ELSE}
asm
        { ->    EAX     length                  }
        { <-    EAX pointer to new string       }
        TEST    EAX,EAX
        JLE     @@lengthLEZero  // length <= 0?
        PUSH    EAX             // save length
        ADD     EAX,EAX         // convert to bytes
        JO      @@overflow
        ADD     EAX,rOff+2      // + record + terminator
        JO      @@overflow
        CALL    _GetMem
        ADD     EAX,rOff
        POP     EDX                              // requested string length
        MOV     [EAX-skew].StrRec.refCnt,1
        MOV     [EAX-skew].StrRec.length,EDX
        MOV     word ptr [EAX+EDX*2],0           // wide null terminator
        MOV     word ptr [EAX-skew].StrRec.elemSize,2
        MOVZX   EDX,Word Ptr DefaultUnicodeCodePage
        MOV     word ptr [EAX-skew].StrRec.codePage,DX
        RET
@@overflow:
        JMP     _IntOver
@@lengthLEZero:
        XOR     EAX,EAX
end;
{$ENDIF}

function Pos(const substr, str: UnicodeString): Integer; overload;
asm
{     ->EAX     Pointer to substr               }
{       EDX     Pointer to string               }
{     <-EAX     Position of substr in str or 0  }

        TEST    EAX,EAX
        JE      @@noWork

        TEST    EDX,EDX
        JE      @@stringEmpty

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX                         { Point ESI to substr           }
        MOV     EDI,EDX                         { Point EDI to s                }

        MOV     ECX,[EDI-4]                     { ECX = Length(s)               }

        PUSH    EDI                             { remember s position to calculate index        }

        MOV     EDX,[ESI-4]                     { EDX = Length(substr)          }

        DEC     EDX                             { EDX = Length(substr) - 1              }
        JS      @@fail                          { < 0 ? return 0                        }
        MOV     AX,[ESI]                        { AL = first char of substr             }
        ADD     ESI,2                           { Point ESI to 2'nd char of substr      }

        SUB     ECX,EDX                         { #positions in s to look at    }
                                                { = Length(s) - Length(substr) + 1      }
        JLE     @@fail
@@loop:
        REPNE   SCASW
        JNE     @@fail
        MOV     EBX,ECX                         { save outer loop counter               }
        PUSH    ESI                             { save outer loop substr pointer        }
        PUSH    EDI                             { save outer loop s pointer             }

        MOV     ECX,EDX
        REPE    CMPSW
        POP     EDI                             { restore outer loop s pointer  }
        POP     ESI                             { restore outer loop substr pointer     }
        JE      @@found
        MOV     ECX,EBX                         { restore outer loop counter    }
        JMP     @@loop

@@fail:
        POP     EDX                             { get rid of saved s pointer    }
        XOR     EAX,EAX
        JMP     @@exit

@@stringEmpty:
        XOR     EAX,EAX
        JMP     @@noWork

@@found:
        POP     EDX                             { restore pointer to first char of s    }
        MOV     EAX,EDI                         { EDI points of char after match        }
        SUB     EAX,EDX                         { the difference is the correct index   }
        SHR     EAX,1
@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
@@noWork:
end;

procedure _UGetDir(D: Byte; var S: UnicodeString);
{$IFDEF MSWINDOWS}
var
  Drive: array[0..3] of WideChar;
  DirBuf, SaveBuf: array[0..MAX_PATH] of WideChar;
begin
  if D <> 0 then
  begin
    Drive[0] := WideChar(D + Ord('A') - 1);
    Drive[1] := ':';
    Drive[2] := #0;
    GetCurrentDirectoryW(SizeOf(SaveBuf) div SizeOf(WideChar), SaveBuf);
    SetCurrentDirectoryW(Drive);
  end;
  GetCurrentDirectoryW(SizeOf(DirBuf) div SizeOf(WideChar), DirBuf);
  if D <> 0 then SetCurrentDirectoryW(SaveBuf);
  S := DirBuf;
{$ENDIF MSWINDOWS}
{$IF defined(LINUX) or defined(MACOSX)}
var
  DirBuf: array[0..MAX_PATH] of AnsiChar;
begin
  __getcwd(DirBuf, sizeof(DirBuf));
  S := UnicodeString(DirBuf);
{$IFEND LINUX or MACOSX}
end;

procedure SetCodePage(var S: RawByteString; CodePage: Word; Convert: Boolean);
var
  W: UnicodeString;
  NewLen: Integer;
begin
  if (StringCodePage(S) = CodePage) or (Length(S) = 0) then
    Exit;
  if Convert then
  begin
    if StringElementSize(S) = 1 then
      W := UnicodeString(S)  // This up-converts to Unicode utf-16 using the existing codepage in the payload
    else
      W := UnicodeString(Pointer(S));  // Payload is already utf-16 so just reference it
    // now find out how large the resulting string will be
    NewLen := CharFromWChar(nil, 0, PWideChar(W), Length(W), CodePage);
    SetLength(S, NewLen);
    // finally actually convert the payload based on the new CodePage
    if NewLen > 0 then
      CharFromWChar(PAnsiChar(S), Length(S), PWideChar(W), Length(W), CodePage);
  end
  else
    UniqueString(AnsiString(S));
  if Length(S) > 0 then
    PWord(Integer(S) - 12)^ := CodePage;
end;

// function StringOfChar(ch: WideChar; Count: Integer): UnicodeString; overload;


type
  PPTypeInfo = ^PTypeInfo;
  PTypeInfo = ^TTypeInfo;
  TTypeInfo = packed record
    Kind: Byte;
    Name: ShortString;
   {TypeData: TTypeData}
  end;

  TFieldInfo = packed record
    TypeInfo: PPTypeInfo;
    Offset: Cardinal;
  end;

  PFieldTable = ^TFieldTable;
  TFieldTable = packed record
    X: Word;
    Size: Cardinal;
    Count: Cardinal;
    Fields: array [0..0] of TFieldInfo;
  end;

{ ===========================================================================
  InitializeRecord, InitializeArray, and Initialize are PIC safe even though
  they alter EBX because they only call each other.  They never call out to
  other functions and they don't access global data.

  FinalizeRecord, Finalize, and FinalizeArray are PIC safe because they call
  Pascal routines which will have EBX fixup prologs.
  ===========================================================================}

procedure   _InitializeRecord(p: Pointer; typeInfo: Pointer);
{$IFDEF PUREPASCAL}
var
  FT: PFieldTable;
  I: Cardinal;
begin
  FT := PFieldTable(Integer(typeInfo) + Byte(PTypeInfo(typeInfo).Name[0]));
  for I := FT.Count-1 downto 0 do
    _InitializeArray(Pointer(Cardinal(P) + FT.Fields[I].Offset), FT.Fields[I].TypeInfo^, 1);
end;
{$ELSE}
asm
        { ->    EAX pointer to record to be initialized }
        {       EDX pointer to type info                }

        XOR     ECX,ECX

        PUSH    EBX
        MOV     CL,[EDX+1]                  { type name length }

        PUSH    ESI
        PUSH    EDI

        MOV     EBX,EAX                     // PIC safe. See comment above
        LEA     ESI,[EDX+ECX+2+8]           { address of destructable fields }
        MOV     EDI,[EDX+ECX+2+4]           { number of destructable fields }
        TEST    EDI,EDI
        JZ      @@exit

@@loop:

        MOV     EDX,[ESI]
        MOV     EAX,[ESI+4]
        ADD     EAX,EBX
        MOV     EDX,[EDX]
        MOV     ECX,1
        CALL    _InitializeArray
        ADD     ESI,8
        DEC     EDI
        JG      @@loop

@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$ENDIF}


const
  tkLString   = 10;
  tkWString   = 11;
  tkVariant   = 12;
  tkArray     = 13;
  tkRecord    = 14;
  tkInterface = 15;
  tkDynArray  = 17;
  tkUString   = 18;
  tkMRecord   = 19;

procedure InitializeArray(p: Pointer; typeInfo: Pointer; elemCount: Cardinal);
asm
  JMP _InitializeArray
end;

procedure       _InitializeArray(p: Pointer; typeInfo: Pointer; elemCount: Cardinal);
{$IFDEF PUREPASCAL}
var
  FT: PFieldTable;
begin
  if elemCount = 0 then Exit;
  case PTypeInfo(typeInfo).Kind of
    tkLString, tkWString, tkInterface, tkDynArray, tkUString:
      while elemCount > 0 do
      begin
        PInteger(P)^ := 0;
        Inc(Integer(P), 4);
        Dec(elemCount);
      end;
    tkVariant:
      while elemCount > 0 do
      begin
        PInteger(P)^ := 0;
        PInteger(Integer(P)+4)^ := 0;
        PInteger(Integer(P)+8)^ := 0;
        PInteger(Integer(P)+12)^ := 0;
        Inc(Integer(P), sizeof(Variant));
        Dec(elemCount);
      end;
    tkArray:
      begin
        FT := PFieldTable(Integer(typeInfo) + Byte(PTypeInfo(typeInfo).Name[0]));
        while elemCount > 0 do
        begin
          _InitializeArray(P, FT.Fields[0].TypeInfo^, FT.Count);
          Inc(Integer(P), FT.Size);
          Dec(elemCount);
        end;
      end;
    tkRecord:
      begin
        FT := PFieldTable(Integer(typeInfo) + Byte(PTypeInfo(typeInfo).Name[0]));
        while elemCount > 0 do
        begin
          _InitializeRecord(P, typeInfo);
          Inc(Integer(P), FT.Size);
          Dec(elemCount);
        end;
      end;
  else
    Error(reInvalidPtr);
  end;
end;
{$ELSE}
asm
        { ->    EAX     pointer to data to be initialized       }
        {       EDX     pointer to type info describing data    }
        {       ECX number of elements of that type             }

        TEST    ECX, ECX
        JZ      @@zerolength

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EBX,EAX             // PIC safe.  See comment above
        MOV     ESI,EDX
        MOV     EDI,ECX

        XOR     EDX,EDX
        MOV     AL,[ESI]
        MOV     DL,[ESI+1]
        XOR     ECX,ECX

        CMP     AL,tkLString
        JE      @@LString
        CMP     AL,tkWString
        JE      @@WString
        CMP     AL,tkVariant
        JE      @@Variant
        CMP     AL,tkArray
        JE      @@Array
        CMP     AL,tkRecord
        JE      @@Record
        CMP     AL,tkInterface
        JE      @@Interface
        CMP     AL,tkDynArray
        JE      @@DynArray
        CMP     AL,tkUString
        JE      @@UString
        MOV     AL,reInvalidPtr
        POP     EDI
        POP     ESI
        POP     EBX
        JMP     Error

@@LString:
@@WString:
@@Interface:
@@DynArray:
@@UString:
        MOV     [EBX],ECX
        ADD     EBX,4
        DEC     EDI
        JG      @@LString
        JMP     @@exit

@@Variant:
        MOV     [EBX   ],ECX
        MOV     [EBX+ 4],ECX
        MOV     [EBX+ 8],ECX
        MOV     [EBX+12],ECX
        ADD     EBX,16
        DEC     EDI
        JG      @@Variant
        JMP     @@exit

@@Array:
        PUSH    EBP
        MOV     EBP,EDX
@@ArrayLoop:
        MOV     EDX,[ESI+EBP+2+8]    // address of destructable fields typeinfo
        MOV     EAX,EBX
        ADD     EBX,[ESI+EBP+2]      // size in bytes of the array data
        MOV     ECX,[ESI+EBP+2+4]    // number of destructable fields
        MOV     EDX,[EDX]
        CALL    _InitializeArray
        DEC     EDI
        JG      @@ArrayLoop
        POP     EBP
        JMP     @@exit

@@Record:
        PUSH    EBP
        MOV     EBP,EDX
@@RecordLoop:
        MOV     EAX,EBX
        ADD     EBX,[ESI+EBP+2]
        MOV     EDX,ESI
        CALL    _InitializeRecord
        DEC     EDI
        JG      @@RecordLoop
        POP     EBP

@@exit:

        POP     EDI
        POP     ESI
        POP     EBX
@@zerolength:
end;
{$ENDIF}

procedure       _Initialize(p: Pointer; typeInfo: Pointer);
{$IFDEF PUREPASCAL}
begin
  _InitializeArray(p, typeInfo, 1);
end;
{$ELSE}
asm
        MOV     ECX,1
        JMP     _InitializeArray
end;
{$ENDIF}

procedure _FinalizeRecord(p: Pointer; typeInfo: Pointer);
{$IFDEF PUREPASCAL}
var
  FT: PFieldTable;
  I: Cardinal;
begin
  FT := PFieldTable(Integer(typeInfo) + Byte(PTypeInfo(typeInfo).Name[0]));
  for I := 0 to FT.Count-1 do
    _FinalizeArray(Pointer(Cardinal(P) + FT.Fields[I].Offset), FT.Fields[I].TypeInfo^, 1);
end;
{$ELSE}
asm
        { ->    EAX pointer to record to be finalized   }
        {       EDX pointer to type info                }

        XOR     ECX,ECX

        PUSH    EBX
        MOV     CL,[EDX+1]

        PUSH    ESI
        PUSH    EDI

        MOV     EBX,EAX
        LEA     ESI,[EDX+ECX+2+8]
        MOV     EDI,[EDX+ECX+2+4]
        TEST    EDI,EDI
        JZ      @@exit

@@loop:

        MOV     EDX,[ESI]
        MOV     EAX,[ESI+4]
        ADD     EAX,EBX
        MOV     EDX,[EDX]
        MOV     ECX,1
        CALL    _FinalizeArray
        ADD     ESI,8
        DEC     EDI
        JG      @@loop

@@exit:
        MOV     EAX,EBX

        POP     EDI
        POP     ESI
        POP     EBX
end;
{$ENDIF}

procedure _VarClr(var v: TVarData);
begin
  if Assigned(VarClearProc) then
    VarClearProc(v)
  else
    Error(reVarInvalidOp);
end;

procedure _FinalizeArray(p: Pointer; typeInfo: Pointer; elemCount: Cardinal);
{$IFDEF PUREPASCAL}
var
  FT: PFieldTable;
begin
  if elemCount = 0 then Exit;
  case PTypeInfo(typeInfo).Kind of
    tkLString: _LStrArrayClr(P^, elemCount);
    tkWString: _WStrArrayClr(P^, elemCount);
    tkUString: _UStrArrayClr(P^, elemCount);
    tkVariant:
      while elemCount > 0 do
      begin
        _VarClr(PVarData(P)^);
        Inc(Integer(P), sizeof(Variant));
        Dec(elemCount);
      end;
    tkArray:
      begin
        FT := PFieldTable(Integer(typeInfo) + Byte(PTypeInfo(typeInfo).Name[0]));
        while elemCount > 0 do
        begin
          _FinalizeArray(P, FT.Fields[0].TypeInfo^, FT.Count);
          Inc(Integer(P), FT.Size);
          Dec(elemCount);
        end;
      end;
    tkRecord:
      begin
        FT := PFieldTable(Integer(typeInfo) + Byte(PTypeInfo(typeInfo).Name[0]));
        while elemCount > 0 do
        begin
          _FinalizeRecord(P, typeInfo);
          Inc(Integer(P), FT.Size);
          Dec(elemCount);
        end;
      end;
    tkInterface:
      while elemCount > 0 do
      begin
        _IntfClear(IInterface(P^));
        Inc(Integer(P), 4);
        Dec(elemCount);
      end;
    tkDynArray:
      while elemCount > 0 do
      begin
        _DynArrayClear(P, typeInfo);
        Inc(Integer(P), 4);
        Dec(elemCount);
      end;
  else
    Error(reInvalidPtr);
  end;
end;
{$ELSE}
asm
        { ->    EAX     pointer to data to be finalized         }
        {       EDX     pointer to type info describing data    }
        {       ECX number of elements of that type             }

        { This code appears to be PIC safe.  The functions called from
          here either don't make external calls or call Pascal
          routines that will fix up EBX in their prolog code
          (FreeMem, VarClr, IntfClr).  }

        CMP     ECX, 0                        { no array -> nop }
        JE      @@zerolength

        PUSH    EAX
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EBX,EAX
        MOV     ESI,EDX
        MOV     EDI,ECX

        XOR     EDX,EDX
        MOV     AL,[ESI]
        MOV     DL,[ESI+1]

        CMP     AL,tkLString
        JE      @@LString

        CMP     AL,tkUString
        JE      @@UString

        CMP     AL,tkWString
        JE      @@WString

        CMP     AL,tkVariant
        JE      @@Variant

        CMP     AL,tkArray
        JE      @@Array

        CMP     AL,tkRecord
        JE      @@Record

        CMP     AL,tkInterface
        JE      @@Interface

        CMP     AL,tkDynArray
        JE      @@DynArray

        JMP     @@error

@@LString:
        CMP     ECX,1
        MOV     EAX,EBX
        JG      @@LStringArray
        CALL    _LStrClr
        JMP     @@exit
@@LStringArray:
        MOV     EDX,ECX
        CALL    _LStrArrayClr
        JMP     @@exit

@@WString:
        CMP     ECX,1
        MOV     EAX,EBX
        JG      @@WStringArray
        CALL    _WStrClr
        JMP     @@exit
@@WStringArray:
        MOV     EDX,ECX
        CALL    _WStrArrayClr
        JMP     @@exit

@@UString:
        CMP     ECX,1
        MOV     EAX,EBX
        JG      @@UStringArray
        CALL    _UStrClr
        JMP     @@exit
@@UStringArray:
        MOV     EDX,ECX
        CALL    _UStrArrayClr
        JMP     @@exit

@@Variant:
        MOV     EAX,EBX
        ADD     EBX,16
        CALL    _VarClr
        DEC     EDI
        JG      @@Variant
        JMP     @@exit
@@Array:
        PUSH    EBP
        MOV     EBP,EDX
@@ArrayLoop:
        MOV     EDX,[ESI+EBP+2+8]
        MOV     EAX,EBX
        ADD     EBX,[ESI+EBP+2]
        MOV     ECX,[ESI+EBP+2+4]
        MOV     EDX,[EDX]
        CALL    _FinalizeArray
        DEC     EDI
        JG      @@ArrayLoop
        POP     EBP
        JMP     @@exit

@@Record:
        PUSH    EBP
        MOV     EBP,EDX
@@RecordLoop:
        { inv: EDI = number of array elements to finalize }

        MOV     EAX,EBX
        ADD     EBX,[ESI+EBP+2]
        MOV     EDX,ESI
        CALL    _FinalizeRecord
        DEC     EDI
        JG      @@RecordLoop
        POP     EBP
        JMP     @@exit

@@Interface:
        MOV     EAX,EBX
        ADD     EBX,4
        CALL    _IntfClear
        DEC     EDI
        JG      @@Interface
        JMP     @@exit

@@DynArray:
        MOV     EAX,EBX
        MOV     EDX,ESI
        ADD     EBX,4
        CALL    _DynArrayClear
        DEC     EDI
        JG      @@DynArray
        JMP     @@exit

@@error:
        POP     EDI
        POP     ESI
        POP     EBX
        POP     EAX
        MOV     AL,reInvalidPtr
        JMP     Error

@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
        POP     EAX
@@zerolength:
end;
{$ENDIF}

procedure _Finalize(p: Pointer; typeInfo: Pointer);
{$IFDEF PUREPASCAL}
begin
  _FinalizeArray(p, typeInfo, 1);
end;
{$ELSE}
asm
        MOV     ECX,1
        JMP     _FinalizeArray
end;
{$ENDIF}

procedure       _AddRefRecord{ p: Pointer; typeInfo: Pointer };
asm
        { ->    EAX pointer to record to be referenced  }
        {       EDX pointer to type info        }

        XOR     ECX,ECX

        PUSH    EBX
        MOV     CL,[EDX+1]

        PUSH    ESI
        PUSH    EDI

        MOV     EBX,EAX
        LEA     ESI,[EDX+ECX+2+8]
        MOV     EDI,[EDX+ECX+2+4]

@@loop:

        MOV     EDX,[ESI]
        MOV     EAX,[ESI+4]
        ADD     EAX,EBX
        MOV     EDX,[EDX]
        MOV     ECX, 1
        CALL    _AddRefArray
        ADD     ESI,8
        DEC     EDI
        JG      @@loop

        POP     EDI
        POP     ESI
        POP     EBX
end;

procedure _VarAddRef(var v: TVarData);
begin
  if Assigned(VarAddRefProc) then
    VarAddRefProc(v)
  else
    Error(reVarInvalidOp);
end;

procedure       _AddRefArray{ p: Pointer; typeInfo: Pointer; elemCount: Longint};
asm
        { ->    EAX     pointer to data to be referenced        }
        {       EDX     pointer to type info describing data    }
        {       ECX number of elements of that type             }

        { This code appears to be PIC safe.  The functions called from
          here either don't make external calls (LStrAddRef, WStrAddRef) or
          are Pascal routines that will fix up EBX in their prolog code
          (VarAddRef, IntfAddRef).  }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        TEST  ECX,ECX
        JZ    @@exit

        MOV     EBX,EAX
        MOV     ESI,EDX
        MOV     EDI,ECX

        XOR     EDX,EDX
        MOV     AL,[ESI]
        MOV     DL,[ESI+1]

        CMP     AL,tkLString
        JE      @@LString
        CMP     AL,tkWString
        JE      @@WString
        CMP     AL,tkUString
        JE      @@UString
        CMP     AL,tkVariant
        JE      @@Variant
        CMP     AL,tkArray
        JE      @@Array
        CMP     AL,tkRecord
        JE      @@Record
        CMP     AL,tkInterface
        JE      @@Interface
        CMP     AL,tkDynArray
        JE      @@DynArray
        MOV     AL,reInvalidPtr
        POP     EDI
        POP     ESI
        POP     EBX
        JMP     Error

@@LString:
@@UString:
{$IF defined(LINUX) or defined(MACOSX)}
@@WString:
{$IFEND LINUX or MACOSX}
        MOV     EAX,[EBX]
        ADD     EBX,4
        CALL    _LStrAddRef
        DEC     EDI
        JG      @@LString
        JMP     @@exit

{$IFDEF MSWINDOWS}
@@WString:
        MOV     EAX,EBX
        ADD     EBX,4
        CALL    _WStrAddRef
        DEC     EDI
        JG      @@WString
        JMP     @@exit
{$ENDIF MSWINDOWS}
@@Variant:
        MOV     EAX,EBX
        ADD     EBX,16
        CALL    _VarAddRef
        DEC     EDI
        JG      @@Variant
        JMP     @@exit

@@Array:
        PUSH    EBP
        MOV     EBP,EDX
@@ArrayLoop:
        MOV     EDX,[ESI+EBP+2+8]
        MOV     EAX,EBX
        ADD     EBX,[ESI+EBP+2]
        MOV     ECX,[ESI+EBP+2+4]
        MOV     EDX,[EDX]
        CALL    _AddRefArray
        DEC     EDI
        JG      @@ArrayLoop
        POP     EBP
        JMP     @@exit

@@Record:
        PUSH    EBP
        MOV     EBP,EDX
@@RecordLoop:
        MOV     EAX,EBX
        ADD     EBX,[ESI+EBP+2]
        MOV     EDX,ESI
        CALL    _AddRefRecord
        DEC     EDI
        JG      @@RecordLoop
        POP     EBP
        JMP     @@exit

@@Interface:
        MOV     EAX,[EBX]
        ADD     EBX,4
        CALL    _IntfAddRef
        DEC     EDI
        JG      @@Interface
        JMP     @@exit

@@DynArray:
        MOV     EAX,[EBX]
        ADD     EBX,4
        CALL    _DynArrayAddRef
        DEC     EDI
        JG      @@DynArray
@@exit:

        POP     EDI
        POP     ESI
        POP     EBX
end;


procedure       _AddRef{ p: Pointer; typeInfo: Pointer};
asm
        MOV     ECX,1
        JMP     _AddRefArray
end;

procedure _VarCopy(var Dest: TVarData; const Src: TVarData);
begin
  if Assigned(VarCopyProc) then
    VarCopyProc(Dest, Src)
  else
    Error(reVarInvalidOp);
end;

procedure       _CopyRecord{ dest, source, typeInfo: Pointer };
asm
        { ->    EAX pointer to dest             }
        {       EDX pointer to source           }
        {       ECX pointer to typeInfo         }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        MOV     EBX,EAX
        MOV     ESI,EDX

        XOR     EAX,EAX
        MOV     AL,[ECX+1]

        LEA     EDI,[ECX+EAX+2+8]
        MOV     EBP,[EDI-4]
        XOR     EAX,EAX
        MOV     ECX,[EDI-8]
        TEST    EBP,EBP
        JZ      @@moveWhole
        PUSH    ECX
@@loop:
        MOV     ECX,[EDI+4]
        SUB     ECX,EAX
        JLE     @@nomove1
        MOV     EDX,EAX
        ADD     EAX,ESI
        ADD     EDX,EBX
        CALL    Move
@@noMove1:
        MOV     EAX,[EDI+4]

        MOV     EDX,[EDI]
        MOV     EDX,[EDX]
        MOV     CL,[EDX]

        CMP     CL,tkLString
        JE      @@LString
        CMP     CL,tkWString
        JE      @@WString
        CMP     CL,tkUString
        JE      @@UString
        CMP     CL,tkVariant
        JE      @@Variant
        CMP     CL,tkArray
        JE      @@Array
        CMP     CL,tkRecord
        JE      @@Record
        CMP     CL,tkInterface
        JE      @@Interface
        CMP     CL,tkDynArray
        JE      @@DynArray
        MOV     AL,reInvalidPtr
        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
        JMP     Error

@@LString:
        MOV     EDX,[ESI+EAX]
        ADD     EAX,EBX
        CALL    _LStrAsg
        MOV     EAX,4
        JMP     @@common

@@UString:
        MOV     EDX,[ESI+EAX]
        ADD     EAX,EBX
        CALL    _UStrAsg
        MOV     EAX,4
        JMP     @@common

@@WString:
        MOV     EDX,[ESI+EAX]
        ADD     EAX,EBX
        CALL    _WStrAsg
        MOV     EAX,4
        JMP     @@common

@@Variant:
        LEA     EDX,[ESI+EAX]
        ADD     EAX,EBX
        CALL    _VarCopy
        MOV     EAX,16
        JMP     @@common

@@Array:
        XOR     ECX,ECX
        MOV     CL,[EDX+1]
        PUSH    dword ptr [EDX+ECX+2]
        PUSH    dword ptr [EDX+ECX+2+4]
        MOV     ECX,[EDX+ECX+2+8]
        MOV     ECX,[ECX]
        LEA     EDX,[ESI+EAX]
        ADD     EAX,EBX
        CALL    _CopyArray
        POP     EAX
        JMP     @@common

@@Record:
        XOR     ECX,ECX
        MOV     CL,[EDX+1]
        MOV     ECX,[EDX+ECX+2]
        PUSH    ECX
        MOV     ECX,EDX
        LEA     EDX,[ESI+EAX]
        ADD     EAX,EBX
        CALL    _CopyRecord
        POP     EAX
        JMP     @@common

@@Interface:
        MOV     EDX,[ESI+EAX]
        ADD     EAX,EBX
        CALL    _IntfCopy
        MOV     EAX,4
        JMP     @@common

@@DynArray:
        MOV     ECX,EDX
        MOV     EDX,[ESI+EAX]
        ADD     EAX,EBX
        CALL    _DynArrayAsg
        MOV     EAX,4

@@common:
        ADD     EAX,[EDI+4]
        ADD     EDI,8
        DEC     EBP
        JNZ     @@loop

        POP     ECX
@@moveWhole:
        SUB     ECX,EAX
        JLE     @@noMove2
        LEA     EDX,[EBX+EAX]
        ADD     EAX,ESI
        CALL    Move
@@noMove2:

        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
end;


procedure       _CopyObject{ dest, source: Pointer; vmtPtrOffs: Longint; typeInfo: Pointer };
asm
        { ->    EAX pointer to dest             }
        {       EDX pointer to source           }
        {       ECX offset of vmt in object     }
        {       [ESP+4] pointer to typeInfo     }

        ADD     ECX,EAX                         { pointer to dest vmt }
        PUSH    dword ptr [ECX]                 { save dest vmt }
        PUSH    ECX
        MOV     ECX,[ESP+4+4+4]
        CALL    _CopyRecord
        POP     ECX
        POP     dword ptr [ECX]                 { restore dest vmt }
        RET     4

end;

procedure       _CopyArray{ dest, source, typeInfo: Pointer; cnt: Integer };
asm
        { ->    EAX pointer to dest             }
        {       EDX pointer to source           }
        {       ECX pointer to typeInfo         }
        {       [ESP+4] count                   }
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        MOV     EBX,EAX
        MOV     ESI,EDX
        MOV     EDI,ECX
        MOV     EBP,[ESP+4+4*4]

        MOV     CL,[EDI]

        CMP     CL,tkLString
        JE      @@LString
        CMP     CL,tkWString
        JE      @@WString
        CMP     CL,tkUString
        JE      @@UString
        CMP     CL,tkVariant
        JE      @@Variant
        CMP     CL,tkArray
        JE      @@Array
        CMP     CL,tkRecord
        JE      @@Record
        CMP     CL,tkInterface
        JE      @@Interface
        CMP     CL,tkDynArray
        JE      @@DynArray
        MOV     AL,reInvalidPtr
        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
        JMP     Error

@@LString:
        MOV     EAX,EBX
        MOV     EDX,[ESI]
        CALL    _LStrAsg
        ADD     EBX,4
        ADD     ESI,4
        DEC     EBP
        JNE     @@LString
        JMP     @@exit

@@WString:
        MOV     EAX,EBX
        MOV     EDX,[ESI]
        CALL    _WStrAsg
        ADD     EBX,4
        ADD     ESI,4
        DEC     EBP
        JNE     @@WString
        JMP     @@exit

@@UString:
        MOV     EAX,EBX
        MOV     EDX,[ESI]
        CALL    _UStrAsg
        ADD     EBX,4
        ADD     ESI,4
        DEC     EBP
        JNE     @@UString
        JMP     @@exit

@@Variant:
        MOV     EAX,EBX
        MOV     EDX,ESI
        CALL    _VarCopy
        ADD     EBX,16
        ADD     ESI,16
        DEC     EBP
        JNE     @@Variant
        JMP     @@exit

@@Array:
        XOR     ECX,ECX
        MOV     CL,[EDI+1]
        LEA     EDI,[EDI+ECX+2]
@@ArrayLoop:
        MOV     EAX,EBX
        MOV     EDX,ESI
        MOV     ECX,[EDI+8]
        MOV     ECX,[ECX]
        PUSH    dword ptr [EDI+4]
        CALL    _CopyArray
        ADD     EBX,[EDI]
        ADD     ESI,[EDI]
        DEC     EBP
        JNE     @@ArrayLoop
        JMP     @@exit

@@Record:
        MOV     EAX,EBX
        MOV     EDX,ESI
        MOV     ECX,EDI
        CALL    _CopyRecord
        XOR     EAX,EAX
        MOV     AL,[EDI+1]
        ADD     EBX,[EDI+EAX+2]
        ADD     ESI,[EDI+EAX+2]
        DEC     EBP
        JNE     @@Record
        JMP     @@exit

@@Interface:
        MOV     EAX,EBX
        MOV     EDX,[ESI]
        CALL    _IntfCopy
        ADD     EBX,4
        ADD     ESI,4
        DEC     EBP
        JNE     @@Interface
        JMP     @@exit

@@DynArray:
        MOV     EAX,EBX
        MOV     EDX,[ESI]
        MOV     ECX,EDI
        CALL    _DynArrayAsg
        ADD     EBX,4
        ADD     ESI,4
        DEC     EBP
        JNE     @@DynArray

@@exit:
        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
        RET     4
end;


function _New(size: Longint; typeInfo: Pointer): Pointer;
{$IFDEF PUREPASCAL}
begin
  GetMem(Result, size);
  if Result <> nil then
    _Initialize(Result, typeInfo);
end;
{$ELSE}
asm
        { ->    EAX size of object to allocate  }
        {       EDX pointer to typeInfo         }

        PUSH    EDX
        CALL    _GetMem
        POP     EDX
        TEST    EAX,EAX
        JE      @@exit
        PUSH    EAX
        CALL    _Initialize
        POP     EAX
@@exit:
end;
{$ENDIF}

procedure _Dispose(p: Pointer; typeInfo: Pointer);
{$IFDEF PUREPASCAL}
begin
  _Finalize(p, typeinfo);
  FreeMem(p);
end;
{$ELSE !PUREPASCAL}
asm
        { ->    EAX     Pointer to object to be disposed        }
        {       EDX     Pointer to type info            }

        PUSH    EAX
        CALL    _Finalize
        POP     EAX
        CALL    _FreeMem
end;
{$ENDIF !PUREPASCAL}

{ ----------------------------------------------------- }
{       Wide character support                          }
{ ----------------------------------------------------- }

function WideCharToString(Source: PWideChar): UnicodeString;
begin
  WideCharToStrVar(Source, Result);
end;

function WideCharLenToString(Source: PWideChar; SourceLen: Integer): UnicodeString;
begin
  WideCharLenToStrVar(Source, SourceLen, Result);
end;

procedure WideCharToStrVar(Source: PWideChar; var Dest: UnicodeString);
begin
  _UStrFromPWChar(Dest, Source);
end;

procedure WideCharLenToStrVar(Source: PWideChar; SourceLen: Integer;
  var Dest: UnicodeString);
begin
  _UStrFromPWCharLen(Dest, Source, SourceLen);
end;

procedure WideCharLenToStrVar(Source: PWideChar; SourceLen: Integer;
  var Dest: AnsiString);
begin
  _LStrFromPWCharLen(Dest, Source, SourceLen, DefaultSystemCodePage);
end;

function StringToWideChar(const Source: UnicodeString; Dest: PWideChar;
  DestSize: Integer): PWideChar;
begin
  //Check to see if enough storage is allocated
  if Length(Source) + 1 > DestSize then
  begin
    if DestSize > 0 then
    begin
      Dest[0] := #0;
      Result := Dest;
    end
    else
    begin
      Result := '';
    end;
    Exit;
  end;

  Move(Source[1], Dest[0], Length(Source) * SizeOf(WideChar));
  Dest[Length(Source)] := #0;
  Result := Dest;
end;

{ ----------------------------------------------------- }
{       OLE string support                              }
{ ----------------------------------------------------- }

function OleStrToString(Source: PWideChar): UnicodeString;
begin
  OleStrToStrVar(Source, Result);
end;

procedure OleStrToStrVar(Source: PWideChar; var Dest: AnsiString);
begin
  WideCharLenToStrVar(Source, Length(WideString(Pointer(Source))), Dest);
end;

procedure OleStrToStrVar(Source: PWideChar; var Dest: UnicodeString);
begin
  WideCharLenToStrVar(Source, Length(WideString(Pointer(Source))), Dest);
end;

function StringToOleStr(const Source: AnsiString): PWideChar;
begin
  Result := nil;
  _WStrFromPCharLen(WideString(Pointer(Result)), PAnsiChar(Pointer(Source)), Length(Source));
end;

function StringToOleStr(const Source: UnicodeString): PWideChar; overload;
begin
  Result := nil;
  _WStrFromPWCharLen(WideString(Pointer(Result)), PWideChar(Pointer(Source)), Length(Source));
end;

{ ----------------------------------------------------- }
{       Variant manager support   (obsolete)            }
{ ----------------------------------------------------- }

procedure GetVariantManager(var VarMgr: TVariantManager);
begin
  FillChar(VarMgr, sizeof(VarMgr), 0);
end;

procedure SetVariantManager(const VarMgr: TVariantManager);
begin
end;

function IsVariantManagerSet: Boolean;
begin
  Result := False;
end;

procedure _IntfDispCall;
asm
{$IFDEF PIC}
        PUSH    EAX
        PUSH    ECX
        CALL    GetGOT
        POP     ECX
        LEA     EAX,[EAX].OFFSET DispCallByIDProc
        MOV     EAX,[EAX]
        XCHG    EAX,[ESP]
        RET
{$ELSE}
        JMP     DispCallByIDProc
{$ENDIF}
end;

procedure _DispCallByIDError;
asm
        MOV     AL,reVarDispatch
        JMP     Error
end;

procedure _IntfVarCall;
asm
end;

// 64 bit integer helper routines
//
// These functions always return the 64-bit result in EAX:EDX

// ------------------------------------------------------------------------------
//  64-bit signed multiply
// ------------------------------------------------------------------------------
//
//  Param 1(EAX:EDX), Param 2([ESP+8]:[ESP+4])  ; before reg pushing
//

procedure __llmul;
asm
        push  edx
        push  eax

  // Param2 : [ESP+16]:[ESP+12]  (hi:lo)
  // Param1 : [ESP+4]:[ESP]      (hi:lo)

        mov   eax, [esp+16]
        mul   dword ptr [esp]
        mov   ecx, eax

        mov   eax, [esp+4]
        mul   dword ptr [esp+12]
        add   ecx, eax

        mov   eax, [esp]
        mul   dword ptr [esp+12]
        add   edx, ecx

        pop   ecx
        pop   ecx

        ret     8
end;

// ------------------------------------------------------------------------------
//  64-bit signed multiply, with overflow check (98.05.15: overflow not supported yet)
// ------------------------------------------------------------------------------
//
//  Param1 ~= U   (Uh, Ul)
//  Param2 ~= V   (Vh, Vl)
//
//  Param 1(EAX:EDX), Param 2([ESP+8]:[ESP+4])  ; before reg pushing
//
//  compiler-helper function
//  O-flag set on exit   => result is invalid
//  O-flag clear on exit => result is valid

procedure __llmulo;
asm
        push   edx
        push   eax

        // Param2 : [ESP+16]:[ESP+12]  (hi:lo)
        // Param1 : [ESP+4]:[ESP]      (hi:lo)

        mov    eax, [esp+16]
        mul    dword ptr [esp]
        mov    ecx, eax

        mov    eax, [esp+4]
        mul    dword ptr [esp+12]
        add    ecx, eax

        mov    eax, [esp]
        mul    dword ptr [esp+12]
        add    edx, ecx

        pop    ecx
        pop    ecx

        ret    8
end;

(* ***** BEGIN LICENSE BLOCK *****
 *
 * The function __lldiv is licensed under the CodeGear license terms.
 *
 * The initial developer of the original code is Fastcode
 *
 * Portions created by the initial developer are Copyright (C) 2002-2004
 * the initial developer. All Rights Reserved.
 *
 * Contributor(s): AMD, John O'Harrow and Dennis Christensen
 *
 * ***** END LICENSE BLOCK ***** *)

// ------------------------------------------------------------------------------
//  64-bit signed division
// ------------------------------------------------------------------------------

//
//  Dividend = Numerator, Divisor = Denominator
//
//  Dividend(EAX:EDX), Divisor([ESP+8]:[ESP+4])  ; before reg pushing
//
//

procedure __lldiv; //JOH Version
asm
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EBX, [ESP+16]
        MOV     ECX, [ESP+20]
        MOV     ESI, EDX
        MOV     EDI, ECX
        SAR     ESI, 31
        XOR     EAX, ESI
        XOR     EDX, ESI
        SUB     EAX, ESI
        SBB     EDX, ESI          // EDX:EAX := abs(Dividend)
        SAR     EDI, 31
        XOR     ESI, EDI          // 0 if X and Y have same sign
        XOR     EBX, EDI
        XOR     ECX, EDI
        SUB     EBX, EDI
        SBB     ECX, EDI          // ECX:EBX := abs(Divisor)
        JNZ     @@BigDivisor      // divisor > 32^32-1
        CMP     EDX, EBX          // only one division needed ? (ecx = 0)
        JB      @@OneDiv          // yes, one division sufficient
        MOV     ECX, EAX          // save dividend-lo in ecx
        MOV     EAX, EDX          // get dividend-hi
        XOR     EDX, EDX          // zero extend it into edx:eax
        DIV     EBX               // quotient-hi in eax
        XCHG    EAX, ECX          // ecx = quotient-hi, eax =dividend-lo
@@OneDiv:
        DIV     EBX               // eax = quotient-lo
        MOV     EDX, ECX          // edx = quotient-hi(quotient in edx:eax)
        JMP     @SetSign
@@BigDivisor:
        SUB     ESP, 12           // Create three local variables.
        MOV     [ESP  ], EAX      // dividend_lo
        MOV     [ESP+4], EBX      // divisor_lo
        MOV     [ESP+8], EDX      // dividend_hi
        MOV     EDI, ECX          //  edi:ebx and ecx:esi
        SHR     EDX, 1            // shift both
        RCR     EAX, 1            //  divisor and
        ROR     EDI, 1            //   and dividend
        RCR     EBX, 1            //    right by 1 bit
        BSR     ECX, ECX          // ecx = number of remaining shifts
        SHRD    EBX, EDI, CL      // scale down divisor and
        SHRD    EAX, EDX, CL      //   dividend such that divisor
        SHR     EDX, CL           //    less than 2^32 (i.e. fits in ebx)
        ROL     EDI, 1            // restore original divisor (edi:esi)
        DIV     EBX               // compute quotient
        MOV     EBX, [ESP]        // dividend_lo
        MOV     ECX, EAX          // save quotient
        IMUL    EDI, EAX          // quotient * divisor hi-word (low only)
        MUL     DWORD PTR [ESP+4] // quotient * divisor low word
        ADD     EDX, EDI          // edx:eax = quotient * divisor
        SUB     EBX, EAX          // dividend-lo - (quot.*divisor)-lo
        MOV     EAX, ECX          // get quotient
        MOV     ECX, [ESP+8]      // dividend_hi
        SBB     ECX, EDX          // subtract divisor * quot. from dividend
        SBB     EAX, 0            // Adjust quotient if remainder is negative.
        XOR     EDX, EDX          // clear hi-word of quot (eax<=FFFFFFFFh)
        ADD     ESP, 12           // Remove local variables.
@SetSign:
        XOR     EAX, ESI          // If (quotient < 0),
        XOR     EDX, ESI          //   compute 1's complement of result.
        SUB     EAX, ESI          // If (quotient < 0),
        SBB     EDX, ESI          //   compute 2's complement of result.
@Done:
        POP     EDI
        POP     ESI
        POP     EBX
        RET     8
end;

// ------------------------------------------------------------------------------
//  64-bit signed division with overflow check (98.05.15: not implementated yet)
// ------------------------------------------------------------------------------

//
//  Dividend = Numerator, Divisor = Denominator
//
//  Dividend(EAX:EDX), Divisor([ESP+8]:[ESP+4])
//  Param 1 (EAX:EDX), Param 2([ESP+8]:[ESP+4])
//
//  Param1 ~= U   (Uh, Ul)
//  Param2 ~= V   (Vh, Vl)
//
//  compiler-helper function
//  O-flag set on exit   => result is invalid
//  O-flag clear on exit => result is valid
//

procedure __lldivo;
asm
  // check for overflow condition: min(int64) DIV -1
        push  esi
        mov esi, [esp+12]   // Vh
        and esi, [esp+8]    // Vl
        cmp esi, 0ffffffffh   // V = -1?
        jne @@divok

        mov esi, eax
        or  esi, edx
        cmp esi, 80000000H    // U = min(int64)?
        jne @@divok

@@divOvl:
        mov eax, esi
        pop esi
        dec eax                     // turn on O-flag
        ret

@@divok:
        pop esi
        push  dword ptr [esp+8]   // Vh
        push  dword ptr [esp+8]   // Vl (offset is changed from push)
        call  __lldiv
        and eax, eax    // turn off O-flag
        ret 8
end;

// ------------------------------------------------------------------------------
//  64-bit unsigned division
// ------------------------------------------------------------------------------

//  Dividend(EAX(hi):EDX(lo)), Divisor([ESP+8](hi):[ESP+4](lo))  // before reg pushing
procedure __lludiv;
asm
        push    ebp
        push    ebx
        push    esi
        push    edi
//
//       Now the stack looks something like this:
//
//               24[esp]: divisor (high dword)
//               20[esp]: divisor (low dword)
//               16[esp]: return EIP
//               12[esp]: previous EBP
//                8[esp]: previous EBX
//                4[esp]: previous ESI
//                 [esp]: previous EDI
//

//       dividend is pushed last, therefore the first in the args
//       divisor next.
//
        mov     ebx,20[esp]             // get the first low word
        mov     ecx,24[esp]             // get the first high word

        or      ecx,ecx
        jnz     @__lludiv@slow_ldiv     // both high words are zero

        or      edx,edx
        jz      @__lludiv@quick_ldiv

        or      ebx,ebx
        jz      @__lludiv@quick_ldiv    // if ecx:ebx == 0 force a zero divide
          // we don't expect this to actually
          // work

@__lludiv@slow_ldiv:
        mov     ebp,ecx
        mov     ecx,64                  // shift counter
        xor     edi,edi                 // fake a 64 bit dividend
        xor     esi,esi

@__lludiv@xloop:
        shl     eax,1                   // shift dividend left one bit
        rcl     edx,1
        rcl     esi,1
        rcl     edi,1
        cmp     edi,ebp                 // dividend larger?
        jb      @__lludiv@nosub
        ja      @__lludiv@subtract
        cmp     esi,ebx                 // maybe
        jb      @__lludiv@nosub

@__lludiv@subtract:
        sub     esi,ebx
        sbb     edi,ebp                 // subtract the divisor
        inc     eax                     // build quotient

@__lludiv@nosub:
        loop    @__lludiv@xloop
//
//       When done with the loop the four registers values' look like:
//
//       |     edi    |    esi     |    edx     |    eax     |
//       |        remainder        |         quotient        |
//

@__lludiv@finish:
        pop     edi
        pop     esi
        pop     ebx
        pop     ebp
        ret     8

@__lludiv@quick_ldiv:
        div     ebx                     // unsigned divide
        xor     edx,edx
        jmp     @__lludiv@finish
end;

// ------------------------------------------------------------------------------
//  64-bit modulo
// ------------------------------------------------------------------------------

//  Dividend(EAX:EDX), Divisor([ESP+8]:[ESP+4])  // before reg pushing
procedure __llmod;
asm
        push    ebp
        push    ebx
        push    esi
        push    edi
        xor   edi,edi
//
//       dividend is pushed last, therefore the first in the args
//       divisor next.
//
        mov     ebx,20[esp]             // get the first low word
        mov     ecx,24[esp]             // get the first high word
        or      ecx,ecx
        jnz     @__llmod@slow_ldiv      // both high words are zero

        or      edx,edx
        jz      @__llmod@quick_ldiv

        or      ebx,ebx
        jz      @__llmod@quick_ldiv     // if ecx:ebx == 0 force a zero divide
          // we don't expect this to actually
          // work
@__llmod@slow_ldiv:
//
//               Signed division should be done.  Convert negative
//               values to positive and do an unsigned division.
//               Store the sign value in the next higher bit of
//               di (test mask of 4).  Thus when we are done, testing
//               that bit will determine the sign of the result.
//
        or      edx,edx                 // test sign of dividend
        jns     @__llmod@onepos
        neg     edx
        neg     eax
        sbb     edx,0                   // negate dividend
        or      edi,1

@__llmod@onepos:
        or      ecx,ecx                 // test sign of divisor
        jns     @__llmod@positive
        neg     ecx
        neg     ebx
        sbb     ecx,0                   // negate divisor

@__llmod@positive:
        mov     ebp,ecx
        mov     ecx,64                  // shift counter
        push    edi                     // save the flags
//
//       Now the stack looks something like this:
//
//               24[esp]: divisor (high dword)
//               20[esp]: divisor (low dword)
//               16[esp]: return EIP
//               12[esp]: previous EBP
//                8[esp]: previous EBX
//                4[esp]: previous ESI
//                 [esp]: previous EDI
//
        xor     edi,edi                 // fake a 64 bit dividend
        xor     esi,esi

@__llmod@xloop:
        shl     eax,1                   // shift dividend left one bit
        rcl     edx,1
        rcl     esi,1
        rcl     edi,1
        cmp     edi,ebp                 // dividend larger?
        jb      @__llmod@nosub
        ja      @__llmod@subtract
        cmp     esi,ebx                 // maybe
        jb      @__llmod@nosub

@__llmod@subtract:
        sub     esi,ebx
        sbb     edi,ebp                 // subtract the divisor
        inc     eax                     // build quotient

@__llmod@nosub:
        loop    @__llmod@xloop
//
//       When done with the loop the four registers values' look like:
//
//       |     edi    |    esi     |    edx     |    eax     |
//       |        remainder        |         quotient        |
//
        mov     eax,esi
        mov     edx,edi                 // use remainder

        pop     ebx                     // get control bits
        test    ebx,1                   // needs negative
        jz      @__llmod@finish
        neg     edx
        neg     eax
        sbb     edx,0                    // negate

@__llmod@finish:
        pop     edi
        pop     esi
        pop     ebx
        pop     ebp
        ret     8

@__llmod@quick_ldiv:
        div     ebx                     // unsigned divide
        xchg  eax,edx
        xor     edx,edx
        jmp     @__llmod@finish
end;

// ------------------------------------------------------------------------------
//  64-bit signed modulo with overflow (98.05.15: overflow not yet supported)
// ------------------------------------------------------------------------------

//  Dividend(EAX:EDX), Divisor([ESP+8]:[ESP+4])
//  Param 1 (EAX:EDX), Param 2([ESP+8]:[ESP+4])
//
//  Param1 ~= U   (Uh, Ul)
//  Param2 ~= V   (Vh, Vl)
//
//  compiler-helper function
//  O-flag set on exit   => result is invalid
//  O-flag clear on exit => result is valid
//

procedure __llmodo;
asm
  // check for overflow condition: min(int64) MOD -1
        push  esi
        mov esi, [esp+12]   // Vh
        and esi, [esp+8]    // Vl
        cmp esi, 0ffffffffh   // V = -1?
        jne @@modok

        mov esi, eax
        or  esi, edx
        cmp esi, 80000000H    // U = min(int64)?
        jne @@modok

@@modOvl:
        mov eax, esi
        pop esi
        dec eax                     // turn on O-flag
        ret

@@modok:
        pop esi
        push  dword ptr [esp+8]       // Vh
        push  dword ptr [esp+8] // Vl (offset is changed from push)
        call  __llmod
        and eax, eax    // turn off O-flag
        ret 8
end;

// ------------------------------------------------------------------------------
//  64-bit unsigned modulo
// ------------------------------------------------------------------------------
//  Dividend(EAX(hi):EDX(lo)), Divisor([ESP+8](hi):[ESP+4](lo))  // before reg pushing

procedure __llumod;
asm
        push    ebp
        push    ebx
        push    esi
        push    edi
//
//       Now the stack looks something like this:
//
//               24[esp]: divisor (high dword)
//               20[esp]: divisor (low dword)
//               16[esp]: return EIP
//               12[esp]: previous EBP
//                8[esp]: previous EBX
//                4[esp]: previous ESI
//                 [esp]: previous EDI
//

//       dividend is pushed last, therefore the first in the args
//       divisor next.
//
        mov     ebx,20[esp]             // get the first low word
        mov     ecx,24[esp]             // get the first high word
        or      ecx,ecx
        jnz     @__llumod@slow_ldiv     // both high words are zero

        or      edx,edx
        jz      @__llumod@quick_ldiv

        or      ebx,ebx
        jz      @__llumod@quick_ldiv    // if ecx:ebx == 0 force a zero divide
          // we don't expect this to actually
          // work
@__llumod@slow_ldiv:
        mov     ebp,ecx
        mov     ecx,64                  // shift counter
        xor     edi,edi                 // fake a 64 bit dividend
        xor     esi,esi                 //

@__llumod@xloop:
        shl     eax,1                   // shift dividend left one bit
        rcl     edx,1
        rcl     esi,1
        rcl     edi,1
        cmp     edi,ebp                 // dividend larger?
        jb      @__llumod@nosub
        ja      @__llumod@subtract
        cmp     esi,ebx                 // maybe
        jb      @__llumod@nosub

@__llumod@subtract:
        sub     esi,ebx
        sbb     edi,ebp                 // subtract the divisor
        inc     eax                     // build quotient

@__llumod@nosub:
        loop    @__llumod@xloop
//
//       When done with the loop the four registers values' look like:
//
//       |     edi    |    esi     |    edx     |    eax     |
//       |        remainder        |         quotient        |
//
        mov     eax,esi
        mov     edx,edi                 // use remainder

@__llumod@finish:
        pop     edi
        pop     esi
        pop     ebx
        pop     ebp
        ret     8

@__llumod@quick_ldiv:
        div     ebx                     // unsigned divide
        xchg  eax,edx
        xor     edx,edx
        jmp     @__llumod@finish
end;

// ------------------------------------------------------------------------------
//  64-bit shift left
// ------------------------------------------------------------------------------

//
// target (EAX:EDX) count (ECX)
//
procedure __llshl;
asm
        cmp cl, 32
        jl  @__llshl@below32
        cmp cl, 64
        jl  @__llshl@below64
        xor edx, edx
        xor eax, eax
        ret

@__llshl@below64:
        mov edx, eax
        shl edx, cl
        xor eax, eax
        ret

@__llshl@below32:
        shld  edx, eax, cl
        shl eax, cl
        ret
end;

// ------------------------------------------------------------------------------
//  64-bit signed shift right
// ------------------------------------------------------------------------------
// target (EAX:EDX) count (ECX)

procedure __llshr;
asm
        cmp cl, 32
        jl  @__llshr@below32
        cmp cl, 64
        jl  @__llshr@below64
        sar edx, 1fh
        mov eax,edx
        ret

@__llshr@below64:
        mov eax, edx
        cdq
        sar eax,cl
        ret

@__llshr@below32:
        shrd  eax, edx, cl
        sar edx, cl
        ret
end;

// ------------------------------------------------------------------------------
//  64-bit unsigned shift right
// ------------------------------------------------------------------------------

// target (EAX:EDX) count (ECX)
procedure __llushr;
asm
        cmp cl, 32
        jl  @__llushr@below32
        cmp cl, 64
        jl  @__llushr@below64
        xor edx, edx
        xor eax, eax
        ret

@__llushr@below64:
        mov eax, edx
        xor edx, edx
        shr eax, cl
        ret

@__llushr@below32:
        shrd  eax, edx, cl
        shr edx, cl
        ret
end;

function _StrUInt64Digits(val: UInt64; width: Integer; sign: Boolean): ShortString;
var
  d: array[0..31] of Char;  { need 19 digits and a sign }
  i, k: Integer;
  spaces: Integer;
begin
  { Produce an ASCII representation of the number in reverse order }
  i := 0;
  repeat
    d[i] := Chr( (val mod 10) + Ord('0') );
    Inc(i);
    val := val div 10;
  until val = 0;
  if sign then
  begin
    d[i] := '-';
    Inc(i);
  end;

  { Fill the Result with the appropriate number of blanks }
  if width > 255 then
    width := 255;
  k := 1;
  spaces := width - i;
  while k <= spaces do
  begin
    Result[k] := AnsiChar(' ');
    Inc(k);
  end;

  { Fill the Result with the number }
  while i > 0 do
  begin
    Dec(i);
    Result[k] := AnsiChar(d[i]);
    Inc(k);
  end;

  { Result is k-1 characters long }
  SetLength(Result, k-1);
end;

function _StrInt64(val: Int64; width: Integer): ShortString;
begin
  Result := _StrUInt64Digits(Abs(val), width, val < 0);
end;

function _Str0Int64(val: Int64): ShortString;
begin
  Result := _StrInt64(val, 0);
end;

function _StrUInt64(val: UInt64; width: Integer): ShortString;
begin
  Result := _StrUInt64Digits(val, width, False);
end;

function _Str0UInt64(val: Int64): ShortString;
begin
  Result := _StrUInt64(val, 0);
end;

procedure       _WriteInt64;
asm
{       PROCEDURE _WriteInt64( VAR t: Text; val: Int64; with: Longint);        }
{     ->EAX     Pointer to file record  }
{       [ESP+4] Value                   }
{       EDX     Field width             }

        SUB     ESP,32          { VAR s: String[31];    }

        PUSH    EAX
        PUSH    EDX

        PUSH    dword ptr [ESP+8+32+8]    { Str( val : 0, s );    }
        PUSH    dword ptr [ESP+8+32+8]
        XOR     EAX,EAX
        LEA     EDX,[ESP+8+8]
        CALL    _StrInt64

        POP     ECX
        POP     EAX

        MOV     EDX,ESP         { Write( t, s : width );}
        CALL    _WriteString

        ADD     ESP,32
        RET     8
end;

procedure       _Write0Int64;
asm
{       PROCEDURE _Write0Long( VAR t: Text; val: Longint);      }
{     ->EAX     Pointer to file record  }
{       EDX     Value                   }
        XOR     EDX,EDX
        JMP     _WriteInt64
end;

procedure       _WriteUInt64;
asm
{       PROCEDURE _WriteInt64( VAR t: Text; val: Int64; with: Longint);        }
{     ->EAX     Pointer to file record  }
{       [ESP+4] Value                   }
{       EDX     Field width             }

        SUB     ESP,32          { VAR s: String[31];    }

        PUSH    EAX
        PUSH    EDX

        PUSH    dword ptr [ESP+8+32+8]    { Str( val : 0, s );    }
        PUSH    dword ptr [ESP+8+32+8]
        XOR     EAX,EAX
        LEA     EDX,[ESP+8+8]
        CALL    _StrUInt64

        POP     ECX
        POP     EAX

        MOV     EDX,ESP         { Write( t, s : width );}
        CALL    _WriteString

        ADD     ESP,32
        RET     8
end;

procedure       _Write0UInt64;
asm
{       PROCEDURE _Write0Long( VAR t: Text; val: Longint);      }
{     ->EAX     Pointer to file record  }
{       EDX     Value                   }
        XOR     EDX,EDX
        JMP     _WriteUInt64
end;

function _ValInt64L(const s: AnsiString; var code: Integer): Int64;
begin
  Result := _ValInt64(string(s), code);
end;

procedure _ReadInt64;
asm
      // -> EAX Pointer to text record
      // <- EAX:EDX Result

        PUSH  EBX
        PUSH  ESI
        PUSH  EDI
        SUB   ESP,36      // var numbuf: String[32];
        PUSH  0           // String Length
        PUSH  -1          // Refcount (-1 = string constant)
        PUSH  $00010000   // elemSize = 1, codePage = CP_ACP

        MOV   ESI,EAX
        CALL  _SeekEof
        DEC   AL
        JZ    @@eof

        LEA   EDI,[ESP+skew]     // EDI -> numBuf[0]
        MOV   BL,32
@@loop:
        MOV   EAX,ESI
        CALL  _ReadChar
        CMP   AL,' '
        JBE   @@endNum
        STOSB
        DEC   BL
        JNZ   @@loop
@@convert:
        MOV   byte ptr [EDI],0
        LEA   EAX,[ESP+skew]     // EAX -> numBuf
        MOV   ECX,EDI
        SUB   ECX,EAX
        MOV   [EAX-Skew].StrRec.length,ECX
        PUSH  EDX                // allocate code result
        MOV   EDX,ESP            // pass pointer to code

        CALL  _ValInt64L         // convert
        POP   ECX                // pop code result into ECX
        TEST  ECX,ECX
        JZ    @@exit
        MOV   EAX,106
        CALL  SetInOutRes

@@exit:
        ADD   ESP,36 + 4 + 4 + 2 + 2 // length, refCnt, elemSize, codePage
        POP   EDI
        POP   ESI
        POP   EBX
        RET

@@endNum:
        CMP   AH,cEof
        JE    @@convert
        DEC   [ESI].TTextRec.BufPos
        JMP   @@convert

@@eof:
        XOR   EAX,EAX
        JMP   @@exit
end;

function _ValInt64(const s: string; var code: Integer): Int64;
var
  i: Integer;
  dig: Integer;
  sign: Boolean;
  empty: Boolean;
begin
  i := 1;
  dig := 0;
  Result := 0;
  if s = '' then
  begin
    code := i;
    exit;
  end;
  while s[i] = Char(' ') do
    Inc(i);
  sign := False;
  if s[i] =  Char('-') then
  begin
    sign := True;
    Inc(i);
  end
  else if s[i] =  Char('+') then
    Inc(i);
  empty := True;
  if (s[i] =  Char('$')) or (Upcase(s[i]) =  Char('X'))
    or ((s[i] =  Char('0')) and (Upcase(s[i+1]) =  Char('X'))) then
  begin
    if s[i] =  Char('0') then
      Inc(i);
    Inc(i);
    while True do
    begin
      case   Char(s[i]) of
       Char('0').. Char('9'): dig := Ord(s[i]) -  Ord('0');
       Char('A').. Char('F'): dig := Ord(s[i]) - (Ord('A') - 10);
       Char('a').. Char('f'): dig := Ord(s[i]) - (Ord('a') - 10);
      else
        break;
      end;
      if (Result < 0) or (Result > (High(Int64) shr 3)) then
        Break;
      Result := Result shl 4 + dig;
      Inc(i);
      empty := False;
    end;
    if sign then
      Result := - Result;
  end
  else
  begin
    while True do
    begin
      case  Char(s[i]) of
       Char('0').. Char('9'): dig := Ord(s[i]) - Ord('0');
      else
        break;
      end;
      if (Result < 0) or (Result > (High(Int64) div 10)) then
        break;
      Result := Result*10 + dig;
      Inc(i);
      empty := False;
    end;
    if sign then
      Result := - Result;
    if (Result <> 0) and (sign <> (Result < 0)) then
      Dec(i);
  end;
  if (s[i] <> Char(#0)) or empty then
    code := i
  else
    code := 0;
end;

procedure _DynArrayLength;
asm
{       FUNCTION _DynArrayLength(const a: array of ...): Longint; }
{     ->EAX     Pointer to array or nil                           }
{     <-EAX     High bound of array + 1 or 0                      }
        TEST    EAX,EAX
        JZ      @@skip
        MOV     EAX,[EAX-4]
@@skip:
end;

procedure _DynArrayHigh;
asm
{       FUNCTION _DynArrayHigh(const a: array of ...): Longint; }
{     ->EAX     Pointer to array or nil                         }
{     <-EAX     High bound of array or -1                       }
        CALL  _DynArrayLength
        DEC     EAX
end;

procedure CopyArray(dest, source, typeInfo: Pointer; cnt: Integer);
asm
        PUSH    dword ptr [EBP+8]
        CALL    _CopyArray
end;

procedure FinalizeArray(p, typeInfo: Pointer; cnt: Cardinal);
asm
        JMP     _FinalizeArray
end;

procedure DynArrayClear(var a: Pointer; typeInfo: Pointer);
asm
  CALL    _DynArrayClear
end;

procedure DynArraySetLength(var a: Pointer; typeInfo: Pointer; dimCnt: Longint; lengthVec: PLongint);
var
  i: Integer;
  newLength, oldLength, minLength: Longint;
  elSize: Longint;
  neededSize: Longint;
  p, pp: Pointer;
begin
  p := a;

  // Fetch the new length of the array in this dimension, and the old length
  newLength := PLongint(lengthVec)^;
  if newLength <= 0 then
  begin
    if newLength < 0 then
      Error(reRangeError);
    DynArrayClear(a, typeInfo);
    exit;
  end;

  oldLength := 0;
  if p <> nil then
  begin
    Dec(PLongint(p));
    oldLength := PLongint(p)^;
    Dec(PLongint(p));
  end;

  // Calculate the needed size of the heap object
  Inc(PAnsiChar(typeInfo), Length(PDynArrayTypeInfo(typeInfo).name));
  elSize := PDynArrayTypeInfo(typeInfo).elSize;
  if PDynArrayTypeInfo(typeInfo).elType <> nil then
    typeInfo := PDynArrayTypeInfo(typeInfo).elType^
  else
    typeInfo := nil;
  neededSize := newLength*elSize;
  if neededSize div newLength <> elSize then
    Error(reRangeError);
  Inc(neededSize, Sizeof(Longint)*2);
  if neededSize < 0 then
    Error(reRangeError);

  // If the heap object isn't shared (ref count = 1), just resize it. Otherwise, we make a copy
  if (p = nil) or (PLongint(p)^ = 1) then
  begin
    pp := p;
    if (newLength < oldLength) and (typeInfo <> nil) then
      FinalizeArray(PAnsiChar(p) + Sizeof(Longint)*2 + newLength*elSize, typeInfo, oldLength - newLength);
    ReallocMem(pp, neededSize);
    p := pp;
  end
  else
  begin
    Dec(PLongint(p)^);
    GetMem(p, neededSize);
    minLength := oldLength;
    if minLength > newLength then
      minLength := newLength;
    if typeInfo <> nil then
    begin
      FillChar((PAnsiChar(p) + Sizeof(Longint)*2)^, minLength*elSize, 0);
      CopyArray(PAnsiChar(p) + Sizeof(Longint)*2, a, typeInfo, minLength)
    end
    else
      Move(PAnsiChar(a)^, (PAnsiChar(p) + Sizeof(Longint)*2)^, minLength*elSize);
  end;

  // The heap object will now have a ref count of 1 and the new length
  PLongint(p)^ := 1;
  Inc(PLongint(p));
  PLongint(p)^ := newLength;
  Inc(PLongint(p));

  // Set the new memory to all zero bits
  FillChar((PAnsiChar(p) + elSize * oldLength)^, elSize * (newLength - oldLength), 0);

  // Take care of the inner dimensions, if any
  if dimCnt > 1 then
  begin
    Inc(lengthVec);
    Dec(dimCnt);
    for i := 0 to newLength-1 do
      DynArraySetLength(PPointerArray(p)[i], typeInfo, dimCnt, lengthVec);
  end;
  a := p;
end;

procedure _DynArraySetLength;
asm
{       PROCEDURE _DynArraySetLength(var a: dynarray; typeInfo: PDynArrayTypeInfo; dimCnt: Longint; lengthVec: ^Longint) }
{     ->EAX     Pointer to dynamic array (= pointer to pointer to heap object) }
{       EDX     Pointer to type info for the dynamic array                     }
{       ECX     number of dimensions                                           }
{       [ESP+4] dimensions                                                     }
        PUSH    ESP
        ADD     dword ptr [ESP],4
        CALL    DynArraySetLength
end;

procedure _DynArrayCopy(a: Pointer; typeInfo: Pointer; var Result: Pointer);
begin
  if a <> nil then
    _DynArrayCopyRange(a, typeInfo, 0, PLongint(PAnsiChar(a)-4)^, Result)
  else
    _DynArrayClear(Result, typeInfo);
end;

procedure _DynArrayCopyRange(a: Pointer; typeInfo: Pointer; index, count : Integer; var Result: Pointer);
var
  arrayLength: Integer;
  elSize: Integer;
  typeInf: PDynArrayTypeInfo;
  p: Pointer;
begin
  p := nil;
  if a <> nil then
  begin
    typeInf := typeInfo;

    // Limit index and count to values within the array
    if index < 0 then
    begin
      Inc(count, index);
      index := 0;
    end;
    arrayLength := PLongint(PAnsiChar(a)-4)^;
    if index > arrayLength then
      index := arrayLength;
    if count > arrayLength - index then
      count := arrayLength - index;
    if count < 0 then
      count := 0;

    if count > 0 then
    begin
      // Figure out the size and type descriptor of the element type
      Inc(PAnsiChar(typeInf), Length(typeInf.name));
      elSize := typeInf.elSize;
      if typeInf.elType <> nil then
        typeInf := typeInf.elType^
      else
        typeInf := nil;

      // Allocate the amount of memory needed
      GetMem(p, count*elSize + Sizeof(Longint)*2);

      // The reference count of the new array is 1, the length is count
      PLongint(p)^ := 1;
      Inc(PLongint(p));
      PLongint(p)^ := count;
      Inc(PLongint(p));
      Inc(PAnsiChar(a), index*elSize);

      // If the element type needs destruction, we must copy each element,
      // otherwise we can just copy the bits
      if count > 0 then
      begin
        if typeInf <> nil then
        begin
          FillChar(p^, count*elSize, 0);
          CopyArray(p, a, typeInf, count)
        end
        else
          Move(a^, p^, count*elSize);
      end;
    end;
  end;
  DynArrayClear(Result, typeInfo);
  Result := p;
end;

procedure _DynArrayClear;
asm
{     ->EAX     Pointer to dynamic array (Pointer to pointer to heap object }
{       EDX     Pointer to type info                                        }

        {       Nothing to do if Pointer to heap object is nil }
        MOV     ECX,[EAX]
        TEST    ECX,ECX
        JE      @@exit

        {       Set the variable to be finalized to nil }
        MOV     dword ptr [EAX],0

        {       Decrement ref count. Nothing to do if not zero now. }
   LOCK DEC     dword ptr [ECX-8]
        JNE     @@exit

  {       Save the source - we're supposed to return it }
        PUSH    EAX
        MOV     EAX,ECX

        {       Fetch the type descriptor of the elements }
        XOR     ECX,ECX
        MOV     CL,[EDX].TDynArrayTypeInfo.name;
        MOV     EDX,[EDX+ECX].TDynArrayTypeInfo.elType;

        {       If it's non-nil, finalize the elements }
        TEST    EDX,EDX
        JE      @@noFinalize
        MOV     ECX,[EAX-4]
        TEST    ECX,ECX
        JE      @@noFinalize
        MOV     EDX,[EDX]
        CALL    _FinalizeArray
@@noFinalize:
        {       Now deallocate the array }
        SUB     EAX,8
        CALL    _FreeMem
        POP     EAX
@@exit:
end;


procedure _DynArrayAsg;
asm
{     ->EAX     Pointer to destination (pointer to pointer to heap object }
{       EDX     source (pointer to heap object }
{       ECX     Pointer to rtti describing dynamic array }

        PUSH    EBX
        MOV     EBX,[EAX]

        {       Increment ref count of source if non-nil }

        TEST    EDX,EDX
        JE      @@skipInc
   LOCK INC     dword ptr [EDX-8]
@@skipInc:
        {       Dec ref count of destination - if it becomes 0, clear dest }
        TEST    EBX,EBX
        JE  @@skipClear
   LOCK DEC     dword ptr[EBX-8]
        JNZ     @@skipClear
        PUSH    EAX
        PUSH    EDX
        MOV     EDX,ECX
        INC     dword ptr[EBX-8]
        CALL    _DynArrayClear
        POP     EDX
        POP     EAX
@@skipClear:
        {       Finally store source into destination }
        MOV     [EAX],EDX

        POP     EBX
end;

procedure _DynArrayAddRef;
asm
{     ->EAX     Pointer to heap object }
        TEST    EAX,EAX
        JE      @@exit
   LOCK INC     dword ptr [EAX-8]
@@exit:
end;


function DynArrayIndex(const P: Pointer; const Indices: array of Integer; const TypInfo: Pointer): Pointer;
asm
        {     ->EAX     P                       }
  {       EDX     Pointer to Indices      }
        {       ECX     High bound of Indices   }
        {       [EBP+8] TypInfo                 }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        MOV     ESI,EDX
        MOV     EDI,[EBP+8]
        MOV     EBP,EAX

        XOR     EBX,EBX                 {  for i := 0 to High(Indices) do       }
        TEST    ECX,ECX
        JGE     @@start
@@loop:
        MOV     EBP,[EBP]
@@start:
        XOR     EAX,EAX
        MOV     AL,[EDI].TDynArrayTypeInfo.name
        ADD     EDI,EAX
        MOV     EAX,[ESI+EBX*4]         {    P := P + Indices[i]*TypInfo.elSize }
        MUL     [EDI].TDynArrayTypeInfo.elSize
        MOV     EDI,[EDI].TDynArrayTypeInfo.elType
        TEST    EDI,EDI
        JE      @@skip
        MOV     EDI,[EDI]
@@skip:
        ADD     EBP,EAX
        INC     EBX
        CMP     EBX,ECX
        JLE     @@loop

@@loopEnd:

        MOV     EAX,EBP

        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
end;


{ Returns the DynArrayTypeInfo of the Element Type of the specified DynArrayTypeInfo }
function DynArrayElTypeInfo(typeInfo: PDynArrayTypeInfo): PDynArrayTypeInfo;
begin
  Result := nil;
  if typeInfo <> nil then
  begin
    Inc(PAnsiChar(typeInfo), Length(typeInfo.name));
    if typeInfo.elType <> nil then
      Result := typeInfo.elType^;
  end;
end;

{ Returns # of dimemsions of the DynArray described by the specified DynArrayTypeInfo}
function DynArrayDim(typeInfo: PDynArrayTypeInfo): Integer;
begin
  Result := 0;
  while (typeInfo <> nil) and (typeInfo.kind = tkDynArray) do
  begin
    Inc(Result);
    typeInfo := DynArrayElTypeInfo(typeInfo);
  end;
end;

{ Returns size of the Dynamic Array}
function DynArraySize(a: Pointer): Integer;
asm
        TEST EAX, EAX
        JZ   @@exit
        MOV  EAX, [EAX-4]
@@exit:
end;

// Returns whether array is rectangular
function IsDynArrayRectangular(const DynArray: Pointer; typeInfo: PDynArrayTypeInfo): Boolean;
var
  Dim, I, J, Size, SubSize: Integer;
  P: Pointer;
begin
  // Assume we have a rectangular array
  Result := True;

  P := DynArray;
  Dim := DynArrayDim(typeInfo);

  {NOTE: Start at 1. Don't need to test the first dimension - it's rectangular by definition}
  for I := 1 to dim-1 do
  begin
    if P <> nil then
    begin
      { Get size of this dimension }
      Size := DynArraySize(P);

      { Get Size of first sub. dimension }
      SubSize := DynArraySize(PPointerArray(P)[0]);

      { Walk through every dimension making sure they all have the same size}
      for J := 1  to Size-1 do
        if DynArraySize(PPointerArray(P)[J]) <> SubSize then
        begin
          Result := False;
          Exit;
        end;

      { Point to next dimension}
      P := PPointerArray(P)[0];
    end;
  end;
end;

// Returns Bounds of Dynamic array as an array of integer containing the 'high' of each dimension
function DynArrayBounds(const DynArray: Pointer; typeInfo: PDynArrayTypeInfo): TBoundArray;
var
  Dim, I: Integer;
  P: Pointer;
begin
  P := DynArray;

  Dim := DynArrayDim(typeInfo);
  SetLength(Result, Dim);

  for I := 0 to dim-1 do
    if P <> nil then
    begin
      Result[I] := DynArraySize(P)-1;
      P := PPointerArray(P)[0]; // Assume rectangular arrays
    end;
end;

{ Decrements to next lower index - Returns True if successful }
{ Indices: Indices to be decremented }
{ Bounds : High bounds of each dimension }
function DecIndices(var Indices: TBoundArray; const Bounds: TBoundArray): Boolean;
var
  I, J: Integer;
begin
  { Find out if we're done: all at zeroes }
  Result := False;
  for I := Low(Indices)  to High(Indices) do
    if Indices[I] <> 0  then
    begin
      Result := True;
      break;
    end;
  if not Result then
    Exit;

  { Two arrays must be of same length }
  Assert(Length(Indices) = Length(Bounds));

  { Find index of item to tweak }
  for I := High(Indices) downto Low(Bounds) do
  begin
    // If not reach zero, dec and bail out
    if Indices[I] <> 0 then
    begin
      Dec(Indices[I]);
      Exit;
    end
    else
    begin
      J := I;
      while Indices[J] = 0 do
      begin
        // Restore high bound when we've reached zero on a particular dimension
        Indices[J] := Bounds[J];
        // Move to higher dimension
        Dec(J);
        Assert(J >= 0);
      end;
      Dec(Indices[J]);
      Exit;
    end;
  end;
end;

{ Package/Module registration/unregistration }

{$IFDEF MSWINDOWS}
const
  LCID_SUPPORTED          = $00000002;  { supported locale ids }
  LOCALE_SABBREVLANGNAME  = $00000003;  { abbreviated language name }
  LOCALE_SISO639LANGNAME  = $00000059;  { ISO abbreviated language name }
  LOCALE_SISO3166CTRYNAME = $0000005A;  { ISO abbreviated country name }
  LOCALE_SNAME            = $0000005c;  { locale name (ie: en-us) }
  LOCALE_SPARENT          = $0000006d;  { Fallback name for resources }
  LOCALE_NAME_MAX_LENGTH  = 85;
  MUI_LANGUAGE_NAME       = $8;  { Use ISO language (culture) name convention }
  MUI_UI_FALLBACK         = $30; { Retrieve a complete thread preferred UI languages list }
  LOAD_LIBRARY_AS_DATAFILE = 2;
  HKEY_CURRENT_USER = $80000001;
  HKEY_LOCAL_MACHINE = $80000002;
  KEY_ALL_ACCESS = $000F003F;
  KEY_READ = $000F0019;

  OlderLocaleOverrideKey = 'Software\Borland\Delphi\Locales'; // do not localize
  OldLocaleOverrideKey = 'Software\Borland\Locales'; // do not localize
  NewLocaleOverrideKey = 'Software\CodeGear\Locales'; // do not localize
{$ENDIF}

function FindModule(Instance: LongWord): PLibModule;
begin
  Result := LibModuleList;
  while Result <> nil do
  begin
    if (Instance = Result.Instance) or
       (Instance = Result.CodeInstance) or
       (Instance = Result.DataInstance) or
       (Instance = Result.ResInstance) then
      Exit;
    Result := Result.Next;
  end;
end;

function FindHInstance(Address: Pointer): LongWord;
{$IFDEF MSWINDOWS}
var
  MemInfo: TMemInfo;
begin
  VirtualQuery(Address, MemInfo, SizeOf(MemInfo));
  if MemInfo.State = $1000{MEM_COMMIT} then
    Result := LongWord(MemInfo.AllocationBase)
  else
    Result := 0;
end;
{$ENDIF}
{$IFDEF POSIX}
var
  Info: TDLInfo;
begin
  if (dladdr(Address, Info) = 0) or (Info.BaseAddress = ExeBaseAddress) then
    Info.Filename := nil;   // if it's not in a library, assume the exe
  Result := LongWord(dlopen(Info.Filename, RTLD_LAZY));
  if Result <> 0 then
    dlclose(Result);
end;
{$ENDIF}

function FindClassHInstance(ClassType: TClass): LongWord;
begin
  Result := FindHInstance(Pointer(ClassType));
end;

{$IFDEF POSIX}
function GetModuleFileName(Module: HMODULE; Buffer: PChar; BufLen: Integer): Integer;
var
  Addr: Pointer;
  Info: TDLInfo;
  FoundInModule: HMODULE;
  Temp: Integer;
begin
  Result := 0;
  if BufLen <= 0 then Exit;
  if (Module = MainInstance) or (Module = 0) then
  begin
    // First, try the dlsym approach.
    // dladdr fails to return the name of the main executable
    // in glibc prior to 2.1.91

{   Look for a dynamic symbol exported from this program.
    _DYNAMIC is not required in a main program file.
    If the main program is compiled with Delphi, it will always
    have a resource section, named @Sysinit@ResSym.
    If the main program is not compiled with Delphi, dlsym
    will search the global name space, potentially returning
    the address of a symbol in some other shared object library
    loaded by the program.  To guard against that, we check
    that the address of the symbol found is within the
    main program address range.  }

    dlerror;   // clear error state;  dlsym doesn't
    Addr := dlsym(Module, '@Sysinit@ResSym');
    if (Addr <> nil) and (dlerror = nil)
      and (dladdr(Addr, Info) <> 0)
      and (Info.FileName <> nil)
      and (Info.BaseAddress = ExeBaseAddress) then
    begin
      Result := _strlen(Info.FileName);
      if Result >= BufLen then Result := BufLen-1;

      // dlinfo may not give a full path.  Compare to /proc/self/exe,
      // take longest result.
{$IFDEF LINUX}
      Temp := _readlink('/proc/self/exe', Buffer, BufLen);
      if Temp >= BufLen then Temp := BufLen-1;
      if Temp > Result then
        Result := Temp
      else
        Move(Info.FileName^, Buffer^, Result);
{$ENDIF LINUX}
{$IFDEF MACOSX}
      Utf8ToUnicode(Buffer, BufLen, Info.FileName, Result);
{$ENDIF MACOSX}
      Buffer[Result] := #0;
      Exit;
    end;

{$IFDEF LINUX}
    // Try inspecting the /proc/ virtual file system
    // to find the program filename in the process info
    Result := _readlink(AnsiString('/proc/self/exe'), Buffer, BufLen);
    if Result <> -1 then
    begin
      if Result >= BufLen then Result := BufLen-1;
      Buffer[Result] := #0;
    end;
{$ENDIF LINUX}
{$IFDEF AllowParamStrModuleName}
{   Using ParamStr(0) to obtain a module name presents a potential
    security hole.  Resource modules are loaded based upon the filename
    of a given module.  We use dlopen() to load resource modules, which
    means the .init code of the resource module will be executed.
    Normally, resource modules contain no code at all - they're just
    carriers of resource data.
    An unpriviledged user program could launch our trusted,
    priviledged program with a bogus parameter list, tricking us
    into loading a module that contains malicious code in its
    .init section.
    Without this ParamStr(0) section, GetModuleFilename cannot be
    misdirected by unpriviledged code (unless the system program loader
    or the /proc file system or system root directory has been compromised).
    Resource modules are always loaded from the same directory as the
    given module.  Trusted code (programs, packages, and libraries)
    should reside in directories that unpriviledged code cannot alter.

    If you need GetModuleFilename to have a chance of working on systems
    where glibc < 2.1.91 and /proc is not available, and your
    program will not run as a priviledged user (or you don't care),
    you can define AllowParamStrModuleNames and rebuild the System unit
    and baseCLX package.  Note that even with ParamStr(0) support
    enabled, GetModuleFilename can still fail to find the name of
    a module.  C'est la Unix.  }

    if Result = -1 then // couldn't access the /proc filesystem
    begin               // return less accurate ParamStr(0)

{     ParamStr(0) returns the name of the link used
      to launch the app, not the name of the app itself.
      Also, if this app was launched by some other program,
      there is no guarantee that the launching program has set
      up our environment at all.  (example: Apache CGI) }

      if (ArgValues = nil) or (ArgValues^ = nil) or
        (PCharArray(ArgValues^)[0] = nil) then
      begin
        Result := 0;
        Exit;
      end;
      Result := _strlen(PCharArray(ArgValues^)[0]);
      if Result >= BufLen then Result := BufLen-1;
      Move(PCharArray(ArgValues^)[0]^, Buffer^, Result);
      Buffer[Result] := #0;
    end;
{$ENDIF}
  end
  else
  begin
{   For shared object libraries, we can rely on the dlsym technique.
    Look for a dynamic symbol in the requested module.
    Don't assume the module was compiled with Delphi.
    We look for a dynamic symbol with the name _DYNAMIC.  This
    exists in all ELF shared object libraries that export
    or import symbols;  If someone has a shared object library that
    contains no imports or exports of any kind, this will probably fail.
    If dlsym can't find the requested symbol in the given module, it
    will search the global namespace and could return the address
    of a symbol from some other module that happens to be loaded
    into this process.  That would be bad, so we double check
    that the module handle of the symbol found matches the
    module handle we asked about.}

    dlerror;   // clear error state;  dlsym doesn't
    Addr := dlsym(Module, '_DYNAMIC');
    if (Addr <> nil) and (dlerror = nil)
      and (dladdr(Addr, Info) <> 0) then
    begin
      if Info.BaseAddress = ExeBaseAddress then
        Info.FileName := nil;
      FoundInModule := HMODULE(dlopen(Info.FileName, RTLD_LAZY));
      if FoundInModule <> 0 then
        dlclose(FoundInModule);
      if Module = FoundInModule then
      begin
        if Assigned(Info.FileName) then
        begin
          Result := _strlen(Info.FileName);
          if Result >= BufLen then Result := BufLen-1;
          Move(Info.FileName^, Buffer^, Result);
        end
        else
          Result := 0;
        Buffer[Result] := #0;
      end;
    end;
  end;
  if Result < 0 then Result := 0;
end;
{$ENDIF}

function DelayLoadResourceModule(Module: PLibModule): LongWord;
var
  FileName: array[0..MAX_PATH] of Char;
begin
  if Module.ResInstance = 0 then
  begin
    GetModuleFileName(Module.Instance, FileName, SizeOf(FileName));
    Module.ResInstance := LoadResourceModule(FileName);
    if Module.ResInstance = 0 then
      Module.ResInstance := Module.Instance;
  end;
  Result := Module.ResInstance;
end;

function FindResourceHInstance(Instance: LongWord): LongWord;
var
  CurModule: PLibModule;
begin
  CurModule := LibModuleList;
  while CurModule <> nil do
  begin
    if (Instance = CurModule.Instance) or
       (Instance = CurModule.CodeInstance) or
       (Instance = CurModule.DataInstance) then
    begin
      Result := DelayLoadResourceModule(CurModule);
      Exit;
    end;
    CurModule := CurModule.Next;
  end;
  Result := Instance;
end;

{$IFDEF MSWINDOWS}
type
  TLanguageEntry = record
    ID: WORD;
    List: PAnsiChar;
  end;

{$I LocaleData.INC }

var
  GetThreadPreferredUILanguages : function(dwFlags: LONGWORD; pulNumLanguages: Pointer; pwszLanguagesBuffer:
    PWideChar; pcchLanguagesBuffer: Pointer): Boolean; stdcall;
  GetThreadUILanguage : function : WORD; stdcall;
  SetThreadUILanguage : function (LangId: WORD): WORD; stdcall;
  UseThreadUILanguageAPI: Boolean;
  CrSec: CriticalSection;
  CachedLangID: Word;
  CachedLanguageNames: array[0.. LOCALE_NAME_MAX_LENGTH-1] of Char;

procedure InitializeLocaleData;
begin
  InitializeCriticalSection(CrSec);
  CachedLangID := $7f; //  LANG_INVARIANT
  UseThreadUILanguageAPI := (GetVersion and $000000FF) >= 6;
  if UseThreadUILanguageAPI then
  begin
    @GetThreadPreferredUILanguages := GetProcAddress(GetModuleHandle(kernel), 'GetThreadPreferredUILanguages');
    @GetThreadUILanguage:= GetProcAddress(GetModuleHandle(kernel), 'GetThreadUILanguage');
    @SetThreadUILanguage := GetProcAddress(GetModuleHandle(kernel), 'SetThreadUILanguage');
  end;
end;

procedure FinalizeLocaleDate;
begin
  DeleteCriticalSection(CrSec);
end;

function GetUILanguages(const LANGID: WORD): string;

  function LastHyphenPos(S : String) : integer;
  var
    I: integer;
  begin
    for I := Length(S) downto 1 do
      if S[I] = '-' then exit (I-1);
    Result := 0;
  end;

  function ConvertResToUILanguages(ResBuffer: PAnsiChar): String;
  var
    I: Integer;
    Separator,
    ALanguage: String;
  begin
    Result := String(PAnsiChar(ResBuffer));
    for I := 1 to Length(Result) do
      if Result[I] = ',' then exit;
    ALanguage := Result;
    Result := '';
    while ALanguage <> '' do
    begin
      Result := Result + Separator + ALanguage;
      Separator := ',';
      ALanguage := Copy(ALanguage, 1, LastHyphenPos(ALanguage));
    end;
  end;

  function GetPreferredLangForOldOS(LANGID: Word): string;
  var
    Language, Region : array[0.. LOCALE_NAME_MAX_LENGTH-1] of Char;
    H, L, I: Cardinal;
  begin
    Result := '';
    // Lookup exceptional languages table.
    if (NumberOfLocaleData > 0) and (LocaleTable[0].ID <= LANGID) and (LANGID <= LocaleTable[NumberOfLocaleData-1].ID) then
    begin
      H := NumberOfLocaleData-1;
      L := 0;
      while H >= L do
      begin
        I := (H + L) div 2;
        if LocaleTable[I].ID > LANGID then H := I - 1
        else if LocaleTable[I].ID < LANGID then L :=  I + 1
        else
        begin
          Result := ConvertResToUILanguages(LocaleTable[I].List);
          Break;
        end;
      end;
    end;
    if (Result = '') and IsValidLocale(LANGID, LCID_SUPPORTED) then
    begin
      // Generate language names: <language>-<country> and <language>
      GetLocaleInfo(LANGID, LOCALE_SISO639LANGNAME, Language, LOCALE_NAME_MAX_LENGTH);
      GetLocaleInfo(LANGID, LOCALE_SISO3166CTRYNAME, Region, LOCALE_NAME_MAX_LENGTH);
      Result := String(Language) + '-' + String(Region) + ',' + String(Language);
    end;
  end;

var
  bufSize: Integer;
  UILanguages: PChar;
  I: Cardinal;
  numLang: integer;
  SavedThreadUILanguage: WORD;
begin
  EnterCriticalSection(CrSec);
  if CachedLangID = LANGID then
  begin
    Result := CachedLanguageNames;
    LeaveCriticalSection(CrSec);
    exit;
  end;
  LeaveCriticalSection(CrSec);

  Result := '';
  if IsValidLocale(LANGID, LCID_SUPPORTED) then
  begin
    if UseThreadUILanguageAPI then
    begin
      SavedThreadUILanguage := GetThreadUILanguage;
      SetThreadUILanguage(LANGID);
      UILanguages := Nil;
      bufSize := 0;
      try
        if GetThreadPreferredUILanguages(MUI_LANGUAGE_NAME or MUI_UI_FALLBACK, @numLang, nil, @bufSize) then
        begin
          GetMem(UILanguages, bufSize * sizeof(Char));
          GetThreadPreferredUILanguages(MUI_LANGUAGE_NAME or MUI_UI_FALLBACK, @numLang, UILanguages, @bufSize);
          for I := 0 to BufSize - 2 do
            if UILanguages[I] = #0 then UILanguages[I] := ',';
          Result := UILanguages;
        end;
      finally
        SetThreadUILanguage(SavedThreadUILanguage);
        if UILanguages <> nil then FreeMem(UILanguages);
      end;
    end
    else
    begin
      Result := GetPreferredLangForOldOS(LANGID);
      if LangID <> GetSystemDefaultUILanguage then
      begin
        if Result <> '' then Result := Result + ',';
        Result := Result + GetPreferredLangForOldOS(GetSystemDefaultUILanguage);
      end;
    end;
  end;

  EnterCriticalSection(CrSec);
  CachedLangID := LANGID;
  lstrcpyn(CachedLanguageNames, PChar(Result), SizeOf(CachedLanguageNames));
  LeaveCriticalSection(CrSec);
end;

function GetLocaleOverride(AppName: string): string;

  function FindBS(Current: PChar): PChar;
  begin
    Result := Current;
    while (Result^ <> #0) and (Result^ <> '\') do
      Result := CharNext(Result);
  end;

  function ToLongPath(AFileName: PChar; BufLen: Integer): PChar;
  var
    CurrBS, NextBS: PChar;
    Handle, L: Integer;
    FindData: TWin32FindData;
    Buffer: array[0..MAX_PATH] of Char;
    GetLongPathName: function (ShortPathName: PChar; LongPathName: PChar;
      cchBuffer: Integer): Integer stdcall;
  const
    {$IFNDEF UNICODE}
    longPathName = 'GetLongPathNameA';
    {$ELSE}
    longPathName = 'GetLongPathNameW';
    {$ENDIF}
  begin
    Result := AFileName;
    Handle := GetModuleHandle(kernel);
    if Handle <> 0 then
    begin
      @GetLongPathName := GetProcAddress(Handle, longPathName);
      if Assigned(GetLongPathName) and
        (GetLongPathName(AFileName, Buffer, Length(Buffer)) <> 0) then
      begin
        lstrcpyn(AFileName, Buffer, BufLen);
        Exit;
      end;
    end;

    if AFileName[0] = '\' then
    begin
      if AFileName[1] <> '\' then Exit;
      CurrBS := FindBS(AFileName + 2);  // skip server name
      if CurrBS^ = #0 then Exit;
      CurrBS := FindBS(CurrBS + 1);     // skip share name
      if CurrBS^ = #0 then Exit;
    end else
      CurrBS := AFileName + 2;          // skip drive name

    L := CurrBS - AFileName;
    lstrcpyn(Buffer, AFileName, L + 1);
    while CurrBS^ <> #0 do
    begin
      NextBS := FindBS(CurrBS + 1);
      if L + (NextBS - CurrBS) + 1 > Length(Buffer) then Exit;
      lstrcpyn(Buffer + L, CurrBS, (NextBS - CurrBS) + 1);

      Handle := FindFirstFile(Buffer, FindData);
      if (Handle = -1) then Exit;
      FindClose(Handle);

      if L + 1 + _strlen(FindData.cFileName) + 1 > Length(Buffer) then Exit;
      Buffer[L] := '\';
      lstrcpyn(Buffer + L + 1, FindData.cFileName, Length(Buffer) - L - 1);
      Inc(L, _strlen(FindData.cFileName) + 1);
      CurrBS := NextBS;
    end;
    lstrcpyn(AFileName, Buffer, BufLen);
  end;

var
  HostAppName: array [0..MAX_PATH] of Char;
  LocaleOverride: PChar;
  Key: LongWord;
  LocSize: Integer;
begin
  if AppName = '' then
    GetModuleFileName(0, HostAppName, Length(HostAppName)) // Get host application name
  else
    lstrcpyn(HostAppName, PChar(AppName), Length(HostAppName));
  if HostAppName[0] = #$0 then exit;
  LocaleOverride := nil;

  if (RegOpenKeyEx(HKEY_CURRENT_USER, NewLocaleOverrideKey, 0, KEY_READ, Key) = 0) or
   (RegOpenKeyEx(HKEY_LOCAL_MACHINE, NewLocaleOverrideKey, 0, KEY_READ, Key) = 0) or
   (RegOpenKeyEx(HKEY_CURRENT_USER, OldLocaleOverrideKey, 0, KEY_READ, Key) = 0) or
   (RegOpenKeyEx(HKEY_CURRENT_USER, OlderLocaleOverrideKey, 0, KEY_READ, Key) = 0) then
  try
    ToLongPath(HostAppName, Length(HostAppName));
    if RegQueryValueEx(Key, HostAppName, nil, nil, nil, @LocSize) = 0 then
    begin
      GetMem(LocaleOverride, LocSize);
      RegQueryValueEx(Key, HostAppName, nil, nil, LocaleOverride, @LocSize);
      Result := LocaleOverride;
    end
    else if RegQueryValueEx(Key, '', nil, nil, nil, @LocSize) = 0 then
    begin
      GetMem(LocaleOverride, LocSize);
      RegQueryValueEx(Key, '', nil, nil, LocaleOverride, @LocSize);
      Result := LocaleOverride;
    end;
  finally
    if LocaleOverride <> nil then
      FreeMem(LocaleOverride);
    RegCloseKey(Key);
  end;
end;
{$ENDIF MSWINDOWS}

function LoadResourceModule(ModuleName: PChar; CheckOwner: Boolean): LongWord;
{$IFDEF MACOSX}
begin
   // not implemented yet
   Error(reAssertionFailed);
end;
{$ENDIF MAXOSX}
{$IFDEF LINUX}
var
  FileName: array [0..MAX_PATH] of Char;
  LangCode: PChar;  // Language and country code.  Example: en_US
  P: PChar;
  ModuleNameLen, FileNameLen, i: Integer;
  st1, st2: TStatStruct;
begin
  LangCode := __getenv('LANG');
  Result := 0;
  if (LangCode = nil) or (LangCode^ = #0) or (ModuleName = nil) then Exit;

  // look for modulename.en_US  (ignoring codeset and modifier suffixes)
  P := LangCode;
  while P^ in ['a'..'z', 'A'..'Z', '_'] do
    Inc(P);
  if P = LangCode then Exit;

  if CheckOwner and (__xstat(STAT_VER_LINUX, ModuleName, st1) = -1) then
    Exit;

  ModuleNameLen := _strlen(ModuleName);
  if (ModuleNameLen + P - LangCode) >= MAX_PATH then Exit;
  Move(ModuleName[0], Filename[0], ModuleNameLen);
  Filename[ModuleNameLen] := '.';
  Move(LangCode[0], Filename[ModuleNameLen + 1], P - LangCode);
  FileNameLen := ModuleNameLen + 1 + (P - LangCode);
  Filename[FileNameLen] := #0;

{ Security check:  make sure the user id (owner) and group id of
  the base module matches the user id and group id of the resource
  module we're considering loading.  This is to prevent loading
  of malicious code dropped into the base module's directory by
  a hostile user.  The app and all its resource modules must
  have the same owner and group.  To disable this security check,
  call this function with CheckOwner set to False. }

  if (not CheckOwner) or
    ((__xstat(STAT_VER_LINUX, FileName, st2) <> -1)
    and (st1.st_uid = st2.st_uid)
    and (st1.st_gid = st2.st_gid)) then
  begin
    Result := dlopen(Filename, RTLD_LAZY);
    if Result <> 0 then Exit;
  end;

  // look for modulename.en    (ignoring country code and suffixes)
  i := ModuleNameLen + 1;
  while (i <= FileNameLen) and (Filename[i] in ['a'..'z', 'A'..'Z']) do
    Inc(i);
  if (i = ModuleNameLen + 1) or (i > FileNameLen) then Exit;
  FileName[i] := #0;

  { Security check.  See notes above.  }
  if (not CheckOwner) or
    ((__xstat(STAT_VER_LINUX, FileName, st2) <> -1)
    and (st1.st_uid = st2.st_uid)
    and (st1.st_gid = st2.st_gid)) then
  begin
    Result := dlopen(FileName, RTLD_LAZY);
  end;
end;
{$ENDIF LINUX}
{$IFDEF MSWINDOWS}
var
  HostAppName: array [0..MAX_PATH] of Char;
  ResModuleName : string;
begin
  Result := 0;
  GetModuleFileName(0, HostAppName, Length(HostAppName));
  ResModuleName := GetResourceModuleName(HostAppName, ModuleName);
  if ResModuleName <> '' then
    Result := LoadLibraryEx(PChar(ResModuleName), 0, LOAD_LIBRARY_AS_DATAFILE)
end;

function GetResourceModuleName(HostAppName, ModuleName: string): string;
var
  FileNameBody: array [0..MAX_PATH] of Char;
  ExtPart: PChar;
  MaxExtLength: integer;

  function ResouceDLLExists: Boolean;
  var
    Handle: Integer;
    FindData: TWin32FindData;
  begin
    Handle := FindFirstFile(FileNameBody, FindData);
    Result := Handle <> -1;
    if Result then
      FindClose(Handle);
  end;

  function LoadLanguageList(L: String): Boolean;
  Var
    aLang, P: PChar;
  begin
    Result := True;
    P := PChar(L);
    while (P^ <> #0) do
    begin
      aLang := P;
      while (P^ <> ',') and (P^ <> #0) do Inc(P);
      if P^ = ',' then
      begin
        P^ := #0;
        Inc(P);
      end;
      lstrcpyn(ExtPart, aLang, MaxExtLength);
      if ResouceDLLExists then exit;
    end;
    Result := False;
  end;

  function Load3LettersModule: Boolean;
  begin
    Result := True;
    GetLocaleInfo(GetUserDefaultUILanguage, LOCALE_SABBREVLANGNAME, ExtPart, MaxExtLength);
    if ResouceDLLExists then Exit;
    ExtPart[2] := #$0;
    if ResouceDLLExists then Exit;
    Result := False;
  end;

var
  Found: Boolean;
  LocaleOverrideKey: string;
begin
  Result := '';
  Found := False;
  lstrcpyn(FileNameBody, PChar(ModuleName), Length(FileNameBody));
  ExtPart := PChar(@FileNameBody) + _strlen(FileNameBody) - 1;
  while (ExtPart^ <> '.') and (ExtPart <> FileNameBody) do Dec(ExtPart);
  if ExtPart <> FileNameBody then
  begin
    Inc(ExtPart);
    ExtPart[0] := #0;
    MaxExtLength := Length(FileNameBody) - (ExtPart - PChar(@FileNameBody));
    LocaleOverrideKey := GetLocaleOverride(HostAppName);
    if LocaleOverrideKey <> '' then
      Found := LoadLanguageList(LocaleOverrideKey)
    else
    begin
      Found := LoadLanguageList(GetUILanguages(GetUserDefaultUILanguage));
      if (not Found) and (not UseThreadUILanguageAPI) then
        Found := LoadLanguageList(GetUILanguages(GetSystemDefaultUILanguage));
      if not Found then Found := Load3LettersModule;
    end;
  end;
  if Found then
    Result := FileNameBody;
end;
{$ENDIF MSWINDOWS}

procedure EnumModules(Func: TEnumModuleFunc; Data: Pointer); assembler;
begin
  EnumModules(TEnumModuleFuncLW(Func), Data);
end;

procedure EnumResourceModules(Func: TEnumModuleFunc; Data: Pointer);
begin
  EnumResourceModules(TEnumModuleFuncLW(Func), Data);
end;

procedure EnumModules(Func: TEnumModuleFuncLW; Data: Pointer);
var
  CurModule: PLibModule;
begin
  CurModule := LibModuleList;
  while CurModule <> nil do
  begin
    if not Func(CurModule.Instance, Data) then Exit;
    CurModule := CurModule.Next;
  end;
end;

procedure EnumResourceModules(Func: TEnumModuleFuncLW; Data: Pointer);
var
  CurModule: PLibModule;
begin
  CurModule := LibModuleList;
  while CurModule <> nil do
  begin
    if not Func(DelayLoadResourceModule(CurModule), Data) then Exit;
    CurModule := CurModule.Next;
  end;
end;

procedure AddModuleUnloadProc(Proc: TModuleUnloadProc);
begin
  AddModuleUnloadProc(TModuleUnloadProcLW(Proc));
end;

procedure RemoveModuleUnloadProc(Proc: TModuleUnloadProc);
begin
  RemoveModuleUnloadProc(TModuleUnloadProcLW(Proc));
end;

procedure AddModuleUnloadProc(Proc: TModuleUnloadProcLW);
var
  P: PModuleUnloadRec;
begin
  New(P);
  P.Next := ModuleUnloadList;
  @P.Proc := @Proc;
  ModuleUnloadList := P;
end;

procedure RemoveModuleUnloadProc(Proc: TModuleUnloadProcLW);
var
  P, C: PModuleUnloadRec;
begin
  P := ModuleUnloadList;
  if (P <> nil) and (@P.Proc = @Proc) then
  begin
    ModuleUnloadList := ModuleUnloadList.Next;
    Dispose(P);
  end else
  begin
    C := P;
    while C <> nil do
    begin
      if (C.Next <> nil) and (@C.Next.Proc = @Proc) then
      begin
        P := C.Next;
        C.Next := C.Next.Next;
        Dispose(P);
        Break;
      end;
      C := C.Next;
    end;
  end;
end;

procedure NotifyModuleUnload(HInstance: LongWord);
var
  P: PModuleUnloadRec;
begin
  P := ModuleUnloadList;
  while P <> nil do
  begin
    try
      P.Proc(HInstance);
    except
      // Make sure it doesn't stop notifications
    end;
    P := P.Next;
  end;
{$IFDEF LINUX}
  InvalidateModuleCache;
{$ENDIF}
end;

procedure RegisterModule(LibModule: PLibModule);
begin
  LibModule.Next := LibModuleList;
  LibModuleList := LibModule;
end;

procedure UnregisterModule(LibModule: PLibModule);
var
  CurModule: PLibModule;
begin
  try
    NotifyModuleUnload(LibModule.Instance);
  finally
    if LibModule = LibModuleList then
      LibModuleList := LibModule.Next
    else
    begin
      CurModule := LibModuleList;
      while CurModule <> nil do
      begin
        if CurModule.Next = LibModule then
        begin
          CurModule.Next := LibModule.Next;
          Break;
        end;
        CurModule := CurModule.Next;
      end;
    end;
  end;
end;

function _IntfClear(var Dest: IInterface): Pointer;
{$IFDEF PUREPASCAL}
var
  P: Pointer;
begin
  Result := @Dest;
  if Dest <> nil then
  begin
    P := Pointer(Dest);
    Pointer(Dest) := nil;
    IInterface(P)._Release;
  end;
end;
{$ELSE}
asm
        MOV     EDX,[EAX]
        TEST    EDX,EDX
        JE      @@1
        MOV     DWORD PTR [EAX],0
        PUSH    EAX
        PUSH    EDX
        MOV     EAX,[EDX]
        CALL    DWORD PTR [EAX] + VMTOFFSET IInterface._Release
        POP     EAX
@@1:
end;
{$ENDIF}

procedure _IntfCopy(var Dest: IInterface; const Source: IInterface);
{$IFDEF PUREPASCAL}
var
  P: Pointer;
begin
  P := Pointer(Dest);
  if Source <> nil then
    Source._AddRef;
  Pointer(Dest) := Pointer(Source);
  if P <> nil then
    IInterface(P)._Release;
end;
{$ELSE}
asm
{
  The most common case is the single assignment of a non-nil interface
  to a nil interface.  So we streamline that case here.  After this,
  we give essentially equal weight to other outcomes.

    The semantics are:  The source intf must be addrefed *before* it
    is assigned to the destination.  The old intf must be released
    after the new intf is addrefed to support self assignment (I := I).
    Either intf can be nil.  The first requirement is really to make an
    error case function a little better, and to improve the behaviour
    of multithreaded applications - if the addref throws an exception,
    you don't want the interface to have been assigned here, and if the
    assignment is made to a global and another thread references it,
    again you don't want the intf to be available until the reference
    count is bumped.
}
        TEST    EDX,EDX         // is source nil?
        JE      @@NilSource
        PUSH    EDX             // save source
        PUSH    EAX             // save dest
        MOV     EAX,[EDX]       // get source vmt
        PUSH    EDX             // source as arg
        CALL    DWORD PTR [EAX] + VMTOFFSET IInterface._AddRef
        POP     EAX             // retrieve dest
        MOV     ECX, [EAX]      // get current value
        POP     [EAX]           // set dest in place
        TEST    ECX, ECX        // is current value nil?
        JNE     @@ReleaseDest   // no, release it
        RET                     // most common case, we return here
@@ReleaseDest:
        MOV     EAX,[ECX]       // get current value vmt
        PUSH    ECX             // current value as arg
        CALL    DWORD PTR [EAX] + VMTOFFSET IInterface._Release
        RET

{   Now we're into the less common cases.  }
@@NilSource:
        MOV     ECX, [EAX]      // get current value
        TEST    ECX, ECX        // is it nil?
        MOV     [EAX], EDX      // store in dest (which is nil)
        JE  @@Done
        MOV     EAX, [ECX]      // get current vmt
        PUSH    ECX             // current value as arg
        CALL    DWORD PTR [EAX] + VMTOFFSET IInterface._Release
@@Done:
end;
{$ENDIF}

procedure _IntfCast(var Dest: IInterface; const Source: IInterface; const IID: TGUID);
{$IFDEF PUREPASCAL}
// PIC:  EBX must be correct before calling QueryInterface
var
  Temp: IInterface;
begin
  if Source = nil then
    Dest := nil
  else
  begin
    Temp := nil;
    if Source.QueryInterface(IID, Temp) <> 0 then
      Error(reIntfCastError)
    else
      Dest := Temp;
  end;
end;
{$ELSE}
asm
        TEST    EDX,EDX
        JE      _IntfClear
        PUSH    EDI
        MOV     EDI, EAX   // ptr to dest
        PUSH    0
        PUSH    ESP        // ptr to temp
        PUSH    ECX        // ptr to GUID
        PUSH    EDX        // ptr to source
@@1:    MOV     EAX,[EDX]
        CALL    DWORD PTR [EAX] + VMTOFFSET IInterface.QueryInterface
        TEST    EAX,EAX
        JE      @@2
        MOV     AL,reIntfCastError
        JMP     Error
@@2:    MOV     EAX, [EDI]
        TEST    EAX, EAX
        JE      @@3
        PUSH    EAX
        MOV     EAX,[EAX]
        CALL    DWORD PTR [EAX] + VMTOFFSET IInterface._Release
@@3:    POP     EAX          // value of temp
        MOV     [EDI], EAX
        POP     EDI
end;
{$ENDIF}

procedure _IntfAddRef(const Dest: IInterface);
begin
  if Dest <> nil then Dest._AddRef;
end;

procedure TInterfacedObject.AfterConstruction;
begin
// Release the constructor's implicit refcount
  InterlockedDecrement(FRefCount);
end;

procedure TInterfacedObject.BeforeDestruction;
begin
  if RefCount <> 0 then
    Error(reInvalidPtr);
end;

// Set an implicit refcount so that refcounting
// during construction won't destroy the object.
class function TInterfacedObject.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TInterfacedObject(Result).FRefCount := 1;
end;

function TInterfacedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TInterfacedObject._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TInterfacedObject._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

{ TAggregatedObject }

constructor TAggregatedObject.Create(const Controller: IInterface);
begin
  // weak reference to controller - don't keep it alive
  FController := Pointer(Controller);
end;

function TAggregatedObject.GetController: IInterface;
begin
  Result := IInterface(FController);
end;

function TAggregatedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := IInterface(FController).QueryInterface(IID, Obj);
end;

function TAggregatedObject._AddRef: Integer;
begin
  Result := IInterface(FController)._AddRef;
end;

function TAggregatedObject._Release: Integer; stdcall;
begin
  Result := IInterface(FController)._Release;
end;

{ TContainedObject }

function TContainedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

{ TClassHelperBase }

constructor TClassHelperBase._Create(Instance: TObject);
begin
  inherited Create;
  FInstance := Instance;
end;

function _CheckAutoResult(ResultCode: HResult): HResult;
{$IF Defined(PIC) or Defined(PUREPASCAL) or defined(ALIGN_STACK)}
begin
  if ResultCode < 0 then
  begin
    if Assigned(SafeCallErrorProc) then
      SafeCallErrorProc(ResultCode, Pointer(-1));  // loses error address
    Error(reSafeCallError);
  end;
  Result := ResultCode;
end;
{$ELSE}
asm
        TEST    EAX,EAX
        JNS     @@2
        MOV     ECX,SafeCallErrorProc
        TEST    ECX,ECX
        JE      @@1
        MOV     EDX,[ESP]
        CALL    ECX
@@1:    MOV     AL,reSafeCallError
        JMP     Error
@@2:
end;
{$IFEND}

function  CompToDouble(Value: Comp): Double; cdecl;
begin
  Result := Value;
end;

procedure  DoubleToComp(Value: Double; var Result: Comp); cdecl;
begin
  Result := Value;
end;

function  CompToCurrency(Value: Comp): Currency; cdecl;
begin
  Result := Value;
end;

procedure  CurrencyToComp(Value: Currency; var Result: Comp); cdecl;
begin
  Result := Value;
end;

function GetMemory(Size: Integer): Pointer; cdecl;
begin
  Result := MemoryManager.GetMem(Size);
end;

function FreeMemory(P: Pointer): Integer; cdecl;
begin
  if P = nil then
    Result := 0
  else
    Result := MemoryManager.FreeMem(P);
end;

function ReallocMemory(P: Pointer; Size: Integer): Pointer; cdecl;
begin
  if P = nil then
    Result := GetMemory(Size)
  else
  Result := MemoryManager.ReallocMem(P, Size);
end;

procedure SetLineBreakStyle(var T: Text; Style: TTextLineBreakStyle);
begin
  if TTextRec(T).Mode = fmClosed then
    TTextRec(T).Flags := (TTextRec(T).Flags and not tfCRLF) or (tfCRLF * Byte(Style))
  else
    SetInOutRes(107);  // can't change mode of open file
end;

// UnicodeToUTF8(3):
// Scans the source data to find the null terminator, up to MaxBytes
// Dest must have MaxBytes available in Dest.

function UnicodeToUtf8(Dest: PAnsiChar; Source: PWideChar; MaxBytes: Integer): Integer;
begin
  Result := UnicodeToUtf8(Dest, MaxBytes, Source, Cardinal(-1));
end;

// UnicodeToUtf8(4):
// MaxDestBytes includes the null terminator (last char in the buffer will be set to null)
// Function result includes the null terminator.
// Nulls in the source data are not considered terminators - SourceChars must be accurate

function UnicodeToUtf8(Dest: PAnsiChar; MaxDestBytes: Cardinal; Source: PWideChar; SourceChars: Cardinal): Cardinal;
{$IFDEF MACOSX}
begin
   Error(reMacNotImplemented);
end;
{$ELSE !MACOSX}
begin
  Result := 0;
  if Source = nil then Exit;
  if Dest <> nil then
  begin
    Result := Cardinal(WideCharToMultiByte(CP_UTF8, 0, Source, Integer(SourceChars), Dest, Integer(MaxDestBytes), nil, nil));
    if (Result > 0) and (Result <= MaxDestBytes) and (Dest[Result - 1] <> #0) then
    begin
      if Result = MaxDestBytes then
      begin
        while (Result > 1) and (Byte(Dest[Result - 1]) > $7F) and (Byte(Dest[Result - 1]) and $80 <> 0) and (Byte(Dest[Result - 1]) and $C0 <> $C0) do
          Dec(Result);
      end else
        Inc(Result);
      Dest[Result - 1] := #0;
    end;
  end else
    Result := Cardinal(WideCharToMultiByte(CP_UTF8, 0, Source, Integer(SourceChars), nil, 0, nil, nil));
end;
{$ENDIF !MACOSX}

function Utf8ToUnicode(Dest: PWideChar; Source: PAnsiChar; MaxChars: Integer): Integer;
begin
  Result := Utf8ToUnicode(Dest, MaxChars, Source, Cardinal(-1));
end;

function Utf8ToUnicode(Dest: PWideChar; MaxDestChars: Cardinal; Source: PAnsiChar; SourceBytes: Cardinal): Cardinal;
{$IFDEF MACOSX}
begin
   Error(reMacNotImplemented);
end;
{$ELSE !MACOSX}
begin
  Result := 0;
  if Source = nil then Exit;
  if (Dest <> nil) and (MaxDestChars > 0) then
  begin
    Result := Cardinal(MultibyteToWideChar(CP_UTF8, 0, Source, Integer(SourceBytes), Dest, Integer(MaxDestChars)));
    if (Result > 0) and (Result <= MaxDestChars) and (Dest[Result - 1] <> #0) then
    begin
      if Result = MaxDestChars then
      begin
        if (Result > 1) and (Word(Dest[Result - 1]) >= $DC00) and (Word(Dest[Result - 1]) <= $DFFF) then
          Dec(Result);
      end else
        Inc(Result);
      Dest[Result - 1] := #0;
    end;
  end else
    Result := Cardinal(MultibyteToWideChar(CP_UTF8, 0, Source, Integer(SourceBytes), nil, 0));
end;
{$ENDIF !MACOSX}

function Utf8Encode(const WS: WideString): RawByteString;
var
  L: Integer;
  Temp: UTF8String;
begin
  Result := '';
  if WS = '' then Exit;
  L := Length(WS);
  SetLength(Temp, L * 3); // SetLength includes space for null terminator

  L := UnicodeToUtf8(PAnsiChar(Temp), Length(Temp) + 1, PWideChar(WS), L);
  if L > 0 then
    SetLength(Temp, L - 1)
  else
    Temp := '';
  Result := Temp;
  if Result <> '' then
    PStrRec(Integer(Result) - SizeOf(StrRec)).codePage := CP_UTF8;
end;

function Utf8Encode(const US: UnicodeString): RawByteString;
var
  L: Integer;
  Temp: UTF8String;
begin
  Result := '';
  if US = '' then Exit;
  L := Length(US);
  SetLength(Temp, L * 3); // SetLength includes space for null terminator

  L := UnicodeToUtf8(PAnsiChar(Temp), Length(Temp) + 1, PWideChar(US), L);
  if L > 0 then
    SetLength(Temp, L - 1)
  else
    Temp := '';
  Result := Temp;
  if Result <> '' then
    PStrRec(Integer(Result) - SizeOf(StrRec)).codePage := CP_UTF8;
end;

function UTF8Encode(const A: RawByteString): RawByteString;
begin
  if StringCodePage(A) = CP_UTF8 then
    Result := A
  else
    Result := UTF8Encode(UnicodeString(A));
end;

function Max(I1, I2: Integer): Integer; inline;
begin
  if I1 > I2 then
    Result := I1
  else
    Result := I2;
end;

function UTF8EncodeToShortString(const WS: WideString): ShortString;
begin
  Result[0] := AnsiChar(Max(0, UnicodeToUtf8(@Result[1], High(Result), PWideChar(WS), Length(WS)) - 1));
end;

function UTF8EncodeToShortString(const US: UnicodeString): ShortString;
begin
  Result[0] := AnsiChar(Max(0, UnicodeToUtf8(@Result[1], High(Result), PWideChar(US), Length(US)) - 1));
end;

function UTF8EncodeToShortString(const A: RawByteString): ShortString;
begin
  if StringCodePage(A) = CP_UTF8 then
    Result := A
  else
    Result := UTF8EncodeToShortString(UnicodeString(A));
end;

function Utf8Decode(const S: RawByteString): WideString;
var
  L: Integer;
  Temp: WideString;
begin
  Result := '';
  if S = '' then Exit;
  L := Length(S);
  SetLength(Temp, L);

  L := Utf8ToUnicode(PWideChar(Temp), L + 1, PAnsiChar(S), L);
  if L > 0 then
    SetLength(Temp, L - 1)
  else
    Temp := '';
  Result := Temp;
end;

function UTF8ToWideString(const S: RawByteString): WideString; inline;
begin
  Result := UTF8Decode(S);
end;

function UTF8ToUnicodeString(const S: RawByteString): UnicodeString;
var
  L: Integer;
  Temp: UnicodeString;
begin
  Result := '';
  if S = '' then Exit;
  L := Length(S);
  SetLength(Temp, L);

  L := Utf8ToUnicode(PWideChar(Temp), L + 1, PAnsiChar(S), L);
  if L > 0 then
    SetLength(Temp, L - 1)
  else
    Temp := '';
  Result := Temp;
end;

function UTF8ToUnicodeString(const S: PAnsiChar): UnicodeString; overload;
var
  L: Integer;
  Temp: UnicodeString;
begin
  Result := '';
  if S = '' then Exit;
{$IFDEF MSWINDOWS}
  L := _strlenA(S);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  L := _strlen(S);
{$ENDIF POSIX}
  SetLength(Temp, L);

  L := Utf8ToUnicode(PWideChar(Temp), L + 1, S, L);
  if L > 0 then
    SetLength(Temp, L - 1)
  else
    Temp := '';
  Result := Temp;
end;

function UTF8ToUnicodeString(const S: ShortString): UnicodeString; overload;
var
  L: Integer;
  Temp: UnicodeString;
begin
  Result := '';
  if S = '' then Exit;
  L := Length(S);
  SetLength(Temp, L);

  L := Utf8ToUnicode(PWideChar(Temp), L + 1, @S[1], L);
  if L > 0 then
    SetLength(Temp, L - 1)
  else
    Temp := '';
  Result := Temp;
end;

function UTF8ToString(const S: RawByteString): string;
begin
{$IFDEF UNICODE}
  Result := UTF8ToUnicodeString(S);
{$ELSE}
  Result := UTf8ToAnsi(S);
{$ENDIF}
end;

function UTF8ToString(const S: ShortString): string;
begin
{$IFDEF UNICODE}
  Result := UTF8ToUnicodeString(S);
{$ELSE}
  Result := UTf8ToAnsi(S);
{$ENDIF}
end;

function UTF8ToString(const S: PAnsiChar): string;
begin
{$IFDEF UNICODE}
  Result := UTF8ToUnicodeString(S);
{$ELSE}
  Result := UTf8ToAnsi(S);
{$ENDIF}
end;

function UTF8ToString(const S: array of AnsiChar): string;
begin
{$IFDEF UNICODE}
  Result := UTF8ToUnicodeString(@S[0]);
{$ELSE}
  Result := UTf8ToAnsi(S);
{$ENDIF}
end;

function AnsiToUtf8(const S: string): RawByteString;
begin
  Result := Utf8Encode(S);
end;

function Utf8ToAnsi(const S: RawByteString): string;
begin
  Result := string(Utf8ToUnicodeString(S));
end;

{$IFDEF LINUX}

function GetCPUType: Integer;
asm
      PUSH      EBX
    // this code assumes ESP is 4 byte aligned
    // test for 80386:  see if bit #18 of EFLAGS (Alignment fault) can be toggled
      PUSHFD
      POP       EAX
      MOV       ECX, EAX
      XOR       EAX, $40000   // flip AC bit in EFLAGS
      PUSH      EAX
      POPFD
      PUSHFD
      POP       EAX
      XOR       EAX, ECX      // zero = 80386 CPU (can't toggle AC bit)
      MOV       EAX, CPUi386
      JZ        @@Exit
      PUSH      ECX
      POPFD                    // restore original flags before next test

      // test for 80486:  see if bit #21 of EFLAGS (CPUID supported) can be toggled
      MOV       EAX, ECX        // get original EFLAGS
      XOR       EAX, $200000    // flip CPUID bit in EFLAGS
      PUSH      EAX
      POPFD
      PUSHFD
      POP       EAX
      XOR       EAX, ECX    // zero = 80486 (can't toggle EFLAGS bit #21)
      MOV       EAX, CPUi486
      JZ        @@Exit

      // Use CPUID instruction to get CPU family
      XOR       EAX, EAX
      CPUID
      CMP       EAX, 1
      JL        @@Exit          // unknown processor response: report as 486
      XOR       EAX, EAX
      INC       EAX       // we only care about info level 1
      CPUID
      AND       EAX, $F00
      SHR       EAX, 8
      // Test8086 values are one less than the CPU model number, for historical reasons
      DEC       EAX

@@Exit:
      POP       EBX
end;


const
  sResSymExport = '@Sysinit@ResSym';
  sResStrExport = '@Sysinit@ResStr';
  sResHashExport = '@Sysinit@ResHash';

type
  TElf32Sym = record
    Name: Cardinal;
    Value: Pointer;
    Size: Cardinal;
    Info: Byte;
    Other: Byte;
    Section: Word;
  end;
  PElf32Sym = ^TElf32Sym;

  TElfSymTab = array [0..0] of TElf32Sym;
  PElfSymTab = ^TElfSymTab;

  TElfWordTab = array [0..2] of Cardinal;
  PElfWordTab = ^TElfWordTab;


{ If Name encodes a numeric identifier, return it, else return -1.  }
function NameToId(Name: PChar): Longint;
var digit: Longint;
begin
  if Longint(Name) and $ffff0000 = 0 then
  begin
    Result := Longint(Name) and $ffff;
  end
  else if Name^ = '#' then
  begin
    Result := 0;
    inc (Name);
    while (Ord(Name^) <> 0) do
    begin
      digit := Ord(Name^) - Ord('0');
      if (LongWord(digit) > 9) then
      begin
        Result := -1;
        exit;
      end;
      Result := Result * 10 + digit;
      inc (Name);
    end;
  end
  else
    Result := -1;
end;


// Return ELF hash value for NAME converted to lower case.
function ElfHashLowercase(Name: PChar): Cardinal;
var
  g: Cardinal;
  c: Char;
begin
  Result := 0;
  while name^ <> #0 do
  begin
    c := name^;
    case c of
      'A'..'Z': Inc(c, Ord('a') - Ord('A'));
    end;
    Result := (Result shl 4) + Ord(c);
    g := Result and $f0000000;
    Result := (Result xor (g shr 24)) and not g;
    Inc(name);
  end;
end;

type
  PFindResourceCache = ^TFindResourceCache;
  TFindResourceCache = record
    ModuleHandle: HMODULE;
    Version: Cardinal;
    SymbolTable: PElfSymTab;
    StringTable: PChar;
    HashTable: PElfWordTab;
    BaseAddress: Cardinal;
  end;

threadvar
  FindResourceCache: TFindResourceCache;

function GetResourceCache(ModuleHandle: HMODULE): PFindResourceCache;
var
  info: TDLInfo;
begin
  Result := @FindResourceCache;
  if (ModuleHandle <> Result^.ModuleHandle) or (ModuleCacheVersion <> Result^.Version) then
  begin
    Result^.SymbolTable := dlsym(ModuleHandle, sResSymExport);
    Result^.StringTable := dlsym(ModuleHandle, sResStrExport);
    Result^.HashTable := dlsym(ModuleHandle, sResHashExport);
    Result^.ModuleHandle := ModuleHandle;
    if (dladdr(Result^.HashTable, Info) = 0) or (Info.BaseAddress = ExeBaseAddress) then
      Result^.BaseAddress := 0   // if it's not in a library, assume the exe
    else
      Result^.BaseAddress := Cardinal(Info.BaseAddress);
    Result^.Version := ModuleCacheVersion;
  end;
end;

function FindResource(ModuleHandle: HMODULE; ResourceName: PChar; ResourceType: PChar): TResourceHandle;
var
  P: PFindResourceCache;
  nid, tid: Longint;
  ucs2_key: array [0..2] of WideChar;
  key: array [0..127] of Char;
  len: Integer;
  pc: PChar;
  ch: Char;
  nbucket: Cardinal;
  bucket, chain: PElfWordTab;
  syndx: Cardinal;
begin
  Result := 0;
  if ResourceName = nil then Exit;
  P := GetResourceCache(ModuleHandle);

  tid := NameToId (ResourceType);
  if tid = -1 then Exit;  { not supported (yet?) }

  { This code must match util-common/elfres.c }
  nid := NameToId (ResourceName);
  if nid = -1 then
  begin
    ucs2_key[0] := WideChar(2*tid+2);
    ucs2_key[1] := WideChar(0);
    len := UnicodeToUtf8 (key, ucs2_key, SizeOf (key)) - 1;
    pc := key+len;
    while Ord(ResourceName^) <> 0 do
    begin
      ch := ResourceName^;
      if Ord(ch) > 127 then exit; { insist on 7bit ASCII for now }
      if ('A' <= ch) and (ch <= 'Z') then Inc(ch, Ord('a') - Ord('A'));
      pc^ := ch;
      inc (pc);
      if pc = key + SizeOf(key) then exit;
      inc (ResourceName);
    end;
    pc^ := Char(0);
  end
  else
  begin
    ucs2_key[0] := WideChar(2*tid+1);
    ucs2_key[1] := WideChar(nid);
    ucs2_key[2] := WideChar(0);
    UnicodeToUtf8 (key, ucs2_key, SizeOf (key));
  end;

  with P^ do
  begin
    nbucket := HashTable[0];
  //  nsym := HashTable[1];
    bucket := @HashTable[2];
    chain := @HashTable[2+nbucket];

    syndx := bucket[ElfHashLowercase(key) mod nbucket];
    while (syndx <> 0)
      and (strcasecmp(key, @StringTable[SymbolTable[syndx].Name]) <> 0) do
      syndx := chain[syndx];

    if syndx = 0 then
      Result := 0
    else
      Result := TResourceHandle(@SymbolTable[syndx]);
  end;
end;

function LoadResource(ModuleHandle: HMODULE; ResHandle: TResourceHandle): HGLOBAL;
var
  P: PFindResourceCache;
begin
  if ResHandle <> 0 then
  begin
    P := GetResourceCache(ModuleHandle);
    Result := HGLOBAL(PElf32Sym(ResHandle)^.Value);
    Inc (Cardinal(Result), P^.BaseAddress);
  end
  else
    Result := 0;
end;

function SizeofResource(ModuleHandle: HMODULE; ResHandle: TResourceHandle): Integer;
begin
  if ResHandle <> 0 then
    Result := PElf32Sym(ResHandle)^.Size
  else
    Result := 0;
end;

function LockResource(ResData: HGLOBAL): Pointer;
begin
  Result := Pointer(ResData);
end;

function UnlockResource(ResData: HGLOBAL): LongBool;
begin
  Result := False;
end;

function FreeResource(ResData: HGLOBAL): LongBool;
begin
  Result := True;
end;
{$ENDIF}

{ ResString support function }

{$IFDEF MSWINDOWS}
function LoadResString(ResStringRec: PResStringRec): string;
var
  Buffer: array [0..4095] of Char;
begin
  if ResStringRec = nil then Exit;
  if ResStringRec.Identifier < 64*1024 then
    SetString(Result, Buffer,
      LoadString(FindResourceHInstance(ResStringRec.Module^),
        ResStringRec.Identifier, Buffer, Length(Buffer)))
  else
    Result := PChar(ResStringRec.Identifier);
end;
{$ENDIF}

{$IFDEF LINUX}

const
  ResStringTableLen = 16;

type
  ResStringTable = array [0..ResStringTableLen-1] of LongWord;

//function _printf(Format: PChar): Integer; cdecl; varargs;
//external libc name 'printf';

function LoadResString(ResStringRec: PResStringRec): string;
var
  Handle: TResourceHandle;
  Tab: ^ResStringTable;
  ResMod: HMODULE;
begin
  if ResStringRec = nil then Exit;
  ResMod := FindResourceHInstance(ResStringRec^.Module^);
  Handle := FindResource(ResMod,
       PChar(ResStringRec^.Identifier div ResStringTableLen),
       PChar(6));   // RT_STRING
  Tab := Pointer(LoadResource(ResMod, Handle));
  if Tab = nil then
    Result := ''
  else
  begin
    Result := PWideChar(PAnsiChar(Tab) + Tab[ResStringRec^.Identifier mod ResStringTableLen]);
  end;
end;
{$ENDIF LINUX}

{$IFDEF MACOSX}
function LoadResString(ResStringRec: PResStringRec): string;
begin
   Error(reMacNotImplemented);
end;
{$ENDIF MACOSX}

{$IFDEF LINUX}
{ The Win32 program loader sets up the first 64k of process address space
  with no read or write access, to help detect use of invalid pointers
  (whose integer value is 0..64k).  Linux doesn't do this.  Mac OS/X
  does allow this, and the linker will ensure that this is the case by
  reserving 64k at the start of the image.

  Parts of the Delphi RTL and IDE design environment
  rely on the notion that pointer values in the [0..64k] range are
  invalid pointers.  To accomodate this in Linux, we reserve the range
  at startup.  If the range is already allocated, we keep going anyway. }

var
  ZeroPageReserved: Boolean = False;

procedure ReserveZeroPage;
const
  PROT_NONE = 0;
  MAP_PRIVATE   = $02;
  MAP_FIXED     = $10;
  MAP_ANONYMOUS = $20;
var
  P: Pointer;
begin
  if IsLibrary then Exit;  // page reserve is app's job, not .so's

  if not ZeroPageReserved then
  begin
    P := mmap(nil, High(Word), PROT_NONE,
      MAP_ANONYMOUS or MAP_PRIVATE or MAP_FIXED, 0, 0);
    ZeroPageReserved := P = nil;
    if (Integer(P) <> -1) and (P <> nil) then  // we didn't get it
      munmap(P, High(Word));
  end;
end;

procedure ReleaseZeroPage;
begin
  if ZeroPageReserved then
  begin
    munmap(nil, High(Word) - 4096);
    ZeroPageReserved := False;
  end;
end;
{$ENDIF}

var
  xxNull: UCS4Char = 0;
  xxPNull: PUCS4Char = @xxNull;
function PUCS4Chars(const S: UCS4String): PUCS4Char;
begin
  if Length(S) > 0 then
	Result := @S[0]
  else
    Result := xxPNull;
end;

function WideStringToUCS4String(const S: WideString): UCS4String;
var
  I: Integer;
  CharCount: Integer;
begin
  CharCount := 0;
  SetLength(Result, Length(S) + 1);
  I := 0;
  while I < Length(S) do
  begin
    if ((S[I + 1] >= #$D800) and (S[I + 1] <= #$DFFF)) and (I + 1 < Length(S)) then
    begin
      Result[I] := UCS4Char((Cardinal(S[I + 1]) and $000003FF) shl 10 or (Cardinal(S[I + 2]) and $000003FF) + $00010000);
      Inc(I);
    end
    else
      Result[I] := UCS4Char(S[I + 1]);
    Inc(CharCount);
    Inc(I);
  end;
  Result[CharCount] := 0;
  SetLength(Result, CharCount + 1);
end;

function UCS4StringToWideString(const S: UCS4String): WideString;
var
  I: Integer;
  CharCount: Integer;
begin
  SetLength(Result, Length(S) * 2 - 1); //Maximum possible number of characters
  CharCount := 0;

  I := 0;
  while I < Length(S) - 1 do
  begin
    if S[I] >= $10000 then
    begin
      Inc(CharCount);
      Result[CharCount] := WideChar((((S[I] - $00010000) shr 10) and $000003FF) or $D800);
      Inc(CharCount);
      Result[CharCount] := WideChar(((S[I] - $00010000) and $000003FF)or $DC00);
    end
    else
    begin
      Inc(CharCount);
      Result[CharCount] := WideChar(S[I]);
    end;

    Inc(I);
  end;

  SetLength(Result, CharCount);
end;

{$IFDEF MSWINDOWS}
function GetACP: LongWord; stdcall; external 'kernel32.dll' name 'GetACP';

function LCIDToCodePage(ALcid: LongWord): Integer;
const
  CP_ACP = 0;                                // system default code page
  LOCALE_IDEFAULTANSICODEPAGE = $00001004;   // default ansi code page
var
  ResultCode: Integer;
  Buffer: array [0..6] of Char;
begin
  GetLocaleInfo(ALcid, LOCALE_IDEFAULTANSICODEPAGE, Buffer, Length(Buffer));
  Val(Buffer, Result, ResultCode);
  if ResultCode <> 0 then
    Result := CP_ACP;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
procedure SetMultiByteConversionCodePage(CodePage: Integer);
begin
  DefaultSystemCodePage := CodePage;
end;

function GetCPUCount: Integer;
var
  SysInfo: TSystemInfo;
begin
  GetSystemInfo(SysInfo);
  Result := SysInfo.dwNumberOfProcessors;
end;

{$ENDIF}

{$IFDEF MSWINDOWS}
procedure SetUtf8CompareLocale;
var
  OSVersion: Cardinal;
  MajorVersion: Cardinal;
  MinorVersion: Cardinal;
begin
  OSVersion := GetVersion;
  MajorVersion := OSVersion and $000000FF;
  MinorVersion := (OSVersion and $0000FF00) shr 8;

  if ((MajorVersion = 5) and (MinorVersion >= 1)) or
     (MajorVersion > 5) then
    UTF8CompareLocale := LOCALE_INVARIANT
  else
    UTF8CompareLocale := $0409;
end;

{$L delayhlp.obj}

procedure LocalAlloc; external 'kernel32.dll' name 'LocalAlloc';
procedure LocalFree; external 'kernel32.dll' name 'LocalFree';
procedure LoadLibraryA; external 'kernel32.dll' name 'LoadLibraryA';
procedure lstrcmpiA; external 'kernel32.dll' name 'lstrcmpiA';
function InterlockedExchangePtr(Dest: Pointer; Value: Pointer): Pointer; stdcall; external 'kernel32.dll' name 'InterlockedExchange';
procedure __delayLoadHelper; external;
procedure __FUnloadDelayLoadedDLL; external;
procedure ___pfnDliNotifyHook; external;
procedure ___pfnDliFailureHook; external;
procedure _InitDelayHelp; external;
procedure _ShutdownDelayHelp; external;

procedure _memcpy(var Dest; const Src; Count: Integer); cdecl;
begin
  Move(Src, Dest, Count);
end;

procedure _memset(var Dest; Value: Byte; Count: Integer); cdecl;
begin
  FillChar(Dest, Count, Value);
end;

function SetDliNotifyHook(HookProc: TDelayedLoadHook): TDelayedLoadHook;
begin
  Result := InterlockedExchangePtr(pfnDliNotifyHook, @HookProc);
end;

function DliNotifyHook: TDelayedLoadHook;
begin
  Result := InterlockedExchangePtr(pfnDliNotifyHook, PPointer(pfnDliNotifyHook)^);
end;

function SetDliFailureHook(HookProc: TDelayedLoadHook): TDelayedLoadHook;
begin
  Result := InterlockedExchangePtr(pfnDliFailureHook, @HookProc);
end;

function DliFailureHook: TDelayedLoadHook;
begin
  Result := InterlockedExchangePtr(pfnDliFailureHook, PPointer(pfnDliFailureHook)^);
end;

procedure _DelayLoadHelper;
asm
     JMP [delayLoadHelper]
end;

procedure UnloadDelayLoadedDLL(szDll: PAnsiChar);
asm
     JMP [UnloadDelayLoadedDLLPtr]
end;

{$ENDIF MSWINDOWS}

initialization
{$IFDEF MSWINDOWS}
  _InitDelayHelp;
  InitializeMemoryManager;
  InitializeLocaleData;
{$ENDIF}
{$IFDEF POSIX}
  _setlocale(LC_ALL, '');
{$ENDIF POSIX}

  FileMode := 2;

{$IFDEF MSWINDOWS}
  RaiseExceptionProc := @RaiseException;
  RTLUnwindProc := @RTLUnwind;
{$ENDIF MSWINDOWS}

{$IFDEF LINUX}
  FileAccessRights := S_IRUSR or S_IWUSR or S_IRGRP or S_IWGRP or S_IROTH or S_IWOTH;
  Test8086 := GetCPUType;
  IsConsole := True;
  FindResourceCache.ModuleHandle := LongWord(-1);
  ReserveZeroPage;
  CPUCount := 1;
{$ELSEIF defined(MSWINDOWS)}
  Test8086 := 2;
  CPUCount := GetCPUCount;
{$ELSEIF defined(MACOSX)}
  { MACOSXTODO: look into cpu type and cpu count APIs for OS/X }
  FileAccessRights := S_IRUSR or S_IWUSR or S_IRGRP or S_IWGRP or S_IROTH or S_IWOTH;
  Test8086 := 2;
  IsConsole := True;
//  FindResourceCache.ModuleHandle := LongWord(-1);
  CPUCount := 1;
{$IFEND LINUX et al}

  DispCallByIDProc := @_DispCallByIDError;

  _FpuInit();

  TTextRec(Input).Mode := fmClosed;
  TTextRec(Output).Mode := fmClosed;
  TTextRec(ErrOutput).Mode := fmClosed;

{$IFDEF MSWINDOWS}
  CmdLine := GetCommandLine;
  CmdShow := GetCmdShow;
  DefaultSystemCodePage := GetACP;
  DefaultUnicodeCodePage := CP_UTF16; // UTF16 - Do not pass to MultiByteToWideChar or WideCharToMultiByte
{$ENDIF MSWINDOWS}
  MainThreadID := GetCurrentThreadID;

{$IFDEF MSWINDOWS}
  SetUtf8CompareLocale;
{$ENDIF MSWINDOWS}

finalization
  Close(Input);
  Close(Output);
  Close(ErrOutput);
{$IFDEF LINUX}
  ReleaseZeroPage;
{$ENDIF LINUX}
{$IFDEF MSWINDOWS}
  FinalizeLocaleDate;
  {Uninitialize the default memory manager, and free all memory allocated by
   this memory manager.}
  FinalizeMemoryManager;
  _ShutdownDelayHelp;
{$ENDIF MSWINDOWS}
end.
