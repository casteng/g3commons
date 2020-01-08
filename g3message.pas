{
@abstract(Base message unit)

Base message class and message management classes

@author(George Bakhtadze (avagames@gmail.com))
}

unit g3message;
{$I g3config.inc}

interface

const
  // Message pool grow step
  MessagesCapacityStep = 16;
  // Initial capacity of messages pool in bytes
  MessagePoolInitialCapacity = 65536;
  MessagePoolMaxCapacity = 65536 * 256;

type
  // Message flag
  TMessageFlag = (// Message has been discarded. No need to forward it further.
                  mfInvalid,
                  // Message is directed to a particular recipient
                  mfRecipient,
                  // Message is a notification message from parent to immediate childs
                  mfChilds,
                  // Message is a broadcasted message from some item down through hierarchy
                  mfBroadcast,
                  // Message's destination is core handler
                  mfCore,
                  // Message has extended lifetime (10000 ticks)
                  mfDurable,
                  // Message is asyncronous
                  mfAsync);
  // Message flag set
  TMessageFlags = set of TMessageFlag;

  { @Abstract(Base class for all message classes)
    Messages are stored in specific pool (see @Link(TMessagePool)) to speed-up allocation and avoid memory leaks. <br>
    As a consequence, messages can be created in such way: <i>SomeObject.HandleMessage(TMessage.Create)</i> without risk of a memory leak. <br>
    TODO: Temporary restriction: Do not use in message classes types which need finalization (such as dynamic arrays or long strings) this will cause memory leaks. Use short strings instead. }
  TMessage = class(TObject)
  private
    FFlags: TMessageFlags;
  public
    function IsValid(): Boolean;
    // This method overridden to store messages in specific pool
    class function NewInstance: TObject; override;
    // If you erroneously deallocate a message manually the overridden implementation of this method will signal you
    procedure FreeInstance; override;
    // Call this method if you don't want the message to be broadcasted further
    procedure Invalidate;
    // Message flags
    property Flags: TMessageFlags read FFlags write FFlags;
  end;

  // Message class reference
  CMessage = class of TMessage;

  TSubSystem = class
  public
    procedure HandleMessage(const Msg: TMessage); virtual;
  end;

  // Message pool data structure
  TPool = record
    Store: Pointer;
    Size:  Cardinal;
  end;
  PPool = ^TPool;

  { @Abstract(Message pool class)
    The class implements memory management for all instances of @Link(TMessage) and its descendant classes }
  TMessagePool = class
  private
    CurrentPool, BackPool: PPool;
    FCapacity: Cardinal;
    procedure SetCapacity(ACapacity: Cardinal);
    procedure SwapPools;
    function Allocate(Size: Cardinal): Pointer;
  public
    constructor Create;
    destructor Destroy; override;

    // Begins message handling. Should be called once per main applicatin cycle
    procedure BeginHandle;
    // Ends message handling and clears messages. Should be called once per main applicatin cycle after <b>BeginHandle</b>
    procedure EndHandle;
  end;

  // Array of messages
  TMessages = array of TMessage;

  // Message handler delegate
  TMessageHandler = procedure(const Msg: TMessage) of object;

  { @Abstract(Asynchronous messages queue implementation)
    The class provides the possibility to handle asynchronous messages. <br>
    Message handlers can generate other asynchronous messages which will be handled during next handling cycle.
    If you use this class there is no need to call any methods of @Link(TMessagePool). }
  TMessageSubsystem = class
  private
    HandleStarted: Boolean;
    BackMessages, Messages:  TMessages;
    TotalMessages, TotalBackMessages, CurrentMessageIndex: Integer;
    procedure SwapPools;
  public
    { Locks current message queue. Should be called before message handling cycle. <br>
      All asynchronous messages generated during handling will be available during next handling cycle. <br>
      Calls @Link(TMessagePool).BeginHandle so application has no need to call it. }
    procedure BeginHandle;
    // Should be called after handling cycle. Calls @Link(TMessagePool).EndHandle so application has no need to call it
    procedure EndHandle;
    // Add an asynchronous message to the queue
    procedure Add(const Msg: TMessage);
    { Extracts a message from the queue if any, places it to <b>Msg</b> and returns @True if there was a message in queue.
      Otherwise returns @False and @nil in <b>Msg</b>. Should be called only between BeginHandle and EndHandle calls. }
    function ExtractMessage(out Msg: TMessage): Boolean;
  end;

  // Base class for notification messages
  TNotificationMessage = class(TMessage)
  end;

  // This message is sent to an object when it should reset its timer if any
  TSyncTimeMsg = class(TNotificationMessage)
  end;

  // Pause begin message
  TPauseMsg = class(TMessage)
  end;
  // Pause end message
  TResumeMsg = class(TMessage)
  end;
  // Progress report message
  TProgressMsg = class(TMessage)
  public
    // Progress indicator ranging from 0 to 1
    Progress: Single;
    constructor Create(AProgress: Single);
  end;

  // Base class for operating system messages
  TOSMessage = class(TMessage)
  end;

  // Indicates that application started to shut down
  TAppClosingMsg = class(TOSMessage)
  end;

  // Indicates that application has been activated
  TAppActivateMsg = class(TOSMessage)
  end;

  // Indicates than application has been deactivated
  TAppDeactivateMsg = class(TOSMessage)
  end;

  // Indicates than application's main window position has been changed
  TWindowMoveMsg = class(TOSMessage)
  public
    NewX, NewY: Single;
    // X, Y - new window position in screen coordinates
    constructor Create(X, Y: Single);
  end;

  // Indicates than application's main window size has been changed
  TWindowResizeMsg = class(TOSMessage)
  public
    OldWidth, OldHeight, NewWidth, NewHeight: Single;
    // <b>OldWidth, OldHeight</b> - old size of the window, <b>NewWidth, NewHeight</b> - new size
    constructor Create(AOldWidth, AOldHeight, ANewWidth, ANewHeight: Single);
  end;

  // Indicates than application's main window has been minimized
  TWindowMinimizeMsg = class(TOSMessage)
  end;

  // Indicates than OS request window to be repainted
  TWindowPaintMsg = class(TOSMessage)
  end;

  // If some data may be referenced by pointer and the pointer to the data has changed this message is broadcasted with new pointer
  TDataAdressChangeMsg = class(TNotificationMessage)
  public
    OldData, NewData: Pointer;
    DataReady: Boolean;
    // <b>AOldValue</b> - old pointer, <b>ANewValue</b> - new pointer to the data, <b>ADataReady</b> - determines wheter the data is ready to use
    constructor Create(AOldValue, ANewValue: Pointer; ADataReady: Boolean);
  end;

  // This message is broadcasted when some data has been modified
  TDataModifyMsg = class(TNotificationMessage)
  public
    // Pointer, identifying the data. usually it's the address of the data in memory
    Data: Pointer;
    // AData - a pointer, identifying the data. usually it's the address of the data in memory
    constructor Create(AData: Pointer);
  end;

var
  MessagePool: TMessagePool;

implementation

{$IFDEF G3_DEBUG}
uses g3Log;
{$ENDIF}

{$IF not Declared(PtrUInt)}
type
  {$IF Declared(NativeUInt)}
  PtrUInt = NativeUInt;
  {$ELSE}
  PtrUInt = Cardinal;
  {$IFEND}
  PPtrUInt = ^PtrUInt;
{$IFEND}

{ TMessage }

function TMessage.IsValid(): Boolean;
begin
  Result := not (mfInvalid in FFlags);
end;

class function TMessage.NewInstance: TObject;
begin
//  Result := InitInstance(MessagePool.Allocate(InstanceSize));
  Result := TObject(MessagePool.Allocate(InstanceSize));
  PPtrUInt(Result)^ := PtrUInt(Self);
end;

procedure TMessage.FreeInstance;
begin
  Assert(False, 'TMessage and descendants should not be freed manually');
end;

procedure TMessage.Invalidate;
begin
  Include(FFlags, mfInvalid);
end;

{ TWindowMoveMsg }

constructor TWindowMoveMsg.Create(X, Y: Single);
begin
  NewX := X; NewY := Y;
end;

{ TWindowResizeMsg }

constructor TWindowResizeMsg.Create(AOldWidth, AOldHeight, ANewWidth, ANewHeight: Single);
begin
  OldWidth  := AOldWidth;
  OldHeight := AOldHeight;
  NewWidth  := ANewWidth;
  NewHeight := ANewHeight;
end;

{ TDataAdressChangeMsg }

constructor TDataAdressChangeMsg.Create(AOldValue, ANewValue: Pointer; ADataReady: Boolean);
begin
  OldData   := AOldValue;
  NewData   := ANewValue;
  DataReady := ADataReady;
end;

{ TDataModifyMsg }

constructor TDataModifyMsg.Create(AData: Pointer);
begin
  Data := AData;
end;

{ TMessageSubsystem }

procedure TMessageSubsystem.SwapPools;
var t: TMessages;
begin
  t            := BackMessages;
  BackMessages := Messages;
  Messages       := t;
  t              := nil;

  TotalBackMessages := TotalMessages;
  TotalMessages := 0;
end;

procedure TMessageSubsystem.BeginHandle;
begin
  HandleStarted := True;
  SwapPools;
  CurrentMessageIndex := 0;
  MessagePool.BeginHandle;
end;

procedure TMessageSubsystem.EndHandle;
begin
  Assert(HandleStarted, 'TMessageSubsystem.EndHandle: Invalid call');
  HandleStarted := False;
  MessagePool.EndHandle;
end;

procedure TMessageSubsystem.Add(const Msg: TMessage);
begin
  if Length(Messages) <= TotalMessages then SetLength(Messages, Length(Messages) + MessagesCapacityStep);
  Messages[TotalMessages] := Msg;
  Inc(TotalMessages);
end;

function TMessageSubsystem.ExtractMessage(out Msg: TMessage): Boolean;
begin                                           // ToDo: Needs testing
  Assert(HandleStarted, 'TMessageSubsystem.ExtractMessage: Should be called only between BeginHandle and EndHandle pair');
  Msg := nil;
  if CurrentMessageIndex < TotalBackMessages then begin
    Msg := BackMessages[CurrentMessageIndex];
    Inc(CurrentMessageIndex);
  end;
  Result := Msg <> nil;
end;

{ TMessagePool }

procedure TMessagePool.SetCapacity(ACapacity: Cardinal);
begin
  FCapacity := ACapacity;
  ReAllocMem(CurrentPool^.Store, ACapacity);
  ReAllocMem(BackPool^.Store, ACapacity);
end;

procedure TMessagePool.SwapPools;
var Temp: Pointer;
begin
  Temp := BackPool;
  BackPool := CurrentPool;
  CurrentPool := Temp;
end;

constructor TMessagePool.Create;
begin
  New(CurrentPool);
  CurrentPool^.Store := nil;
  CurrentPool^.Size  := 0;
  New(BackPool);
  BackPool^.Store := nil;
  BackPool^.Size  := 0;
  SetCapacity(MessagePoolInitialCapacity);
end;

destructor TMessagePool.Destroy;
begin
  SetCapacity(0);
  Dispose(CurrentPool);
  Dispose(BackPool);
  inherited;
end;

function TMessagePool.Allocate(Size: Cardinal): Pointer;
var NewCapacity: Integer;
begin
  {$IFDEF G3_DEBUG}
  if CurrentPool^.Size + Size >= MessagePoolMaxCapacity then
    g3Log.Fatal('Message pool is full');
  {$ENDIF}
  Assert(CurrentPool^.Size + Size < MessagePoolMaxCapacity, 'Message pool is full');
  // Todo: Handle this situation
  if CurrentPool^.Size + Size > FCapacity then begin
    NewCapacity := FCapacity + MessagePoolInitialCapacity;
    if NewCapacity > MessagePoolMaxCapacity then NewCapacity := MessagePoolMaxCapacity;
    SetCapacity(NewCapacity);
  end;

  Result := Pointer(PtrUInt(CurrentPool^.Store) + CurrentPool^.Size);
  Inc(CurrentPool^.Size, Size);
end;

procedure TMessagePool.BeginHandle;
begin
  SwapPools;
end;

procedure TMessagePool.EndHandle;
begin
  BackPool^.Size := 0;
end;

{ TProgressMsg }

constructor TProgressMsg.Create(AProgress: Single);
begin
  Progress := AProgress;
end;

{ TSubSystem }

procedure TSubSystem.HandleMessage(const Msg: TMessage);
begin
// no action
end;

initialization
  MessagePool := TMessagePool.Create();
finalization
  MessagePool.Free;
  MessagePool := nil;
end.
