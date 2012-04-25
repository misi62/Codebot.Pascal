
(********************************************************)
(*                                                      *)
(*  Private Distribution of Codebot Class Library       *)
(*  Version: 2.00.00                                    *)
(*  Date: March 2009                                    *)
(*                                                      *)
(********************************************************)

unit StrConst;

{$I CODEBOT.INC}

interface

resourcestring
  SInvalidGraphicFormat = 'Invalid graphic file format';
  SInvalidGraphicSize = 'Graphic size operation failed';
  SCouldNotLockBits = 'Could not lock bitmap bits';
  SRangeIndexError = 'Index outside of range bounds';
  SLauncherFileError = 'Cannot launch specified filename';
  SLauncherTerminateError = 'Cannot terminate application';
  SInvalidMode = 'Pipe does not support this operation';
  SNoStorageSpecified = 'No storage specified in call to open stream';
  SNotConnected = 'Pipe not connected';
  SStorageNotOpen = 'Storage not open';
  SStreamNotOpen = 'Stream not open';
  SCannotOpenStructure = 'Cannot open structure object';
  SMutexCreateError = 'Unable to create mutex';
  SMapppingCreateError = 'Unable to create file mapping';
  SViewMapError = 'Cannot map view of file';
  SFileOpenError = 'Cannot open file';
  SNotLocked = 'Data not locked';
  SElapsedTime = 'Cannot get elapsed time';
  STimerError = 'Cannot %s timer';
  SCannotFocusSprite = 'Cannot focus a disabled or invisible sprite';
  SNameNotUnique = 'Name "%s" is not unique';
  SSocketCreateError = 'Error creating socket';
  SWinSocketError = 'Windows socket error: %s (%d), on API ''%s''';
  SInvalidPropertyKind = 'Invalid property kind';
  SInvalidPropertyValue = 'Invalid property value';
  SUnexpectedToken = 'Unexpected token at position %d';
  SOpenFailed = 'Unable to open com port';
  SWriteFailed = 'WriteFile function failed';
  SReadFailed = 'ReadFile function failed';
  SInvalidAsync = 'Invalid Async parameter';
  SPurgeFailed = 'PurgeComm function failed';
  SAsyncCheck = 'Unable to get async status';
  SSetStateFailed = 'SetCommState function failed';
  STimeoutsFailed = 'SetCommTimeouts failed';
  SSetupComFailed = 'SetupComm function failed';
  SClearComFailed = 'ClearCommError function failed';
  SModemStatFailed = 'GetCommModemStatus function failed';
  SEscapeComFailed = 'EscapeCommFunction function failed';
  STransmitFailed = 'TransmitCommChar function failed';
  SSyncMeth = 'Cannot set SyncMethod while connected';
  SEnumPortsFailed = 'EnumPorts function failed';
  SStoreFailed = 'Failed to store settings';
  SLoadFailed = 'Failed to load settings';
  SRegFailed = 'Terminal link (un)registration failed';
  SLedStateFailed = 'Cannot change led state if com port is selected';
  SAbstractStructure = 'Abstract structure error';
  SNoParentStructure = 'No parent structure available';
  SInvalidStructureName = '"%s" is not a valid structure name';
  SDuplicateName = 'Duplicate names not allowed';
  SNoOpenStructure = 'Invalid file format';
  SCannotPerformOperation = 'Cannot perform operation';
  SNoConnection = 'Connection manager has not been initialized';
  SNoParentSpecified = 'No parent rule specified.';
  SSafeReadWriteFail = 'Safe stream read/write operation failed.';
  SInvalidStructureType = 'Invalid structure type.';
  SInvalidToken = 'Invalid token encountered at row %d column %d';
  SCantWriteStream = 'Can''t write to stream';
  SCantReadStream = 'Can''t read from stream';
  SInvalidStreamFormat = 'Invalid stream resource';
  SFunctionUndefined = 'The function ''%s'' is undefined';

implementation

end.
