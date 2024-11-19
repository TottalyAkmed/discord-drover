library drover;

uses
  System.SysUtils,
  Winapi.Windows,
  DDetours,
  PsAPI,
  TlHelp32,
  WinSock,
  WinSock2,
  IniFiles,
  System.RegularExpressions,
  System.NetEncoding,
  SocketManager,
  Options;

var
  RealGetFileVersionInfoA: function(lptstrFilename: LPSTR; dwHandle, dwLen: DWORD; lpData: Pointer): bool; stdcall;
  RealGetFileVersionInfoW: function(lptstrFilename: LPWSTR; dwHandle, dwLen: DWORD; lpData: Pointer): bool; stdcall;
  RealGetFileVersionInfoSizeA: function(lptstrFilename: LPSTR; var lpdwHandle: DWORD): DWORD; stdcall;
  RealGetFileVersionInfoSizeW: function(lptstrFilename: LPWSTR; var lpdwHandle: DWORD): DWORD; stdcall;
  RealVerFindFileA: function(uFlags: DWORD; szFileName, szWinDir, szAppDir, szCurDir: LPSTR; var lpuCurDirLen: UINT;
    szDestDir: LPSTR; var lpuDestDirLen: UINT): DWORD; stdcall;
  RealVerFindFileW: function(uFlags: DWORD; szFileName, szWinDir, szAppDir, szCurDir: LPWSTR; var lpuCurDirLen: UINT;
    szDestDir: LPWSTR; var lpuDestDirLen: UINT): DWORD; stdcall;
  RealVerInstallFileA: function(uFlags: DWORD; szSrcFileName, szDestFileName, szSrcDir, szDestDir, szCurDir,
    szTmpFile: LPSTR; var lpuTmpFileLen: UINT): DWORD; stdcall;
  RealVerInstallFileW: function(uFlags: DWORD; szSrcFileName, szDestFileName, szSrcDir, szDestDir, szCurDir,
    szTmpFile: LPWSTR; var lpuTmpFileLen: UINT): DWORD; stdcall;
  RealVerLanguageNameA: function(wLang: DWORD; szLang: LPSTR; nSize: DWORD): DWORD; stdcall;
  RealVerLanguageNameW: function(wLang: DWORD; szLang: LPWSTR; nSize: DWORD): DWORD; stdcall;
  RealVerQueryValueA: function(pBlock: Pointer; lpSubBlock: LPSTR; var lplpBuffer: Pointer; var puLen: UINT)
    : bool; stdcall;
  RealVerQueryValueW: function(pBlock: Pointer; lpSubBlock: LPWSTR; var lplpBuffer: Pointer; var puLen: UINT)
    : bool; stdcall;

  RealGetEnvironmentVariableW: function(lpName: LPCWSTR; lpBuffer: LPWSTR; nSize: DWORD): DWORD; stdcall;
  RealCreateProcessW: function(lpApplicationName: LPCWSTR; lpCommandLine: LPWSTR;
    lpProcessAttributes, lpThreadAttributes: PSecurityAttributes; bInheritHandles: bool; dwCreationFlags: DWORD;
    lpEnvironment: Pointer; lpCurrentDirectory: LPCWSTR; const lpStartupInfo: TStartupInfoW;
    var lpProcessInformation: TProcessInformation): bool; stdcall;
  RealGetCommandLineW: function: LPWSTR; stdcall;

  RealSocket: function(af, type_, protocol: integer): TSocket; stdcall;
  RealWSASocket: function(af, type_, protocol: integer; lpProtocolInfo: LPWSAPROTOCOL_INFO; g: GROUP; dwFlags: DWORD)
    : TSocket; stdcall;
  RealWSASend: function(s: TSocket; lpBuffers: LPWSABUF; dwBufferCount: DWORD; lpNumberOfBytesSent: PDWORD;
    dwFlags: DWORD; lpOverlapped: LPWSAOVERLAPPED; lpCompletionRoutine: LPWSAOVERLAPPED_COMPLETION_ROUTINE)
    : integer; stdcall;
  RealWSASendTo: function(s: TSocket; lpBuffers: LPWSABUF; dwBufferCount: DWORD; lpNumberOfBytesSent: LPDWORD;
    dwFlags: DWORD; const lpTo: TSockAddr; iTolen: integer; lpOverlapped: LPWSAOVERLAPPED;
    lpCompletionRoutine: LPWSAOVERLAPPED_COMPLETION_ROUTINE): integer; stdcall;
  RealSend: function(s: TSocket; const buf; len, flags: integer): integer; stdcall;
  RealRecv: function(s: TSocket; var buf; len, flags: integer): integer; stdcall;

  currentProcessDir: string;
  sockManager: TSocketManager;
  droverOptions: TDroverOptions;
  proxyValue: TProxyValue;

function MyGetFileVersionInfoA(lptstrFilename: LPSTR; dwHandle, dwLen: DWORD; lpData: Pointer): bool; stdcall;
begin
  result := RealGetFileVersionInfoA(lptstrFilename, dwHandle, dwLen, lpData);
end;

function MyGetFileVersionInfoW(lptstrFilename: LPWSTR; dwHandle, dwLen: DWORD; lpData: Pointer): bool; stdcall;
begin
  result := RealGetFileVersionInfoW(lptstrFilename, dwHandle, dwLen, lpData);
end;

function MyGetFileVersionInfoSizeA(lptstrFilename: LPSTR; var lpdwHandle: DWORD): DWORD; stdcall;
begin
  result := RealGetFileVersionInfoSizeA(lptstrFilename, lpdwHandle);
end;

function MyGetFileVersionInfoSizeW(lptstrFilename: LPWSTR; var lpdwHandle: DWORD): DWORD; stdcall;
begin
  result := RealGetFileVersionInfoSizeW(lptstrFilename, lpdwHandle);
end;

function MyVerFindFileA(uFlags: DWORD; szFileName, szWinDir, szAppDir, szCurDir: LPSTR; var lpuCurDirLen: UINT;
  szDestDir: LPSTR; var lpuDestDirLen: UINT): DWORD; stdcall;
begin
  result := RealVerFindFileA(uFlags, szFileName, szWinDir, szAppDir, szCurDir, lpuCurDirLen, szDestDir, lpuDestDirLen);
end;

function MyVerFindFileW(uFlags: DWORD; szFileName, szWinDir, szAppDir, szCurDir: LPWSTR; var lpuCurDirLen: UINT;
  szDestDir: LPWSTR; var lpuDestDirLen: UINT): DWORD; stdcall;
begin
  result := RealVerFindFileW(uFlags, szFileName, szWinDir, szAppDir, szCurDir, lpuCurDirLen, szDestDir, lpuDestDirLen);
end;

function MyVerInstallFileA(uFlags: DWORD; szSrcFileName, szDestFileName, szSrcDir, szDestDir, szCurDir,
  szTmpFile: LPSTR; var lpuTmpFileLen: UINT): DWORD; stdcall;
begin
  result := RealVerInstallFileA(uFlags, szSrcFileName, szDestFileName, szSrcDir, szDestDir, szCurDir, szTmpFile,
    lpuTmpFileLen);
end;

function MyVerInstallFileW(uFlags: DWORD; szSrcFileName, szDestFileName, szSrcDir, szDestDir, szCurDir,
  szTmpFile: LPWSTR; var lpuTmpFileLen: UINT): DWORD; stdcall;
begin
  result := RealVerInstallFileW(uFlags, szSrcFileName, szDestFileName, szSrcDir, szDestDir, szCurDir, szTmpFile,
    lpuTmpFileLen);
end;

function MyVerLanguageNameA(wLang: DWORD; szLang: LPSTR; nSize: DWORD): DWORD; stdcall;
begin
  result := RealVerLanguageNameA(wLang, szLang, nSize);
end;

function MyVerLanguageNameW(wLang: DWORD; szLang: LPWSTR; nSize: DWORD): DWORD; stdcall;
begin
  result := RealVerLanguageNameW(wLang, szLang, nSize);
end;

function MyVerQueryValueA(pBlock: Pointer; lpSubBlock: LPSTR; var lplpBuffer: Pointer; var puLen: UINT): bool; stdcall;
begin
  result := RealVerQueryValueA(pBlock, lpSubBlock, lplpBuffer, puLen);
end;

function MyVerQueryValueW(pBlock: Pointer; lpSubBlock: LPWSTR; var lplpBuffer: Pointer; var puLen: UINT): bool; stdcall;
begin
  result := RealVerQueryValueW(pBlock, lpSubBlock, lplpBuffer, puLen);
end;

function MyGetEnvironmentVariableW(lpName: LPCWSTR; lpBuffer: LPWSTR; nSize: DWORD): DWORD; stdcall;
var
  s: string;
  newValue: string;
begin
  if proxyValue.isSpecified then
  begin
    s := lpName;
    if (Pos('http_proxy', s) > 0) or (Pos('HTTP_PROXY', s) > 0) or (Pos('https_proxy', s) > 0) or
      (Pos('HTTPS_PROXY', s) > 0) then
    begin
      newValue := proxyValue.FormatToHttpEnv;
      StringToWideChar(newValue, lpBuffer, nSize);
      result := Length(newValue);
      exit;
    end;
  end;

  result := RealGetEnvironmentVariableW(lpName, lpBuffer, nSize);
end;

procedure CopyFilesToNewVersionFolderIfNeeded(lpApplicationName: LPCWSTR);
var
  launchingDir: string;
  srcOptionsPath, srcDllPath, dstOptionsPath, dstDllPath: string;
begin
  if lpApplicationName = nil then
    exit;

  if not SameText(ExtractFileName(lpApplicationName), 'Discord.exe') then
    exit;

  if not SameText(ExtractFileName(ParamStr(0)), 'Discord.exe') then
    exit;

  launchingDir := IncludeTrailingPathDelimiter(ExtractFilePath(lpApplicationName));

  srcOptionsPath := currentProcessDir + OPTIONS_FILENAME;
  srcDllPath := currentProcessDir + DLL_FILENAME;
  dstOptionsPath := launchingDir + OPTIONS_FILENAME;
  dstDllPath := launchingDir + DLL_FILENAME;

  if FileExists(launchingDir + 'Discord.exe') and FileExists(srcOptionsPath) and FileExists(srcDllPath) and
    not FileExists(dstOptionsPath) and not FileExists(dstDllPath) then
  begin
    CopyFile(PChar(srcOptionsPath), PChar(dstOptionsPath), true);
    CopyFile(PChar(srcDllPath), PChar(dstDllPath), true);
  end;
end;

function MyCreateProcessW(lpApplicationName: LPCWSTR; lpCommandLine: LPWSTR;
  lpProcessAttributes, lpThreadAttributes: PSecurityAttributes; bInheritHandles: bool; dwCreationFlags: DWORD;
  lpEnvironment: Pointer; lpCurrentDirectory: LPCWSTR; const lpStartupInfo: TStartupInfoW;
  var lpProcessInformation: TProcessInformation): bool; stdcall;
begin
  CopyFilesToNewVersionFolderIfNeeded(lpApplicationName);

  result := RealCreateProcessW(lpApplicationName, lpCommandLine, lpProcessAttributes, lpThreadAttributes,
    bInheritHandles, dwCreationFlags, lpEnvironment, lpCurrentDirectory, lpStartupInfo, lpProcessInformation);
end;

function MyGetCommandLineW: LPWSTR; stdcall;
var
  s: string;
begin
  s := RealGetCommandLineW;
  if proxyValue.isSpecified then
  begin
    if SameText(ExtractFileName(ParamStr(0)), 'Discord.exe') then
      s := s + ' --proxy-server=' + proxyValue.FormatToChromeProxy;
  end;
  result := PChar(s);
end;

function MySocket(af, type_, protocol: integer): TSocket; stdcall;
begin
  result := RealSocket(af, type_, protocol);
  sockManager.Add(result, type_, protocol);
end;

function MyWSASocket(af, type_, protocol: integer; lpProtocolInfo: LPWSAPROTOCOL_INFO; g: GROUP; dwFlags: DWORD)
  : TSocket; stdcall;
begin
  result := RealWSASocket(af, type_, protocol, lpProtocolInfo, g, dwFlags);
  sockManager.Add(result, type_, protocol);
end;

function AddHttpProxyAuthorizationHeader(socketManagerItem: TSocketManagerItem; lpBuffers: LPWSABUF;
  dwBufferCount: DWORD; lpNumberOfBytesSent: PDWORD; dwFlags: DWORD; lpOverlapped: LPWSAOVERLAPPED;
  lpCompletionRoutine: LPWSAOVERLAPPED_COMPLETION_ROUTINE): boolean;
var
  pck, injectedData, filler: RawByteString;
  uaStartPos, uaEndPos, uaLen, fillerLen: integer;
begin
  result := false;

  if (not proxyValue.isSpecified) or (not proxyValue.isHttp) or (not proxyValue.isAuth) or (not socketManagerItem.isTcp)
  then
    exit;

  if (dwBufferCount <> 1) or (lpBuffers.len < 1) then
    exit;

  SetLength(pck, lpBuffers.len);
  Move(lpBuffers.buf^, pck[1], lpBuffers.len);

  if Pos(RawByteString(#13#10 + 'Proxy-Authorization: '), pck) > 0 then
    exit;

  uaStartPos := Pos(RawByteString('User-Agent:'), pck);
  if uaStartPos < 1 then
    exit;

  uaEndPos := Pos(RawByteString(#13#10), pck, uaStartPos);
  if uaEndPos < 1 then
    exit;

  uaLen := uaEndPos - uaStartPos;

  injectedData := 'Proxy-Authorization: Basic ' +
    RawByteString(TNetEncoding.Base64.EncodeBytesToString(BytesOf(RawByteString(proxyValue.login + ':' +
    proxyValue.password))));

  fillerLen := uaLen - Length(injectedData);
  if fillerLen < 6 then
    exit;

  filler := #13#10 + 'X: ' + RawByteString(StringOfChar('X', fillerLen - 5));
  injectedData := injectedData + filler;
  if Length(injectedData) <> uaLen then
    exit;

  Move(injectedData[1], pck[uaStartPos], uaLen);
  Move(pck[1], lpBuffers.buf^, lpBuffers.len);

  result := true;
end;

function MyWSASend(sock: TSocket; lpBuffers: LPWSABUF; dwBufferCount: DWORD; lpNumberOfBytesSent: PDWORD;
  dwFlags: DWORD; lpOverlapped: LPWSAOVERLAPPED; lpCompletionRoutine: LPWSAOVERLAPPED_COMPLETION_ROUTINE)
  : integer; stdcall;
var
  sockManagerItem: TSocketManagerItem;
begin
  if sockManager.IsFirstSend(sock, sockManagerItem) then
  begin
    AddHttpProxyAuthorizationHeader(sockManagerItem, lpBuffers, dwBufferCount, lpNumberOfBytesSent, dwFlags,
      lpOverlapped, lpCompletionRoutine);
  end;

  result := RealWSASend(sock, lpBuffers, dwBufferCount, lpNumberOfBytesSent, dwFlags, lpOverlapped,
    lpCompletionRoutine);
end;

function MyWSASendTo(sock: TSocket; lpBuffers: LPWSABUF; dwBufferCount: DWORD; lpNumberOfBytesSent: LPDWORD;
  dwFlags: DWORD; const lpTo: TSockAddr; iTolen: integer; lpOverlapped: LPWSAOVERLAPPED;
  lpCompletionRoutine: LPWSAOVERLAPPED_COMPLETION_ROUTINE): integer; stdcall;
var
  payload: byte;
  sockManagerItem: TSocketManagerItem;
begin
  if sockManager.IsFirstSend(sock, sockManagerItem) then
  begin
    if sockManagerItem.isUdp and (lpBuffers.len = 74) then
    begin
      payload := 0;
      sendto(sock, Pointer(@payload)^, 1, 0, @lpTo, iTolen);
      payload := 1;
      sendto(sock, Pointer(@payload)^, 1, 0, @lpTo, iTolen);
      Sleep(50);
    end;
  end;

  result := RealWSASendTo(sock, lpBuffers, dwBufferCount, lpNumberOfBytesSent, dwFlags, lpTo, iTolen, lpOverlapped,
    lpCompletionRoutine);
end;

function ConvertHttpToSocks5(socketManagerItem: TSocketManagerItem; const buf; len, flags: integer): boolean;
var
  s, targetHost: RawByteString;
  targetPort: word;
  fdSet: TFDSet;
  tv: TTimeVal;
  i: integer;
  match: TMatch;
  sock: TSocket;
begin
  result := false;

  if (not proxyValue.isSpecified) or (not proxyValue.isSocks5) or (not socketManagerItem.isTcp) then
    exit;

  i := 8;
  if len < i then
    exit;
  SetLength(s, i);
  Move(buf, s[1], i);
  if s <> 'CONNECT ' then
    exit;

  SetLength(s, len);
  Move(buf, s[1], len);
  match := TRegEx.match(string(s), '\ACONNECT ([a-z\d.-]+):(\d+)', [roIgnoreCase]);
  if not match.Success then
    exit;
  targetHost := RawByteString(match.Groups[1].Value);
  targetPort := StrToIntDef(match.Groups[2].Value, 0);

  sock := socketManagerItem.sock;

  s := #$05#$01#$00;
  i := Length(s);
  if RealSend(sock, s[1], i, flags) <> i then
    exit;

  FD_ZERO(fdSet);
  _FD_SET(sock, fdSet);
  tv.tv_sec := 10;
  tv.tv_usec := 0;

  if select(0, @fdSet, nil, nil, @tv) < 1 then
    exit;
  if not FD_ISSET(sock, fdSet) then
    exit;

  i := 2;
  SetLength(s, i);
  if RealRecv(sock, s[1], i, 0) <> i then
    exit;

  if s <> #$05#$00 then
    exit;

  s := #$05#$01#$00#$03 + RawByteString(AnsiChar(Length(targetHost))) + targetHost +
    RawByteString(AnsiChar(Hi(targetPort))) + RawByteString(AnsiChar(Lo(targetPort)));
  i := Length(s);
  if RealSend(sock, s[1], i, flags) <> i then
    exit;

  sockManager.SetFakeHttpProxyFlag(sock);

  result := true;
end;

function MySend(sock: TSocket; const buf; len, flags: integer): integer; stdcall;
var
  sockManagerItem: TSocketManagerItem;
begin
  if sockManager.IsFirstSend(sock, sockManagerItem) then
  begin
    if ConvertHttpToSocks5(sockManagerItem, buf, len, flags) then
      exit(len);
  end;

  result := RealSend(sock, buf, len, flags);
end;

function MyRecv(sock: TSocket; var buf; len, flags: integer): integer; stdcall;
var
  s: RawByteString;
  i: integer;
begin
  result := RealRecv(sock, buf, len, flags);

  if (result > 0) and sockManager.ResetFakeHttpProxyFlag(sock) then
  begin
    if result >= 10 then
    begin
      // Potential issue: real server data may mix with the SOCKS5 response
      SetLength(s, result);
      Move(buf, s[1], result);
      if Copy(s, 1, 3) = #$05#$00#$00 then
      begin
        s := 'HTTP/1.1 200 Connection Established' + #13#10 + #13#10;
        i := Length(s);
        if i <= len then
        begin
          Move(s[1], buf, i);
          exit(i);
        end;
      end;
    end;
  end;
end;

function GetSystemFolder: string;
var
  s: string;
begin
  SetLength(s, MAX_PATH);
  GetSystemDirectory(PChar(s), MAX_PATH);
  result := IncludeTrailingPathDelimiter(PChar(s));
end;

procedure LoadOriginalVersionDll;
var
  hOriginal: THandle;
begin
  hOriginal := LoadLibrary(PChar(GetSystemFolder + 'version.dll'));
  if hOriginal = 0 then
    raise Exception.Create('Error.');

  @RealGetFileVersionInfoA := GetProcAddress(hOriginal, 'GetFileVersionInfoA');
  @RealGetFileVersionInfoW := GetProcAddress(hOriginal, 'GetFileVersionInfoW');
  @RealGetFileVersionInfoSizeA := GetProcAddress(hOriginal, 'GetFileVersionInfoSizeA');
  @RealGetFileVersionInfoSizeW := GetProcAddress(hOriginal, 'GetFileVersionInfoSizeW');
  @RealVerFindFileA := GetProcAddress(hOriginal, 'VerFindFileA');
  @RealVerFindFileW := GetProcAddress(hOriginal, 'VerFindFileW');
  @RealVerInstallFileA := GetProcAddress(hOriginal, 'VerInstallFileA');
  @RealVerInstallFileW := GetProcAddress(hOriginal, 'VerInstallFileW');
  @RealVerLanguageNameA := GetProcAddress(hOriginal, 'VerLanguageNameA');
  @RealVerLanguageNameW := GetProcAddress(hOriginal, 'VerLanguageNameW');
  @RealVerQueryValueA := GetProcAddress(hOriginal, 'VerQueryValueA');
  @RealVerQueryValueW := GetProcAddress(hOriginal, 'VerQueryValueW');
end;

function IsNekoBoxExists: bool;
var
  hSnapshot: THandle;
  pe32: TProcessEntry32;
  processName: string;
begin
  result := false;
  hSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if hSnapshot = INVALID_HANDLE_VALUE then
    exit;
  try
    pe32.dwSize := SizeOf(TProcessEntry32);

    if Process32First(hSnapshot, pe32) then
    begin
      repeat
        processName := LowerCase(StrPas(pe32.szExeFile));
        if (Pos('nekobox', processName) > 0) or (Pos('nekoray', processName) > 0) then
        begin
          result := true;
          exit;
        end;

      until not Process32Next(hSnapshot, pe32);
    end;
  finally
    CloseHandle(hSnapshot);
  end;
end;

exports
  MyGetFileVersionInfoA name 'GetFileVersionInfoA',
  MyGetFileVersionInfoW name 'GetFileVersionInfoW',
  MyGetFileVersionInfoSizeA name 'GetFileVersionInfoSizeA',
  MyGetFileVersionInfoSizeW name 'GetFileVersionInfoSizeW',
  MyVerFindFileA name 'VerFindFileA',
  MyVerFindFileW name 'VerFindFileW',
  MyVerInstallFileA name 'VerInstallFileA',
  MyVerInstallFileW name 'VerInstallFileW',
  MyVerLanguageNameA name 'VerLanguageNameA',
  MyVerLanguageNameW name 'VerLanguageNameW',
  MyVerQueryValueA name 'VerQueryValueA',
  MyVerQueryValueW name 'VerQueryValueW';

begin
  currentProcessDir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  sockManager := TSocketManager.Create;

  droverOptions := LoadOptions(currentProcessDir + OPTIONS_FILENAME);

  if droverOptions.useNekoboxProxy and IsNekoBoxExists then
    proxyValue.ParseFromString(droverOptions.nekoboxProxy)
  else
    proxyValue.ParseFromString(droverOptions.proxy);

  LoadOriginalVersionDll;

  RealGetEnvironmentVariableW := InterceptCreate(@GetEnvironmentVariableW, @MyGetEnvironmentVariableW, nil);
  RealCreateProcessW := InterceptCreate(@CreateProcessW, @MyCreateProcessW, nil);
  RealGetCommandLineW := InterceptCreate(@GetCommandLineW, @MyGetCommandLineW, nil);

  RealSocket := InterceptCreate(@socket, @MySocket, nil);
  RealWSASocket := InterceptCreate(@WSASocket, @MyWSASocket, nil);
  RealWSASend := InterceptCreate(@WSASend, @MyWSASend, nil);
  RealWSASendTo := InterceptCreate(@WSASendTo, @MyWSASendTo, nil);
  RealSend := InterceptCreate(@send, @MySend, nil);
  RealRecv := InterceptCreate(@recv, @MyRecv, nil);

end.
