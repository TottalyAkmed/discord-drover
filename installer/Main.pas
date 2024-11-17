unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, System.Win.Registry, System.RegularExpressions,
  System.IOUtils, Options, TlHelp32, Vcl.Menus, ShellApi;

type
  TVersion = array [0 .. 3] of integer;

  TfrmMain = class(TForm)
    pType: TPanel;
    rbHttp: TRadioButton;
    rbSocks: TRadioButton;
    btnInstall: TButton;
    btnUninstall: TButton;
    eHost: TEdit;
    ePort: TEdit;
    lHost: TLabel;
    lPort: TLabel;
    MainMenu: TMainMenu;
    miAbout: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnInstallClick(Sender: TObject);
    procedure btnUninstallClick(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
  private
    currentProcessDir: string;
    messageCaption: PChar;

    function FindMostSuitableOptionsPath: string;
    procedure FindDiscordBaseDirs(list: TStringList);
    procedure FindDiscordDirs(list: TStringList);
    function GetNewestDiscordDir(list: TStringList): string;
    function IsDiscordRunning: boolean;
    function ShowDiscordRunningMessage: boolean;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
var
  optPath: string;
  opt: TDroverOptions;
  proxyValue: TProxyValue;
begin
  messageCaption := PChar(Application.Title);
  currentProcessDir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));

  optPath := FindMostSuitableOptionsPath;

  if optPath <> '' then
  begin
    opt := LoadOptions(optPath);
  end
  else
  begin
    opt := Default (TDroverOptions);
    opt.proxy := '';
  end;

  proxyValue.ParseFromString(opt.proxy);

  if proxyValue.isSpecified then
  begin
    eHost.Text := proxyValue.host;
    ePort.Text := IntToStr(proxyValue.port);
    rbHttp.Checked := proxyValue.isHttp;
    rbSocks.Checked := proxyValue.isSocks5;
  end;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  self.ActiveControl := btnInstall;
end;

procedure TfrmMain.miAboutClick(Sender: TObject);
begin
  ShellExecute(0, 'open', 'https://github.com/hdrover/discord-drover', nil, nil, SW_SHOWNORMAL);
end;

procedure TfrmMain.btnInstallClick(Sender: TObject);
var
  dirs, errors: TStringList;
  dir, dllPath, s: string;
  TaskDialog: TTaskDialog;
  opt: TDroverOptions;
  proto, host: string;
  port: integer;
begin
  if ShowDiscordRunningMessage then
    exit;

  dllPath := currentProcessDir + DLL_FILENAME;
  if not FileExists(dllPath) then
  begin
    Application.MessageBox(PChar(Format('The file ''%s'' is missing.', [DLL_FILENAME])), messageCaption, MB_ICONERROR);
    exit;
  end;

  proto := '';
  if rbHttp.Checked then
    proto := 'http';
  if rbSocks.Checked then
    proto := 'socks5';

  host := Trim(eHost.Text);
  port := StrToIntDef(Trim(ePort.Text), 0);

  if proto = '' then
  begin
    Application.MessageBox('Protocol is not specified.', messageCaption, MB_ICONERROR);
    exit;
  end;
  if host = '' then
  begin
    Application.MessageBox('Invalid host specified.', messageCaption, MB_ICONERROR);
    exit;
  end;
  if (port < 1) or (port > 65535) then
  begin
    Application.MessageBox('Invalid port specified.', messageCaption, MB_ICONERROR);
    exit;
  end;

  opt.proxy := Format('%s://%s:%d', [proto, host, port]);

  dirs := TStringList.Create;
  errors := TStringList.Create;
  try
    FindDiscordDirs(dirs);
    if dirs.Count = 0 then
    begin
      Application.MessageBox('The Discord folder was not found.', messageCaption, MB_ICONERROR);
      exit;
    end;

    s := currentProcessDir + OPTIONS_FILENAME;
    if not SaveOptions(s, opt) then
      errors.Add(s);

    for dir in dirs do
    begin
      s := dir + OPTIONS_FILENAME;
      if not SaveOptions(s, opt) then
        errors.Add(s);

      s := dir + DLL_FILENAME;
      if not CopyFile(PChar(dllPath), PChar(s), false) then
        errors.Add(s);
    end;

    if errors.Count > 0 then
    begin
      TaskDialog := TTaskDialog.Create(nil);
      try
        TaskDialog.Caption := messageCaption;
        TaskDialog.Title := 'Installation error';
        TaskDialog.Text := 'Some files could not be written.';
        TaskDialog.ExpandedText := Trim(errors.Text);
        TaskDialog.CommonButtons := [tcbClose];
        TaskDialog.Flags := [tfExpandFooterArea];
        TaskDialog.MainIcon := tdiError;
        TaskDialog.Execute;
      finally
        TaskDialog.Free;
      end;
    end
    else
    begin
      Application.MessageBox('Installation complete!', messageCaption, MB_ICONINFORMATION);
    end;
  finally
    dirs.Free;
    errors.Free;
  end;
end;

procedure TfrmMain.btnUninstallClick(Sender: TObject);
const
  FILENAMES: array [0 .. 1] of string = (OPTIONS_FILENAME, DLL_FILENAME);
var
  dirs, errors: TStringList;
  dir, filename, s: string;
  TaskDialog: TTaskDialog;
begin
  if ShowDiscordRunningMessage then
    exit;

  dirs := TStringList.Create;
  errors := TStringList.Create;
  try
    FindDiscordDirs(dirs);
    for dir in dirs do
    begin
      for filename in FILENAMES do
      begin
        s := dir + filename;
        if FileExists(s) and (not DeleteFile(s)) then
          errors.Add(s);
      end;
    end;

    if errors.Count > 0 then
    begin
      TaskDialog := TTaskDialog.Create(nil);
      try
        TaskDialog.Caption := messageCaption;
        TaskDialog.Title := 'Uninstallation error';
        TaskDialog.Text := 'Some files could not be deleted.';
        TaskDialog.ExpandedText := Trim(errors.Text);
        TaskDialog.CommonButtons := [tcbClose];
        TaskDialog.Flags := [tfExpandFooterArea];
        TaskDialog.MainIcon := tdiError;
        TaskDialog.Execute;
      finally
        TaskDialog.Free;
      end;
    end
    else
    begin
      Application.MessageBox('Uninstallation complete. All files have been successfully removed.', messageCaption,
        MB_ICONINFORMATION);
    end;
  finally
    dirs.Free;
    errors.Free;
  end;
end;

function TfrmMain.FindMostSuitableOptionsPath: string;
var
  dirs: TStringList;
  s, dir: string;
begin
  dirs := TStringList.Create;
  try
    FindDiscordDirs(dirs);
    dir := GetNewestDiscordDir(dirs);
  finally
    dirs.Free;
  end;

  if dir <> '' then
  begin
    s := dir + OPTIONS_FILENAME;
    if FileExists(s) then
      exit(s);
  end;

  s := currentProcessDir + OPTIONS_FILENAME;
  if FileExists(s) then
    exit(s);

  result := '';
end;

procedure TfrmMain.FindDiscordBaseDirs(list: TStringList);
var
  reg: TRegistry;
  match: TMatch;
  s, installLoc: string;
begin
  reg := TRegistry.Create(KEY_QUERY_VALUE);
  try
    reg.RootKey := HKEY_CURRENT_USER;

    if reg.OpenKeyReadOnly('Software\Microsoft\Windows\CurrentVersion\Uninstall\Discord') then
    begin
      if reg.ValueExists('InstallLocation') then
      begin
        s := reg.ReadString('InstallLocation');
        if s <> '' then
        begin
          s := IncludeTrailingPathDelimiter(s);
          if DirectoryExists(s) then
          begin
            installLoc := s;
            list.Add(s);
          end;
        end;
      end;
      reg.CloseKey;
    end;

    if reg.OpenKeyReadOnly('Software\Classes\Discord\shell\open\command') then
    begin
      if reg.ValueExists('') then
      begin
        s := reg.ReadString('');
        if s <> '' then
        begin
          match := TRegEx.match(s, '\A"(.+\\)app-');
          if match.Success then
          begin
            s := match.Groups[1].Value;
            if (not SameText(s, installLoc)) and DirectoryExists(s) then
            begin
              list.Add(s);
            end;
          end;
        end;
      end;
      reg.CloseKey;
    end;
  finally
    reg.Free;
  end;
end;

procedure TfrmMain.FindDiscordDirs(list: TStringList);
var
  baseDirs: TStringList;
  subfolders: TArray<string>;
  s, subfolder, baseDir: string;
begin
  baseDirs := TStringList.Create;
  try
    FindDiscordBaseDirs(baseDirs);
    for baseDir in baseDirs do
    begin
      if TDirectory.Exists(baseDir) then
      begin
        subfolders := TDirectory.GetDirectories(baseDir, 'app-*', TSearchOption.soTopDirectoryOnly);
        for subfolder in subfolders do
        begin
          s := IncludeTrailingPathDelimiter(subfolder);
          if FileExists(s + 'Discord.exe') then
          begin
            list.Add(s);
          end;
        end;
      end;
    end;
  finally
    baseDirs.Free;
  end;
end;

function TfrmMain.GetNewestDiscordDir(list: TStringList): string;
var
  i, partsLen: integer;
  dir: string;
  match: TMatch;
  maxVer, curVer: TVersion;
  parts: TArray<string>;
begin
  if list.Count = 0 then
    exit('');

  result := list[0];
  maxVer := Default (TVersion);

  for dir in list do
  begin
    match := TRegEx.match(dir, 'app-([\d.]+)');
    if not match.Success then
      continue;
    parts := match.Groups[1].Value.Split(['.']);
    partsLen := Length(parts);
    for i := 0 to High(curVer) do
    begin
      if i < partsLen then
        curVer[i] := StrToIntDef(parts[i], 0)
      else
        curVer[i] := 0;
    end;

    for i := 0 to High(curVer) do
    begin
      if curVer[i] <> maxVer[i] then
      begin
        if curVer[i] > maxVer[i] then
        begin
          maxVer := curVer;
          result := dir;
        end;
        break;
      end;
    end;
  end;
end;

function TfrmMain.IsDiscordRunning: boolean;
var
  snapshot: THandle;
  processEntry: TProcessEntry32;
begin
  result := false;
  snapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if snapshot = INVALID_HANDLE_VALUE then
    exit;

  try
    processEntry.dwSize := SizeOf(processEntry);
    if Process32First(snapshot, processEntry) then
    begin
      repeat
        if SameText(ExtractFileName(processEntry.szExeFile), 'Discord.exe') then
        begin
          result := true;
          break;
        end;
      until not Process32Next(snapshot, processEntry);
    end;
  finally
    CloseHandle(snapshot);
  end;
end;

function TfrmMain.ShowDiscordRunningMessage: boolean;
begin
  if IsDiscordRunning then
  begin
    result := true;
    Application.MessageBox('Please exit Discord before proceeding.', messageCaption, MB_ICONERROR);
  end
  else
  begin
    result := false;
  end;
end;

end.
