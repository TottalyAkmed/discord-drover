# Discord Drover (Proxy Settings for Discord)

Discord Drover is a program that forces the Discord application for Windows to use a specified proxy server (HTTP or SOCKS5) for TCP connections (chat, updates). This may be necessary because the original Discord application lacks proxy settings, and the global system proxy is also not used.

Additionally, the program slightly modifies Discord's outgoing UDP traffic, which helps bypass some local restrictions on voice chats.

The program works locally at the specific process level (without drivers) and does not affect the operating system globally. This approach serves as an alternative to using a global VPN (such as TUN interfaces and others).

## Installation

The latest version of the program can be downloaded from the [latest release page](https://github.com/hdrover/discord-drover/releases/latest).

### Automatic Installation

For an easier setup, use the included installer `drover.exe`. Run the program and fill in the proxy settings, then click **Install** to automatically place the necessary files in the correct folder.

In regions like the UAE, where Discord works but voice chat is blocked, you can use **Direct mode** to bypass voice chat restrictions without a proxy.

To uninstall the program and remove all associated files, run `drover.exe` again and click **Uninstall**.

### Manual Installation

If you prefer manual installation, copy the `version.dll` and `drover.ini` files into the folder containing the `Discord.exe` file (not `Update.exe`). The proxy is specified in the `drover.ini` file under the `proxy` parameter.

### Example `drover.ini` Configuration:

```ini
[drover]
; Proxy can use http or socks5 protocols
proxy = http://127.0.0.1:1080

;use-nekobox-proxy = 1
;nekobox-proxy = http://127.0.0.1:2080
```

- **proxy**: Defines the main proxy server to use for Discord (HTTP or SOCKS5). If left empty, no proxy will be used, but UDP manipulation will still occur to bypass voice chat restrictions (same as Direct mode in the installer).
- **use-nekobox-proxy**: Enables the feature to detect if NekoBox is running and use a different proxy if found.
- **nekobox-proxy**: The proxy used when NekoBox is detected, typically `127.0.0.1:2080`.

## Features

- Forces Discord to use a specified proxy for TCP connections.
- Slight interference with UDP traffic for bypassing voice chat restrictions. In Direct mode, no proxy is used, only UDP manipulation is performed.
- Supports HTTP proxies with authentication (login and password).
- No drivers or system-level modifications are required.
- Works locally at the process level, offering an alternative to global VPN solutions.
- Supports Discord Canary and PTB versions in addition to the main version.
