<div align="center">
<a href="https://github.com/nerdyslacker/barev-im"><img src="barevim.png" width="120"/></a>
</div>
<h1 align="center">Barev IM [ընկեր]</h1>

<div align="center">

[![License](https://img.shields.io/github/license/nerdyslacker/barev-im)](https://github.com/nerdyslacker/barev-im/blob/main/LICENSE)
[![GitHub release](https://img.shields.io/github/release/nerdyslacker/barev-im)](https://github.com/nerdyslacker/barev-im/releases)
![GitHub All Releases](https://img.shields.io/github/downloads/nerdyslacker/barev-im/total)
[![GitHub stars](https://img.shields.io/github/stars/nerdyslacker/barev-im)](https://github.com/nerdyslacker/barev-im/stargazers)
[![GitHub forks](https://img.shields.io/github/forks/nerdyslacker/barev-im)](https://github.com/nerdyslacker/barev-im/fork)

<strong>A graphical user interface for the Barev protocol built on top of the barev-pascal library. (Trying to learn Pascal :D)</strong>

</div>

## Requirements

- Lazarus IDE (or Free Pascal Compiler with LCL)
- barev-pascal library (https://github.com/norayr/barev-pascal)
- Yggdrasil network connection

## Building

### Using Lazarus IDE

1. Open Lazarus IDE
2. Open the project file: `File -> Open -> barevim.lpr`
3. Add the Barev units to your project search path:
   - `Project -> Project Options -> Compiler Options -> Paths`
   - Add the path to the Barev library units
4. Build: `Run -> Build` (or press Shift+F9)
5. Run: `Run -> Run` (or press F9)

### Using Command Line

```bash
# Make sure the Barev units are in your unit search path
lazbuild barevim.lpr

# Or with fpc directly
fpc -Fu/path/to/barev/units barevim.lpr
```

### Getting Started

1. **Enter Your Details**
   - Fill in your nickname in the "My Nick" field
   - Enter your Yggdrasil IPv6 address in the "My IPv6" field
   - Set your listening port in the "My Port" field (default: 5299)
     - If running multiple instances on the same machine, use different ports (e.g., 5299, 5300)
     - If running in separate Docker containers, all can use 5299
   - Click "Connect" to start the client

2. **Add Buddies**
   - Click "Add Buddy" button
   - Enter buddy's nickname
   - Enter buddy's IPv6 address
   - Enter buddy's port (default: 5299)
   - The buddy will appear in the buddy list

3. **Remove Buddies**
   - Select a buddy from the list
   - Click "Remove Buddy" button
   - Confirm deletion
   - Buddy will be removed from list and saved configuration

4. **Start Chatting**
   - Double-click a buddy in the buddy list to connect
   - Type your message in the text field at the bottom
   - Press Enter or click "Send" to send the message
   - Messages appear in the chat window with timestamps

<picture>
    <img alt="screenshot" src="screenshot.png" />
</picture>

## Configuration and Persistence

The application automatically saves your settings and buddy list to a configuration file:

**Linux/Unix**: `~/.config/barevim/barev.ini`

### Configuration File Format

```ini
[User]
Nick=alice
IPv6=201:af82:9f2f:7809::1
Port=5299

[Window]
Left=100
Top=100
Width=800
Height=600

[Buddy_0]
Nick=bob
IPv6=201:7a74:aa1e:101a::a1
Port=5299

[Buddy_1]
Nick=charlie
IPv6=202:baad:cafe:1234::1
Port=5300
```

## Credits

[Norayr Chilingarian](https://github.com/norayr)
