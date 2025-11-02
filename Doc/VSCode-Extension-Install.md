# Installing the SLIECE VS Code extension

The VS Code extension for SLIECE is located in the `vsCodeExtension/sliece` folder.

To package and install it, open a terminal in that folder and run the following commands:

```bash
cd vsCodeExtension/sliece
vsce package
code --install-extension sliece-0.0.1.vsix
```

Notes:
- `vsce` must be installed globally (via npm) if it's not already present: `npm install -g vsce`.
- The `code` command (Visual Studio Code's command-line interface) must be available in your PATH. If it isn't, open VS Code and enable it from the Command Palette: "Shell Command: Install 'code' command in PATH".