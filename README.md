# IPL Highlighting

VS Code syntax highlighting for IPL — a small, focused grammar to give IPL files clear, readable colors in the editor.

Marketplace: https://marketplace.visualstudio.com/items?itemName=I-had-a-bad-idea.IPL-Highlighting

## Features

- Syntax highlighting for IPL source files using a TextMate grammar.
- Lightweight and dependency-free — the grammar is included in this repository under `syntaxes/ipl.tmLanguage.json`.

## Installation

Install from the Visual Studio Marketplace (link above), or install the extension VSIX built from this repo.

## Usage

- Open an IPL file in VS Code with the extension installed.

## Development / Local testing

To run and test the extension locally:

1. Open this repository in Visual Studio Code.
2. Press F5 (Run Extension) to launch an Extension Development Host window with the grammar available.
3. Open an IPL file in the host window to verify highlighting.

If you add or update the grammar in `syntaxes/ipl.tmLanguage.json`, reload the Extension Development Host to see changes.

## Contributing

Contributions are welcome. Good first steps:

- Open an issue for feature requests or bugs.
- For changes to the grammar, edit `syntaxes/ipl.tmLanguage.json` and test using the Extension Development Host (F5).
- Send a pull request describing the change and a short test case (sample IPL snippet) that demonstrates the improvement.

## License

This project is licensed under the terms in the `LICENSE` file.

---

If anything is unclear or you'd like help improving the grammar or packaging an updated VSIX release, open an issue and I'll help.