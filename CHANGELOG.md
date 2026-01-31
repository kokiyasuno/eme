# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.1.0] - 2025-01-27

### Added

- Initial release
- Selection Mode with Emacs-style movement keys (n/p/f/b)
- Insert Mode with C-g/ESC to exit
- Match Mode for text objects (inner/around)
- Wrap, delete surround, replace surround operations
- Actions: delete, copy, yank, change, replace-char
- Comment toggle, format, indent operations
- Join lines, open line, duplicate operations
- Case toggle, undo/redo, sort/reverse lines
- Search integration with isearch
- Goto line, scroll, recenter commands
- Macro recording and playback
- Register operations (position, text)
- Customizable cursor types
- Mode-line indicators ([S]/[I])
- Mode transition hooks
- Tree-sitter support for text objects (optional)
- Comprehensive test suite with buttercup
- GitHub Actions CI configuration
