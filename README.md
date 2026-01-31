# eme

Modal editing for Emacs using Emacs keybindings (n/p/f/b) instead of Vim-style hjkl.

## Overview

eme provides a "selection → action" paradigm inspired by Kakoune and Helix, where movement creates selection, then actions operate on that selection. Unlike Evil, this package uses familiar Emacs keybindings.

## Requirements

- Emacs 29.1 or later
- No external dependencies (tree-sitter support is optional)

## Installation

### From source

```elisp
(add-to-list 'load-path "/path/to/eme/eme")
(require 'eme)
(eme-mode 1)
```

### With use-package (from source)

```elisp
(use-package eme
  :load-path "/path/to/eme/eme"
  :config
  (eme-mode 1))
```

## Modes

### Selection Mode (default)

Movement creates selection. Press `i` to enter Insert Mode.

Mode-line indicator: `[S]`

### Insert Mode

Standard Emacs editing. Press `C-g` or `ESC` to return to Selection Mode.

Mode-line indicator: `[I]`

## Keybindings

### Movement (Selection Mode)

All movement creates selection. If a region is already active (anchor set), movement extends the selection.

| Key | Command | Description |
|-----|---------|-------------|
| `f` | forward-word | Move forward one word |
| `b` | backward-word | Move backward one word |
| `M-f` | forward-sexp | Move forward one s-expression |
| `M-b` | backward-sexp | Move backward one s-expression |
| `n` | next-line | Move to next line (selects entire line) |
| `p` | previous-line | Move to previous line (selects entire line) |
| `N` | forward-paragraph | Move forward one paragraph |
| `P` | backward-paragraph | Move backward one paragraph |
| `M-n` | next-defun | Move to next defun |
| `M-p` | previous-defun | Move to previous defun |
| `a` | beginning-of-line | Move to beginning of line |
| `e` | end-of-line | Move to end of line |
| `A` | beginning-of-sentence | Move to beginning of sentence |
| `E` | end-of-sentence | Move to end of sentence |
| `M-a` | beginning-of-defun | Move to beginning of defun |
| `M-e` | end-of-defun | Move to end of defun |

Note: `F` and `B` are reserved for future jump features.

### Selection Expansion (v/V)

Expand or contract selection semantically.

| Key | Command | Description |
|-----|---------|-------------|
| `v` | expand-region | Expand selection (word → symbol → sexp-chain → string → list → defun) |
| `V` | contract-region | Contract selection (reverse of expand) |

Expansion follows semantic boundaries: word → symbol → sexp-chain → string content → string with quotes → list content → list with delimiters → defun. Use `C-u N v` to expand N times at once.

**Sexp-chain**: A symbol followed by function calls `()`, array access `[]`, or method chains `.method()` is expanded as a single unit.

```
test(arg).method()[0]
  ^
  v → test           (symbol)
  v → test(arg).method()[0]  (sexp-chain)
  v → containing structure...
```

### Thing Selection (t prefix)

Select text objects based on delimiters with auto-detection.

| Key | Command | Description |
|-----|---------|-------------|
| `t i` | thing-inner | Select inner of enclosing delimiter (auto-detected) |
| `t b` | thing-bounds | Select bounds including delimiters (auto-detected) |
| `t t` | thing-expand | Expand to next outer delimiter (repeat t) |
| `t (` | thing-inner-char | Select inner of specific delimiter |

**Auto-detection**: When you press `t i` or `t b`, eme automatically detects the nearest enclosing delimiter (parentheses, brackets, quotes, etc.) using Emacs's syntax-ppss. No need to specify which delimiter.

**Expansion**: After an initial selection, press `t` again to expand to the next outer delimiter. A hollow cursor indicates this transient state.

**Explicit specification**: Press `t` followed by a delimiter character (`(`, `[`, `{`, `"`, `'`, `` ` ``) to select that specific delimiter type.

Example workflow:
```
(defun foo ()
  (let ((x "hello|world")))  ; cursor at |
```
- `t i` → selects `hello|world` (string inner, auto-detected)
- `t` → selects `"hello|world"` (string bounds)
- `t` → selects `x "hello|world"` (list inner)
- `t` → selects `(x "hello|world")` (list bounds)

### Anchor

| Key | Command | Description |
|-----|---------|-------------|
| `.` | toggle-anchor | Toggle selection anchor |
| `x` | exchange-point-and-mark | Flip selection direction |

### Actions

| Key | Command | Description |
|-----|---------|-------------|
| `d` | delete | Delete selection (save to kill-ring) |
| `D` | delete-no-save | Delete selection (no kill-ring) |
| `w` | copy | Copy selection to kill-ring |
| `y` | yank | Paste from kill-ring |
| `c` | change | Delete and enter Insert Mode |
| `r` | replace-char | Replace selection with character |
| `;` | comment-toggle | Toggle comment |
| `=` | format | Format/indent selection |
| `>` | indent-increase | Increase indent |
| `<` | indent-decrease | Decrease indent |
| `j` | join-lines | Join lines |
| `o` | open-line-below | Open line below |
| `O` | open-line-above | Open line above |
| `+` | duplicate | Duplicate selection/line |
| `` ` `` | toggle-case | Toggle case |
| `u` | undo | Undo |
| `U` | redo | Redo |

### Delimiter Operations (m/M)

Context-aware delimiter selection and manipulation with 2-stroke design.

| Key | Command | Description |
|-----|---------|-------------|
| `m` | delimiter-inner | Select inner of nearest delimiter (auto-detect) |
| `M` | delimiter-bounds | Select bounds including delimiters |

**After `m` selection** (with delimiter context):

| Sequence | Action |
|----------|--------|
| `m` + same delimiter | Delete delimiter (2 strokes) |
| `m` + different delimiter | Change delimiter (2 strokes) |
| `m` + action key | Normal action on selection |
| `m` (repeat) | Expand to outer delimiter |

**Wrap mode** (selection exists without delimiter context):

| Sequence | Action |
|----------|--------|
| selection + `m` + delimiter | Wrap selection (3 strokes) |

Example:
```
(defun foo ()
  (let ((x "hello|world")))  ; cursor at |
```
- `m` → selects `hello|world` (string inner, context remembers `"`)
- `m "` → deletes quotes: `helloworld`
- Or `m (` → changes to parens: `(helloworld)`

Example wrap workflow:
- `f` (select word `hello`)
- `m "` → wraps: `"hello"`

Example expand workflow:
- `m` → selects string inner
- `m` → expands to list inner: `x "hello"`
- `m` → expands to outer list inner: `(x "hello")`

### Search

| Key | Command | Description |
|-----|---------|-------------|
| `/` | search-forward | Start incremental search |
| `M-/` | search-regexp | Start regexp search |
| `s` | search-next | Go to next match |
| `S` | search-previous | Go to previous match |

### Goto/View

| Key | Command | Description |
|-----|---------|-------------|
| `g` | goto-line | Go to line number |
| `,` | xref-pop | Return from definition jump |
| `C-v` | scroll-up | Scroll down (page forward) |
| `M-v` | scroll-down | Scroll up (page backward) |
| `l` | recenter | Recenter current line |

### Macros

| Key | Command | Description |
|-----|---------|-------------|
| `q` | macro-toggle-record | Start/stop macro recording |
| `Q` | macro-play | Play last macro |

### Registers (single quote prefix)

| Key | Command | Description |
|-----|---------|-------------|
| `' a` | register-jump | Jump to position in register a |
| `' a SPC` | register-point-to | Save position to register a |
| `' a w` | register-copy-to | Copy selection to register a |
| `' a y` | register-insert | Insert from register a |

### Mode Transition

| Key | Command | Description |
|-----|---------|-------------|
| `i` | enter-insert-mode | Enter Insert Mode |
| `C-g` | exit-insert-mode | Exit Insert Mode (from Insert) |
| `ESC` | exit-insert-mode | Exit Insert Mode (from Insert) |

## Customization

```elisp
;; Cursor types
(setq eme-selection-cursor-type 'box)  ; box, bar, hbar
(setq eme-insert-cursor-type 'bar)

;; Thing selection cursor (transient state)
(setq eme-thing-cursor-type 'hollow)

;; Delimiter characters recognized by m/M operations
;; Note: Action keys (d, w, c, etc.) are always treated as actions
(setq eme-delimiter-chars '(?\" ?\' ?\( ?\) ?\[ ?\] ?\{ ?\} ?\` ?\< ?\>))

;; Excluded modes (modal editing won't activate)
(add-to-list 'eme-excluded-modes 'shell-mode)
```

## Hooks

```elisp
;; Run code when entering Selection Mode
(add-hook 'eme-selection-mode-enter-hook
          (lambda () (message "Entered Selection Mode")))

;; Run code when entering Insert Mode
(add-hook 'eme-insert-mode-enter-hook
          (lambda () (message "Entered Insert Mode")))
```

## Tree-sitter Support

eme automatically uses tree-sitter when available (e.g., `js-ts-mode`, `python-ts-mode`). This provides more accurate AST-based selection for:

- **expand-region (v)**: Walks up the AST for precise expansion
- **delimiter operations (m/M)**: Uses AST for delimiter detection
- **thing selection (t)**: AST-aware inner/bounds selection

When tree-sitter is not available, eme falls back to syntax-based methods (`syntax-ppss`).

Example with tree-sitter (more granular expansion):
```
allStores.userStore.restoreCredentials()
          ^
v1: userStore                              (identifier)
v2: allStores.userStore                    (member_expression)
v3: allStores.userStore.restoreCredentials (member_expression)
v4: allStores.userStore.restoreCredentials() (call_expression)
```

## Philosophy

- **Emacs-native**: Uses familiar Emacs keybindings (n/p/f/b)
- **Selection first**: Movement creates selection, actions operate on it
- **Frequency-based**: Most common operations on easiest keys (f/b for words, not characters)
- **Simple**: No external dependencies, minimal code
- **Customizable**: All keybindings can be changed

## Migration from Previous Versions

If you were using an earlier version of eme (or emacs-modal-editing):

| Old | New | Notes |
|-----|-----|-------|
| `f`/`b` | `f`/`b` | Now moves by word (was character) |
| `F`/`B` | - | Reserved for future jump features |
| `v`/`V` | `C-v`/`M-v` | Scroll moved to match Emacs |
| - | `v`/`V` | New: expand/contract selection |
| `m i`/`m a` | `t i`/`t b` | Text objects moved to thing system |
| `m w` | selection + `m` + delimiter | Wrap now uses context-aware system |
| `m d` | `m` + same delimiter | Delete delimiter now 2 strokes |
| `m r` | `m` + different delimiter | Change delimiter now 2 strokes |
| - | `m`/`M` | New: inner/bounds delimiter selection |

## Comparison with Other Packages

| Feature | Evil | Meow | eme |
|---------|------|------|-----|
| Keybinding style | Vim | Custom | Emacs (n/p/f/b) |
| Paradigm | Action → Motion | Selection → Action | Selection → Action |
| hjkl dependency | Yes | Optional | No |
| Complexity | High | Medium | Low |

## License

GPL-3.0-or-later
