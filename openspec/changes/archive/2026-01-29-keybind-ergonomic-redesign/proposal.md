## Why

現在のキーバインド設計には以下の問題がある：

1. **頻度と押しやすさの不一致**: `f` = 文字移動（使用頻度3%）、`F` = 単語移動（使用頻度22%）と、最も押しやすいキーに最も使わない操作が割り当てられている
2. **2ストローク原則の欠如**: `m i w`（inner word ）のような3ストローク以上の操作が多い
3. **選択モデルの一貫性欠如**: 移動コマンドによって選択の振る舞いが異なり、予測可能性が低い
4. **Vim からの不要な継承**: inner/around の区別は delimiter 系でのみ必要だが、全テキストオブジェクトに適用されている

Vim のキーストローク統計によると、単語移動（w/b ）が14%、行移動（j/k ）が13%で最頻出。文字移動は3%未満。人間工学に基づいた再設計が必要。

## What Changes

### 移動キーバインドの再配置（頻度ベース）

- **BREAKING** `f`/`b` = 文字移動 → 単語移動に変更
- **BREAKING** `F`/`B` = 単語移動 → 未割り当て（Phase 3 で jump 機能予定）
- `M-f`/`M-b` = sexp 移動（変更なし）
- `n`/`p` = 行、`N`/`P` = 段落、`M-n`/`M-p` = defun （順序維持）
- `a`/`e` = 行境界、`A`/`E` = 文境界、`M-a`/`M-e` = defun 境界（順序維持）

### 選択モデルの統一

- **BREAKING** 全移動コマンド = 選択（Kakoune/Helix モデル）
- `n`/`p` = 行選択（カラム位置保持）に戻す（前回 change で純粋移動にしたものを再変更）

### Thing システムの導入（delimiter 操作の効率化）

- 新キー `t` = thing prefix
- `t i` = inner （最近接 delimiter の内容を auto-detect 選択）
- `t b` = bounds （delimiter を含めて選択）
- `t` の繰り返しで外側に拡張（transient keymap ）
- `t i (` / `t b [` 等で明示指定（fallback 、3ストローク）

### expand-region の追加

- `v` = 段階的に選択を拡大（word → string → list → defun ）
- `V` = 選択を縮小

### inner/around モデルの廃止

- **BREAKING** Match Mode の `m i` / `m a` prefix を廃止
- 移動 = 選択で inner 相当の動作を実現
- delimiter 系のみ `t i` / `t b` で inner/bounds を区別
- `m` キーは surround 操作専用に変更（`m w` = wrap, `m d` = delete-surround, `m r` = replace-surround ）

### パッケージ名の変更

- **BREAKING** `emacs-modal-editing` → `eme` に改名
- `emacs-modal-editing-mode` → `eme-mode`
- `emacs-modal-editing-local-mode` → `eme-local-mode`
- ディレクトリ `emacs-modal-editing/` → `eme/`
- 全ファイル名の `emacs-modal-editing` prefix を `eme` に変更

## Capabilities

### New Capabilities

- `thing-selection`: delimiter の auto-detect 選択、inner/bounds 、transient keymap による拡張
- `expand-region`: 段階的な選択拡大・縮小機能

### Modified Capabilities

- `selection-movement`: f/b の単語移動化、全移動 = 選択モデル、n/p の行選択化
- `line-text-object`: Match Mode 廃止に伴い、`m a l` / `m i l` を削除（n/p が行選択を担当）

## Impact

### 変更対象ファイル

| ファイル | 変更内容 |
|---------|---------|
| `emacs-modal-editing/` → `eme/` | ディレクトリ名変更 |
| `emacs-modal-editing.el` → `eme.el` | パッケージエントリポイント改名 |
| `eme-selection.el` | f/b を単語移動に、n/p を行選択に再実装 |
| `eme-thing.el` (新規) | thing システムの実装（auto-detect, transient-map ） |
| `eme-expand.el` (新規) | expand-region の実装 |
| `eme-match.el` | inner/around map の廃止、surround 専用に簡素化 |
| `eme-keybinds.el` | 全キーバインドの再配置 |
| `test/*.el` | 全テストの書き換え |
| `README.md` | パッケージ名、キーバインド表の全面更新 |

### 破壊的変更

既存ユーザーへの影響が大きい。v0.1.0 リリース前のため許容。

- パッケージ名が `emacs-modal-editing` → `eme` に変更
- `emacs-modal-editing-mode` → `eme-mode` に変更
- `f`/`b`/`F`/`B` の意味が完全に変わる
- `m i`/`m a` prefix が廃止される（`m` は surround 専用に）
- `n`/`p` が純粋移動から行選択に戻る

### パフォーマンス

- thing の auto-detect は構文解析が必要だが、`syntax-ppss` を使用すれば十分高速
- transient keymap のオーバーヘッドは無視できるレベル
- expand-region は既存パッケージ（expand-region.el ）を参考に最適化

### ロールバック

`eme-mode` を無効化すれば元の Emacs キーバインドに戻る。パッケージのアンインストールで完全削除。

### 既存パッケージとの差別化

| 項目         | Evil     | Meow       | eme                     |
|--------------|----------|------------|-------------------------|
| 移動モデル   | Vim 互換 | Kakoune 式 | Kakoune 式 + 頻度最適化 |
| 2ストローク  | 一部3+   | 一部3+     | 原則2以下               |
| inner/around | Vim 式   | thing 統合 | delimiter 限定          |
| 設計思想     | Vim 再現 | Emacs 統合 | 人間工学優先            |

### MELPA 登録

v0.1.0 で機能を固め、v0.2.0 で MELPA 申請予定。本 change は v0.1.0 の一部。
