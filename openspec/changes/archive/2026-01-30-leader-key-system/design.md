## Context

### Current State

eme は Selection Mode と Insert Mode の2モードシステムを採用している。現在の問題点:

1. **Insert Mode からのアクセス制限**: Insert Mode では eme のコマンドにアクセスできない。Selection Mode に戻る必要がある
2. **特殊バッファの扱い**: dired や magit のような特殊バッファは `eme-excluded-modes` で eme から完全に除外されている。これにより eme の機能（コピー、検索など）も使用できない
3. **Emacs prefix コマンドへのアクセス**: Selection Mode では `C-c` や `C-x` prefix を使うには一時的に Insert Mode に入る必要がある

### Constraints

- cl-lib の使用禁止
- 外部ライブラリへの依存最小化
- Emacs 29.1+ 対象

### Stakeholders

- 既存 eme ユーザー（`eme-excluded-modes` 設定を持つ可能性）
- dired/magit/vterm などの特殊モードユーザー
- which-key ユーザー

## Goals / Non-Goals

**Goals:**

- どのモードからでも統一的に Emacs コマンドにアクセス可能にする
- 特殊バッファの既存キーバインドを維持しつつ、eme 機能を利用可能にする
- which-key がインストール済みならシームレスに連携する
- シンプルで覚えやすいキーマッピング

**Non-Goals:**

- Spacemacs/Doom の完全なリーダーキー体系の再現
- which-key を必須依存にすること
- 既存の Selection Mode キーバインドの大幅な変更（SPC 以外）
- 複雑な多段プレフィックス階層

## Decisions

### Decision 1: Leader Key の割り当て

**選択**: Selection Mode では `SPC`、Insert Mode では `M-SPC`

**理由**:
- `SPC` は Selection Mode で最もアクセスしやすいキー
- Insert Mode では `SPC` は通常入力に必要なため `M-SPC` を使用
- `M-SPC` は Emacs 標準の `just-one-space` だが、Leader としての利便性が優先

**代替案**:
- `C-SPC`: マーク設定と競合、使用頻度が高い
- `\`: Vim 互換だが eme の設計思想に合わない
- `;`: Selection Mode で他の用途（コメントトグル）に使用中

### Decision 2: デフォルト prefix

**選択**: Leader 起動直後は `C-c` prefix

**理由**:
- `C-c` はユーザー定義キーバインドの標準 prefix
- major-mode 固有コマンドも `C-c` 以下にある
- 最も使用頻度が高い

### Decision 3: Prefix 切り替え機構

**選択**:
- `x` → `C-x` prefix に切り替え
- `h` → `C-h` prefix に切り替え（ヘルプ）
- `u` → `C-u` (universal-argument) を実行

**理由**:
- 覚えやすい（x = C-x, h = help）
- 1ストロークで prefix 変更可能

### Decision 4: C- 修飾の動的付与

**選択**: Leader モード内で `SPC` を押すと次のキーに `C-` 修飾を付与

例: `SPC SPC w` → `C-c C-w`

**理由**:
- `C-c C-*` 形式のコマンドは多い（例: `C-c C-c` で確定）
- `SPC` の連続押しは直感的

### Decision 5: ユーザー定義キーマップ

**選択**: `m` キーで `eme-user-map` にアクセス

**理由**:
- `m` は "my" の意味で覚えやすい
- Selection Mode での `m`（delimiter）との区別は Leader モード内なので問題なし

### Decision 6: Insert Start Modes

**選択**: `eme-excluded-modes` を廃止し、`eme-insert-start-modes` を導入

**理由**:
- excluded では eme 機能を完全に使用不可になる
- Insert Start なら既存キーバインドを維持しつつ Leader Key で eme 機能にアクセス可能
- 特殊モードでも `M-SPC` で検索やコピーなどの eme 機能を利用できる

**デフォルト値**:
```elisp
(defcustom eme-insert-start-modes
  '(special-mode        ; base mode for read-only special buffers
    dired-mode
    magit-mode
    magit-status-mode
    magit-log-mode
    magit-diff-mode
    vterm-mode
    term-mode
    eshell-mode
    Info-mode
    help-mode
    compilation-mode
    grep-mode)
  "Modes that start in Insert Mode when eme is enabled.")
```

### Decision 7: which-key 連携

**選択**: which-key の存在を実行時に検出し、存在すれば自動連携

**理由**:
- which-key がなくても Leader Key は動作する
- インストール済みなら自動的にプレフィックスヒントを表示
- 明示的な設定不要

**実装**:
```elisp
(defun eme-leader--maybe-show-which-key (prefix)
  "Show which-key popup if available."
  (when (and (fboundp 'which-key--show-keymap)
             (boundp 'which-key-mode)
             which-key-mode)
    (which-key--show-keymap prefix (lookup-key global-map prefix))))
```

### Decision 8: ファイル構成

**新規ファイル**: `eme-leader.el`
- Leader Key システムの実装
- ユーザー定義キーマップ `eme-user-map`
- which-key 連携

**修正ファイル**:
- `eme-core.el`: `eme-excluded-modes` → `eme-insert-start-modes` 変更
- `eme-keybinds.el`: Leader Key バインド追加

### Decision 9: Leader モード状態管理

**選択**: バッファローカル変数 + transient keymap

```elisp
(defvar-local eme-leader--active nil
  "Non-nil when leader mode is active.")

(defvar-local eme-leader--prefix nil
  "Current prefix in leader mode. One of 'c-c, 'c-x, 'c-h.")

(defvar-local eme-leader--next-control nil
  "Non-nil to add C- modifier to next key.")
```

**理由**:
- バッファローカルで独立した状態管理
- transient keymap で一時的なキーバインドを実現
- `set-transient-map` を使用

## Risks / Trade-offs

### [Risk] eme-excluded-modes 利用者への影響
→ **Mitigation**: ドキュメントで移行手順を明記。`eme-excluded-modes` が設定されている場合に警告メッセージを表示し、`eme-insert-start-modes` への移行を促す。

### [Risk] SPC キーの上書き
→ **Mitigation**: Selection Mode では現在 `SPC` は未使用。ただし、ユーザーがカスタマイズしている可能性があるため、設定で Leader Key を変更可能にする（`eme-leader-key` カスタム変数）。

### [Risk] which-key 検出の信頼性
→ **Mitigation**: `fboundp` と `boundp` の両方でチェック。which-key がロード済みかつ有効な場合のみ連携。

### [Risk] M-SPC の既存動作との競合
→ **Mitigation**: Insert Mode では他のキーバインドをほぼ持たないため、`M-SPC` を Leader に割り当てても影響は限定的。ユーザーは `just-one-space` を `M-\` で代用可能。

### [Trade-off] シンプルさ vs 機能性
- 多段プレフィックス（`SPC x s` など）は実装しない
- 基本の3つの prefix（C-c, C-x, C-h）+ C- 修飾で十分な柔軟性を提供

## Migration Plan

### Phase 1: 実装

1. `eme-leader.el` の新規作成
2. `eme-core.el` の変数変更
3. `eme-keybinds.el` への Leader Key バインド追加
4. テストの追加

### Phase 2: 移行支援

1. `eme-excluded-modes` が設定されている場合の警告表示
2. 自動変換ヘルパー関数の提供（オプション）:
   ```elisp
   (defun eme-migrate-excluded-to-insert-start ()
     "Migrate eme-excluded-modes to eme-insert-start-modes."
     (interactive)
     (when eme-excluded-modes
       (setq eme-insert-start-modes
             (append eme-insert-start-modes eme-excluded-modes))
       (setq eme-excluded-modes nil)
       (message "Migrated %d modes to eme-insert-start-modes"
                (length eme-excluded-modes))))
   ```

### Rollback Strategy

1. `eme-leader.el` を削除
2. `eme-core.el` の変数を元に戻す
3. `eme-keybinds.el` から Leader Key バインドを削除

## Open Questions

1. **universal-argument の扱い**: `SPC u` で `C-u` を実行するが、その後のキー入力をどう扱うか？→ 現時点では `C-u` を実行して Leader モードを終了し、次のキー入力を待つ

2. **Leader Key 中の C-g**: Leader モードをキャンセルして Selection Mode に戻る（確定）

3. **派生モードの自動検出**: `special-mode` を `eme-insert-start-modes` に含めることで派生モードを自動的にカバーするか？→ `derived-mode-p` でチェックする方針
