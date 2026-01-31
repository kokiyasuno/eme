## Context

Emacsのモーダル編集は Evil が事実上の標準だが、Vimのエミュレーションという性質上、複雑で重い。Meow や Xah-fly-keys は軽量だが、Emacsユーザーが慣れ親しんだキーバインド（n/p/f/b）とは異なる体系を採用している。

本パッケージは Emacs 29.1+ を対象とし、Emacs 組み込み機能を最大限活用することで、外部依存を最小化し、シンプルな設計を実現する。

### 制約

- cl-lib の使用禁止（Emacs 組み込みの代替を使用）
- 外部依存ゼロ（オプションで tree-sitter）
- パフォーマンスクリティカルな箇所を最小化

## Goals / Non-Goals

**Goals:**

- Emacsユーザーが直感的に使えるモーダル編集を提供する
- 選択 → アクションのパラダイムを Emacs キーバインドで実現する
- シンプルで保守しやすいコードベースを維持する
- 完全にカスタマイズ可能なキーバインドを提供する

**Non-Goals:**

- Vim/Evil との互換性（意図的に異なる設計）
- リーダーキー機能（Phase 2 で対応）
- avy風ジャンプ機能（Phase 3 で自前実装）
- マルチセレクション（Phase 4 で対応）
- すべてのエディタ機能の再実装（Emacs 標準機能を活用）

## Decisions

### 1. ファイル構成

```
emacs-modal-editing/
├── emacs-modal-editing.el      ; メインエントリポイント、グローバルモード定義
├── eme-core.el                 ; モード切り替え基盤、状態管理
├── eme-selection.el            ; Selection Mode の移動・選択コマンド
├── eme-match.el                ; Match Mode (Text Objects, Wrap)
├── eme-actions.el              ; アクションコマンド (d/w/y/c等)
├── eme-search.el               ; 検索統合 (isearch連携)
├── eme-goto-view.el            ; Goto/View/Scroll 操作
├── eme-macro-register.el       ; マクロ・レジスタ操作
├── eme-keybinds.el             ; デフォルトキーバインド定義
└── test/
    └── emacs-modal-editing-test.el
```

**理由**: 機能ごとにファイルを分離することで、保守性とテスト容易性を確保。`eme-` プレフィックスで名前空間を確保。

**代替案**: 単一ファイルに全て含める → 却下（可読性・保守性の低下）

### 2. モード実装方式

`define-minor-mode` を使用し、キーマップを切り替える:

```elisp
(defvar eme-selection-mode-map (make-sparse-keymap))
(defvar eme-insert-mode-map (make-sparse-keymap))
(defvar eme-match-mode-map (make-sparse-keymap))

(define-minor-mode eme-selection-mode
  "Selection mode for modal editing."
  :keymap eme-selection-mode-map
  ...)
```

**理由**: Emacs 標準のマイナーモード機構を活用。`emulation-mode-map-alists` を使用して他のマイナーモードより優先度を高くする。

**代替案**:
- `overriding-local-map` → 却下（他のモードを完全に上書きしてしまう）
- 独自のキーマップ切り替え機構 → 却下（車輪の再発明）

### 3. 選択の実装

Emacs 標準の `mark` と `region` を使用:

```elisp
;; 移動時に選択を設定
(defun eme-forward-char ()
  "Move forward and select."
  (interactive)
  (unless (region-active-p)
    (push-mark (point) t t))
  (forward-char 1))
```

**理由**: Emacs の既存機能との完全な互換性。`kill-ring`、`region-active-p` 等がそのまま使える。

**代替案**: オーバーレイで独自選択を実装 → 却下（複雑化、既存機能との非互換）

### 4. アンカー（拡張）の実装

`mark` をそのまま活用:

```elisp
(defun eme-toggle-anchor ()
  "Toggle selection anchor."
  (interactive)
  (if (region-active-p)
      (deactivate-mark)
    (push-mark (point) t t)))
```

**理由**: Emacs の `C-SPC` と同じ概念。シンプルで予測可能。

### 5. Match Mode の実装

プレフィックスキーマップとして実装:

```elisp
(defvar eme-match-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "i" 'eme-match-inner-prefix)
    (define-key map "a" 'eme-match-around-prefix)
    (define-key map "w" 'eme-match-wrap-prefix)
    (define-key map "d" 'eme-match-delete-surround)
    (define-key map "r" 'eme-match-replace-surround)
    map))

(define-key eme-selection-mode-map "m" eme-match-mode-map)
```

**理由**: `m` を押すと Match Mode のキーマップがアクティブになり、次のキーで操作を決定。Emacs 標準のプレフィックスキー方式。

### 6. Text Objects の実装

各オブジェクトタイプに対して bounds 関数を定義:

```elisp
(defun eme-bounds-of-word ()
  "Return bounds of word at point."
  (bounds-of-thing-at-point 'word))

(defun eme-bounds-of-inner-paren ()
  "Return bounds inside parentheses."
  (when-let ((start (nth 1 (syntax-ppss))))
    (cons (1+ start)
          (save-excursion
            (goto-char start)
            (forward-sexp)
            (1- (point))))))
```

**理由**: `bounds-of-thing-at-point` と `syntax-ppss` を活用。tree-sitter が利用可能な場合は構文ノードを使用。

### 7. cl-lib 代替

| cl-lib 関数 | 代替実装 |
|-------------|---------|
| `cl-loop` | `while` + `dolist` |
| `cl-defstruct` | `defvar` + alist または plist |
| `cl-case` | `pcase` (Emacs 24.1+) |
| `cl-letf` | `advice-add` / `unwind-protect` |
| `cl-remove-if` | `seq-filter` (Emacs 25.1+) |

### 8. カスタマイズ変数

命名規則: `eme-{category}-{name}`

```elisp
(defgroup emacs-modal-editing nil
  "Modal editing for Emacs."
  :group 'editing
  :prefix "eme-")

(defcustom eme-selection-cursor-type 'box
  "Cursor type in selection mode."
  :type '(choice (const box) (const bar) (const hbar))
  :group 'emacs-modal-editing)

(defcustom eme-insert-cursor-type 'bar
  "Cursor type in insert mode."
  :type '(choice (const box) (const bar) (const hbar))
  :group 'emacs-modal-editing)
```

### 9. 他パッケージとの統合ポイント

| パッケージ | 統合方法 |
|-----------|---------|
| which-key | キーマップにヒント文字列を設定（自動対応）|
| tree-sitter | オプショナル、`treesit-*` 関数で Text Objects 拡張 |
| mode-line | `eme-mode-line-indicator` でモード表示 |

### 10. Hook と Advice の使用箇所

```elisp
;; Hook: モード切り替え時
(defvar eme-selection-mode-enter-hook nil)
(defvar eme-selection-mode-exit-hook nil)
(defvar eme-insert-mode-enter-hook nil)
(defvar eme-insert-mode-exit-hook nil)

;; Advice: なし（使用を避ける）
;; 理由: デバッグ困難、他パッケージとの衝突リスク
```

## Risks / Trade-offs

### [R1] Emacs 標準キーとの衝突

一部のキー（`v`, `g` 等）は Emacs 標準で異なる意味を持つ。

→ **緩和策**: Selection Mode でのみ有効。Insert Mode では Emacs 標準が使える。ユーザーはキーバインドをカスタマイズ可能。

### [R2] 学習コスト

新しいパラダイム（選択 → アクション）は Vim ユーザーには馴染みがない。

→ **緩和策**: チュートリアル・ドキュメントを充実させる。Emacs ユーザーには n/p/f/b が自然なため学習コストは低い。

### [R3] tree-sitter 非対応環境

Emacs 29 未満または tree-sitter 未設定の環境では構文ベース Text Objects が使えない。

→ **緩和策**: 正規表現ベースのフォールバックを提供。基本的な Text Objects（word, paren, quote）は syntax-ppss で対応可能。

## Open Questions

1. **モードラインインジケータのデザイン**: どのような表示が最適か？（例: `[S]` / `[I]` / アイコン）
2. **repeat-mode との統合**: Emacs 28+ の repeat-mode をサポートすべきか？
3. **minibuffer での挙動**: minibuffer 内では Insert Mode に自動切り替えすべきか？
