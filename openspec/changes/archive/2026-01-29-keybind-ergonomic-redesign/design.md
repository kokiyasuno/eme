## Context

### 現状

- パッケージ名: `emacs-modal-editing`（ディレクトリ `emacs-modal-editing/`）
- 移動モデル: コマンドによって選択の振る舞いが異なる（n/p は純粋移動、f/b は fresh selection）
- キー配置: `f`/`b` = 文字移動、`F`/`B` = 単語移動（頻度と押しやすさが逆転）
- テキストオブジェクト: `m i`/`m a` prefix による inner/around（3ストローク以上）

### 制約

- cl-lib 使用禁止
- 外部ライブラリへの依存なし
- Emacs 29.1+ 対応
- 既存の `emulation-mode-map-alists` による優先度管理は維持

## Goals / Non-Goals

**Goals:**

- 全移動コマンドを統一的な選択モデルに統合する（Kakoune/Helix 式）
- 使用頻度の高い操作を押しやすいキーに配置する
- ほとんどの編集操作を 2 ストロークで完了できるようにする
- delimiter 操作を効率化する thing システムを導入する
- パッケージ名を `eme` に簡素化する

**Non-Goals:**

- Vim 互換性の維持
- 文字単位移動（`f`/`b`）の維持（単語移動に変更）
- inner/around の全テキストオブジェクトへの適用（delimiter 限定）
- Phase 3 の jump 機能の実装（`F`/`B` は未割り当てとする）

## Decisions

### D1: f/b を単語移動に変更し、文字移動を廃止

**選択:** `f`/`b` = 単語移動、`F`/`B` = 未割り当て、文字移動は提供しない

**理由:** Vim のキーストローク統計によると、単語移動が 14%、文字移動は 3% 未満。最も押しやすいキー（小文字）に最も使う操作を配置する。文字移動は Insert Mode の `C-f`/`C-b` で代用可能。

**代替案:**
- `f`/`b` = 文字、`F`/`B` = 単語（現状維持）: 頻度と押しやすさの不一致が解消されない
- `f`/`b` = 単語、`F`/`B` = 文字: 文字移動の需要が低いため Shift を使う価値がない

**実装:**
```elisp
(defun eme-forward-word ()
  "Move forward one word and select."
  (interactive)
  (eme--with-fresh-selection
   (forward-word 1)))
```

### D2: n/p を行選択（カラム保持）に変更

**選択:** `n`/`p` = 行選択（`line-move` + カラム保持 + fresh selection）

**理由:** 前回の change で n/p を純粋移動にしたが、「全移動 = 選択」モデルとの一貫性が失われた。Helix も `j`/`k` は選択をリセットする（幅 1 の選択 = 実質的に行を選択）。行選択はカラム保持が自然。

**代替案:**
- 純粋移動（前回の実装）: 「移動 = 選択」モデルとの一貫性がない
- 斜め選択（Kakoune 式）: 実用価値がないと検証済み

**実装:**
```elisp
(defun eme-next-line ()
  "Move to next line and select (column preserved)."
  (interactive)
  (let ((col (current-column)))
    (eme--with-fresh-selection
     (forward-line 1)
     (move-to-column col))))
```

### D3: thing システムの導入（t prefix + transient keymap）

**選択:** `t` = thing prefix、`t i` = inner、`t b` = bounds、`t` 繰り返しで拡張

**理由:**
- delimiter 操作（括弧、クォート内の選択）を 2 ストロークで実現
- auto-detect により delimiter の種類を指定する必要がない
- transient keymap により `t i t t` で外側に拡張可能

**代替案:**
- `m i`/`m a` 維持（現状）: 3 ストローク以上で 2 ストローク原則に反する
- `i`/`I` 直接（Insert と衝突）: `i` は Insert Mode への遷移に使用中
- expand-region のみ: delimiter の inner/bounds の区別ができない

**実装:**
```elisp
(defvar eme-thing-map
  (let ((map (make-sparse-keymap)))
    (define-key map "i" #'eme-thing-inner)
    (define-key map "b" #'eme-thing-bounds)
    map)
  "Keymap for thing selection.")

(defun eme-thing-inner ()
  "Select inner of nearest delimiter."
  (interactive)
  (eme--select-nearest-inner)
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map "t" #'eme-thing-expand)
     map)
   t))
```

### D4: auto-detect に syntax-ppss を使用

**選択:** `syntax-ppss` で現在のカーソル位置が括弧/文字列内かを判定

**理由:** Emacs 組み込みの構文解析関数で、追加依存なし。高速（キャッシュあり）。括弧と文字列の両方を検出可能。

**代替案:**
- tree-sitter: より正確だが、tree-sitter 未対応のモードで動作しない
- 独自パーサー: 車輪の再発明、メンテナンスコスト高

**実装:**
```elisp
(defun eme--detect-enclosing-delimiter ()
  "Detect the nearest enclosing delimiter at point.
Returns (TYPE START END) or nil."
  (let ((ppss (syntax-ppss)))
    (cond
     ;; In string
     ((nth 3 ppss)
      (let ((start (nth 8 ppss)))
        (list 'string start (save-excursion
                              (goto-char start)
                              (forward-sexp 1)
                              (point)))))
     ;; In paren/bracket
     ((nth 1 ppss)
      (let ((start (nth 1 ppss)))
        (list 'paren start (save-excursion
                             (goto-char start)
                             (forward-sexp 1)
                             (point)))))
     (t nil))))
```

### D5: expand-region を v/V に割り当て

**選択:** `v` = expand、`V` = contract

**理由:** Helix の select mode (`v`) と類似。押しやすい位置。mnemonic は弱いが頻度ベースの原則に合致。

**代替案:**
- `+`/`-`: 正規表現風だが Shift が必要
- 別のキー: `v` より押しやすいキーがない

**実装:** expand-region.el を参考に、semantic な拡張順序（word → symbol → string → list → defun）を実装。

### D6: m を surround 専用に変更

**選択:** `m` = surround prefix（`m w` = wrap, `m d` = delete, `m r` = replace）

**理由:** inner/around を thing システムに移行した後、`m` prefix の残りは surround 操作のみ。`s` は検索に使用中のため `m` を維持。

### D7: パッケージ名を eme に変更

**選択:** `emacs-modal-editing` → `eme`

**理由:**
- 内部 prefix として既に `eme-` を全関数に使用
- 短くて覚えやすい
- ディレクトリ名、ファイル名、モード名すべてで一貫

**変更対象:**
- ディレクトリ: `emacs-modal-editing/` → `eme/`
- エントリポイント: `emacs-modal-editing.el` → `eme.el`
- グローバルモード: `emacs-modal-editing-mode` → `eme-mode`
- ローカルモード: `emacs-modal-editing-local-mode` → `eme-local-mode`

## Risks / Trade-offs

**[文字移動の廃止]** → 文字単位の精密な移動ができなくなる。ミティゲーション: Insert Mode の `C-f`/`C-b` で代用可能。Phase 3 の jump 機能で補完予定。

**[n/p の行選択化による副作用]** → 前回純粋移動にした経緯がある。ミティゲーション: 全移動 = 選択モデルを徹底することで一貫性を確保。Helix も同様のモデル。

**[syntax-ppss の限界]** → 正規表現文字列内の括弧など、構文的に曖昧なケースで誤検出の可能性。ミティゲーション: 明示指定（`t i (` 等）でフォールバック可能。

**[transient keymap の学習コスト]** → `t i t t` の操作は直感的でない可能性。ミティゲーション: hollow cursor で状態を視覚的に表示。

**[破壊的変更の多さ]** → 既存ユーザーへの影響大。ミティゲーション: v0.1.0 リリース前なので外部ユーザーなし。

## ファイル構成

```
eme/
├── eme.el                 # パッケージエントリポイント、require の集約
├── eme-core.el            # 基本設定、モード定義、カーソル設定
├── eme-selection.el       # 移動コマンド（f/b/n/p 等）
├── eme-thing.el (新規)    # thing システム（auto-detect, transient-map）
├── eme-expand.el (新規)   # expand-region (v/V)
├── eme-match.el           # surround 操作のみに簡素化
├── eme-actions.el         # アクション（d/c/y 等）
├── eme-search.el          # 検索統合
├── eme-goto-view.el       # goto/view コマンド
├── eme-macro-register.el  # マクロ・レジスタ
└── eme-keybinds.el        # キーバインド定義
```

### advice / hook の使用

- `minibuffer-setup-hook`: ミニバッファで自動 Insert Mode
- `post-command-hook`: カーソルタイプの更新（既存）
- advice: なし（既存のものも維持）

### カスタマイズ変数

命名規則: `eme-<category>-<name>`

- `eme-selection-cursor-type`: Selection Mode のカーソル形状
- `eme-insert-cursor-type`: Insert Mode のカーソル形状
- `eme-thing-cursor-type` (新規): thing 選択待ち時のカーソル形状（hollow）
- `eme-excluded-modes`: eme を無効にするモード一覧

### パフォーマンスクリティカルな部分

- `eme--detect-enclosing-delimiter`: 頻繁に呼ばれる。`syntax-ppss` のキャッシュに依存。
- `eme--with-fresh-selection`: 全移動で使用。マクロなのでオーバーヘッド最小。

### 他パッケージとの統合

- **which-key**: 既存のキーマップ構造で自動対応。`t` prefix が which-key で表示される。
- **hydra**: 使用しない（transient keymap で十分）
- **expand-region.el**: 参考にするが依存しない（自前実装）
