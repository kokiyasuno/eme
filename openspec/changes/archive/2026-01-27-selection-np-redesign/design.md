## Context

現在の `eme-next-line` / `eme-previous-line` は `forward-line` + `push-mark` で行全体を選択する実装だが、`forward-line` はカラム0に移動するため、ユーザーが期待するカラム保持ができない。Emacsの mark/point モデルでは、カーソル位置（point）が選択範囲の端と一致するため、「行全体を選択しつつカラム位置を保持する」ことが構造的に不可能。

Kakoune/Helix の調査により、`j`/`k` は行全体選択ではなくカーソル移動+選択拡張であることが判明。行選択は別コマンド。この設計に合わせる。

合わせて、キーバインドの意味的整合性を改善する:
- `r` = replace（Kakoune準拠）
- `s`/`S` = search next/previous（ペア）
- `x` = exchange（選択方向反転）

### 現在の状態

```
eme-selection.el:
  eme-next-line     → forward-line 1 + push-mark(EOL) + goto-char(BOL)
  eme-previous-line → forward-line -1 + push-mark(EOL) + goto-char(BOL)

eme-keybinds.el:
  x → eme-replace-char
  r → eme-search-previous
  S → (未割り当て)
```

### 制約

- cl-lib 使用禁止
- 外部ライブラリへの依存なし
- Emacs 29.1+ 対応
- 既存の `eme--with-fresh-selection` / `eme--with-selection` マクロを活用
- `emulation-mode-map-alists` による優先度管理は既存の仕組みを維持

## Goals / Non-Goals

**Goals:**

- `n`/`p` でカラム位置を保持しつつ、アンカーからカーソルまでの選択を作成する
- `x` で選択方向を反転（mark と point を入れ替え）する
- `r` を replace-char に、`S` を search-previous に再割り当てする
- 行をテキストオブジェクトとして Match Mode に追加する（`m i l` / `m a l`）
- 既存テストを新しい挙動に更新する

**Non-Goals:**

- overlay ベースの独自選択機構の実装
- 複数カーソル / 複数選択のサポート
- Vim 互換性の考慮

## Decisions

### D1: `n`/`p` の実装に `line-move` をマクロなしで使用する

**選択:** `line-move` を直接使用（マクロなし）

**理由:** `line-move` は Emacs 標準の行移動関数で、`goal-column` と `temporary-goal-column` を考慮してカラム位置を保持する。`next-line` / `previous-line` の内部実装でもある。`forward-line` はカラムを保持しない。

`n`/`p` は最も頻繁に使われるキーであり、以下の要件がある:
- アンカーなし（region 非アクティブ）: 単純にカーソル移動。選択を作成しない。
- アンカーあり（region アクティブ）: `line-move` がカーソルを移動すると、Emacs の mark/point モデルにより region が自動的に拡張される。

マクロを使用しない理由: `eme--with-selection` マクロには `(unless (region-active-p) (push-mark (point) t t))` があり、region が非アクティブの場合でも常に mark を設定してしまう。これにより `n`/`p` が毎回選択を作成する問題が発生した。マクロを外して `condition-case` のみで境界処理を行うことで、アンカーの有無による動作の分離が正しく実現される。

**代替案:**
- `eme--with-selection` + `line-move`: マクロが常に mark を設定するため、アンカーなしでも選択が作成される。不採用。
- `eme--with-fresh-selection` + `line-move`: 毎回 fresh selection が作成されるが、使い道がほぼない斜めの選択が生成される。不採用。
- `forward-line` + 手動カラム復元: `move-to-column` で保持可能だが、`line-move` が既にこの処理を含む。車輪の再発明。
- `next-line` 直接呼び出し: `interactive` 前提の挙動（`this-command` 設定等）があり、プログラムからの呼び出しに不向き。

**実装:**
```elisp
(defun eme-next-line ()
  "Move to the next line. Extend selection if anchor is set."
  (interactive)
  (condition-case nil
      (line-move 1)
    (beginning-of-buffer nil)
    (end-of-buffer nil)))

(defun eme-previous-line ()
  "Move to the previous line. Extend selection if anchor is set."
  (interactive)
  (condition-case nil
      (line-move -1)
    (beginning-of-buffer nil)
    (end-of-buffer nil)))
```

### D2: `x` に `eme-exchange-point-and-mark` を割り当てる

**選択:** `exchange-point-and-mark` のラッパーを作成し `x` にバインド

**理由:** Kakoune の `alt-;`（選択方向反転）に相当。選択ファーストのエディタでは、選択範囲を変えずにカーソル位置だけ反対側に移動する操作が重要。`x` は e**x**change の mnemonic。

**実装:** Emacs 標準の `exchange-point-and-mark` は `(exchange-point-and-mark)` で呼べるが、`transient-mark-mode` 環境では引数なしだと region を deactivate する場合がある。ラッパーで明示的に region を維持する。

```elisp
(defun eme-exchange-point-and-mark ()
  "Exchange point and mark, keeping region active."
  (interactive)
  (when (region-active-p)
    (exchange-point-and-mark)))
```

**代替案:**
- `exchange-point-and-mark` を直接バインド: 引数なしだと `transient-mark-mode` 下で region が deactivate される可能性あり。ラッパーで安全に。

### D3: `r` → replace-char, `S` → search-previous

**選択:** キーバインドの再配置のみ。関数実装は変更なし。

**理由:**
- `r` = replace は Kakoune の標準。元の `r` = reverse-search は Emacs の `C-r` からの連想だったが、Kakoune 準拠を優先。
- `s`/`S` = search next/previous は方向のペアとして自然。移動系では `n`/`N`, `f`/`F` が既にこのパターン。

### D4: 行テキストオブジェクトを Match Mode に追加

**選択:** `eme-match-inner-map` と `eme-match-around-map` に `l` キーで行オブジェクトを追加。

**定義:**
- `m i l` (inner line): 行の先頭インデント後から行末まで（`back-to-indentation` 〜 `line-end-position`）
- `m a l` (around line): 行全体（`line-beginning-position` 〜 次行の `line-beginning-position`、つまり改行を含む）

**理由:** 行選択を専用キーではなく、既存のテキストオブジェクトパターン（`m i {obj}` / `m a {obj}`）に統合する。`l` = **l**ine の mnemonic。

**代替案:**
- 専用キー（`x` 等）: ユーザーが直感的でないと判断
- `a` + `.` + `e` の組み合わせ: 3キー操作。`m i l` も3キーだが、テキストオブジェクトとしての一貫性がある

**実装:**
```elisp
(defun eme-bounds-of-inner-line ()
  "Return bounds of line content (after indentation to end of line)."
  (save-excursion
    (let ((start (progn (back-to-indentation) (point)))
          (end (line-end-position)))
      (cons start end))))

(defun eme-bounds-of-around-line ()
  "Return bounds of entire line including newline."
  (cons (line-beginning-position)
        (min (1+ (line-end-position)) (point-max))))
```

### D5: extend 系コマンドも `line-move` に統一

**選択:** `eme-extend-next-line` / `eme-extend-previous-line` も `line-move` を使用。

**理由:** `eme--with-selection` マクロを使う extend 系も同様にカラム保持すべき。`forward-line` のままだとカラム0に移動してしまう。

## Risks / Trade-offs

**[`line-move` のエラーハンドリング]** → `line-move` はバッファ先頭/末尾で `beginning-of-buffer` / `end-of-buffer` エラーを発生させる。`n`/`p` では直接 `condition-case` でキャッチ。extend 系では `eme--with-selection` マクロの `condition-case` でキャッチ。

**[`line-move` の `interactive` 依存]** → `line-move` は本来 `next-line` の内部関数。非 interactive コンテキストでの使用は公式にサポートされていないが、実際には広く使用されており安定している。`line-move-visual` 変数の影響を受ける点に注意（visual line mode との互換性）。ミティゲーション: テストで両方のモード（logical / visual）を検証する。

**[キーバインド変更の学習コスト]** → `x`, `r`, `S` の3キーが変更される。ミティゲーション: MELPA公開前の変更なので、外部ユーザーへの影響なし。README のキーバインド表を更新。

**[`m i l` の3キーストローク]** → 行選択に3キー必要。Kakoune の `x`（1キー）より遅い。トレードオフとして、キーバインドの意味的一貫性を優先。頻度的に行選択は移動ほど多くない。

## ファイル構成

変更対象ファイル:

| ファイル | 変更内容 |
|---------|---------|
| `eme-selection.el` | `eme-next-line`, `eme-previous-line` の実装変更。`eme-exchange-point-and-mark` 追加 |
| `eme-match.el` | `eme-bounds-of-inner-line`, `eme-bounds-of-around-line`, `eme-select-inner-line`, `eme-select-around-line` 追加。inner/around map に `l` キー追加 |
| `eme-keybinds.el` | `x`, `r`, `S` のキーバインド変更 |
| `test/eme-selection-test.el` | `n`/`p` テスト書き換え、exchange テスト追加 |
| `test/eme-match-test.el` | 行テキストオブジェクトのテスト追加 |
| `README.md` | キーバインド表更新 |

advice や hook の追加使用: なし。既存の仕組みのみ。

パフォーマンスクリティカルな部分: なし。`line-move` は `C-n`/`C-p` で常時使用される標準関数。

他パッケージとの統合ポイント: which-key は既存のキーマップ構造で自動対応。変更不要。
