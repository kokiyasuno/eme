## Why

現在の `n`/`p` は行全体を選択する実装だが、Emacsのmark/pointモデルでは「行全体を選択しつつカラム位置を保持する」ことが構造的に不可能である。`forward-line` はカラム0に移動し、`line-move` はカラムを保持するが行全体の選択とは両立しない。Kakoune/Helixの設計を調査した結果、`j`/`k` は行全体を選択するのではなく「カーソル移動 + アンカーからの選択拡張」であり、行選択は別コマンド (`x`) であることが判明した。この設計に合わせることで、カラム保持の問題を解消し、Emacsの標準mark/pointモデルとの整合性を保つ。

## What Changes

- **BREAKING**: `n`/`p` の挙動を「行全体選択」から「カーソル移動（アンカーがあれば選択拡張）」に変更。`n` は `line-move 1` でカーソルを次行に移動する。アンカー（`.` で設定）がある場合のみ選択を拡張する。アンカーがない場合は単純移動。カラム位置は `line-move` により保持される。
- **BREAKING**: `r` のキーバインドを `eme-search-previous` から `eme-replace-char` に変更。Kakouneの `r` (replace) と一致させる。
- **BREAKING**: `S` のキーバインドを新設し `eme-search-previous` を割り当て。`s`/`S` で search next/previous のペアとする。
- **BREAKING**: `x` のキーバインドを `eme-replace-char` から `eme-exchange-point-and-mark`（選択方向の反転）に変更。Kakouneの `alt-;` に相当。
- 行選択はMatch Modeのテキストオブジェクトとして提供: `m i l` (inner line) / `m a l` (around line)。
- `eme-next-line` / `eme-previous-line` の実装を `eme--with-selection` + `line-move` に変更。アンカーがなければ単純移動、あれば選択拡張。
- 対応するextend系コマンド (`eme-extend-next-line` / `eme-extend-previous-line`) も `line-move` を使用するよう統一。
- テストを更新して新しい挙動を検証。

## Capabilities

### New Capabilities

- `line-text-object`: 行をテキストオブジェクトとしてMatch Modeに追加。`m i l` で行内容（インデント除く）、`m a l` で行全体（改行含む）を選択。

### Modified Capabilities

- `selection-movement`: `n`/`p` の挙動を「行全体選択」から「カーソル移動（アンカー時のみ選択拡張、カラム保持）」に変更。`x` を exchange に、`r` を replace-char に、`S` を search-previous に再割り当て。

## Impact

- **eme-selection.el**: `eme-next-line`, `eme-previous-line` の実装変更。`eme-exchange-point-and-mark` の新規追加。
- **eme-actions.el**: `eme-replace-char` のキーバインド変更の影響なし（関数自体は変更不要）。
- **eme-keybinds.el**: `x` → `eme-exchange-point-and-mark`, `r` → `eme-replace-char`, `S` → `eme-search-previous` のキーバインド変更。
- **eme-match.el**: `l` (line) テキストオブジェクトの追加（inner/around）。
- **test/eme-selection-test.el**: `n`/`p` のテスト全面書き換え。
- **test/eme-match-test.el**: 行テキストオブジェクトのテスト追加。
- **README.md**: キーバインド表の更新。
- パフォーマンスへの影響: なし。`line-move` はEmacs標準関数であり、overlay等の追加メカニズムは不要。
- 外部依存の追加: なし。
- ロールバック: `emacs-modal-editing-mode` を無効化すれば全キーバインドが解除される。既存の仕組みで対応可能。
- MELPA登録: この変更はMELPA登録前の設計修正であり、公開APIへの影響はない。
- 既存パッケージとの差別化: meow は `meow-line` でカラム保持しない。本パッケージは `n`/`p` で `line-move` によりカラムを保持しつつ選択を拡張し、行選択はテキストオブジェクト (`m i l`) として提供する。Kakoune/Helixの設計思想により忠実な実装となる。
