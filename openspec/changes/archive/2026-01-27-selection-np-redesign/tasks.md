## 1. n/p 行移動の実装変更

- [x] 1.1 `eme-selection.el`: `eme-next-line` を `eme--with-selection` + `line-move 1` に変更する
- [x] 1.2 `eme-selection.el`: `eme-previous-line` を `eme--with-selection` + `line-move -1` に変更する
- [x] 1.3 `test/eme-selection-test.el`: `eme-next-line` のテストを書き換え（カラム保持の検証、fresh selection の検証）
- [x] 1.4 `test/eme-selection-test.el`: `eme-previous-line` のテストを書き換え（カラム保持の検証）
- [x] 1.5 `test/eme-selection-test.el`: 短い行へのカラムクランプのテストを追加
- [x] 1.6 `test/eme-selection-test.el`: バッファ先頭/末尾での境界処理テストを追加

## 2. extend 系行移動の変更

- [x] 2.1 `eme-selection.el`: `eme-extend-next-line` を `line-move 1` に変更する（`forward-line 1` から）
- [x] 2.2 `eme-selection.el`: `eme-extend-previous-line` を `line-move -1` に変更する（`forward-line -1` から）
- [x] 2.3 `test/eme-selection-test.el`: extend 系行移動のカラム保持テストを追加

## 3. exchange-point-and-mark の実装

- [x] 3.1 `eme-selection.el`: `eme-exchange-point-and-mark` 関数を追加（region アクティブ時のみ `exchange-point-and-mark` を呼ぶラッパー）
- [x] 3.2 `test/eme-selection-test.el`: exchange のテストを追加（アクティブ region での入れ替え、region 非アクティブ時の無操作、二重呼び出しでの復元）

## 4. 行テキストオブジェクトの実装

- [x] 4.1 `eme-match.el`: `eme-bounds-of-inner-line` 関数を追加（`back-to-indentation` から `line-end-position`）
- [x] 4.2 `eme-match.el`: `eme-bounds-of-around-line` 関数を追加（`line-beginning-position` から改行を含む次行先頭）
- [x] 4.3 `eme-match.el`: `eme-select-inner-line` コマンドを追加
- [x] 4.4 `eme-match.el`: `eme-select-around-line` コマンドを追加
- [x] 4.5 `eme-match.el`: `eme-match-inner-map` に `l` → `eme-select-inner-line` を追加
- [x] 4.6 `eme-match.el`: `eme-match-around-map` に `l` → `eme-select-around-line` を追加
- [x] 4.7 `test/eme-match-test.el`: inner line テストを追加（インデント付き行、インデントなし行、空行、空白のみの行）
- [x] 4.8 `test/eme-match-test.el`: around line テストを追加（中間行、最終行、空行）
- [x] 4.9 `test/eme-match-test.el`: inner/around マップの `l` バインドテストを追加

## 5. キーバインドの再割り当て

- [x] 5.1 `eme-keybinds.el`: `x` のバインドを `eme-replace-char` から `eme-exchange-point-and-mark` に変更
- [x] 5.2 `eme-keybinds.el`: `r` のバインドを `eme-search-previous` から `eme-replace-char` に変更
- [x] 5.3 `eme-keybinds.el`: `S` のバインドに `eme-search-previous` を追加

## 6. ドキュメント更新

- [x] 6.1 `README.md`: キーバインド表を更新（`x` = exchange, `r` = replace-char, `S` = search-previous, `m i l` / `m a l` を追加）
- [x] 6.2 `eme-keybinds.el`: ファイル先頭のコメントのキーバインド一覧を更新

## 7. 手動テスト

- [x] 7.1 [手動] `n`/`p` でカラム位置が保持されることを確認
- [x] 7.2 [手動] `x` で選択方向が反転することを確認
- [x] 7.3 [手動] `r` で文字置換が動作することを確認
- [x] 7.4 [手動] `s`/`S` で検索 next/previous が動作することを確認
- [x] 7.5 [手動] `m i l` / `m a l` で行選択が動作することを確認
