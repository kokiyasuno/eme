## 1. eme-thing.el 基盤構築

- [x] 1.1 `eme-thing-providers` alist を定義（空の状態）
- [x] 1.2 `eme-thing-bounds` 関数を実装（provider lookup + 呼び出し）
- [x] 1.3 `eme-thing-all-bounds` 関数を実装（全 provider 呼び出し + ソート + 重複除去）
- [x] 1.4 `eme-thing-outer` 関数を実装（外側 bounds 検出）
- [x] 1.5 `eme-thing--treesit-available-p` をバッファローカルキャッシュ付きで実装

## 2. 標準 thing provider 実装

- [x] 2.1 `eme-thing--word` provider を実装（`bounds-of-thing-at-point` 使用）
- [x] 2.2 `eme-thing--symbol` provider を実装
- [x] 2.3 `eme-thing--sexp` provider を実装（`forward-sexp`/`backward-sexp` 使用）
- [x] 2.4 `eme-thing--sexp-chain` provider を実装（eme-expand.el からロジック移植）
- [x] 2.5 `eme-thing--line` provider を実装
- [x] 2.6 `eme-thing--sentence` provider を実装
- [x] 2.7 `eme-thing--paragraph` provider を実装
- [x] 2.8 `eme-thing--defun` provider を実装
- [x] 2.9 `eme-thing--buffer` provider を実装
- [x] 2.10 `eme-thing--string` provider を実装（syntax-ppss + tree-sitter ）
- [x] 2.11 `eme-thing--list` provider を実装（syntax-ppss + tree-sitter ）
- [x] 2.12 `eme-thing--delimiter` provider を実装（自動検出 + 文字指定対応）
- [x] 2.13 全 provider を `eme-thing-providers` に登録

## 3. tree-sitter 統合

- [x] 3.1 eme-treesit.el から `eme-treesit-available-p` を eme-thing.el に移植
- [x] 3.2 eme-treesit.el から `eme-treesit-expansion-bounds` を eme-thing.el に移植
- [x] 3.3 eme-treesit.el から `eme-treesit-find-enclosing-delimiters` を eme-thing.el に移植
- [x] 3.4 delimiter provider に tree-sitter フォールバックを実装
- [x] 3.5 string/list provider に tree-sitter フォールバックを実装

## 4. 既存ファイルのリファクタリング

- [x] 4.1 eme-thing.el: 既存の `eme-thing-inner`/`eme-thing-bounds` を新 API のラッパーに変更
- [x] 4.2 eme-expand.el: `eme--expand-bounds-at-point` を `eme-thing-all-bounds` 呼び出しに置換
- [x] 4.3 eme-expand.el: 独自 bounds 検出コード（sexp-chain 含む）を削除
- [x] 4.4 eme-delimiter.el: delimiter 検出コードを `eme-thing-bounds` 呼び出しに置換
- [x] 4.5 eme-delimiter.el: outer 拡張を `eme-thing-outer` 呼び出しに置換
- [x] 4.6 eme-selection.el: word/sexp bounds 取得を `eme-thing-bounds` 呼び出しに置換
- [x] 4.7 eme-selection.el: line bounds 取得を `eme-thing-bounds` 呼び出しに置換
- [x] 4.8 eme-actions.el: line bounds 取得を `eme-thing-bounds` 呼び出しに置換

## 5. eme-treesit.el 削除

- [x] 5.1 eme-treesit.el への require を eme-thing.el への require に変更（全ファイル）
- [x] 5.2 eme-treesit.el ファイルを削除
- [x] 5.3 eme.el の require リストを更新（不要 - eme-treesit.el は元々 require されていなかった）

## 6. テスト

- [x] 6.1 eme-thing-test.el: `eme-thing-bounds` の基本テストを追加
- [x] 6.2 eme-thing-test.el: 各 provider のテストを追加
- [x] 6.3 eme-thing-test.el: `eme-thing-all-bounds` のテストを追加
- [x] 6.4 eme-thing-test.el: `eme-thing-outer` のテストを追加
- [x] 6.5 eme-expand-test.el: thing API 使用後の動作確認テストを追加
- [x] 6.6 eme-delimiter-test.el: thing API 使用後の動作確認テストを追加
- [x] 6.7 eme-selection-test.el: thing API 使用後の動作確認テストを追加
- [ ] 6.8 全テストを実行して pass を確認（CI 実行待ち）

## 7. 検証 [手動]

- [x] 7.1 [手動] `v` による expand-region が正しく動作することを確認
- [x] 7.2 [手動] `m`/`M` による delimiter 操作が正しく動作することを確認
- [x] 7.3 [手動] `t i`/`t b` による thing 選択が正しく動作することを確認
- [x] 7.4 [手動] `f`/`b`/`n`/`p` による移動+選択が正しく動作することを確認
- [x] 7.5 [手動] tree-sitter 有効/無効両方で動作確認
