## 1. eme-exchange-point-and-mark の実装

- [x] 1.1 `eme-selection.el` に `eme-exchange-point-and-mark` 関数を追加
- [x] 1.2 バイトコンパイルで警告がないことを確認

## 2. 未使用変数の削除

- [x] 2.1 `eme-core.el:312` の `(let ((buf (current-buffer)))` から `buf` を削除

## 3. docstring の整形（80文字以内）

- [x] 3.1 `eme-selection.el` の docstring を整形（5箇所）
- [x] 3.2 `eme-delimiter.el` の docstring を整形（1箇所）

## 4. 検証

- [x] 4.1 全ファイルのバイトコンパイルで警告ゼロを確認
- [x] 4.2 `x` キーで選択範囲の始点・終点交換が動作することを確認（手動）
