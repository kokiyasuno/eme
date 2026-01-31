## 1. eme-core.el の変更

- [x] 1.1 `eme--anchor-bounds` 変数を追加（選択範囲を記憶する cons セル）
- [x] 1.2 `eme--anchor-pos` 変数を削除（不要になる）
- [x] 1.3 `eme-selection-or-region-bounds` 関数を削除

## 2. eme-selection.el のアンカー機能再実装

- [x] 2.1 `eme-toggle-anchor` を再実装：
  - アンカー ON 時: 現在の eme-selection bounds を `eme--anchor-bounds` に記憶
  - アンカー OFF 時: `eme-selection-clear` と `eme--anchor-bounds` を nil に設定
- [x] 2.2 アンカーマージ用ヘルパー関数 `eme--merge-with-anchor` を追加：
  - 引数: 新しい範囲 (start . end)
  - 動作: anchor-bounds と新範囲をマージ [min(anchor-start, new-start), max(anchor-end, new-end)]

## 3. eme-selection.el の行移動コマンド修正

- [x] 3.1 `eme-next-line` を修正：
  - アンカーなし: eme-selection で行全体を選択（fresh selection ）
  - アンカーあり: `eme--merge-with-anchor` でマージして eme-selection 設定
  - `deactivate-mark` を削除、Emacs region を使用しない
- [x] 3.2 `eme-previous-line` を修正（同様の変更）

## 4. eme-selection.el の単語移動コマンド修正

- [x] 4.1 `eme-forward-word` を修正：
  - `push-mark` を削除
  - eme-selection で単語範囲を選択
  - アンカーあり時は `eme--merge-with-anchor` でマージ
- [x] 4.2 `eme-backward-word` を修正（同様の変更）

## 5. eme-selection.el の S 式移動コマンド修正

- [x] 5.1 `eme-forward-sexp` を修正（push-mark → eme-selection ）
- [x] 5.2 `eme-backward-sexp` を修正（同様）

## 6. eme-selection.el のその他移動コマンド修正

- [x] 6.1 `eme-forward-paragraph` / `eme-backward-paragraph` を修正
- [x] 6.2 `eme-next-defun` / `eme-previous-defun` を修正
- [x] 6.3 `eme-beginning-of-line` / `eme-end-of-line` を修正
- [x] 6.4 `eme-beginning-of-sentence` / `eme-end-of-sentence` を修正
- [x] 6.5 `eme-beginning-of-defun` / `eme-end-of-defun` を修正

## 7. eme-selection.el のマクロ修正

- [x] 7.1 `eme--with-selection` マクロを削除または修正（push-mark 使用を廃止）
- [x] 7.2 `eme--with-fresh-selection` マクロを削除または修正（push-mark 使用を廃止）
- [x] 7.3 extend-* コマンド群を eme-selection ベースに修正または削除

## 8. eme-actions.el の修正

- [x] 8.1 `eme--get-selection-bounds` を簡略化（`eme-selection-bounds` のみ使用）
- [x] 8.2 `eme--clear-selection` から `deactivate-mark` を削除、`eme--anchor-bounds` のクリアを追加
- [x] 8.3 `eme-copy` で選択を維持するよう確認（既に実装済みの可能性）
- [x] 8.4 `eme-undo` / `eme-redo` で `eme--anchor-bounds` をクリアするよう修正

## 9. eme-expand.el の修正

- [x] 9.1 `region-active-p` → `eme-selection-active-p` に置換
- [x] 9.2 `push-mark` → `eme-selection-set` に置換
- [x] 9.3 `deactivate-mark` → `eme-selection-clear` に置換

## 10. eme-delimiter.el の修正

- [x] 10.1 `region-active-p` → `eme-selection-active-p` に置換
- [x] 10.2 `push-mark` → `eme-selection-set` に置換
- [x] 10.3 `deactivate-mark` → `eme-selection-clear` に置換

## 11. テスト更新

- [x] 11.1 eme-selection-test.el の region 関連テストを eme-selection ベースに書き換え
- [x] 11.2 eme-actions-test.el のテストを更新
- [x] 11.3 アンカー機能のテストを追加（bounds マージ動作の検証）
- [x] 11.4 [手動] n → . → n で2行選択、n → . → p で元の行が選択されることを確認

## 12. クリーンアップ

- [x] 12.1 未使用の region 関連変数・関数の削除
- [x] 12.2 コメント・ docstring の更新（region → eme-selection ）
- [x] 12.3 全テスト実行して PASS を確認（バイトコンパイル成功）
