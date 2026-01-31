## 1. パッケージ名変更

- [x] 1.1 ディレクトリ名を `emacs-modal-editing/` から `eme/` に変更
- [x] 1.2 `emacs-modal-editing.el` を `eme.el` に改名し、provide/require を更新
- [x] 1.3 `emacs-modal-editing-mode` を `eme-mode` に変更
- [x] 1.4 `emacs-modal-editing-local-mode` を `eme-local-mode` に変更
- [x] 1.5 全ファイルの `emacs-modal-editing` prefix を `eme` に置換
- [x] 1.6 README.md のパッケージ名、インストール手順を更新

## 2. 単語移動の実装（f/b キー）

- [x] 2.1 `eme-forward-word` を実装（移動 + fresh selection ）
- [x] 2.2 `eme-backward-word` を実装（移動 + fresh selection ）
- [x] 2.3 アンカーあり時の選択拡張動作を実装
- [x] 2.4 `f`/`b` キーを新しい単語移動関数にバインド
- [x] 2.5 `F`/`B` キーのバインドを削除（未割り当てに）
- [x] 2.6 単語移動のテストを作成（buttercup ）

## 3. 独自選択機構の実装（eme-selection ）

- [x] 3.1 `eme-selection` overlay 変数を定義（eme-core.el ）
- [x] 3.2 `eme-selection-active-p` を実装
- [x] 3.3 `eme-selection-bounds` を実装
- [x] 3.4 `eme-selection-set` を実装（overlay 作成・更新）
- [x] 3.5 `eme-selection-clear` を実装（overlay 削除）
- [x] 3.6 region face と同様の見た目になるよう face 設定
- [x] 3.7 アクションコマンドを eme-selection に対応させる

## 4. 行移動の行選択化（n/p キー）

- [x] 4.1 `eme-next-line` を修正（eme-selection で行全体を選択）
- [x] 4.2 `eme-previous-line` を修正（eme-selection で行全体を選択）
- [x] 4.3 カラム保持ロジックを維持
- [x] 4.4 アンカーあり時は Emacs region で選択拡張
- [x] 4.5 行移動のテストを更新

## 5. 統一選択モデルの実装

- [x] 5.1 `eme--with-fresh-selection` マクロを確認・拡張
- [x] 5.2 全移動コマンドで fresh selection または選択拡張を統一
- [x] 5.3 `a`/`e` コマンドを fresh selection 対応に修正
- [x] 5.4 統一選択モデルのテストを作成

## 6. thing システムの実装

- [x] 6.1 `eme-thing.el` ファイルを新規作成
- [x] 6.2 `eme--detect-enclosing-delimiter` を実装（syntax-ppss 使用）
- [x] 6.3 `eme-thing-inner` を実装（auto-detect inner 選択）
- [x] 6.4 `eme-thing-bounds` を実装（auto-detect bounds 選択）
- [x] 6.5 `eme-thing-map` keymap を定義（i/b キー）
- [x] 6.6 `eme-thing-prefix` を実装（t キーで thing-map 起動）
- [x] 6.7 transient keymap による拡張機能を実装（t 繰り返し）
- [x] 6.8 hollow cursor 表示を transient 状態で設定
- [x] 6.9 明示的 delimiter 指定のフォールバックを実装（t i ( 等）
- [x] 6.10 thing システムのテストを作成
- [x] 6.11 [手動] 各種 major-mode での動作確認

## 7. expand-region の実装

- [x] 7.1 `eme-expand.el` ファイルを新規作成
- [x] 7.2 拡大順序の定義（word → symbol → string → list → defun ）
- [x] 7.3 `eme-expand-region` を実装
- [x] 7.4 `eme-contract-region` を実装
- [x] 7.5 拡大履歴の管理機構を実装
- [x] 7.6 `v`/`V` キーを expand/contract にバインド
- [x] 7.7 数値引数サポートを追加
- [x] 7.8 expand-region のテストを作成
- [x] 7.9 [手動] 各種 major-mode での拡大動作確認

## 8. Match Mode の簡素化

- [x] 8.1 `eme-match-inner-map` を削除
- [x] 8.2 `eme-match-around-map` を削除
- [x] 8.3 `m i`/`m a` prefix を削除
- [x] 8.4 `m` を surround 専用 prefix に変更
- [x] 8.5 surround キー（`m w`/`m d`/`m r`）の動作を確認
- [x] 8.6 Match Mode 関連テストを更新

## 9. キーバインド統合

- [x] 9.1 `eme-keybinds.el` を全面更新
- [x] 9.2 `t` キーを thing prefix にバインド
- [x] 9.3 `v`/`V` を expand/contract にバインド
- [x] 9.4 `f`/`b` を単語移動にバインド
- [x] 9.5 `F`/`B` のバインドを削除
- [x] 9.6 キーバインド競合テストを作成
- [x] 9.7 [手動] which-key との連携確認

## 10. ドキュメント更新

- [x] 10.1 README.md のキーバインド表を全面更新
- [x] 10.2 thing システムの使用方法を追加
- [x] 10.3 expand-region の使用方法を追加
- [x] 10.4 移行ガイド（旧 → 新）を追加

## 11. Delimiter 操作の再設計

- [x] 11.1 `eme-delimiter-chars` カスタム変数を定義
- [x] 11.2 `eme--delimiter-context` 変数を定義（選択時の delimiter 記憶用）
- [x] 11.3 `eme-delimiter-inner` を実装（m キー、inner 選択 + context 設定）
- [x] 11.4 `eme-delimiter-bounds` を実装（M キー、bounds 選択）
- [x] 11.5 delimiter キー押下時の分岐処理を実装
  - [x] 11.5.1 context あり + 同じ delimiter → 削除
  - [x] 11.5.2 context あり + 違う delimiter → 変更
  - [x] 11.5.3 wrap モード → wrap 実行
- [x] 11.6 wrap モードを実装（選択あり + context なし時の m ）
- [x] 11.7 m 連打での拡大を実装
- [x] 11.8 `m`/`M` キーを新しい関数にバインド
- [x] 11.9 旧 eme-match 関連コードを削除/整理
- [x] 11.10 delimiter 操作のテストを作成
- [x] 11.11 README.md を更新

## 12. sexp-chain 拡張（expand-region 改善）

- [x] 12.1 `eme--expand-sexp-chain-bounds` を実装
  - symbol-end から `(`, `[`, `.` が続く限り forward-sexp
  - チェーン全体 (例: `test(a).method()[0]`) を一つの bounds として返す
- [x] 12.2 `eme--expand-bounds-at-point` に sexp-chain を追加
  - word/symbol の次の拡大単位として挿入
- [x] 12.3 テストを作成
  - `test()` → symbol → sexp-chain → containing
  - `arr[0]` → symbol → sexp-chain → containing
  - `obj.method()` → symbol → sexp-chain → containing
- [x] 12.4 README.md の expand-region セクションを更新

## 13. tree-sitter 統合

- [x] 13.1 `eme-treesit.el` を新規作成
  - tree-sitter 利用可能かの判定関数
  - AST ノード取得・親方向探索のユーティリティ
- [x] 13.2 expand-region を tree-sitter 対応
  - `eme-treesit-expansion-bounds`: 親ノードを登って bounds 収集
  - tree-sitter 利用可能時は sexp-chain より優先
  - フォールバック: 既存の syntax-based ロジック
- [x] 13.3 delimiter (m/M) を tree-sitter 対応
  - 囲む bracket/string ノードを AST から検出
  - フォールバック: 既存の syntax-ppss ロジック
- [x] 13.4 thing (t) を tree-sitter 対応
  - AST ノードベースの inner/bounds 選択
  - フォールバック: 既存ロジック
- [x] 13.5 テストを作成
  - tree-sitter あり/なし両方のケース
- [x] 13.6 README.md に tree-sitter 対応を記載

## 14. 最終確認

- [x] 14.1 全テストの実行と確認
- [x] 14.2 [手動] emacs-lisp-mode での一連の編集操作確認
- [x] 14.3 [手動] python-mode での動作確認 (tree-sitter あり)
- [x] 14.4 [手動] org-mode での動作確認
- [x] 14.5 MELPA 向けパッケージメタデータの確認
