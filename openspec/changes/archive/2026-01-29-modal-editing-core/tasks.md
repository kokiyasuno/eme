## 1. プロジェクト初期セットアップ

- [x] 1.1 ディレクトリ構造を作成（`emacs-modal-editing/`, `test/`）
- [x] 1.2 `emacs-modal-editing.el` にパッケージヘッダーを記述（Author, Version, URL, Package-Requires 等）
- [x] 1.3 buttercup テスト環境をセットアップ（`test/emacs-modal-editing-test.el`）
- [x] 1.4 CI 用の設定ファイルを作成（GitHub Actions ）

## 2. コアモジュール（eme-core.el ）

- [x] 2.1 `eme-core.el` を作成し、基本的な defgroup と defcustom を定義
- [x] 2.2 `eme-selection-cursor-type`, `eme-insert-cursor-type` カスタマイズ変数を実装
- [x] 2.3 `eme-excluded-modes` リストを実装
- [x] 2.4 モードラインインジケータ（`[S]`/`[I]`）を実装
- [x] 2.5 モード遷移フック（`eme-selection-mode-enter-hook` 等）を定義
- [x] 2.6 `eme-core.el` のユニットテストを作成

## 3. モードシステム（mode-system ）

- [x] 3.1 `eme-selection-mode-map` を定義（空の sparse-keymap ）
- [x] 3.2 `eme-insert-mode-map` を定義（ESC, C-g のみバインド）
- [x] 3.3 `emulation-mode-map-alists` への登録処理を実装
- [x] 3.4 `eme-selection-mode` マイナーモードを定義
- [x] 3.5 `eme-insert-mode` マイナーモードを定義
- [x] 3.6 Selection → Insert 遷移（`i` キー）を実装
- [x] 3.7 Insert → Selection 遷移（`C-g`, `ESC`）を実装
- [x] 3.8 カーソルタイプ切り替えを実装
- [x] 3.9 `emacs-modal-editing-local-mode` を定義
- [x] 3.10 `emacs-modal-editing-mode` グローバルモードを定義
- [x] 3.11 ミニバッファでの自動 Insert Mode 切り替えを実装
- [x] 3.12 モードシステムのユニットテストを作成
- [x] 3.13 [手動] モード切り替えの動作確認

## 4. 選択モード移動コマンド（eme-selection.el ）

- [x] 4.1 `eme-selection.el` を作成
- [x] 4.2 文字移動コマンドを実装（`eme-forward-char`, `eme-backward-char`）
- [x] 4.3 単語移動コマンドを実装（`eme-forward-word`, `eme-backward-word`）
- [x] 4.4 S 式移動コマンドを実装（`eme-forward-sexp`, `eme-backward-sexp`）
- [x] 4.5 行移動コマンドを実装（`eme-next-line`, `eme-previous-line`）
- [x] 4.6 段落移動コマンドを実装（`eme-forward-paragraph`, `eme-backward-paragraph`）
- [x] 4.7 defun 移動コマンドを実装（`eme-next-defun`, `eme-previous-defun`）
- [x] 4.8 行境界移動を実装（`eme-beginning-of-line`, `eme-end-of-line`）
- [x] 4.9 文境界移動を実装（`eme-beginning-of-sentence`, `eme-end-of-sentence`）
- [x] 4.10 defun 境界移動を実装（`eme-beginning-of-defun`, `eme-end-of-defun`）
- [x] 4.11 アンカートグル（`eme-toggle-anchor`）を実装
- [x] 4.12 バッファ境界でのエラー抑制を実装
- [x] 4.13 選択モード移動のユニットテストを作成

## 5. Match Mode （eme-match.el ）

- [x] 5.1 `eme-match.el` を作成
- [x] 5.2 `eme-match-mode-map` プレフィックスキーマップを定義
- [x] 5.3 bounds 取得関数群を実装（word, WORD, sentence, paragraph ）
- [x] 5.4 括弧系 bounds 取得を実装（paren, bracket, brace, angle ）
- [x] 5.5 クォート系 bounds 取得を実装（double-quote, single-quote, backtick ）
- [x] 5.6 `eme-select-inner` コマンドを実装
- [x] 5.7 `eme-select-around` コマンドを実装
- [x] 5.8 ネストしたデリミタ処理を実装
- [x] 5.9 tree-sitter 対応 bounds 取得を実装（function, class, argument ）
- [x] 5.10 tree-sitter なしのフォールバックを実装
- [x] 5.11 `eme-wrap` コマンドを実装
- [x] 5.12 `eme-delete-surround` コマンドを実装
- [x] 5.13 `eme-replace-surround` コマンドを実装
- [x] 5.14 Match Mode のユニットテストを作成
- [x] 5.15 [手動] Text Objects の動作確認

## 6. アクション（eme-actions.el ）

- [x] 6.1 `eme-actions.el` を作成
- [x] 6.2 `eme-delete` コマンドを実装（kill-ring 保存）
- [x] 6.3 `eme-delete-no-save` コマンドを実装
- [x] 6.4 `eme-copy` コマンドを実装
- [x] 6.5 `eme-yank` コマンドを実装
- [x] 6.6 `eme-change` コマンドを実装
- [x] 6.7 `eme-replace-char` コマンドを実装
- [x] 6.8 `eme-comment-toggle` コマンドを実装
- [x] 6.9 `eme-format` コマンドを実装
- [x] 6.10 `eme-indent-increase`, `eme-indent-decrease` を実装
- [x] 6.11 `eme-join-lines` コマンドを実装
- [x] 6.12 `eme-open-line-below`, `eme-open-line-above` を実装
- [x] 6.13 `eme-duplicate` コマンドを実装
- [x] 6.14 `eme-toggle-case` コマンドを実装
- [x] 6.15 `eme-undo`, `eme-redo` コマンドを実装
- [x] 6.16 `eme-sort-lines` コマンドを実装
- [x] 6.17 `eme-reverse-lines` コマンドを実装
- [x] 6.18 アクションのユニットテストを作成

## 7. 検索統合（eme-search.el ）

- [x] 7.1 `eme-search.el` を作成
- [x] 7.2 `eme-search-forward` コマンドを実装（isearch 連携）
- [x] 7.3 `eme-search-regexp` コマンドを実装
- [x] 7.4 `eme-search-next` コマンドを実装
- [x] 7.5 `eme-search-previous` コマンドを実装
- [x] 7.6 検索終了時の選択設定を実装
- [x] 7.7 選択テキストでの検索開始を実装
- [x] 7.8 検索のラップ処理と通知を実装
- [x] 7.9 検索統合のユニットテストを作成

## 8. Goto/View （eme-goto-view.el ）

- [x] 8.1 `eme-goto-view.el` を作成
- [x] 8.2 `eme-goto-line` コマンドを実装
- [x] 8.3 `eme-xref-pop` コマンドを実装（`,` キー用）
- [x] 8.4 `eme-scroll-up` コマンドを実装
- [x] 8.5 `eme-scroll-down` コマンドを実装
- [x] 8.6 `eme-recenter` コマンドを実装
- [x] 8.7 Goto/View のユニットテストを作成

## 9. マクロ・レジスタ（eme-macro-register.el ）

- [x] 9.1 `eme-macro-register.el` を作成
- [x] 9.2 `eme-macro-toggle-record` コマンドを実装
- [x] 9.3 `eme-macro-play` コマンドを実装
- [x] 9.4 マクロ記録中のモードライン表示を実装
- [x] 9.5 `eme-register-prefix-map` を定義
- [x] 9.6 `eme-register-jump` コマンドを実装
- [x] 9.7 `eme-register-point-to` コマンドを実装
- [x] 9.8 `eme-register-copy-to` コマンドを実装
- [x] 9.9 `eme-register-insert` コマンドを実装
- [x] 9.10 マクロ・レジスタのユニットテストを作成

## 10. キーバインド定義（eme-keybinds.el ）

- [x] 10.1 `eme-keybinds.el` を作成
- [x] 10.2 Selection Mode の移動キーをバインド（n/p/f/b 系）
- [x] 10.3 Selection Mode のアンカーキーをバインド（`.`）
- [x] 10.4 Match Mode プレフィックスをバインド（`m`）
- [x] 10.5 アクションキーをバインド（d/w/y/c/x/;/=/</>/j/o/O/+/`/u/U ）
- [x] 10.6 検索キーをバインド（/ s r ）
- [x] 10.7 Goto/View キーをバインド（g , v V l ）
- [x] 10.8 マクロキーをバインド（q Q ）
- [x] 10.9 レジスタプレフィックスをバインド（`'`）
- [x] 10.10 g プレフィックスマップを作成（g s, g R ）
- [x] 10.11 [手動] 全キーバインドの動作確認

## 11. メインエントリポイント

- [x] 11.1 `emacs-modal-editing.el` に全モジュールの require を追加
- [x] 11.2 `provide` 文を追加
- [x] 11.3 autoload クッキーを設定
- [x] 11.4 パッケージ全体の統合テストを作成
- [x] 11.5 [手動] パッケージの load/unload テスト

## 12. ドキュメントとリリース準備

- [x] 12.1 README.md を作成（インストール方法、基本的な使い方）
- [x] 12.2 キーバインド一覧表を作成
- [x] 12.3 CHANGELOG.md を作成
- [x] 12.4 LICENSE ファイルを追加

<!-- v1 リリース時に対応
- [ ] 12.5 MELPA 向け package metadata を最終確認
- [ ] 12.6 バージョン v0.1.0 としてタグ付け
-->
