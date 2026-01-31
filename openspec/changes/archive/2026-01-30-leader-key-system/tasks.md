## 1. Core 変更: Insert Start Modes

- [x] 1.1 `eme-core.el` で `eme-excluded-modes` を `eme-insert-start-modes` に変更
- [x] 1.2 デフォルト値の設定（special-mode, dired-mode, magit-mode, vterm-mode など）
- [x] 1.3 `derived-mode-p` を使った派生モード検出の実装
- [x] 1.4 `eme-local-mode` で Insert Start Modes の処理を追加

## 2. Leader Key 基本実装

- [x] 2.1 `eme-leader.el` の新規作成とモジュール構造のセットアップ
- [x] 2.2 Leader モード状態管理変数の定義（`eme-leader--active`, `eme-leader--prefix`, `eme-leader--next-control`）
- [x] 2.3 `eme-user-map` キーマップの定義
- [x] 2.4 `eme-leader-key` カスタム変数の定義
- [x] 2.5 Leader モード起動関数 `eme-leader-start` の実装

## 3. Leader Key Prefix 処理

- [x] 3.1 デフォルト prefix（C-c）でのキー処理の実装
- [x] 3.2 `x` による C-x prefix への切り替え実装
- [x] 3.3 `h` による C-h prefix への切り替え実装
- [x] 3.4 `u` による universal-argument の実行と Leader 終了
- [x] 3.5 `m` によるユーザーマップへのアクセス実装

## 4. C- 修飾の動的付与

- [x] 4.1 `SPC` キーによる C- 修飾フラグ設定の実装
- [x] 4.2 C- 修飾付きキー実行の処理
- [x] 4.3 複数回の SPC 連続押下への対応（`SPC SPC c` → `C-c C-c`）

## 5. Leader モード終了処理

- [x] 5.1 コマンド実行後の自動終了
- [x] 5.2 `C-g` によるキャンセル
- [x] 5.3 `ESC` によるキャンセル
- [x] 5.4 元のモード（Selection/Insert）への復帰

## 6. モードラインインジケータ

- [x] 6.1 Leader モード用インジケータの追加（`[L:C-c]` など）
- [x] 6.2 C- 修飾待ち状態のインジケータ表示
- [x] 6.3 prefix 変更時のインジケータ更新

## 7. which-key 連携

- [x] 7.1 which-key の存在検出（`fboundp`/`boundp`）
- [x] 7.2 Leader 起動時の which-key ポップアップ表示
- [x] 7.3 prefix 切り替え時のポップアップ更新
- [x] 7.4 which-key 未インストール時の動作確認

## 8. キーバインド設定

- [x] 8.1 `eme-keybinds.el` に Selection Mode での `SPC` バインド追加
- [x] 8.2 `eme-insert-mode-map` に `M-SPC` バインド追加
- [x] 8.3 ミニバッファでの `M-SPC` 動作確認

## 9. テスト: Insert Start Modes

- [x] 9.1 Insert Start Mode でのバッファ開始モード確認テスト
- [x] 9.2 派生モード検出テスト（special-mode 派生）
- [x] 9.3 Insert Start Mode からの Selection Mode 遷移テスト

## 10. テスト: Leader Key

- [x] 10.1 Leader モード起動・終了テスト（Selection Mode）
- [x] 10.2 Leader モード起動・終了テスト（Insert Mode）
- [x] 10.3 Prefix 切り替えテスト（C-c → C-x → C-h）
- [x] 10.4 C- 修飾付与テスト
- [x] 10.5 ユーザーマップアクセステスト
- [x] 10.6 C-g/ESC キャンセルテスト

## 11. テスト: which-key 連携

- [x] 11.1 which-key 有効時のポップアップ表示テスト [手動]
- [x] 11.2 which-key 無効時の正常動作テスト

## 12. ドキュメント・統合

- [x] 12.1 `eme.el` での require 追加
- [x] 12.2 既存コードの cl-lib 使用チェック [要確認]
