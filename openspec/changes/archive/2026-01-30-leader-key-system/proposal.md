## Why

現状の eme は Selection Mode と Insert Mode の切り替えで動作するが、Insert Mode からは Selection Mode に戻らないと eme のコマンドを使えない。また、dired や magit のような特殊バッファでは既存のキーバインドを活かしつつ、必要に応じて eme の機能（コピー、検索など）を使いたいケースがある。Leader Key システムを導入することで、どのモードからでも統一的に Emacs コマンドや eme 機能にアクセスできるようにする。

## What Changes

- **Leader Key の導入**: Selection Mode では `SPC`、Insert Mode では `M-SPC` で Leader モードに入り、Emacs の prefix キー（C-c, C-x, C-h）にアクセス可能にする
- **C- 修飾の動的付与**: Leader モード内で `SPC` を押すと次のキーに `C-` 修飾を付与（例: `SPC SPC w` → `C-c C-w`）
- **Prefix 切り替えキー**: `x` で C-x、`h` で C-h、`u` で C-u (universal-argument) に切り替え
- **ユーザー定義キーマップ**: `m` キーで `eme-user-map` に入り、ユーザーが自由に定義したキーバインドを使用可能
- **Insert Start Modes**: special-mode 派生、vterm-mode、dired-mode、magit-mode 等は Insert Mode で開始し、既存キーバインドを維持
- **which-key 自動連携**: which-key がインストールされていれば Leader Key 押下後に候補を自動表示（依存関係ではない）
- **BREAKING**: `eme-excluded-modes` の概念を廃止し、`eme-insert-start-modes` に置き換え（全モードで eme 有効、特定モードは Insert 開始）

## Capabilities

### New Capabilities
- `leader-key`: Leader Key システムの実装。SPC/M-SPC による Leader モード起動、prefix 切り替え、C- 修飾付与、ユーザー定義キーマップへのアクセス

### Modified Capabilities
- `mode-system`: Insert Start Modes の追加。`eme-excluded-modes` を `eme-insert-start-modes` に変更し、特定モードを Insert Mode で開始する仕組みを追加

## Impact

- **コード変更**:
  - `eme-core.el`: `eme-excluded-modes` → `eme-insert-start-modes` への変更、Leader Key 関連カスタム変数の追加
  - 新規 `eme-leader.el`: Leader Key システムの実装
  - `eme-keybinds.el`: Leader Key のバインド追加

- **ユーザー設定への影響**:
  - `eme-excluded-modes` を使用しているユーザーは `eme-insert-start-modes` への移行が必要
  - 既存の `SPC` キーバインド（Selection Mode）がある場合は上書きされる

- **依存関係**:
  - which-key: 依存なし（インストール済みなら自動検出して連携）

- **パフォーマンス**:
  - Leader Key の transient keymap 管理による軽微なオーバーヘッドのみ

- **ロールバック手順**:
  - `eme-leader.el` を削除し、`eme-core.el` の変数を元に戻す
  - `eme-insert-start-modes` → `eme-excluded-modes` に戻す
