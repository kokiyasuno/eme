## ADDED Requirements

### Requirement: Insert Start Modes
システムは特定のモードで Insert Mode から開始する機能を提供しなければならない（SHALL）。

#### Scenario: Insert Start Mode のバッファを開く
- **WHEN** ユーザーが `eme-insert-start-modes` に含まれるモードのバッファを開く
- **THEN** そのバッファは Insert Mode で開始しなければならない（SHALL）
- **THEN** eme-local-mode は有効でなければならない（SHALL）

#### Scenario: dired バッファでの Insert Start
- **WHEN** ユーザーが dired バッファを開く
- **THEN** バッファは Insert Mode で開始しなければならない（SHALL）
- **THEN** dired のキーバインド（例: `n` で次ファイル）がそのまま動作しなければならない（SHALL）

#### Scenario: magit バッファでの Insert Start
- **WHEN** ユーザーが magit-status バッファを開く
- **THEN** バッファは Insert Mode で開始しなければならない（SHALL）
- **THEN** magit のキーバインド（例: `s` でステージ）がそのまま動作しなければならない（SHALL）

#### Scenario: special-mode 派生モードの自動検出
- **WHEN** `eme-insert-start-modes` に `special-mode` が含まれる
- **THEN** `special-mode` から派生したすべてのモードが Insert Mode で開始しなければならない（SHALL）

#### Scenario: Insert Start Mode からの Selection Mode 遷移
- **WHEN** ユーザーが Insert Start Mode のバッファで `C-g` または `ESC` を押す
- **THEN** バッファは Selection Mode に遷移しなければならない（SHALL）
- **THEN** Selection Mode のキーバインドが有効になっていなければならない（SHALL）

#### Scenario: Insert Start Mode での Leader Key
- **WHEN** ユーザーが Insert Start Mode のバッファで `M-SPC` を押す
- **THEN** Leader モードが起動しなければならない（SHALL）
- **THEN** eme の機能（コピー、検索など）にアクセス可能でなければならない（SHALL）

#### Scenario: カスタム Insert Start Modes
- **WHEN** ユーザーが `eme-insert-start-modes` に `my-custom-mode` を追加する
- **THEN** `my-custom-mode` のバッファは Insert Mode で開始しなければならない（SHALL）

### Requirement: デフォルト Insert Start Modes
システムはデフォルトの Insert Start Modes リストを提供しなければならない（SHALL）。

#### Scenario: デフォルトリストの内容
- **WHEN** ユーザーが `eme-insert-start-modes` をカスタマイズしていない
- **THEN** 以下のモードがリストに含まれていなければならない（SHALL）:
  - `special-mode`（派生モードを含む）
  - `dired-mode`
  - `magit-mode` および関連モード
  - `vterm-mode`
  - `term-mode`
  - `eshell-mode`
  - `Info-mode`
  - `help-mode`
  - `compilation-mode`
  - `grep-mode`

## MODIFIED Requirements

### Requirement: バッファローカルモードの有効化
システムはバッファ単位で制御可能なバッファローカルマイナーモード `emacs-modal-editing-local-mode` を提供しなければならない（SHALL）。

#### Scenario: 単一バッファで有効化
- **WHEN** ユーザーがバッファ内で `(emacs-modal-editing-local-mode 1)` を評価する
- **THEN** そのバッファでのみモーダル編集が有効化されなければならない（SHALL）

#### Scenario: Insert Start Mode での有効化
- **WHEN** バッファのmajor-modeが `eme-insert-start-modes` リストに含まれる、または派生している
- **THEN** そのバッファではモーダル編集が有効化され、Insert Mode で開始しなければならない（SHALL）

#### Scenario: 通常モードでの有効化
- **WHEN** バッファのmajor-modeが `eme-insert-start-modes` リストに含まれていない
- **THEN** そのバッファではモーダル編集が有効化され、Selection Mode で開始しなければならない（SHALL）
