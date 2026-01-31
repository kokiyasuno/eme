## ADDED Requirements

### Requirement: Leader Key の起動
システムは Leader Key を提供し、Emacs の prefix コマンドへのアクセスを可能にしなければならない（SHALL）。

#### Scenario: Selection Mode での Leader Key 起動
- **WHEN** ユーザーが Selection Mode で `SPC` を押す
- **THEN** Leader モードが起動しなければならない（SHALL）
- **THEN** デフォルトの prefix は `C-c` でなければならない（SHALL）

#### Scenario: Insert Mode での Leader Key 起動
- **WHEN** ユーザーが Insert Mode で `M-SPC` を押す
- **THEN** Leader モードが起動しなければならない（SHALL）
- **THEN** デフォルトの prefix は `C-c` でなければならない（SHALL）

#### Scenario: Leader Key のカスタマイズ
- **WHEN** ユーザーが `eme-leader-key` を `(kbd "\\")` に設定する
- **THEN** Selection Mode で `\` が Leader Key として機能しなければならない（SHALL）

### Requirement: Prefix の切り替え
システムは Leader モード内で prefix を切り替える機能を提供しなければならない（SHALL）。

#### Scenario: C-x prefix への切り替え
- **WHEN** ユーザーが Leader モードで `x` を押す
- **THEN** 現在の prefix が `C-x` に切り替わらなければならない（SHALL）
- **THEN** 次のキー入力は `C-x` prefix のコマンドを実行しなければならない（SHALL）

#### Scenario: C-h prefix への切り替え
- **WHEN** ユーザーが Leader モードで `h` を押す
- **THEN** 現在の prefix が `C-h` に切り替わらなければならない（SHALL）
- **THEN** 次のキー入力は `C-h` prefix のコマンドを実行しなければならない（SHALL）

#### Scenario: universal-argument の実行
- **WHEN** ユーザーが Leader モードで `u` を押す
- **THEN** `universal-argument` が実行されなければならない（SHALL）
- **THEN** Leader モードは終了しなければならない（SHALL）

### Requirement: C- 修飾の動的付与
システムは Leader モード内で次のキーに `C-` 修飾を付与する機能を提供しなければならない（SHALL）。

#### Scenario: SPC による C- 修飾
- **WHEN** ユーザーが Leader モードで `SPC` を押す
- **THEN** 次のキー入力に `C-` 修飾が付与されなければならない（SHALL）

#### Scenario: C-c C-c の実行
- **WHEN** ユーザーが Leader モードで `SPC c` を押す（`SPC` → Leader 起動、`SPC` → C- 修飾、`c` → キー）
- **THEN** `C-c C-c` が実行されなければならない（SHALL）

#### Scenario: C-x C-s の実行
- **WHEN** ユーザーが Leader モードで `x SPC s` を押す（`x` → C-x prefix、`SPC` → C- 修飾、`s` → キー）
- **THEN** `C-x C-s` が実行されなければならない（SHALL）

### Requirement: ユーザー定義キーマップ
システムは Leader モード内でユーザー定義キーマップへのアクセスを提供しなければならない（SHALL）。

#### Scenario: ユーザーマップへのアクセス
- **WHEN** ユーザーが Leader モードで `m` を押す
- **THEN** `eme-user-map` がアクティブになっていなければならない（SHALL）
- **THEN** 次のキー入力は `eme-user-map` 内のコマンドを実行しなければならない（SHALL）

#### Scenario: ユーザーマップへのバインド追加
- **WHEN** ユーザーが `(define-key eme-user-map "g" #'magit-status)` を評価する
- **THEN** Leader モードで `m g` が `magit-status` を実行しなければならない（SHALL）

### Requirement: Leader モードの終了
システムは Leader モードを終了する複数の方法を提供しなければならない（SHALL）。

#### Scenario: コマンド実行による終了
- **WHEN** ユーザーが Leader モードでコマンドを実行する
- **THEN** Leader モードは終了しなければならない（SHALL）
- **THEN** 元のモード（Selection Mode または Insert Mode）に戻らなければならない（SHALL）

#### Scenario: C-g によるキャンセル
- **WHEN** ユーザーが Leader モードで `C-g` を押す
- **THEN** Leader モードはキャンセルされなければならない（SHALL）
- **THEN** コマンドは実行されてはならない（SHALL NOT）
- **THEN** 元のモード（Selection Mode または Insert Mode）に戻らなければならない（SHALL）

#### Scenario: ESC によるキャンセル
- **WHEN** ユーザーが Leader モードで `ESC` を押す
- **THEN** Leader モードはキャンセルされなければならない（SHALL）

### Requirement: which-key 連携
システムは which-key がインストールされていれば自動的に連携しなければならない（SHALL）。

#### Scenario: which-key 有効時の候補表示
- **WHEN** which-key-mode が有効でユーザーが Leader モードに入る
- **THEN** 現在の prefix に対応するキー候補が表示されなければならない（SHALL）

#### Scenario: which-key 無効時の動作
- **WHEN** which-key がインストールされていない
- **THEN** Leader モードは候補表示なしで正常に動作しなければならない（SHALL）
- **THEN** エラーは発生してはならない（SHALL NOT）

#### Scenario: prefix 切り替え時の候補更新
- **WHEN** which-key が有効でユーザーが Leader モードで `x` を押す
- **THEN** 表示される候補は `C-x` prefix のコマンドに更新されなければならない（SHALL）

### Requirement: モードラインインジケータ
システムは Leader モード中にモードラインでステータスを表示しなければならない（SHALL）。

#### Scenario: Leader モードのインジケータ
- **WHEN** Leader モードがアクティブ
- **THEN** モードラインに現在の prefix を示すインジケータが表示されなければならない（SHALL）

#### Scenario: C- 修飾待ち状態のインジケータ
- **WHEN** Leader モードで `SPC` を押して C- 修飾待ち状態
- **THEN** モードラインインジケータが C- 待ち状態を反映しなければならない（SHALL）

### Requirement: ミニバッファでの動作
システムはミニバッファ内では Leader Key を使用可能にしつつ、通常入力を妨げてはならない（SHALL NOT）。

#### Scenario: ミニバッファでの M-SPC
- **WHEN** ユーザーがミニバッファ入力中に `M-SPC` を押す
- **THEN** Leader モードが起動しなければならない（SHALL）
- **THEN** Leader モード終了後、ミニバッファ入力に戻らなければならない（SHALL）
