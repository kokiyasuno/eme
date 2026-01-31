## ADDED Requirements

### Requirement: グローバルマイナーモードの有効化
システムは全バッファでモーダル編集を有効化するグローバルマイナーモード `emacs-modal-editing-mode` を提供しなければならない（SHALL）。

#### Scenario: グローバルモードを有効化
- **WHEN** ユーザーが `(emacs-modal-editing-mode 1)` を評価する
- **THEN** 既存および将来のすべてのバッファでモーダル編集が有効化されなければならない（SHALL）

#### Scenario: グローバルモードを無効化
- **WHEN** ユーザーが `(emacs-modal-editing-mode -1)` を評価する
- **THEN** すべてのバッファでモーダル編集が無効化され、Emacsは通常動作に戻らなければならない（SHALL）

### Requirement: バッファローカルモードの有効化
システムはバッファ単位で制御可能なバッファローカルマイナーモード `emacs-modal-editing-local-mode` を提供しなければならない（SHALL）。

#### Scenario: 単一バッファで有効化
- **WHEN** ユーザーがバッファ内で `(emacs-modal-editing-local-mode 1)` を評価する
- **THEN** そのバッファでのみモーダル編集が有効化されなければならない（SHALL）

#### Scenario: モード除外
- **WHEN** バッファのmajor-modeが `eme-excluded-modes` リストに含まれる
- **THEN** そのバッファではモーダル編集が自動的に有効化されてはならない（SHALL NOT）

### Requirement: デフォルトでSelection Mode
システムはモーダル編集有効化時にSelection Modeで開始しなければならない（SHALL）。

#### Scenario: 有効化時の初期モード
- **WHEN** バッファでモーダル編集が有効化される
- **THEN** バッファはSelection Modeになっていなければならない（SHALL）

#### Scenario: Selection Modeのモードラインインジケータ
- **WHEN** バッファがSelection Modeにある
- **THEN** モードラインに `[S]` インジケータが表示されなければならない（SHALL）

#### Scenario: Insert Modeのモードラインインジケータ
- **WHEN** バッファがInsert Modeにある
- **THEN** モードラインに `[I]` インジケータが表示されなければならない（SHALL）

### Requirement: Insert Modeへの遷移
システムは `i` キーでSelection ModeからInsert Modeへの遷移を可能にしなければならない（SHALL）。

#### Scenario: Insert Modeに入る
- **WHEN** ユーザーがSelection Modeで `i` を押す
- **THEN** バッファはInsert Modeに遷移しなければならない（SHALL）
- **THEN** カーソルタイプは `eme-insert-cursor-type` に変更されなければならない（SHALL）

#### Scenario: C-gでInsert Modeを抜ける
- **WHEN** ユーザーがInsert Modeで `C-g` を押す
- **THEN** バッファはSelection Modeに遷移しなければならない（SHALL）
- **THEN** カーソルタイプは `eme-selection-cursor-type` に変更されなければならない（SHALL）

#### Scenario: ESCでInsert Modeを抜ける
- **WHEN** ユーザーがInsert Modeで `ESC` を押す
- **THEN** バッファはSelection Modeに遷移しなければならない（SHALL）

### Requirement: キーマップの優先度
システムはモーダルキーマップが他のマイナーモードキーマップより高い優先度を持つことを保証しなければならない（SHALL）。

#### Scenario: キーマップの優先順位
- **WHEN** モーダル編集が有効で、他のマイナーモードが競合するキーを定義している
- **THEN** Selection Modeではモーダル編集キーマップが優先されなければならない（SHALL）

#### Scenario: Insert Modeでのパススルー
- **WHEN** バッファがInsert Modeにある
- **THEN** モード終了キー以外のすべてのキーはEmacsのデフォルト処理にパススルーされなければならない（SHALL）

### Requirement: ミニバッファの処理
システムはミニバッファでは自動的にInsert Modeを使用しなければならない（SHALL）。

#### Scenario: ミニバッファに入る
- **WHEN** ユーザーがミニバッファに入る（例：M-x、find-file）
- **THEN** 前のバッファのモードに関係なく、ミニバッファはInsert Modeでなければならない（SHALL）

#### Scenario: ミニバッファを抜ける
- **WHEN** ユーザーがミニバッファを抜ける
- **THEN** 前のバッファは元のモード状態を保持しなければならない（SHALL）

### Requirement: カーソルタイプのカスタマイズ
システムは各モードのカスタマイズ可能なカーソルタイプを提供しなければならない（SHALL）。

#### Scenario: Selection Modeのカスタムカーソル
- **WHEN** `eme-selection-cursor-type` が `box` に設定されている
- **THEN** Selection Modeではカーソルがボックス型で表示されなければならない（SHALL）

#### Scenario: Insert Modeのカスタムカーソル
- **WHEN** `eme-insert-cursor-type` が `bar` に設定されている
- **THEN** Insert Modeではカーソルが縦棒型で表示されなければならない（SHALL）

### Requirement: モードフック
システムはモード遷移のためのフックを提供しなければならない（SHALL）。

#### Scenario: Selection Mode入場フック
- **WHEN** バッファがSelection Modeに遷移する
- **THEN** `eme-selection-mode-enter-hook` が実行されなければならない（SHALL）

#### Scenario: Selection Mode退場フック
- **WHEN** バッファがSelection Modeを抜ける
- **THEN** `eme-selection-mode-exit-hook` が実行されなければならない（SHALL）

#### Scenario: Insert Mode入場フック
- **WHEN** バッファがInsert Modeに遷移する
- **THEN** `eme-insert-mode-enter-hook` が実行されなければならない（SHALL）

#### Scenario: Insert Mode退場フック
- **WHEN** バッファがInsert Modeを抜ける
- **THEN** `eme-insert-mode-exit-hook` が実行されなければならない（SHALL）
