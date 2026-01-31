## ADDED Requirements

### Requirement: 検索の開始
システムは `/` でインクリメンタル検索を開始しなければならない（SHALL）。

#### Scenario: 検索を開始
- **WHEN** ユーザーがSelection Modeで `/` を押す
- **THEN** isearch-forwardが開始されなければならない（SHALL）
- **THEN** ユーザーは検索パターンの入力を求められなければならない（SHALL）

#### Scenario: 選択で検索
- **WHEN** ユーザーがリージョンを選択して `/` を押す
- **THEN** isearchは選択されたテキストを初期パターンとして開始されなければならない（SHALL）

### Requirement: 次のマッチへのナビゲーション
システムは `s` で次のマッチに移動しなければならない（SHALL）。

#### Scenario: 検索中の次のマッチ
- **WHEN** ユーザーがisearch中に `s` を押す
- **THEN** ポイントは次のマッチに移動しなければならない（SHALL）

#### Scenario: 検索後の次のマッチ
- **WHEN** ユーザーが検索を完了し、Selection Modeで `s` を押す
- **THEN** ポイントは最後の検索パターンの次の出現箇所に移動しなければならない（SHALL）
- **THEN** マッチは選択されなければならない（SHALL）

### Requirement: 前のマッチへのナビゲーション
システムは `r` で前のマッチに移動しなければならない（SHALL）。

#### Scenario: 検索中の前のマッチ
- **WHEN** ユーザーがisearch中に `r` を押す
- **THEN** ポイントは前のマッチに移動しなければならない（SHALL）

#### Scenario: 検索後の前のマッチ
- **WHEN** ユーザーが検索を完了し、Selection Modeで `r` を押す
- **THEN** ポイントは最後の検索パターンの前の出現箇所に移動しなければならない（SHALL）
- **THEN** マッチは選択されなければならない（SHALL）

### Requirement: 選択での検索終了
システムは検索終了時にマッチを選択しなければならない（SHALL）。

#### Scenario: RETで検索終了
- **WHEN** ユーザーがisearch中に `RET` を押す
- **THEN** isearchは終了しなければならない（SHALL）
- **THEN** マッチしたテキストはリージョンとして選択されなければならない（SHALL）

#### Scenario: C-gで検索終了
- **WHEN** ユーザーがisearch中に `C-g` を押す
- **THEN** isearchはキャンセルされなければならない（SHALL）
- **THEN** ポイントは元の位置に戻らなければならない（SHALL）
- **THEN** リージョンはアクティブであってはならない（SHALL NOT）

### Requirement: 正規表現検索のサポート
システムは正規表現検索をサポートしなければならない（SHALL）。

#### Scenario: 正規表現検索
- **WHEN** ユーザーがSelection Modeで `M-/` を押す
- **THEN** isearch-forward-regexpが開始されなければならない（SHALL）

### Requirement: 大文字小文字の区別
システムはEmacsのcase-fold-search設定を尊重しなければならない（SHALL）。

#### Scenario: 大文字小文字を区別しない検索
- **WHEN** `case-fold-search` が t でユーザーが "Hello" を検索する
- **THEN** "hello"、"HELLO"、"Hello" はすべてマッチしなければならない（SHALL）

#### Scenario: 大文字小文字を区別する検索
- **WHEN** 検索パターンに大文字が含まれ `case-fold-search` が nil
- **THEN** 完全に一致する大文字小文字のマッチのみが見つかるべきである（SHALL）

### Requirement: 検索履歴
システムはisearch履歴と統合しなければならない（SHALL）。

#### Scenario: 検索履歴にアクセス
- **WHEN** ユーザーが検索入力中に `M-p` を押す
- **THEN** 前の検索パターンが呼び出されなければならない（SHALL）

### Requirement: マッチなしの処理
システムはマッチなしのシナリオを優雅に処理しなければならない（SHALL）。

#### Scenario: マッチが見つからない
- **WHEN** ユーザーがマッチのないパターンを検索する
- **THEN** ユーザーに「マッチなし」と通知されなければならない（SHALL）
- **THEN** ポイントは元の位置に留まらなければならない（SHALL）

#### Scenario: 末尾での次のマッチ
- **WHEN** ユーザーが最後のマッチで `s` を押す
- **THEN** 検索は最初のマッチにラップしなければならない（SHALL）
- **THEN** ユーザーにラップを通知しなければならない（SHALL）
