## ADDED Requirements

### Requirement: 文字単位の移動と選択
システムは選択を作成する文字レベルの移動を提供しなければならない（SHALL ）。

#### Scenario: 前方文字移動
- **WHEN** ユーザーが Selection Mode で `f` を押す
- **THEN** ポイントは1文字前方に移動しなければならない（SHALL ）
- **THEN** リージョンは前の位置から新しい位置まで有効になっていなければならない（SHALL ）

#### Scenario: 後方文字移動
- **WHEN** ユーザーが Selection Mode で `b` を押す
- **THEN** ポイントは1文字後方に移動しなければならない（SHALL ）
- **THEN** リージョンは前の位置から新しい位置まで有効になっていなければならない（SHALL ）

### Requirement: 単語単位の移動と選択
システムは大文字キーで単語レベルの移動を提供しなければならない（SHALL ）。

#### Scenario: 前方単語移動
- **WHEN** ユーザーが Selection Mode で `F` を押す
- **THEN** ポイントは1単語前方に移動しなければならない（SHALL ）
- **THEN** リージョンは前の位置から新しい位置まで有効になっていなければならない（SHALL ）

#### Scenario: 後方単語移動
- **WHEN** ユーザーが Selection Mode で `B` を押す
- **THEN** ポイントは1単語後方に移動しなければならない（SHALL ）
- **THEN** リージョンは前の位置から新しい位置まで有効になっていなければならない（SHALL ）

### Requirement: S 式単位の移動と選択
システムは Meta 修飾キーで S 式レベルの移動を提供しなければならない（SHALL ）。

#### Scenario: 前方 S 式移動
- **WHEN** ユーザーが Selection Mode で `M-f` を押す
- **THEN** ポイントは1つの S 式分前方に移動しなければならない（SHALL ）
- **THEN** リージョンは前の位置から新しい位置まで有効になっていなければならない（SHALL ）

#### Scenario: 後方 S 式移動
- **WHEN** ユーザーが Selection Mode で `M-b` を押す
- **THEN** ポイントは1つの S 式分後方に移動しなければならない（SHALL ）
- **THEN** リージョンは前の位置から新しい位置まで有効になっていなければならない（SHALL ）

### Requirement: 行単位の移動と選択
システムは行レベルの移動を提供しなければならない（SHALL ）。

#### Scenario: 次の行
- **WHEN** ユーザーが Selection Mode で `n` を押す
- **THEN** ポイントは次の行に移動しなければならない（SHALL ）
- **THEN** リージョンは行を選択して有効になっていなければならない（SHALL ）

#### Scenario: 前の行
- **WHEN** ユーザーが Selection Mode で `p` を押す
- **THEN** ポイントは前の行に移動しなければならない（SHALL ）
- **THEN** リージョンは行を選択して有効になっていなければならない（SHALL ）

### Requirement: 段落単位の移動と選択
システムは大文字キーで段落レベルの移動を提供しなければならない（SHALL ）。

#### Scenario: 次の段落
- **WHEN** ユーザーが Selection Mode で `N` を押す
- **THEN** ポイントは次の段落に移動しなければならない（SHALL ）
- **THEN** リージョンは前の位置から新しい位置まで有効になっていなければならない（SHALL ）

#### Scenario: 前の段落
- **WHEN** ユーザーが Selection Mode で `P` を押す
- **THEN** ポイントは前の段落に移動しなければならない（SHALL ）
- **THEN** リージョンは前の位置から新しい位置まで有効になっていなければならない（SHALL ）

### Requirement: defun 単位の移動と選択
システムは Meta 修飾キーで defun レベルの移動を提供しなければならない（SHALL ）。

#### Scenario: 次の defun
- **WHEN** ユーザーが Selection Mode で `M-n` を押す
- **THEN** ポイントは次の defun に移動しなければならない（SHALL ）
- **THEN** リージョンは前の位置から新しい位置まで有効になっていなければならない（SHALL ）

#### Scenario: 前の defun
- **WHEN** ユーザーが Selection Mode で `M-p` を押す
- **THEN** ポイントは前の defun に移動しなければならない（SHALL ）
- **THEN** リージョンは前の位置から新しい位置まで有効になっていなければならない（SHALL ）

### Requirement: 行境界への移動
システムは行境界への移動を提供しなければならない（SHALL ）。

#### Scenario: 行頭
- **WHEN** ユーザーが Selection Mode で `a` を押す
- **THEN** ポイントは行頭に移動しなければならない（SHALL ）
- **THEN** リージョンは前の位置から新しい位置まで有効になっていなければならない（SHALL ）

#### Scenario: 行末
- **WHEN** ユーザーが Selection Mode で `e` を押す
- **THEN** ポイントは行末に移動しなければならない（SHALL ）
- **THEN** リージョンは前の位置から新しい位置まで有効になっていなければならない（SHALL ）

### Requirement: 文境界への移動
システムは大文字キーで文境界への移動を提供しなければならない（SHALL ）。

#### Scenario: 文頭
- **WHEN** ユーザーが Selection Mode で `A` を押す
- **THEN** ポイントは文頭に移動しなければならない（SHALL ）
- **THEN** リージョンは前の位置から新しい位置まで有効になっていなければならない（SHALL ）

#### Scenario: 文末
- **WHEN** ユーザーが Selection Mode で `E` を押す
- **THEN** ポイントは文末に移動しなければならない（SHALL ）
- **THEN** リージョンは前の位置から新しい位置まで有効になっていなければならない（SHALL ）

### Requirement: defun 境界への移動
システムは Meta 修飾キーで defun 境界への移動を提供しなければならない（SHALL ）。

#### Scenario: defun 先頭
- **WHEN** ユーザーが Selection Mode で `M-a` を押す
- **THEN** ポイントは defun 先頭に移動しなければならない（SHALL ）
- **THEN** リージョンは前の位置から新しい位置まで有効になっていなければならない（SHALL ）

#### Scenario: defun 末尾
- **WHEN** ユーザーが Selection Mode で `M-e` を押す
- **THEN** ポイントは defun 末尾に移動しなければならない（SHALL ）
- **THEN** リージョンは前の位置から新しい位置まで有効になっていなければならない（SHALL ）

### Requirement: 選択拡張のためのアンカー
システムは選択を拡張するためのアンカー機構を提供しなければならない（SHALL ）。

#### Scenario: アンカーを設定
- **WHEN** ユーザーがアクティブなリージョンなしで Selection Mode で `.` を押す
- **THEN** 現在のポイントにマークが設定されなければならない（SHALL ）
- **THEN** 続く移動はアンカーから選択を拡張しなければならない（SHALL ）

#### Scenario: アンカーをオフに切り替え
- **WHEN** ユーザーがアクティブなリージョンありで Selection Mode で `.` を押す
- **THEN** リージョンは非アクティブ化されなければならない（SHALL ）
- **THEN** 続く移動は新しい選択を作成しなければならない（SHALL ）

#### Scenario: 選択中のアンカー
- **WHEN** ユーザーがアクティブな選択を持ち `.` を押す
- **THEN** 現在のポイントにアンカーが設定されなければならない（SHALL ）
- **THEN** 続く移動は新しいアンカー位置から拡張しなければならない（SHALL ）

### Requirement: アンカーなしの移動後の選択
システムはアンカーなしの移動で一時的な選択を作成しなければならない（SHALL ）。

#### Scenario: 一時的な選択
- **WHEN** ユーザーがアンカー設定なしで `f` を押す
- **THEN** リージョンは1文字に対して有効になっていなければならない（SHALL ）
- **WHEN** ユーザーが再度 `f` を押す
- **THEN** 前の選択は新しい1文字選択で置き換えられなければならない（SHALL ）

### Requirement: バッファ境界での移動
システムはバッファ境界を優雅に処理しなければならない（SHALL ）。

#### Scenario: バッファ末尾での前方移動
- **WHEN** ユーザーがバッファ末尾で `f` を押す
- **THEN** ポイントはバッファ末尾に留まらなければならない（SHALL ）
- **THEN** エラーはシグナルされてはならない（SHALL NOT ）

#### Scenario: バッファ先頭での後方移動
- **WHEN** ユーザーがバッファ先頭で `b` を押す
- **THEN** ポイントはバッファ先頭に留まらなければならない（SHALL）
- **THEN** エラーはシグナルされてはならない（SHALL NOT）
