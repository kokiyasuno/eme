## ADDED Requirements (Infrastructure)

### Requirement: 独自選択機構（eme-selection）
emeはEmacsのmark/pointとは独立した独自の選択機構を実装しなければならない（SHALL）。これにより、カーソル位置と選択範囲を独立して管理できる。

#### Rationale
Emacsのregionはmarkからpointまでの範囲で定義される。そのため「カーソルがカラム5にある」かつ「行全体が選択される」という要件を同時に満たせない。独自選択機構により、カーソル（point）の位置と選択範囲を分離する。

#### Scenario: 選択範囲の独立管理
- **GIVEN** 独自選択機構が有効である
- **WHEN** 行全体を選択する操作を行う
- **THEN** 選択範囲は行頭から行末まで設定される
- **THEN** カーソル（point）は任意の位置（例：カラム5）に配置できる

#### Scenario: 選択の視覚的表示
- **GIVEN** eme-selectionが設定されている
- **THEN** 選択範囲はoverlayでハイライト表示される
- **THEN** ハイライトはEmacsのregion faceと同様に見える

#### Scenario: アクションは独自選択に対して動作
- **GIVEN** eme-selectionが `(10 . 50)` に設定されている
- **WHEN** `d`（delete）を実行する
- **THEN** 位置10から50までのテキストが削除される
- **THEN** point位置は関係ない

#### Scenario: アンカー設定時はEmacs regionを使用
- **GIVEN** `.` でアンカーを設定している
- **THEN** Emacsの標準region（mark/point）が使用される
- **THEN** eme-selectionは無効になる

### Requirement: eme-selection API
独自選択機構は以下のAPIを提供しなければならない（SHALL）。

#### API: eme-selection-active-p
- **RETURNS** eme-selectionがアクティブならt、そうでなければnil

#### API: eme-selection-bounds
- **RETURNS** 選択範囲を `(START . END)` のconsセルで返す
- **RETURNS** 選択がなければnil

#### API: eme-selection-set (start end)
- **EFFECT** 選択範囲をSTARTからENDに設定する
- **EFFECT** overlayを更新してハイライト表示する

#### API: eme-selection-clear
- **EFFECT** 選択を解除する
- **EFFECT** overlayを削除する

## MODIFIED Requirements

### Requirement: 行移動はカラム保持付きで、アンカーがある場合のみ選択を拡張する
`eme-next-line` と `eme-previous-line` は次/前の行にカーソルを移動し、現在のカラム位置を保持しなければならない（SHALL）。region がアクティブでない場合、行全体を選択する（fresh selection）。region がアクティブな場合（アンカーが設定されている場合）は選択を拡張する。

#### Scenario: アンカーなしでの次行移動（行選択）
- **WHEN** region がアクティブでない
- **WHEN** カーソルが行1、カラム5にある
- **THEN** `n` はカーソルを行2、カラム5に移動する
- **THEN** 行2の行頭から行末までが選択される（fresh selection）

#### Scenario: アンカーなしでの前行移動（行選択）
- **WHEN** region がアクティブでない
- **WHEN** カーソルが行3、カラム10にある
- **THEN** `p` はカーソルを行2、カラム10に移動する
- **THEN** 行2の行頭から行末までが選択される（fresh selection）

#### Scenario: アンカーありでの次行移動（選択拡張）
- **WHEN** `.` でアンカーを行1、カラム5に設定する
- **WHEN** `n` を押す
- **THEN** カーソルが行2、カラム5に移動する
- **THEN** region がアンカー（行1、カラム5）からカーソル（行2、カラム5）までアクティブになる

#### Scenario: アンカーありで連続移動すると選択が拡張される
- **WHEN** `.` でアンカーを行1に設定する
- **WHEN** `n` を2回押す
- **THEN** カーソルが行3に移動する
- **THEN** region がアンカー（行1）からカーソル（行3）まで拡張される

#### Scenario: 短い行ではカラムがクランプされる
- **WHEN** カーソルが20文字の行1、カラム15にある
- **WHEN** 行2は5文字しかない
- **THEN** `n` はカーソルを行2、行末に移動する

#### Scenario: バッファ末尾での境界処理
- **WHEN** カーソルがバッファの最終行にある
- **THEN** `n` はエラーを発生させない
- **THEN** カーソルは最終行に留まる

#### Scenario: バッファ先頭での境界処理
- **WHEN** カーソルがバッファの最初の行にある
- **THEN** `p` はエラーを発生させない
- **THEN** カーソルは最初の行に留まる

## ADDED Requirements

### Requirement: 単語移動（f/b キー）
`eme-forward-word` と `eme-backward-word` は前方/後方に1単語移動し、移動した単語のみを選択しなければならない（SHALL）。移動経路（空白、改行）は選択に含まない。

#### Rationale
移動した単語のみを選択することで、予測可能な動作を実現する。経路を含めると行をまたぐ際に意図しない範囲が選択される。

#### Scenario: 前方単語移動
- **WHEN** カーソルが `hello| world foo` の位置にある
- **WHEN** `f` を押す
- **THEN** カーソルが `world` の末尾に移動する
- **THEN** `world` が選択される（fresh selection）

#### Scenario: 後方単語移動
- **WHEN** カーソルが `hello world |foo` の位置にある
- **WHEN** `b` を押す
- **THEN** カーソルが `world` の先頭に移動する
- **THEN** `world` が選択される（fresh selection）

#### Scenario: 行をまたぐ単語移動
- **WHEN** カーソルが行末にある
- **WHEN** `f` を押す
- **THEN** 次行の最初の単語に移動する
- **THEN** その単語のみが選択される（改行や空白は含まない）

#### Scenario: バッファ末尾での単語移動
- **WHEN** カーソルがバッファの最後の単語の後にある
- **WHEN** `f` を押す
- **THEN** エラーを発生させない
- **THEN** カーソル位置は変化しない

#### Scenario: アンカーありでの単語移動（選択拡張）
- **WHEN** `.` でアンカーを設定している
- **WHEN** `f` を押す
- **THEN** 選択がアンカーから移動先まで拡張される

#### Scenario: f/b のキーバインド
- **WHEN** `eme-selection-mode-map` で `f` を検索する
- **THEN** `eme-forward-word` が返る
- **WHEN** `eme-selection-mode-map` で `b` を検索する
- **THEN** `eme-backward-word` が返る

### Requirement: F/B キーは未割り当て
`F` と `B` キーは Phase 3 の jump 機能のために予約され、現時点では未割り当てとしなければならない（SHALL）。

#### Scenario: F キーの未割り当て確認
- **WHEN** `eme-selection-mode-map` で `F` を検索する
- **THEN** nil が返る（バインドなし）

#### Scenario: B キーの未割り当て確認
- **WHEN** `eme-selection-mode-map` で `B` を検索する
- **THEN** nil が返る（バインドなし）

### Requirement: 全移動コマンドの統一選択モデル
すべての移動コマンドは、アンカーが設定されていない場合は fresh selection を作成し、`.` でアンカーが明示的に設定されている場合のみ選択を拡張しなければならない（SHALL）。

#### Rationale
連続した移動（f,f など）で意図せず選択が拡張されるのを防ぐ。選択拡張は明示的な操作（`.` でアンカー設定）を要求する。

#### Scenario: f での fresh selection
- **WHEN** アンカーが設定されていない
- **WHEN** `f` を押す
- **THEN** 移動先の単語が選択される（新しい selection）

#### Scenario: f,f での連続 fresh selection
- **WHEN** アンカーが設定されていない
- **WHEN** `f` を2回押す
- **THEN** 2回目の `f` で移動した単語のみが選択される（1回目の選択は上書きされる）

#### Scenario: f での選択拡張
- **WHEN** `.` でアンカーを設定している
- **WHEN** `f` を押す
- **THEN** アンカーから移動先まで選択が拡張される

#### Scenario: n での fresh selection（行選択）
- **WHEN** アンカーが設定されていない
- **WHEN** `n` を押す
- **THEN** 移動先の行が選択される（行頭から行末まで）

#### Scenario: a での fresh selection
- **WHEN** アンカーが設定されていない
- **WHEN** `a` を押す
- **THEN** 行頭までが選択される

#### Scenario: e での fresh selection
- **WHEN** アンカーが設定されていない
- **WHEN** `e` を押す
- **THEN** 行末までが選択される

### Requirement: 文字移動の廃止
文字単位の移動コマンドは Selection Mode では提供しない（SHALL NOT）。文字移動は Insert Mode の `C-f`/`C-b` で代用する。

#### Scenario: 文字移動コマンドの不存在
- **WHEN** `eme-forward-char` を検索する
- **THEN** そのようなコマンドは存在しない

#### Scenario: Insert Mode での文字移動
- **WHEN** Insert Mode で `C-f` を押す
- **THEN** Emacs 標準の `forward-char` が実行される
