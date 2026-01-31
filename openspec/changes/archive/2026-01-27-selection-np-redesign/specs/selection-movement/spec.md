## MODIFIED Requirements

### Requirement: 行移動はカラム保持付きで、アンカーがある場合のみ選択を拡張する
`eme-next-line` と `eme-previous-line` は次/前の行にカーソルを移動し、現在のカラム位置を保持しなければならない（SHALL）。region がアクティブな場合（アンカーが設定されている場合）は選択を拡張する。region が非アクティブの場合は単純にカーソルを移動する。

#### Scenario: アンカーなしでの次行移動（単純移動）
- **WHEN** region がアクティブでない
- **WHEN** カーソルが行1、カラム5にある
- **THEN** `n` はカーソルを行2、カラム5に移動する
- **THEN** region はアクティブにならない

#### Scenario: アンカーなしでの前行移動（単純移動）
- **WHEN** region がアクティブでない
- **WHEN** カーソルが行3、カラム10にある
- **THEN** `p` はカーソルを行2、カラム10に移動する
- **THEN** region はアクティブにならない

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

### Requirement: 選択方向の反転 (exchange)
`eme-exchange-point-and-mark` は region をアクティブに保ったまま point と mark の位置を入れ替えなければならない（SHALL）。選択範囲を変更せずにカーソルを選択の反対側に移動できる。

#### Scenario: アクティブな region で exchange
- **WHEN** mark が位置1、point が位置10にある
- **WHEN** region がアクティブである
- **THEN** `x` は point を位置1、mark を位置10に入れ替える
- **THEN** region はアクティブなままである
- **THEN** 選択範囲（1から10）は変わらない

#### Scenario: exchange でカーソル方向が反転する
- **WHEN** point が位置10（選択末尾）、mark が位置1（選択先頭）にある
- **THEN** `x` の後、point は位置1、mark は位置10になる
- **THEN** もう一度 `x` を押すと、point は位置10、mark は位置1に戻る

#### Scenario: region が非アクティブの場合
- **WHEN** region がアクティブでない
- **THEN** `x` は何もしない（エラーなし、変更なし）

### Requirement: replace-char のキーバインド
Selection Mode で `r` キーは `eme-replace-char` にバインドされなければならない（SHALL）。選択内の各文字をユーザーが入力した文字で置換する。

#### Scenario: replace-char のバインド
- **WHEN** `eme-selection-mode-map` で `r` を検索する
- **THEN** `eme-replace-char` が返る

#### Scenario: r で search-previous が呼ばれないこと
- **WHEN** Selection Mode で `r` を押す
- **THEN** `eme-replace-char` が呼ばれる（`eme-search-previous` ではない）

### Requirement: search-previous のキーバインド
Selection Mode で `S`（shift-s）キーは `eme-search-previous` にバインドされなければならない（SHALL）。最後の検索文字列で後方検索する。

#### Scenario: search-previous のバインド
- **WHEN** `eme-selection-mode-map` で `S` を検索する
- **THEN** `eme-search-previous` が返る

#### Scenario: search next/previous のペア
- **WHEN** `eme-selection-mode-map` で `s` を検索する
- **THEN** `eme-search-next` が返る
- **WHEN** `eme-selection-mode-map` で `S` を検索する
- **THEN** `eme-search-previous` が返る

### Requirement: exchange のキーバインド
Selection Mode で `x` キーは `eme-exchange-point-and-mark` にバインドされなければならない（SHALL）。

#### Scenario: exchange のバインド
- **WHEN** `eme-selection-mode-map` で `x` を検索する
- **THEN** `eme-exchange-point-and-mark` が返る

#### Scenario: x で replace-char が呼ばれないこと
- **WHEN** Selection Mode で `x` を押す
- **THEN** `eme-exchange-point-and-mark` が呼ばれる（`eme-replace-char` ではない）

### Requirement: extend 系行移動もカラムを保持する
`eme-extend-next-line` と `eme-extend-previous-line` は `line-move` を使用し、既存の選択を拡張する際にカラム位置を保持しなければならない（SHALL）。

#### Scenario: 次行への選択拡張
- **WHEN** アンカーが行1、カラム5に設定されている
- **WHEN** `.`（アンカートグル）を押してから extend-next-line を使用する
- **THEN** カーソルが行2、カラム5に移動する
- **THEN** region がアンカー（行1、カラム5）からカーソル（行2、カラム5）まで拡張される

#### Scenario: 前行への選択拡張
- **WHEN** アンカーが行3、カラム10に設定されている
- **WHEN** extend-previous-line を呼び出す
- **THEN** カーソルが行2、カラム10に移動する
- **THEN** region がアンカーからカーソルまで拡張される
