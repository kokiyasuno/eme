## MODIFIED Requirements

### Requirement: delimiter 操作は eme-selection を使用
`eme-delimiter-inner`、`eme-delimiter-bounds`、および関連する操作は Emacs region ではなく eme-selection を使用しなければならない（SHALL）。

#### Rationale
選択システム全体を eme-selection に統一するため。

#### Scenario: Inner 選択
- **GIVEN** カーソルが `"hello"` の `e` の位置にある
- **WHEN** `m` を押す
- **THEN** `hello` が eme-selection で選択される
- **THEN** `push-mark` は呼び出されない

#### Scenario: Bounds 選択
- **GIVEN** カーソルが `"hello"` の `e` の位置にある
- **WHEN** `M` を押す
- **THEN** `"hello"` 全体が eme-selection で選択される
- **THEN** `push-mark` は呼び出されない

### Requirement: 選択状態の確認方法変更
delimiter 操作は `region-active-p` の代わりに `eme-selection-active-p` を使用しなければならない（SHALL）。

#### Scenario: wrap モード判定
- **GIVEN** 選択が存在し delimiter context がない
- **WHEN** `m` を押す
- **WHEN** wrap モードに入るか判定する
- **THEN** `eme-selection-active-p` が使用される
- **THEN** `region-active-p` は使用されない

### Requirement: 操作完了後の選択クリア
delimiter 操作（削除、変更）完了後は `eme-selection-clear` を使用しなければならない（SHALL）。

#### Scenario: Delimiter 削除後のクリア
- **GIVEN** `m` で inner 選択している
- **WHEN** 同じ delimiter キーを押して delimiter を削除する
- **THEN** 操作完了後 `eme-selection-clear` が呼び出される
- **THEN** `deactivate-mark` は呼び出されない

#### Scenario: Delimiter 変更後のクリア
- **GIVEN** `m` で inner 選択している
- **WHEN** 異なる delimiter キーを押して delimiter を変更する
- **THEN** 操作完了後 `eme-selection-clear` が呼び出される
- **THEN** `deactivate-mark` は呼び出されない

### Requirement: wrap 操作での選択設定
wrap 操作は eme-selection を使用しなければならない（SHALL）。

#### Scenario: Wrap
- **GIVEN** `f` で `hello` が eme-selection で選択されている
- **WHEN** `m` を押す
- **WHEN** `"` を押す
- **THEN** `"hello"` になる
- **THEN** 選択がクリアされる（`eme-selection-clear` 使用）
