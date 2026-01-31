## MODIFIED Requirements

### Requirement: delimiter 操作は eme-selection を使用
`eme-delimiter-inner`、`eme-delimiter-bounds`、および関連する操作は `eme-thing-bounds` API を使用して delimiter を検出しなければならない（SHALL）。

#### Rationale
thing-provider への統合により、delimiter 検出ロジックを一元化する。

#### Scenario: Inner 選択
- **GIVEN** カーソルが `"hello"` の `e` の位置にある
- **WHEN** `m` を押す
- **THEN** `(eme-thing-bounds 'delimiter)` を呼び出す
- **THEN** 返り値の `:inner` を使用して `hello` が eme-selection で選択される

#### Scenario: Bounds 選択
- **GIVEN** カーソルが `"hello"` の `e` の位置にある
- **WHEN** `M` を押す
- **THEN** `(eme-thing-bounds 'delimiter)` を呼び出す
- **THEN** 返り値の `:bounds` を使用して `"hello"` 全体が eme-selection で選択される

#### Scenario: 外側への拡張
- **GIVEN** `m` で inner 選択後
- **WHEN** `m` を再度押す
- **THEN** `(eme-thing-outer 'delimiter current-bounds)` を呼び出す
- **THEN** 外側の delimiter の inner が選択される
