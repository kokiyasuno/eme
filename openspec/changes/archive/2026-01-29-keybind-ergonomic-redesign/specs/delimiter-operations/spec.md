## ADDED Requirements

### Requirement: Delimiter 文字の設定
システムは delimiter として認識する文字をカスタマイズ可能にしなければならない（SHALL）。

#### Specification: eme-delimiter-chars
```elisp
(defcustom eme-delimiter-chars '(?\" ?' ?\( ?\) ?\[ ?\] ?\{ ?\} ?`)
  "Characters recognized as delimiters for `m` operations.
Note: Adding action keys (d, w, c, etc.) here is meaningless
as they will be interpreted as actions, not delimiters."
  :type '(repeat character)
  :group 'eme)
```

### Requirement: Inner 選択（m キー）
`m` キーは最も近い delimiter の内側を選択し、delimiter context を記憶しなければならない（SHALL）。

#### Rationale
delimiter context を記憶することで、後続のキー入力で delimiter 操作（削除・変更）を 2ストロークで実現する。

#### Scenario: Inner 選択
- **GIVEN** カーソルが `"hello"` の `e` の位置にある
- **WHEN** `m` を押す
- **THEN** `hello` が選択される（delimiter は含まない）
- **THEN** delimiter context に `"` が記憶される

#### Scenario: ネストした delimiter での inner 選択
- **GIVEN** カーソルが `("hello")` の `e` の位置にある
- **WHEN** `m` を押す
- **THEN** 最も近い delimiter `"` の inner が選択される
- **THEN** `hello` が選択される

### Requirement: Bounds 選択（M キー）
`M` キーは最も近い delimiter を含む全体（bounds）を選択しなければならない（SHALL）。

#### Scenario: Bounds 選択
- **GIVEN** カーソルが `"hello"` の `e` の位置にある
- **WHEN** `M` を押す
- **THEN** `"hello"` 全体が選択される（delimiter を含む）

### Requirement: Delimiter 削除（m + 同じ delimiter）
`m` で選択後、同じ delimiter キーを押すと delimiter のみが削除されなければならない（SHALL）。

#### Rationale
2ストロークで delimiter 削除を実現する。

#### Scenario: Delimiter 削除
- **GIVEN** カーソルが `"hello"` の中にある
- **WHEN** `m` を押す
- **THEN** `hello` が選択され、context に `"` が記憶される
- **WHEN** `"` を押す
- **THEN** delimiter `"` が削除され、`hello` のみが残る
- **THEN** 選択は解除される

### Requirement: Delimiter 変更（m + 違う delimiter）
`m` で選択後、異なる delimiter キーを押すと delimiter が変更されなければならない（SHALL）。

#### Rationale
2ストロークで delimiter 変更を実現する。

#### Scenario: Delimiter 変更
- **GIVEN** カーソルが `"hello"` の中にある
- **WHEN** `m` を押す
- **THEN** `hello` が選択され、context に `"` が記憶される
- **WHEN** `(` を押す
- **THEN** `"hello"` が `(hello)` に変更される
- **THEN** 選択は解除される

### Requirement: 内容削除（m + d）
`m` で選択後、`d` を押すと内容が削除されなければならない（SHALL）。Delimiter は残る。

#### Scenario: 内容削除
- **GIVEN** カーソルが `"hello"` の中にある
- **WHEN** `m` を押す
- **THEN** `hello` が選択される
- **WHEN** `d` を押す
- **THEN** `hello` が削除され、`""` が残る

### Requirement: Wrap（選択 + m + delimiter）
選択が存在し delimiter context がない状態で `m` を押すと wrap モードに入り、次の delimiter キーで選択範囲を囲まなければならない（SHALL）。

#### Rationale
3ストロークで wrap を実現する。頻度が低い操作なので許容範囲。

#### Scenario: Wrap
- **GIVEN** `f` で `hello` が選択されている（delimiter context なし）
- **WHEN** `m` を押す
- **THEN** wrap モードに入る（次の delimiter を待つ）
- **WHEN** `"` を押す
- **THEN** `"hello"` になる

#### Scenario: Wrap with different delimiter
- **GIVEN** `v` で `foo + bar` が選択されている
- **WHEN** `m` を押す
- **WHEN** `(` を押す
- **THEN** `(foo + bar)` になる

### Requirement: m 連打での拡大
`m` を連続して押すと、外側の delimiter に拡大しなければならない（SHALL）。

#### Scenario: m 連打での拡大
- **GIVEN** カーソルが `("hello")` の `e` の位置にある
- **WHEN** `m` を押す
- **THEN** `hello` が選択される（`"` の inner）
- **WHEN** `m` をもう一度押す
- **THEN** `"hello"` が選択される（`(` の inner）
- **WHEN** `m` をもう一度押す
- **THEN** さらに外側があれば拡大、なければそのまま

## ストローク数まとめ

| 操作 | キー | ストローク |
|------|------|-----------|
| Inner 選択 | `m` | 1 |
| Bounds 選択 | `M` | 1 |
| Delimiter 削除 | `m` + `"` | 2 |
| Delimiter 変更 | `m` + `(` | 2 |
| 内容削除 | `m` + `d` | 2 |
| Wrap | 選択 + `m` + `"` | 3 |
