## MODIFIED Requirements

### Requirement: 単語移動はeme-selectionのみ使用
`eme-forward-word` と `eme-backward-word` は `eme-thing-bounds` API を使用して word の bounds を取得しなければならない（SHALL）。

#### Rationale
thing-provider への統合により、bounds 計算ロジックを一元化する。

#### Scenario: 前方単語移動
- **WHEN** カーソルが `hello| world foo` の位置にある
- **WHEN** `f` を押す
- **THEN** カーソルが `world` の末尾に移動する
- **THEN** `(eme-thing-bounds 'word)` を呼び出して bounds を取得する
- **THEN** eme-selection が `world` の範囲に設定される

#### Scenario: 後方単語移動
- **WHEN** カーソルが `hello world |foo` の位置にある
- **WHEN** `b` を押す
- **THEN** カーソルが `world` の先頭に移動する
- **THEN** `(eme-thing-bounds 'word)` を呼び出して bounds を取得する
- **THEN** eme-selection が `world` の範囲に設定される

### Requirement: sexp 移動は eme-thing-bounds を使用
`eme-forward-sexp` と `eme-backward-sexp` は `eme-thing-bounds` API を使用して sexp の bounds を取得しなければならない（SHALL）。

#### Scenario: 前方 sexp 移動
- **GIVEN** カーソルが `(foo| bar) (baz)` の位置にある
- **WHEN** `M-f` を押す
- **THEN** カーソルが次の sexp の末尾に移動する
- **THEN** `(eme-thing-bounds 'sexp)` を呼び出して bounds を取得する

#### Scenario: 後方 sexp 移動
- **GIVEN** カーソルが `(foo) |(bar baz)` の位置にある
- **WHEN** `M-b` を押す
- **THEN** カーソルが前の sexp の先頭に移動する
- **THEN** `(eme-thing-bounds 'sexp)` を呼び出して bounds を取得する

### Requirement: 行移動は eme-thing-bounds を使用
`eme-next-line` と `eme-previous-line` は `eme-thing-bounds` API を使用して line の bounds を取得しなければならない（SHALL）。

#### Scenario: 次行移動
- **WHEN** `n` を押す
- **THEN** カーソルが次の行に移動する
- **THEN** `(eme-thing-bounds 'line)` を呼び出して bounds を取得する
- **THEN** eme-selection が行全体に設定される

#### Scenario: 前行移動
- **WHEN** `p` を押す
- **THEN** カーソルが前の行に移動する
- **THEN** `(eme-thing-bounds 'line)` を呼び出して bounds を取得する
- **THEN** eme-selection が行全体に設定される
