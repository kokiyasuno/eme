## MODIFIED Requirements

### Requirement: syntax-ppss による delimiter 検出
delimiter の検出は `eme-thing-bounds` API を使用しなければならない（SHALL）。内部的に tree-sitter または `syntax-ppss` が使用される。

#### Rationale
thing-provider への統合により、delimiter 検出ロジックを一元化する。

#### Scenario: emacs-lisp-mode での括弧検出
- **WHEN** バッファが `emacs-lisp-mode` で `(defun foo| () nil)` を含む
- **THEN** `t i` は `eme-thing-bounds` を使用して `defun foo () nil` を選択する

#### Scenario: python-mode での文字列検出
- **WHEN** バッファが `python-mode` で `'hello |world'` を含む
- **THEN** `t i` は `eme-thing-bounds` を使用して `hello world` を選択する（シングルクォート対応）

#### Scenario: 角括弧の検出
- **WHEN** バッファが `[item1, |item2]` を含む
- **THEN** `t i` は `eme-thing-bounds` を使用して `item1, item2` を選択する

#### Scenario: 波括弧の検出
- **WHEN** バッファが `{key: |value}` を含む
- **THEN** `t i` は `eme-thing-bounds` を使用して `key: value` を選択する
