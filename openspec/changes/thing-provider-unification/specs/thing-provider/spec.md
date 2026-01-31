## ADDED Requirements

### Requirement: thing provider registry
`eme-thing-providers` は thing type と provider 関数のマッピングを保持する alist でなければならない（SHALL）。

#### Scenario: デフォルトの provider 登録
- **GIVEN** eme-thing.el がロードされている
- **WHEN** `eme-thing-providers` を参照する
- **THEN** word, symbol, sexp, sexp-chain, line, sentence, paragraph, defun, delimiter, string, list, buffer が登録されている

#### Scenario: ユーザーによる provider 追加
- **GIVEN** `my-thing--custom` という関数が定義されている
- **WHEN** `(add-to-list 'eme-thing-providers '(custom . my-thing--custom))` を実行する
- **THEN** `(eme-thing-bounds 'custom)` が `my-thing--custom` を呼び出す

### Requirement: eme-thing-bounds 関数
`eme-thing-bounds` は指定された thing の bounds を plist 形式で返さなければならない（SHALL）。

#### Scenario: word の bounds 取得
- **GIVEN** カーソルが `hello| world` の位置にある
- **WHEN** `(eme-thing-bounds 'word)` を呼び出す
- **THEN** `(:type word :bounds (0 . 5) :inner nil)` のような plist が返る

#### Scenario: delimiter の bounds 取得（自動検出）
- **GIVEN** カーソルが `(hello| world)` の位置にある
- **WHEN** `(eme-thing-bounds 'delimiter)` を呼び出す
- **THEN** `(:type delimiter :char ?\( :bounds (0 . 13) :inner (1 . 12))` のような plist が返る

#### Scenario: delimiter の bounds 取得（文字指定）
- **GIVEN** カーソルが `(outer [inner|] more)` の位置にある
- **WHEN** `(eme-thing-bounds 'delimiter ?\[)` を呼び出す
- **THEN** `[inner]` の bounds が返る（`(` ではなく `[` を優先）

#### Scenario: 該当 thing がない場合
- **GIVEN** カーソルが空白の上にある
- **WHEN** `(eme-thing-bounds 'word)` を呼び出す
- **THEN** nil が返る

#### Scenario: 未登録の thing type
- **WHEN** `(eme-thing-bounds 'unknown-type)` を呼び出す
- **THEN** nil が返る

### Requirement: eme-thing-all-bounds 関数
`eme-thing-all-bounds` は現在位置で取得可能な全ての bounds をサイズ順で返さなければならない（SHALL）。

#### Scenario: 全 bounds の取得
- **GIVEN** カーソルが `(defun foo| () nil)` の位置にある
- **WHEN** `(eme-thing-all-bounds)` を呼び出す
- **THEN** word, symbol, list (inner/outer), defun 等の bounds がサイズ順（小→大）のリストで返る

#### Scenario: 重複する bounds の除去
- **GIVEN** word と symbol が同じ範囲を指す
- **WHEN** `(eme-thing-all-bounds)` を呼び出す
- **THEN** 同一の bounds は1つのみ含まれる

### Requirement: eme-thing-outer 関数
`eme-thing-outer` は現在の bounds より外側の同種 thing bounds を返さなければならない（SHALL）。

#### Scenario: 外側の delimiter 取得
- **GIVEN** カーソルが `(outer (inner|) more)` の位置にある
- **GIVEN** 現在 `(inner)` が選択されている
- **WHEN** `(eme-thing-outer 'delimiter current-bounds)` を呼び出す
- **THEN** `(outer ... more)` の bounds が返る

#### Scenario: 外側がない場合
- **GIVEN** 最も外側の delimiter が選択されている
- **WHEN** `(eme-thing-outer 'delimiter current-bounds)` を呼び出す
- **THEN** nil が返る

### Requirement: tree-sitter 統合
tree-sitter が利用可能な場合、thing provider は tree-sitter を優先的に使用しなければならない（SHALL）。利用不可の場合は syntax-based にフォールバックする。

#### Scenario: tree-sitter 利用可能時
- **GIVEN** バッファで tree-sitter が有効である
- **WHEN** `(eme-thing-bounds 'delimiter)` を呼び出す
- **THEN** tree-sitter AST を使用して bounds を検出する

#### Scenario: tree-sitter 利用不可時
- **GIVEN** バッファで tree-sitter が無効である
- **WHEN** `(eme-thing-bounds 'delimiter)` を呼び出す
- **THEN** `syntax-ppss` を使用して bounds を検出する

#### Scenario: tree-sitter 可用性のキャッシュ
- **GIVEN** バッファの tree-sitter 可用性が確認済み
- **WHEN** 同じバッファで再度 thing bounds を取得する
- **THEN** 可用性チェックはキャッシュされた値を使用する

### Requirement: 標準 thing provider
以下の thing type がデフォルトで提供されなければならない（SHALL）。

#### Scenario: word provider
- **WHEN** `(eme-thing-bounds 'word)` を呼び出す
- **THEN** `bounds-of-thing-at-point` を使用して word の bounds が返る

#### Scenario: symbol provider
- **WHEN** `(eme-thing-bounds 'symbol)` を呼び出す
- **THEN** `bounds-of-thing-at-point` を使用して symbol の bounds が返る

#### Scenario: sexp provider
- **GIVEN** カーソルが sexp の内側にある
- **WHEN** `(eme-thing-bounds 'sexp)` を呼び出す
- **THEN** `forward-sexp`/`backward-sexp` を使用して sexp の bounds が返る

#### Scenario: sexp-chain provider
- **GIVEN** カーソルが `foo.bar()|.baz[0]` の位置にある
- **WHEN** `(eme-thing-bounds 'sexp-chain)` を呼び出す
- **THEN** `foo.bar().baz[0]` 全体の bounds が返る

#### Scenario: line provider
- **WHEN** `(eme-thing-bounds 'line)` を呼び出す
- **THEN** `(line-beginning-position . line-end-position)` が :bounds として返る

#### Scenario: sentence provider
- **GIVEN** カーソルが文の中にある
- **WHEN** `(eme-thing-bounds 'sentence)` を呼び出す
- **THEN** `forward-sentence`/`backward-sentence` を使用して sentence の bounds が返る

#### Scenario: paragraph provider
- **GIVEN** カーソルが段落の中にある
- **WHEN** `(eme-thing-bounds 'paragraph)` を呼び出す
- **THEN** `forward-paragraph`/`backward-paragraph` を使用して paragraph の bounds が返る

#### Scenario: defun provider
- **GIVEN** カーソルが関数定義の中にある
- **WHEN** `(eme-thing-bounds 'defun)` を呼び出す
- **THEN** `beginning-of-defun`/`end-of-defun` を使用して defun の bounds が返る

#### Scenario: buffer provider
- **WHEN** `(eme-thing-bounds 'buffer)` を呼び出す
- **THEN** `(point-min . point-max)` が :bounds として返る

#### Scenario: string provider
- **GIVEN** カーソルが `"hello| world"` の位置にある
- **WHEN** `(eme-thing-bounds 'string)` を呼び出す
- **THEN** 文字列全体の bounds と inner（クォートを除く）が返る

#### Scenario: list provider
- **GIVEN** カーソルが `(hello| world)` の位置にある
- **WHEN** `(eme-thing-bounds 'list)` を呼び出す
- **THEN** リスト全体の bounds と inner（括弧を除く）が返る

### Requirement: Emacs 29.1+ 互換性
全ての thing provider 関数は Emacs 29.1 以降で正しく動作しなければならない（SHALL）。

#### Scenario: Emacs 29.1 での動作
- **GIVEN** Emacs バージョンが 29.1 である
- **WHEN** 任意の thing bounds を取得する
- **THEN** エラーなく bounds が返る

#### Scenario: tree-sitter API の使用
- **GIVEN** tree-sitter を使用する
- **WHEN** `treesit-*` 関数を呼び出す
- **THEN** Emacs 29+ の組み込み tree-sitter API のみを使用する（外部パッケージ不要）
