## ADDED Requirements

### Requirement: thing prefix キー
Selection Mode で `t` キーは thing 選択 prefix として機能しなければならない（SHALL）。`t` を押すと thing 選択用の transient keymap がアクティブになる。

#### Scenario: t キーで thing keymap がアクティブになる
- **WHEN** Selection Mode で `t` を押す
- **THEN** `eme-thing-map` がアクティブになる
- **THEN** カーソルが hollow 形状に変化する（状態表示）

#### Scenario: thing keymap のキーバインド
- **WHEN** `eme-thing-map` で `i` を検索する
- **THEN** `eme-thing-inner` が返る
- **WHEN** `eme-thing-map` で `b` を検索する
- **THEN** `eme-thing-bounds` が返る

### Requirement: auto-detect による inner 選択
`eme-thing-inner` は現在位置から最も近い delimiter を自動検出し、その内容（delimiter を除く）を選択しなければならない（SHALL）。

#### Scenario: 括弧内での inner 選択
- **WHEN** カーソルが `(hello |world)` の位置にある（`|` がカーソル）
- **THEN** `t i` は `hello world` を選択する
- **THEN** point は `h` の位置、mark は `)` の直前にある

#### Scenario: 文字列内での inner 選択
- **WHEN** カーソルが `"hello |world"` の位置にある
- **THEN** `t i` は `hello world` を選択する
- **THEN** クォート文字は選択に含まれない

#### Scenario: ネストした括弧での inner 選択
- **WHEN** カーソルが `(outer (inner| value) more)` の位置にある
- **THEN** `t i` は `inner value` を選択する（最も内側の括弧の内容）

#### Scenario: delimiter 外での inner 選択
- **WHEN** カーソルが delimiter の外側にある
- **THEN** `t i` は何も選択しない
- **THEN** メッセージ「No enclosing delimiter found」が表示される

#### Scenario: 空の delimiter での inner 選択
- **WHEN** カーソルが `()` の内側にある
- **THEN** `t i` は幅ゼロの region を作成する（point と mark が同一位置）

### Requirement: auto-detect による bounds 選択
`eme-thing-bounds` は現在位置から最も近い delimiter を自動検出し、delimiter を含めて選択しなければならない（SHALL）。

#### Scenario: 括弧での bounds 選択
- **WHEN** カーソルが `(hello |world)` の位置にある
- **THEN** `t b` は `(hello world)` を選択する
- **THEN** point は `(` の位置、mark は `)` の直後にある

#### Scenario: 文字列での bounds 選択
- **WHEN** カーソルが `"hello |world"` の位置にある
- **THEN** `t b` は `"hello world"` を選択する
- **THEN** クォート文字が選択に含まれる

#### Scenario: ネストした括弧での bounds 選択
- **WHEN** カーソルが `(outer (inner| value) more)` の位置にある
- **THEN** `t b` は `(inner value)` を選択する（最も内側の括弧全体）

### Requirement: syntax-ppss による delimiter 検出
delimiter の検出は `syntax-ppss` を使用しなければならない（SHALL）。これにより major-mode の syntax-table に基づいた正確な解析が可能になる。

#### Scenario: emacs-lisp-mode での括弧検出
- **WHEN** バッファが `emacs-lisp-mode` で `(defun foo| () nil)` を含む
- **THEN** `t i` は `defun foo () nil` を選択する

#### Scenario: python-mode での文字列検出
- **WHEN** バッファが `python-mode` で `'hello |world'` を含む
- **THEN** `t i` は `hello world` を選択する（シングルクォート対応）

#### Scenario: 角括弧の検出
- **WHEN** バッファが `[item1, |item2]` を含む
- **THEN** `t i` は `item1, item2` を選択する

#### Scenario: 波括弧の検出
- **WHEN** バッファが `{key: |value}` を含む
- **THEN** `t i` は `key: value` を選択する

### Requirement: transient keymap による拡張
thing 選択後、`t` キーを押すと外側の delimiter に選択を拡張しなければならない（SHALL）。

#### Scenario: t 繰り返しで外側に拡張
- **WHEN** `(outer (inner| value) more)` で `t i` を実行した後
- **WHEN** `t` を押す
- **THEN** 選択が `outer (inner value) more` に拡張される（外側の括弧の inner）

#### Scenario: t i t t で2段階拡張
- **WHEN** `((deeply (nested| text)))` で `t i` を実行した後
- **WHEN** `t` を2回押す
- **THEN** 選択が最も外側の括弧の inner まで拡張される

#### Scenario: bounds 後の拡張
- **WHEN** `(outer (inner| value) more)` で `t b` を実行した後
- **WHEN** `t` を押す
- **THEN** 選択が `(outer (inner value) more)` に拡張される（外側の括弧の bounds）

#### Scenario: 拡張限界での動作
- **WHEN** すでに最も外側の delimiter を選択している
- **WHEN** `t` を押す
- **THEN** 選択は変化しない
- **THEN** メッセージ「No outer delimiter found」が表示される

### Requirement: transient keymap のタイムアウト
thing 選択後の transient keymap は一定時間または別のキー入力でキャンセルされなければならない（SHALL）。

#### Scenario: 別のキーで transient keymap 終了
- **WHEN** `t i` で選択した後
- **WHEN** `t` 以外のキー（例: `d`）を押す
- **THEN** transient keymap が終了する
- **THEN** 押されたキー（`d`）が通常通り処理される

#### Scenario: transient keymap 中のカーソル形状
- **WHEN** `t i` で選択した直後
- **THEN** カーソルが hollow 形状になる（transient 状態を示す）
- **WHEN** transient keymap が終了する
- **THEN** カーソルが通常の形状に戻る

### Requirement: 明示的 delimiter 指定
`t i` または `t b` の後に delimiter 文字を入力すると、その delimiter で選択を開始しなければならない（SHALL）。auto-detect のフォールバックとして機能する。

#### Scenario: 明示的に括弧を指定
- **WHEN** `t i (` を入力する
- **THEN** 現在位置から外側に向かって `(` を検索し、その inner を選択する

#### Scenario: 明示的にクォートを指定
- **WHEN** `t i "` を入力する
- **THEN** 現在位置から外側に向かって `"` を検索し、その inner を選択する

#### Scenario: 指定した delimiter が見つからない場合
- **WHEN** `t i [` を入力するが、カーソル位置に `[` がない
- **THEN** 何も選択しない
- **THEN** メッセージ「No enclosing [ found」が表示される

#### Scenario: 閉じ delimiter で指定
- **WHEN** `t i )` を入力する
- **THEN** 対応する `(` と `)` で囲まれた inner を選択する（開き括弧と同じ動作）

### Requirement: thing 選択のキーバインド統合
thing 選択は Selection Mode の keymap で `t` キーからアクセスできなければならない（SHALL）。

#### Scenario: t キーのバインド確認
- **WHEN** `eme-selection-mode-map` で `t` を検索する
- **THEN** `eme-thing-prefix` が返る

#### Scenario: 既存バインドとの競合確認
- **WHEN** Selection Mode の既存バインドを確認する
- **THEN** `t` キーを使用する既存バインドは存在しない

### Requirement: thing 選択は全 major-mode で動作する
thing 選択は `syntax-ppss` に依存し、major-mode の syntax-table を尊重しなければならない（SHALL）。

#### Scenario: fundamental-mode での動作
- **WHEN** バッファが `fundamental-mode` で括弧を含む
- **THEN** `t i` が正しく動作する

#### Scenario: org-mode での動作
- **WHEN** バッファが `org-mode` で `[[link|text]]` を含む
- **THEN** `t i` が `[` 括弧の内容を選択する

#### Scenario: Emacs 29.1+ との互換性
- **WHEN** Emacs バージョンが 29.1 以降である
- **THEN** すべての thing 選択関数が利用可能で正しく機能する
