## ADDED Requirements

### Requirement: Match Modeへの入場
システムは `m` プレフィックスキーでMatch Modeを提供しなければならない（SHALL）。

#### Scenario: Match Modeに入る
- **WHEN** ユーザーがSelection Modeで `m` を押す
- **THEN** システムは次のキーを待って操作を決定しなければならない（SHALL）

#### Scenario: Match Modeをキャンセル
- **WHEN** ユーザーが `m` を押した後に `C-g` を押す
- **THEN** Match Modeはキャンセルされなければならない（SHALL）
- **THEN** 操作は実行されてはならない（SHALL NOT）

### Requirement: 内部テキストオブジェクト
システムは `m i {object}` でテキストオブジェクトの内部コンテンツを選択しなければならない（SHALL）。

#### Scenario: 内部単語
- **WHEN** ユーザーが "hello" の中にポイントがある状態で `m i w` を押す
- **THEN** リージョンは "hello"（単語文字のみ）を選択しなければならない（SHALL）

#### Scenario: 内部WORD
- **WHEN** ユーザーが "foo-bar" の中にポイントがある状態で `m i W` を押す
- **THEN** リージョンは "foo-bar"（空白以外のシーケンス）を選択しなければならない（SHALL）

#### Scenario: 内部括弧
- **WHEN** ユーザーが "(hello world)" の中にポイントがある状態で `m i (` を押す
- **THEN** リージョンは "hello world"（括弧を除く）を選択しなければならない（SHALL）

#### Scenario: 内部ブラケット
- **WHEN** ユーザーが "[hello world]" の中にポイントがある状態で `m i [` を押す
- **THEN** リージョンは "hello world"（ブラケットを除く）を選択しなければならない（SHALL）

#### Scenario: 内部ブレース
- **WHEN** ユーザーが "{hello world}" の中にポイントがある状態で `m i {` を押す
- **THEN** リージョンは "hello world"（ブレースを除く）を選択しなければならない（SHALL）

#### Scenario: 内部アングルブラケット
- **WHEN** ユーザーが "<hello world>" の中にポイントがある状態で `m i <` を押す
- **THEN** リージョンは "hello world"（アングルブラケットを除く）を選択しなければならない（SHALL）

#### Scenario: 内部ダブルクォート
- **WHEN** ユーザーが `"hello world"` の中にポイントがある状態で `m i "` を押す
- **THEN** リージョンは `hello world`（クォートを除く）を選択しなければならない（SHALL）

#### Scenario: 内部シングルクォート
- **WHEN** ユーザーが `'hello world'` の中にポイントがある状態で `m i '` を押す
- **THEN** リージョンは `hello world`（クォートを除く）を選択しなければならない（SHALL）

#### Scenario: 内部バッククォート
- **WHEN** ユーザーが `` `hello world` `` の中にポイントがある状態で `` m i ` `` を押す
- **THEN** リージョンは `hello world`（バッククォートを除く）を選択しなければならない（SHALL）

#### Scenario: 内部文
- **WHEN** ユーザーが文の中にポイントがある状態で `m i s` を押す
- **THEN** リージョンは文のコンテンツを選択しなければならない（SHALL）

#### Scenario: 内部段落
- **WHEN** ユーザーが段落の中にポイントがある状態で `m i p` を押す
- **THEN** リージョンは段落のコンテンツを選択しなければならない（SHALL）

### Requirement: 外部テキストオブジェクト
システムは `m a {object}` でデリミタを含むコンテンツを選択しなければならない（SHALL）。

#### Scenario: 外部単語
- **WHEN** ユーザーが "hello" の中にポイントがある状態で `m a w` を押す
- **THEN** リージョンは "hello "（末尾の空白を含む）を選択しなければならない（SHALL）

#### Scenario: 外部括弧
- **WHEN** ユーザーが "(hello world)" の中にポイントがある状態で `m a (` を押す
- **THEN** リージョンは "(hello world)"（括弧を含む）を選択しなければならない（SHALL）

#### Scenario: 外部ダブルクォート
- **WHEN** ユーザーが `"hello world"` の中にポイントがある状態で `m a "` を押す
- **THEN** リージョンは `"hello world"`（クォートを含む）を選択しなければならない（SHALL）

### Requirement: Tree-sitterテキストオブジェクト
システムは利用可能な場合、tree-sitterベースのテキストオブジェクトを提供しなければならない（SHALL）。

#### Scenario: tree-sitterでの内部関数
- **WHEN** ユーザーがtree-sitter有効バッファで `m i f` を押す
- **THEN** リージョンは関数本体（シグネチャを除く）を選択しなければならない（SHALL）

#### Scenario: tree-sitterでの外部関数
- **WHEN** ユーザーがtree-sitter有効バッファで `m a f` を押す
- **THEN** リージョンは関数定義全体を選択しなければならない（SHALL）

#### Scenario: tree-sitterでの内部クラス
- **WHEN** ユーザーがクラスを持つtree-sitter有効バッファで `m i c` を押す
- **THEN** リージョンはクラス本体を選択しなければならない（SHALL）

#### Scenario: tree-sitterでの内部引数
- **WHEN** ユーザーが関数引数の中で `m i ,` を押す
- **THEN** リージョンは現在の引数を選択しなければならない（SHALL）

#### Scenario: tree-sitterなしでのフォールバック
- **WHEN** ユーザーがtree-sitterなしのバッファで `m i f` を押す
- **THEN** システムは `beginning-of-defun`/`end-of-defun` にフォールバックしなければならない（SHALL）

### Requirement: 選択をラップ
システムは `m w {char}` で選択をデリミタでラップしなければならない（SHALL）。

#### Scenario: 括弧でラップ
- **WHEN** ユーザーが "hello" を選択して `m w (` を押す
- **THEN** テキストは "(hello)" になっていなければならない（SHALL）
- **THEN** ポイントは閉じ括弧の後になければならない（SHALL）

#### Scenario: ブラケットでラップ
- **WHEN** ユーザーが "hello" を選択して `m w [` を押す
- **THEN** テキストは "[hello]" になっていなければならない（SHALL）

#### Scenario: ブレースでラップ
- **WHEN** ユーザーが "hello" を選択して `m w {` を押す
- **THEN** テキストは "{hello}" になっていなければならない（SHALL）

#### Scenario: ダブルクォートでラップ
- **WHEN** ユーザーが `hello` を選択して `m w "` を押す
- **THEN** テキストは `"hello"` になっていなければならない（SHALL）

#### Scenario: シングルクォートでラップ
- **WHEN** ユーザーが `hello` を選択して `m w '` を押す
- **THEN** テキストは `'hello'` になっていなければならない（SHALL）

### Requirement: 囲みを削除
システムは `m d {char}` で囲みデリミタを削除しなければならない（SHALL）。

#### Scenario: 囲みの括弧を削除
- **WHEN** ユーザーが "(hello)" の中にポイントがある状態で `m d (` を押す
- **THEN** テキストは "hello" になっていなければならない（SHALL）

#### Scenario: 囲みのクォートを削除
- **WHEN** ユーザーが `"hello"` の中にポイントがある状態で `m d "` を押す
- **THEN** テキストは `hello` になっていなければならない（SHALL）

#### Scenario: 削除する囲みがない
- **WHEN** ユーザーが囲みの括弧なしで `m d (` を押す
- **THEN** 変更は行われてはならない（SHALL NOT）
- **THEN** ユーザーに通知されなければならない（SHALL）

### Requirement: 囲みを置換
システムは `m r {old} {new}` で囲みデリミタを置換しなければならない（SHALL）。

#### Scenario: 括弧をブラケットに置換
- **WHEN** ユーザーが "(hello)" の中にポイントがある状態で `m r ( [` を押す
- **THEN** テキストは "[hello]" になっていなければならない（SHALL）

#### Scenario: クォートを置換
- **WHEN** ユーザーが `"hello"` の中にポイントがある状態で `m r " '` を押す
- **THEN** テキストは `'hello'` になっていなければならない（SHALL）

### Requirement: ネストしたデリミタの処理
システムはネストしたデリミタを正しく処理しなければならない（SHALL）。

#### Scenario: ネストした括弧での内部選択
- **WHEN** ユーザーが "((inner))" の中にポイントがある状態で `m i (` を押す
- **THEN** リージョンはポイントを含む最も内側の括弧のコンテンツを選択しなければならない（SHALL）

#### Scenario: ネストしたデリミタでの削除
- **WHEN** ユーザーが "((inner))" の中にポイントがある状態で `m d (` を押す
- **THEN** ポイントを含む最も内側の括弧が削除されなければならない（SHALL）
