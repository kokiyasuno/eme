## ADDED Requirements

### Requirement: 削除アクション
システムは `d` で選択を削除しkill-ringに保存しなければならない（SHALL）。

#### Scenario: 選択を削除
- **WHEN** ユーザーがリージョンを選択して `d` を押す
- **THEN** 選択されたテキストは削除されなければならない（SHALL）
- **THEN** 削除されたテキストはkill-ringに保存されなければならない（SHALL）

#### Scenario: 選択なしで削除
- **WHEN** ユーザーがアクティブなリージョンなしで `d` を押す
- **THEN** ポイント位置の文字が削除されなければならない（SHALL）
- **THEN** 削除された文字はkill-ringに保存されなければならない（SHALL）

### Requirement: kill-ringなしの削除
システムは `D` でkill-ringに保存せずに選択を削除しなければならない（SHALL）。

#### Scenario: 保存せずに削除
- **WHEN** ユーザーがリージョンを選択して `D` を押す
- **THEN** 選択されたテキストは削除されなければならない（SHALL）
- **THEN** kill-ringは変更されてはならない（SHALL NOT）

### Requirement: コピーアクション
システムは `w` で選択をkill-ringにコピーしなければならない（SHALL）。

#### Scenario: 選択をコピー
- **WHEN** ユーザーがリージョンを選択して `w` を押す
- **THEN** 選択されたテキストはkill-ringにコピーされなければならない（SHALL）
- **THEN** 選択されたテキストは削除されてはならない（SHALL NOT）
- **THEN** リージョンは非アクティブ化されなければならない（SHALL）

#### Scenario: 選択なしでコピー
- **WHEN** ユーザーがアクティブなリージョンなしで `w` を押す
- **THEN** 現在の行がkill-ringにコピーされなければならない（SHALL）

### Requirement: ヤンクアクション
システムは `y` でkill-ringからペーストしなければならない（SHALL）。

#### Scenario: ポイントにヤンク
- **WHEN** ユーザーがSelection Modeで `y` を押す
- **THEN** kill-ringからテキストがポイントに挿入されなければならない（SHALL）

#### Scenario: ヤンクで選択を置換
- **WHEN** ユーザーがリージョンを選択して `y` を押す
- **THEN** 選択されたテキストはkill-ringのコンテンツで置換されなければならない（SHALL）

### Requirement: 変更アクション
システムは `c` で選択を削除してInsert Modeに入らなければならない（SHALL）。

#### Scenario: 選択を変更
- **WHEN** ユーザーがリージョンを選択して `c` を押す
- **THEN** 選択されたテキストは削除されkill-ringに保存されなければならない（SHALL）
- **THEN** バッファはInsert Modeに入らなければならない（SHALL）

#### Scenario: 選択なしで変更
- **WHEN** ユーザーがアクティブなリージョンなしで `c` を押す
- **THEN** バッファは現在のポイントでInsert Modeに入らなければならない（SHALL）

### Requirement: 文字置換アクション
システムは `x` で選択を入力した文字で置換しなければならない（SHALL）。

#### Scenario: 文字で置換
- **WHEN** ユーザーが "hello" を選択して `x` を押し、その後 `a` を押す
- **THEN** "hello" は "aaaaa"（同じ長さ）に置換されなければならない（SHALL）

#### Scenario: 単一文字を置換
- **WHEN** ユーザーが選択なしで `x` を押し、その後 `a` を押す
- **THEN** ポイント位置の文字は `a` に置換されなければならない（SHALL）

### Requirement: コメントトグルアクション
システムは `;` で選択のコメントをトグルしなければならない（SHALL）。

#### Scenario: 非コメントコードをコメント化
- **WHEN** ユーザーがコメントされていない行を選択して `;` を押す
- **THEN** 選択された行はコメント化されなければならない（SHALL）

#### Scenario: コメントコードを非コメント化
- **WHEN** ユーザーがコメントされた行を選択して `;` を押す
- **THEN** 選択された行は非コメント化されなければならない（SHALL）

#### Scenario: 単一行のコメント
- **WHEN** ユーザーが選択なしで `;` を押す
- **THEN** 現在の行がトグルされなければならない（SHALL）

### Requirement: フォーマットアクション
システムは `=` で選択をフォーマット/インデントしなければならない（SHALL）。

#### Scenario: 選択をフォーマット
- **WHEN** ユーザーがリージョンを選択して `=` を押す
- **THEN** 選択されたリージョンはモードのルールに従ってインデントされなければならない（SHALL）

#### Scenario: 行をフォーマット
- **WHEN** ユーザーが選択なしで `=` を押す
- **THEN** 現在の行はモードのルールに従ってインデントされなければならない（SHALL）

### Requirement: インデントアクション
システムは `>` と `<` でインデント増減を提供しなければならない（SHALL）。

#### Scenario: インデント増加
- **WHEN** ユーザーがリージョンを選択して `>` を押す
- **THEN** 選択された行は `tab-width` 分インデントされなければならない（SHALL）

#### Scenario: インデント減少
- **WHEN** ユーザーがリージョンを選択して `<` を押す
- **THEN** 選択された行は `tab-width` 分アンインデントされなければならない（SHALL）

#### Scenario: 単一行のインデント
- **WHEN** ユーザーが選択なしで `>` を押す
- **THEN** 現在の行は `tab-width` 分インデントされなければならない（SHALL）

### Requirement: 行結合アクション
システムは `j` で行を結合しなければならない（SHALL）。

#### Scenario: 選択された行を結合
- **WHEN** ユーザーが複数行を選択して `j` を押す
- **THEN** 選択された行は1行に結合されなければならない（SHALL）

#### Scenario: 現在行と次の行を結合
- **WHEN** ユーザーが選択なしで `j` を押す
- **THEN** 現在の行と次の行が結合されなければならない（SHALL）

### Requirement: 行を開くアクション
システムは `o` と `O` で新しい行を開かなければならない（SHALL）。

#### Scenario: 下に行を開く
- **WHEN** ユーザーがSelection Modeで `o` を押す
- **THEN** 現在の行の下に新しい行が作成されなければならない（SHALL）
- **THEN** バッファは新しい行でInsert Modeに入らなければならない（SHALL）

#### Scenario: 上に行を開く
- **WHEN** ユーザーがSelection Modeで `O` を押す
- **THEN** 現在の行の上に新しい行が作成されなければならない（SHALL）
- **THEN** バッファは新しい行でInsert Modeに入らなければならない（SHALL）

### Requirement: 複製アクション
システムは `+` で選択を複製しなければならない（SHALL）。

#### Scenario: 選択を複製
- **WHEN** ユーザーがリージョンを選択して `+` を押す
- **THEN** 選択されたテキストは選択の後に複製されなければならない（SHALL）

#### Scenario: 行を複製
- **WHEN** ユーザーが選択なしで `+` を押す
- **THEN** 現在の行は下に複製されなければならない（SHALL）

### Requirement: 大文字小文字トグルアクション
システムは `` ` `` で大文字小文字をトグルしなければならない（SHALL）。

#### Scenario: 選択の大文字小文字をトグル
- **WHEN** ユーザーが "Hello" を選択して `` ` `` を押す
- **THEN** テキストは "hELLO"（各文字がトグル）になっていなければならない（SHALL）

#### Scenario: 選択を大文字化
- **WHEN** ユーザーが小文字の選択を持ち `` ` `` を素早く2回押す
- **THEN** テキストは大文字になっていなければならない（SHALL）

### Requirement: Undo/Redoアクション
システムは `u` でundo、`U` でredoを提供しなければならない（SHALL）。

#### Scenario: Undo
- **WHEN** ユーザーがSelection Modeで `u` を押す
- **THEN** 最後の編集が取り消されなければならない（SHALL）

#### Scenario: Redo
- **WHEN** ユーザーがSelection Modeで `U` を押す
- **THEN** 最後のundoがやり直されなければならない（SHALL）

### Requirement: ソートアクション
システムは `g s` で行をソートしなければならない（SHALL）。

#### Scenario: 選択された行をソート
- **WHEN** ユーザーが複数行を選択して `g s` を押す
- **THEN** 選択された行はアルファベット順にソートされなければならない（SHALL）

### Requirement: 逆順アクション
システムは `g R` で行を逆順にしなければならない（SHALL）。

#### Scenario: 選択された行を逆順
- **WHEN** ユーザーが複数行を選択して `g R` を押す
- **THEN** 選択された行の順序は逆になっていなければならない（SHALL）
