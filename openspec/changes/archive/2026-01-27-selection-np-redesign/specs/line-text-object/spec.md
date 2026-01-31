## ADDED Requirements

### Requirement: 行内容テキストオブジェクト (inner line)
Match Mode は行内容テキストオブジェクト (`m i l`) を提供しなければならない（SHALL）。最初の非空白文字から行末までを選択する。

#### Scenario: インデント付き行の内容を選択
- **WHEN** カーソルが "    hello world"（スペース4つのインデント）を含む行にある
- **THEN** `m i l` は "hello world"（最初の非空白文字から行末まで）を選択する
- **THEN** point は最初の非空白文字の位置にある
- **THEN** mark は行末の位置にある
- **THEN** region がアクティブである

#### Scenario: インデントなしの行の内容を選択
- **WHEN** カーソルが "hello world"（インデントなし）を含む行にある
- **THEN** `m i l` は "hello world"（行全体の内容）を選択する
- **THEN** point はカラム0にある
- **THEN** mark は行末にある

#### Scenario: 空行での内容選択
- **WHEN** カーソルが空行にある
- **THEN** `m i l` は何も選択しない（point と mark が同一位置）
- **THEN** region はアクティブで幅がゼロ

#### Scenario: 空白のみの行での内容選択
- **WHEN** カーソルが空白のみの行 "    " にある
- **THEN** `m i l` は何も選択しない（`back-to-indentation` が行末に到達する）

### Requirement: 行全体テキストオブジェクト (around line)
Match Mode は行全体テキストオブジェクト (`m a l`) を提供しなければならない（SHALL）。行頭から改行文字を含む行全体を選択する。

#### Scenario: バッファ中間の行を全体選択
- **WHEN** カーソルが最終行ではない "hello world\n" の行にある
- **THEN** `m a l` は `line-beginning-position` から次行の先頭まで（改行を含む）を選択する
- **THEN** point は `line-beginning-position` にある
- **THEN** mark は次行の先頭にある

#### Scenario: バッファ最終行の全体選択
- **WHEN** カーソルがバッファの最終行にある（末尾改行なし）
- **THEN** `m a l` は `line-beginning-position` から `point-max` までを選択する
- **THEN** 選択はバッファ末尾を超えない

#### Scenario: 空行の全体選択
- **WHEN** カーソルが空行（改行のみ）にある
- **THEN** `m a l` は改行文字を選択する

### Requirement: 行テキストオブジェクトのキーマップ統合
行テキストオブジェクトは Match Mode の inner および around キーマップで `l` キーからアクセスできなければならない（SHALL）。

#### Scenario: inner line のキーバインド
- **WHEN** Selection Mode で `m i l` を押す
- **THEN** `eme-select-inner-line` が呼ばれる

#### Scenario: around line のキーバインド
- **WHEN** Selection Mode で `m a l` を押す
- **THEN** `eme-select-around-line` が呼ばれる

#### Scenario: 既存バインドとの競合なし
- **WHEN** `eme-match-inner-map` で `l` を検索する
- **THEN** `eme-select-inner-line` が返る
- **WHEN** `eme-match-around-map` で `l` を検索する
- **THEN** `eme-select-around-line` が返る
- **WHEN** inner/around マップの既存バインドを確認する
- **THEN** `l` キーを使用する既存バインドは存在しない

### Requirement: 行テキストオブジェクトは全 major mode で動作する
行テキストオブジェクトは Emacs のバッファプリミティブのみに依存し、アクティブな major mode に関係なく動作しなければならない（SHALL）。

#### Scenario: emacs-lisp-mode での動作
- **WHEN** バッファが `emacs-lisp-mode` でインデントされたコードを含む
- **THEN** `m i l` は最初の非空白文字から行末までを選択する

#### Scenario: fundamental-mode での動作
- **WHEN** バッファが `fundamental-mode` にある
- **THEN** `m i l` と `m a l` が正しく動作する

#### Scenario: Emacs 29.1+ との互換性
- **WHEN** Emacs バージョンが 29.1 以降である
- **THEN** すべての行テキストオブジェクト関数が利用可能で正しく機能する
