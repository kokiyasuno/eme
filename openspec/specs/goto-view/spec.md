## ADDED Requirements

### Requirement: 行へ移動
システムは `g` でgoto-line機能を提供しなければならない（SHALL）。

#### Scenario: 特定の行へ移動
- **WHEN** ユーザーがSelection Modeで `g` を押す
- **THEN** ユーザーは行番号の入力を求められなければならない（SHALL）
- **WHEN** ユーザーが行番号を入力して確定する
- **THEN** ポイントは指定された行の先頭に移動しなければならない（SHALL）

#### Scenario: プレフィックス引数で行へ移動
- **WHEN** ユーザーがSelection Modeで `C-u 42 g` を押す
- **THEN** ポイントはプロンプトなしで42行目に移動しなければならない（SHALL）

### Requirement: バッファ境界ナビゲーション
システムはEmacs標準キーでバッファ境界ナビゲーションを提供しなければならない（SHALL）。

#### Scenario: バッファ先頭へ移動
- **WHEN** ユーザーがSelection Modeで `M-<` を押す
- **THEN** ポイントはバッファ先頭に移動しなければならない（SHALL）

#### Scenario: バッファ末尾へ移動
- **WHEN** ユーザーがSelection Modeで `M->` を押す
- **THEN** ポイントはバッファ末尾に移動しなければならない（SHALL）

### Requirement: 定義へのナビゲーション
システムはEmacs標準キーで定義ジャンプを提供しなければならない（SHALL）。

#### Scenario: 定義へ移動
- **WHEN** ユーザーが識別子上でSelection Modeで `M-.` を押す
- **THEN** xref-find-definitionsが呼び出されなければならない（SHALL）

#### Scenario: 定義から戻る
- **WHEN** ユーザーが定義ジャンプ後にSelection Modeで `,` を押す
- **THEN** xref-pop-marker-stackが呼び出されなければならない（SHALL）
- **THEN** ポイントは前の位置に戻らなければならない（SHALL）

### Requirement: 参照へのナビゲーション
システムはEmacs標準キーで参照検索を提供しなければならない（SHALL）。

#### Scenario: 参照を検索
- **WHEN** ユーザーが識別子上でSelection Modeで `M-?` を押す
- **THEN** xref-find-referencesが呼び出されなければならない（SHALL）

### Requirement: ページ下スクロール
システムは `v` でページ下スクロールを提供しなければならない（SHALL）。

#### Scenario: ページ下
- **WHEN** ユーザーがSelection Modeで `v` を押す
- **THEN** ビューは1ページ分下にスクロールしなければならない（scroll-up-command）（SHALL）

#### Scenario: 末尾でのページ下
- **WHEN** ユーザーがバッファ末尾付近で `v` を押す
- **THEN** ビューはエラーなく可能な限りスクロールしなければならない（SHALL）

### Requirement: ページ上スクロール
システムは `V` でページ上スクロールを提供しなければならない（SHALL）。

#### Scenario: ページ上
- **WHEN** ユーザーがSelection Modeで `V` を押す
- **THEN** ビューは1ページ分上にスクロールしなければならない（scroll-down-command）（SHALL）

#### Scenario: 先頭でのページ上
- **WHEN** ユーザーがバッファ先頭付近で `V` を押す
- **THEN** ビューはエラーなく可能な限りスクロールしなければならない（SHALL）

### Requirement: ビューの再センタリング
システムは `l` で現在行を再センタリングしなければならない（SHALL）。

#### Scenario: 中央に再センタリング
- **WHEN** ユーザーがSelection Modeで `l` を押す
- **THEN** 現在行はウィンドウの中央に配置されなければならない（SHALL）

#### Scenario: 再センタリングのサイクル
- **WHEN** ユーザーが `l` を複数回押す
- **THEN** ビューは中央、上端、下端をサイクルしなければならない（C-lのように）（SHALL）

