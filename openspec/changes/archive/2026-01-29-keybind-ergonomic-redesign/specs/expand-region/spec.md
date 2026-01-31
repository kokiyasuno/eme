## ADDED Requirements

### Requirement: 選択拡大
`eme-expand-region` は現在の選択を段階的に拡大しなければならない（SHALL）。拡大順序は word → symbol → string → list → defun。

#### Scenario: word から symbol への拡大
- **WHEN** カーソルが `foo-bar|` の位置にある（選択なし）
- **WHEN** `v` を押す
- **THEN** `bar` が選択される（カーソル位置の word）
- **WHEN** `v` をもう一度押す
- **THEN** `foo-bar` が選択される（symbol 全体）

#### Scenario: symbol から string への拡大
- **WHEN** `"hello |world"` で `v` を2回押して `world` が選択されている
- **WHEN** `v` をもう一度押す
- **THEN** `hello world` が選択される（string の内容）

#### Scenario: string から list への拡大
- **WHEN** `(func "hello |world")` で string 内容が選択されている
- **WHEN** `v` を押す
- **THEN** `"hello world"` が選択される（string 全体、クォート含む）
- **WHEN** `v` をもう一度押す
- **THEN** `func "hello world"` が選択される（list の内容）

#### Scenario: list から defun への拡大
- **WHEN** `(defun foo () (body|))` で `body` が選択されている
- **WHEN** `v` を繰り返し押す
- **THEN** 最終的に `(defun foo () (body))` 全体が選択される

#### Scenario: defun を超える拡大
- **WHEN** すでに defun 全体が選択されている
- **WHEN** `v` を押す
- **THEN** 選択は変化しない（これ以上拡大できない）
- **THEN** メッセージは表示されない（サイレント）

### Requirement: 選択縮小
`eme-contract-region` は `eme-expand-region` で拡大した選択を1段階縮小しなければならない（SHALL）。

#### Scenario: symbol から word への縮小
- **WHEN** `foo-bar` が選択されている（symbol）
- **WHEN** `V` を押す
- **THEN** 選択が `bar` に縮小される（最後に拡大した前の状態）

#### Scenario: 縮小の限界
- **WHEN** 最小の選択（word）が選択されている
- **WHEN** `V` を押す
- **THEN** 選択が解除される（region 非アクティブ）

#### Scenario: 拡大履歴がない場合の縮小
- **WHEN** 手動で region を設定している（expand 履歴なし）
- **WHEN** `V` を押す
- **THEN** 何も起こらない（変化なし）

### Requirement: 拡大履歴の管理
expand-region は拡大履歴を保持し、縮小時に使用しなければならない（SHALL）。

#### Scenario: 拡大履歴の蓄積
- **WHEN** `v` を3回押して word → symbol → string と拡大する
- **THEN** 履歴に3つの状態が記録される
- **WHEN** `V` を押す
- **THEN** symbol の状態に戻る

#### Scenario: 別の操作で履歴がクリアされる
- **WHEN** `v` で拡大した後
- **WHEN** カーソルを移動する（`f` や `n` など）
- **THEN** 拡大履歴がクリアされる
- **WHEN** 次に `V` を押す
- **THEN** 縮小できない（履歴なし）

#### Scenario: 手動選択後の拡大
- **WHEN** `.` でアンカーを設定し手動で region を作成している
- **WHEN** `v` を押す
- **THEN** 現在の選択を含む最小の semantic unit が選択される
- **THEN** 新しい拡大履歴が開始される

### Requirement: expand-region のキーバインド
Selection Mode で `v` キーは `eme-expand-region`、`V` キーは `eme-contract-region` にバインドされなければならない（SHALL）。

#### Scenario: expand のバインド
- **WHEN** `eme-selection-mode-map` で `v` を検索する
- **THEN** `eme-expand-region` が返る

#### Scenario: contract のバインド
- **WHEN** `eme-selection-mode-map` で `V` を検索する
- **THEN** `eme-contract-region` が返る

#### Scenario: 既存バインドとの競合確認
- **WHEN** Selection Mode の既存バインドを確認する
- **THEN** `v` と `V` を使用する既存バインドは存在しない

### Requirement: semantic 拡大の major-mode 対応
expand-region は major-mode に応じた semantic な拡大を行わなければならない（SHALL）。

#### Scenario: emacs-lisp-mode での拡大
- **WHEN** バッファが `emacs-lisp-mode` にある
- **THEN** sexp、defun などの Lisp 構造を認識して拡大する

#### Scenario: python-mode での拡大
- **WHEN** バッファが `python-mode` にある
- **THEN** インデントベースのブロック構造を認識して拡大する

#### Scenario: fundamental-mode での拡大
- **WHEN** バッファが `fundamental-mode` にある
- **THEN** word → sentence → paragraph の順で拡大する

### Requirement: 数値引数による複数段階拡大
expand-region は数値引数を受け取り、指定回数だけ拡大しなければならない（SHALL）。

#### Scenario: 数値引数で複数段階拡大
- **WHEN** `C-u 3 v` を実行する
- **THEN** 3段階拡大される（word → symbol → string）

#### Scenario: 数値引数で縮小
- **WHEN** `C-u 2 V` を実行する
- **THEN** 2段階縮小される

### Requirement: Emacs 29.1+ 互換性
expand-region 機能は Emacs 29.1 以降で動作しなければならない（SHALL）。

#### Scenario: Emacs 29.1 での動作確認
- **WHEN** Emacs バージョンが 29.1 以降である
- **THEN** `eme-expand-region` と `eme-contract-region` が利用可能で正しく機能する
