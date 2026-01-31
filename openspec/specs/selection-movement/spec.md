## MODIFIED Requirements

### Requirement: 独自選択機構（eme-selection）への完全移行
emeは全ての選択操作を `eme-selection` (overlay) で管理しなければならない（SHALL）。Emacs region (`mark`/`point`) は使用しない。

#### Rationale
Emacs regionはmarkからpointまでの範囲で定義されるため、「カーソルがカラム5にある」かつ「行全体が選択される」という要件を同時に満たせない。また、アンカー設定時の選択拡張でも範囲の記憶が困難。全てを `eme-selection` に統一することで一貫した動作を実現し、将来の multiple selection への拡張も可能にする。

#### Scenario: 選択範囲の独立管理
- **GIVEN** 独自選択機構が有効である
- **WHEN** 行全体を選択する操作を行う
- **THEN** 選択範囲は行頭から行末まで設定される
- **THEN** カーソル（point）は任意の位置（例：カラム5）に配置できる

#### Scenario: 選択の視覚的表示
- **GIVEN** eme-selectionが設定されている
- **THEN** 選択範囲はoverlayでハイライト表示される
- **THEN** ハイライトはEmacsのregion faceと同様に見える

#### Scenario: アクションは独自選択に対して動作
- **GIVEN** eme-selectionが `(10 . 50)` に設定されている
- **WHEN** `d`（delete）を実行する
- **THEN** 位置10から50までのテキストが削除される
- **THEN** point位置は関係ない

### REMOVED: アンカー設定時はEmacs regionを使用
**削除理由**: 全ての選択を `eme-selection` に統一するため、アンカー設定時も `eme-selection` を使用する。

### Requirement: アンカー機構の再設計（eme--anchor-bounds）
アンカーは単一位置ではなく選択範囲（bounds）を記憶しなければならない（SHALL）。移動時は anchor-bounds と新しい範囲をマージして選択を拡張する。

#### Rationale
単一位置（`eme--anchor-pos`）では、ライン選択で「どこからどこまで」が曖昧になる。範囲を記憶することで、ライン選択との整合性を確保し、上下どちらへ移動しても自然に拡張できる。

#### Specification: eme--anchor-bounds
```elisp
(defvar-local eme--anchor-bounds nil
  "Selection bounds when anchor was set.
A cons cell (START . END) representing the selection range at anchor time.
Used to extend eme-selection by merging with new movement range.")
```

#### Scenario: アンカー設定時に選択範囲を記憶
- **GIVEN** `n` で行1を選択している（eme-selection: [line1-beg, line1-end]）
- **WHEN** `.` でアンカーを設定する
- **THEN** `eme--anchor-bounds` に `(line1-beg . line1-end)` が記憶される
- **THEN** `eme--anchor-active` が t になる
- **THEN** eme-selection は維持される

#### Scenario: アンカー設定後の移動でマージ
- **GIVEN** アンカーが設定されている（anchor-bounds: [line1-beg, line1-end]）
- **WHEN** `n` で行2に移動する
- **THEN** 新しい範囲 [line2-beg, line2-end] が計算される
- **THEN** マージ: [min(anchor-start, new-start), max(anchor-end, new-end)]
- **THEN** eme-selection が [line1-beg, line2-end] に更新される（2行選択）

#### Scenario: アンカー解除時は選択クリア
- **GIVEN** アンカーが設定されている
- **WHEN** `.` でアンカーを解除する
- **THEN** eme-selection がクリアされる
- **THEN** `eme--anchor-bounds` が nil になる
- **THEN** `eme--anchor-active` が nil になる

#### Scenario: 逆方向への移動でもマージ
- **GIVEN** 行2でアンカーを設定している（anchor-bounds: [line2-beg, line2-end]）
- **WHEN** `p` で行1に移動する
- **THEN** 新しい範囲 [line1-beg, line1-end] が計算される
- **THEN** マージ: [min(line2-beg, line1-beg), max(line2-end, line1-end)]
- **THEN** eme-selection が [line1-beg, line2-end] に更新される（2行選択）

### Requirement: 行移動はeme-selectionのみ使用
`eme-next-line` と `eme-previous-line` は常に `eme-selection` を使用しなければならない（SHALL）。アンカーの有無に関わらず、Emacs region は使用しない。

#### Scenario: アンカーなしでの次行移動（行選択）
- **WHEN** アンカーが設定されていない
- **WHEN** カーソルが行1、カラム5にある
- **THEN** `n` はカーソルを行2、カラム5に移動する
- **THEN** eme-selection が行2の行頭から行末まで設定される（fresh selection）
- **THEN** Emacs region は使用されない

#### Scenario: アンカーなしでの前行移動（行選択）
- **WHEN** アンカーが設定されていない
- **WHEN** カーソルが行3、カラム10にある
- **THEN** `p` はカーソルを行2、カラム10に移動する
- **THEN** eme-selection が行2の行頭から行末まで設定される（fresh selection）
- **THEN** Emacs region は使用されない

#### Scenario: アンカーありでの次行移動（選択マージ）
- **WHEN** 行1で `n` を押して行1が選択されている
- **WHEN** `.` でアンカーを設定する（anchor-bounds: [line1-beg, line1-end]）
- **WHEN** `n` を押す
- **THEN** カーソルが行2、カラム5に移動する
- **THEN** eme-selection が [line1-beg, line2-end] に設定される（2行選択）
- **THEN** Emacs region は使用されない

#### Scenario: アンカーありで連続移動すると選択がマージされる
- **WHEN** 行1で `.` でアンカーを設定する
- **WHEN** `n` を2回押す
- **THEN** カーソルが行3に移動する
- **THEN** eme-selection が [line1-beg, line3-end] に設定される（3行選択）

### Requirement: 単語移動はeme-selectionのみ使用
`eme-forward-word` と `eme-backward-word` は常に `eme-selection` を使用しなければならない（SHALL）。

#### Scenario: 前方単語移動
- **WHEN** カーソルが `hello| world foo` の位置にある
- **WHEN** `f` を押す
- **THEN** カーソルが `world` の末尾に移動する
- **THEN** eme-selection が `world` の範囲に設定される（fresh selection）
- **THEN** `push-mark` は呼び出されない

#### Scenario: 後方単語移動
- **WHEN** カーソルが `hello world |foo` の位置にある
- **WHEN** `b` を押す
- **THEN** カーソルが `world` の先頭に移動する
- **THEN** eme-selection が `world` の範囲に設定される（fresh selection）
- **THEN** `push-mark` は呼び出されない

#### Scenario: アンカーありでの単語移動（選択マージ）
- **WHEN** `.` でアンカーを設定している
- **WHEN** `f` を押す
- **THEN** anchor-bounds と新しい単語範囲がマージされる
- **THEN** eme-selection が拡張される

### Requirement: 全移動コマンドでEmacs regionを使用しない
すべての移動コマンドは `push-mark`、`region-active-p`、`deactivate-mark` を使用してはならない（SHALL NOT）。代わりに `eme-selection-set`、`eme-selection-active-p`、`eme-selection-clear` を使用する。

#### Scenario: push-mark の不使用
- **WHEN** 任意の移動コマンドを実行する
- **THEN** `push-mark` は呼び出されない

#### Scenario: region-active-p の不使用
- **WHEN** 移動コマンド内で選択状態を確認する
- **THEN** `region-active-p` ではなく `eme-selection-active-p` が使用される

#### Scenario: deactivate-mark の不使用
- **WHEN** 移動コマンドで選択をクリアする
- **THEN** `deactivate-mark` ではなく `eme-selection-clear` が使用される

### REMOVED: eme-selection-or-region-bounds 関数
**削除理由**: Emacs region を使用しなくなるため、`eme-selection-bounds` のみで十分。
