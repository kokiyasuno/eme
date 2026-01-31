## MODIFIED Requirements

### Requirement: 選択境界の取得方法変更
`eme--get-selection-bounds` は `eme-selection-bounds` のみを使用しなければならない（SHALL）。`eme-selection-or-region-bounds` は廃止する。

#### Rationale
Emacs region を完全廃止するため、`region-active-p` や `region-beginning`/`region-end` を参照する必要がなくなる。

#### Specification: eme--get-selection-bounds の簡略化
```elisp
(defun eme--get-selection-bounds ()
  "Get selection bounds from eme-selection.
Returns (BEG . END) or nil if no selection."
  (eme-selection-bounds))
```

#### Scenario: eme-selection からの境界取得
- **GIVEN** eme-selection が `(10 . 50)` に設定されている
- **WHEN** `eme--get-selection-bounds` を呼び出す
- **THEN** `(10 . 50)` が返される

#### Scenario: 選択なしでの境界取得
- **GIVEN** eme-selection が設定されていない
- **WHEN** `eme--get-selection-bounds` を呼び出す
- **THEN** nil が返される
- **THEN** Emacs region の状態は確認されない

### Requirement: 選択クリア方法の変更
`eme--clear-selection` は `deactivate-mark` を呼び出してはならない（SHALL NOT）。`eme-selection-clear` のみを使用する。

#### Specification: eme--clear-selection の簡略化
```elisp
(defun eme--clear-selection ()
  "Clear eme-selection and reset anchor."
  (eme-selection-clear)
  (setq eme--anchor-active nil)
  (setq eme--anchor-bounds nil))
```

#### Scenario: 選択クリア
- **WHEN** `eme--clear-selection` を呼び出す
- **THEN** eme-selection がクリアされる
- **THEN** `eme--anchor-active` が nil になる
- **THEN** `eme--anchor-bounds` が nil になる
- **THEN** `deactivate-mark` は呼び出されない

### Requirement: コピーアクションは選択を維持
`eme-copy` はコピー後も選択を維持しなければならない（SHALL）。

#### Rationale
ユーザーがコピー後に続けて操作できるようにする。

#### Scenario: 選択をコピー後も維持
- **GIVEN** eme-selection が設定されている
- **WHEN** `w` を押す
- **THEN** 選択されたテキストはkill-ringにコピーされる
- **THEN** eme-selection は維持される（クリアされない）

#### Scenario: 選択なしでコピー
- **WHEN** eme-selection が設定されていない状態で `w` を押す
- **THEN** 現在の行がkill-ringにコピーされる

### MODIFIED: Undo/Redo アクション
Undo/Redo 実行時は `deactivate-mark` の代わりに anchor と selection をクリアする。

#### Specification: eme-undo の変更
```elisp
(defun eme-undo ()
  "Undo the last edit."
  (interactive)
  (eme-selection-clear)
  (setq eme--goal-column-valid nil)
  (setq eme--anchor-active nil)
  (setq eme--anchor-bounds nil)
  (undo))
```

#### Scenario: Undo 実行
- **WHEN** `u` を押す
- **THEN** 最後の編集が取り消される
- **THEN** eme-selection がクリアされる
- **THEN** anchor がクリアされる
- **THEN** `deactivate-mark` は呼び出されない

### Requirement: 全アクションでEmacs regionを参照しない
すべてのアクションは `region-active-p`、`region-beginning`、`region-end`、`deactivate-mark` を使用してはならない（SHALL NOT）。

#### Scenario: 削除アクション
- **WHEN** `d` を実行する
- **THEN** `eme-selection-bounds` から境界を取得する
- **THEN** `region-active-p` は確認されない

#### Scenario: 変更アクション
- **WHEN** `c` を実行する
- **THEN** `eme-selection-bounds` から境界を取得する
- **THEN** 選択削除後は `eme-selection-clear` でクリアする
- **THEN** `deactivate-mark` は呼び出されない
