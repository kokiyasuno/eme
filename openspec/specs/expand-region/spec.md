## MODIFIED Requirements

### Requirement: expand-region は eme-selection を使用
`eme-expand-region` と `eme-contract-region` は Emacs region ではなく eme-selection を使用しなければならない（SHALL）。

#### Rationale
選択システム全体を eme-selection に統一するため。

#### Scenario: 拡大時の選択設定
- **WHEN** `v` を押す
- **THEN** 拡大された範囲が eme-selection で設定される
- **THEN** `push-mark` は呼び出されない

#### Scenario: 縮小時の選択設定
- **WHEN** `V` を押す
- **THEN** 縮小された範囲が eme-selection で設定される
- **THEN** `deactivate-mark` は呼び出されない

### Requirement: 選択状態の確認方法変更
expand-region は `region-active-p` の代わりに `eme-selection-active-p` を使用しなければならない（SHALL）。

#### Scenario: 既存選択の確認
- **WHEN** `v` を押す
- **WHEN** 既存の選択状態を確認する
- **THEN** `eme-selection-active-p` が使用される
- **THEN** `region-active-p` は使用されない

### Requirement: 手動選択後の拡大
手動で作成された eme-selection が存在する場合、`v` は現在の選択を含む最小の semantic unit を選択しなければならない（SHALL）。

#### Scenario: 手動選択後の拡大
- **GIVEN** `n` で行を選択している（eme-selection が設定されている）
- **WHEN** `v` を押す
- **THEN** 既存の eme-selection がクリアされる
- **THEN** カーソル位置から semantic 拡大が開始される
- **THEN** 新しい拡大履歴が開始される

### Requirement: 縮小の限界での動作
最小の選択（word）が選択されている状態で `V` を押すと、eme-selection がクリアされなければならない（SHALL）。

#### Scenario: 縮小の限界
- **WHEN** 最小の選択（word）が選択されている
- **WHEN** `V` を押す
- **THEN** eme-selection がクリアされる
- **THEN** `deactivate-mark` は呼び出されない
