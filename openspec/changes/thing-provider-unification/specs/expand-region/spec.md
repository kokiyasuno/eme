## MODIFIED Requirements

### Requirement: expand-region は eme-selection を使用
`eme-expand-region` と `eme-contract-region` は `eme-thing-all-bounds` API を使用して拡大候補を取得しなければならない（SHALL）。

#### Rationale
thing-provider への統合により、bounds 検出ロジックを一元化する。

#### Scenario: 拡大候補の取得
- **WHEN** `v` を押す
- **THEN** `eme-thing-all-bounds` を呼び出して全候補を取得する
- **THEN** 現在の選択より大きい最小の bounds を選択する

#### Scenario: 縮小候補の取得
- **WHEN** `V` を押す
- **THEN** 履歴から前の bounds を復元する
- **THEN** 履歴がなければ `eme-selection-clear` を呼び出す

### Requirement: 手動選択後の拡大
手動で作成された eme-selection が存在する場合、`v` は `eme-thing-all-bounds` を使用して現在の選択を含む最小の semantic unit を選択しなければならない（SHALL）。

#### Scenario: 手動選択後の拡大
- **GIVEN** `n` で行を選択している（eme-selection が設定されている）
- **WHEN** `v` を押す
- **THEN** `eme-thing-all-bounds` を呼び出す
- **THEN** 既存の eme-selection がクリアされる
- **THEN** カーソル位置から semantic 拡大が開始される
- **THEN** 新しい拡大履歴が開始される
