## Why

現在の選択システムは Emacs region (`mark`/`point`) と `eme-selection` (overlay) の二重構造になっており、これが複数の問題を引き起こしている。特にアンカー設定時に選択が壊れる（Emacs region は mark〜point 間しか選択できないため、カーソルが行中にあると行全体を選択できない）。また、将来の multiple selection 実装を見据えると、overlay ベースの `eme-selection` に統一することで自然に拡張可能な設計になる。

## What Changes

- **BREAKING**: Emacs region (`push-mark`, `region-active-p`, `deactivate-mark` 等) の使用を完全廃止
- 全ての選択操作を `eme-selection` (overlay) に統一
- アンカー機能を `eme--anchor-bounds` (選択範囲を記憶) で再実装
- 移動時のアンカー動作を「anchor-bounds と新しい範囲のマージ」に変更
- `eme-selection-or-region-bounds` を `eme-selection-bounds` に簡略化

## Capabilities

### New Capabilities

なし（既存機能の内部実装変更）

### Modified Capabilities

- `selection-movement`: 全移動コマンドを `eme-selection` ベースに変更、アンカー動作の再設計
- `actions`: `eme--get-selection-bounds` の簡略化、`deactivate-mark` の削除
- `expand-region`: `region-active-p` → `eme-selection-active-p`、`push-mark` → `eme-selection-set`
- `delimiter-operations`: 同様の置き換え

## Impact

- **影響ファイル**:
  - `eme-core.el`: `eme--anchor-bounds` 追加、`eme-selection-or-region-bounds` 削除
  - `eme-selection.el`: 全移動コマンドの書き換え、アンカー機能の再実装
  - `eme-actions.el`: region 関連コード削除
  - `eme-expand.el`: region → eme-selection 置き換え
  - `eme-delimiter.el`: 同様

- **ロールバック手順**: git revert で変更前の状態に戻せる

- **パフォーマンス**: overlay は region と同等のパフォーマンス。multiple selection 時はオーバーレイ数に比例するが、通常使用では影響なし

- **外部依存**: なし（Emacs 組み込み機能のみ使用）

- **互換性**: 内部実装の変更のみ。ユーザー向けキーバインドや操作は変わらない
