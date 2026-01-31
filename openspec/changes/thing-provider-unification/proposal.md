## Why

thing（word, symbol, sexp, delimiter, line, defun 等）の bounds 検出ロジックが複数ファイルに重複・分散している。eme-thing.el、eme-delimiter.el、eme-expand.el、eme-selection.el、eme-treesit.el がそれぞれ独自に `syntax-ppss`、`bounds-of-thing-at-point`、`beginning-of-defun` 等を呼び出しており、コードの重複とメンテナンス性の低下を招いている。統一された thing provider を提供することで、一貫性のある API と tree-sitter 統合の一元化を実現する。

## What Changes

- **eme-thing.el を中央 thing provider に再設計**: 全ての thing bounds 検出を担う統一 API を提供
- **eme-treesit.el を eme-thing.el に統合**: tree-sitter 関連コードを thing provider 内に吸収
- **eme-expand.el の簡素化**: 独自 bounds 検出を削除し、thing API を使用
- **eme-delimiter.el の簡素化**: 独自 delimiter 検出を削除し、thing API を使用
- **eme-selection.el の簡素化**: 移動後の bounds 取得を thing API に委譲
- **eme-actions.el の更新**: line bounds 取得を thing API に委譲

## Capabilities

### New Capabilities

- `thing-provider`: 全ての thing bounds 検出を担う中央 provider。provider registry、統一 API（`eme-thing-bounds`、`eme-thing-all-bounds`、`eme-thing-outer`）、tree-sitter 統合を含む

### Modified Capabilities

- `thing-selection`: thing 選択コマンドが thing-provider API を使用するよう変更
- `expand-region`: 拡大/縮小が thing-provider の `eme-thing-all-bounds` を使用するよう変更
- `delimiter-operations`: delimiter 検出が thing-provider API を使用するよう変更
- `selection-movement`: 移動コマンドが thing-provider の bounds 取得を使用するよう変更

## Impact

### コード変更

| ファイル | 変更内容 |
|---------|---------|
| eme-thing.el | 大幅リファクタ: 中央 provider 化 |
| eme-treesit.el | **削除**: eme-thing.el に吸収 |
| eme-expand.el | 簡素化: bounds 検出コード削除 |
| eme-delimiter.el | 簡素化: delimiter 検出コード削除 |
| eme-selection.el | 簡素化: bounds 計算を thing API に委譲 |
| eme-actions.el | 軽微: line bounds を thing API に委譲 |

### API 変更

新規公開 API:
- `eme-thing-bounds (thing &optional arg)` - 指定 thing の bounds を返す
- `eme-thing-all-bounds ()` - 全 thing の bounds をサイズ順で返す
- `eme-thing-outer (thing current-bounds &optional arg)` - 外側の bounds を返す
- `eme-thing-providers` - thing provider の registry（カスタマイズ可能）

### 依存関係

- 外部依存の追加なし
- eme-treesit.el への require が eme-thing.el への require に変更

### パフォーマンス

- 初回呼び出し時のコストは現状と同等
- 複数 thing の bounds を取得する場合、tree-sitter クエリの共有により効率化の可能性あり

### ロールバック

統合前のファイル構成に戻すことで元の状態に復元可能。API 互換性を維持するため、既存の `eme-thing-inner`、`eme-thing-bounds` 等の関数は内部で新 API を呼び出す形で残す。
