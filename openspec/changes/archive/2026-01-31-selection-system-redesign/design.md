## Context

### 現状

選択システムが二重構造になっている：

1. **Emacs region** (`mark`/`point`): 標準的な Emacs の選択機構
   - 使用箇所: `eme-selection.el` の `f`/`b` (ワード移動)、`v` (expand)、アンカー機能
   - 関数: `push-mark`, `region-active-p`, `region-beginning`, `region-end`, `deactivate-mark`

2. **eme-selection** (overlay): 独自の選択機構
   - 使用箇所: `n`/`p` (ライン移動)
   - 関数: `eme-selection-set`, `eme-selection-active-p`, `eme-selection-bounds`, `eme-selection-clear`
   - 利点: カーソル位置と選択範囲が独立（goal column を維持しながら行全体を選択可能）

### 問題

- `n` → `.` でアンカー設定時、eme-selection から Emacs region への変換が失敗（カーソルが行中にあると行全体を選択できない）
- `eme--get-selection-bounds` で両方を統合しようとしているが、挙動が一貫しない
- 将来の multiple selection 実装が困難（Emacs region は単一選択のみ）

### 制約

- Emacs 29.1+ をサポート
- cl-lib 使用禁止
- 外部依存なし

## Goals / Non-Goals

**Goals:**
- 全ての選択操作を `eme-selection` (overlay) に統一
- アンカー機能を `eme--anchor-bounds` で再実装
- 一貫した選択動作を実現
- 将来の multiple selection への拡張性を確保

**Non-Goals:**
- multiple selection の実装（将来の変更として別途実施）
- Emacs region との互換性維持（完全廃止）
- kill-ring 以外の Emacs 組み込み機能との連携変更

## Decisions

### [Decision 1] 選択機構の統一

**選択**: `eme-selection` (overlay) に統一

**理由**:
- カーソル位置と選択範囲の独立性が必要（特にライン選択で goal column 維持）
- overlay はリストで複数管理可能 → multiple selection への自然な拡張
- Emacs region は mark〜point 間のみ選択可能という根本的制約

**代替案**:
- Emacs region を維持しつつハック → 複雑化、根本解決にならない
- 両方維持して使い分け → 現状の問題が解決しない

### [Decision 2] アンカー機能の再設計

**選択**: `eme--anchor-bounds` で選択範囲を記憶し、移動時にマージ

**新しい動作**:
```
n (line1 選択)
  eme-selection: [line1-beg, line1-end]
  anchor-bounds: nil

. (アンカー ON)
  eme-selection: 維持
  anchor-bounds: (line1-beg . line1-end)  ← 選択範囲を記憶

n (line2 へ移動)
  新しい範囲: [line2-beg, line2-end]
  マージ: [min(anchor-start, new-start), max(anchor-end, new-end)]
        = [line1-beg, line2-end]
  eme-selection: [line1-beg, line2-end]  ← 2行選択
  anchor-bounds: 維持

. (アンカー OFF)
  eme-selection: クリア
  anchor-bounds: nil
```

**理由**:
- 単一位置ではなく範囲を記憶することで、ライン選択との整合性を確保
- マージ方式により、上下どちらへ移動しても自然に拡張

**代替案**:
- `eme--anchor-pos` (単一位置) → ライン選択で「どこからどこまで」が曖昧になる

### [Decision 3] 既存変数・関数の変更

| 変更前 | 変更後 |
|--------|--------|
| `eme--anchor-active` | 維持（フラグとして使用） |
| `eme--anchor-pos` | `eme--anchor-bounds` に置き換え |
| `eme-selection-or-region-bounds` | `eme-selection-bounds` に簡略化 |
| `push-mark` 呼び出し | `eme-selection-set` に置き換え |
| `region-active-p` チェック | `eme-selection-active-p` に置き換え |
| `deactivate-mark` 呼び出し | `eme-selection-clear` に置き換え |

## Risks / Trade-offs

### [Risk] 他パッケージとの互換性

Emacs の組み込みコマンドや外部パッケージが `region-active-p` 等を期待する場合、eme の選択が認識されない。

→ **Mitigation**: eme のアクション (`d`, `w`, `c` 等) は既に bounds を直接渡しているため影響なし。外部パッケージとの連携が必要な場合は、明示的に region を設定するラッパーを提供（将来対応）。

### [Risk] 既存テストの失敗

region 関連のテストが失敗する可能性。

→ **Mitigation**: テストを eme-selection ベースに書き換え。

### [Trade-off] Emacs 標準機能からの乖離

Emacs の標準的な選択方式から独自方式へ移行することで、Emacs 経験者の期待と異なる動作になる可能性。

→ **Acceptance**: eme はそもそも Emacs 標準から離れたモーダル編集を提供しており、選択動作の一貫性を優先する。

## File Structure

変更対象ファイル：

| ファイル | 変更内容 |
|----------|----------|
| `eme-core.el` | `eme--anchor-bounds` 追加、`eme-selection-or-region-bounds` 削除 |
| `eme-selection.el` | 全移動コマンドを eme-selection ベースに書き換え、`eme-toggle-anchor` 再実装 |
| `eme-actions.el` | `eme--clear-selection` から `deactivate-mark` 削除、region 参照を削除 |
| `eme-expand.el` | `region-active-p` → `eme-selection-active-p`、`push-mark` → `eme-selection-set` |
| `eme-delimiter.el` | 同様の置き換え |

## Performance Considerations

- overlay 操作は region と同等のパフォーマンス
- 単一選択の場合、overlay は1つのみ
- multiple selection 実装時はオーバーレイ数に比例するが、通常使用（10個程度）では問題なし

## Open Questions

- なし（探索モードで解決済み）
