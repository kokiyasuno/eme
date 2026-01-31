## Context

v1 リリース前のコードベースクリーンアップ。以下の問題が存在：

1. **未実装関数**: `eme-exchange-point-and-mark` が `x` キーにバインドされているが定義がない
2. **未使用変数**: `eme-core.el:312` に未使用の `buf` 変数
3. **スタイル違反**: 6箇所の docstring が80文字を超過

## Goals / Non-Goals

**Goals:**
- バイトコンパイル警告をゼロにする
- `x` キーで選択範囲の始点・終点交換を実現
- コードスタイルの統一

**Non-Goals:**
- 新機能の追加
- アーキテクチャの変更
- テストの追加（既存テストが通れば十分）

## Decisions

### 1. `eme-exchange-point-and-mark` の実装

**選択**: `eme-selection.el` に実装

**理由**:
- 選択関連の関数は `eme-selection.el` に集約されている
- `eme-selection-bounds` と `eme-selection-set` を使用して実装
- Emacs の `exchange-point-and-mark` との整合性を保つ

**実装方針**:
```elisp
(defun eme-exchange-point-and-mark ()
  "Exchange point and mark in eme-selection.
Point moves to the other end of the selection."
  (interactive)
  (when (eme-selection-active-p)
    (let ((bounds (eme-selection-bounds)))
      (if (= (point) (car bounds))
          (goto-char (cdr bounds))
        (goto-char (car bounds))))))
```

### 2. 未使用変数の削除

**場所**: `eme-core.el:312`

**対応**: `(let ((buf (current-buffer)))` から `buf` バインディングを削除

### 3. docstring 整形

**対象ファイル**:
- `eme-selection.el`: 5箇所
- `eme-delimiter.el`: 1箇所

**方針**: 80文字で折り返し、意味が通るように改行

## Risks / Trade-offs

| リスク | 緩和策 |
|--------|--------|
| `exchange-point-and-mark` の動作が期待と異なる | Emacs 標準の動作を参考にし、直感的な挙動を優先 |
| docstring 整形で情報が失われる | 内容は維持し、改行位置のみ調整 |

## Migration Plan

1. 変更を実施
2. バイトコンパイルで警告がないことを確認
3. 既存テストが通ることを確認
4. 手動で `x` キーの動作を確認

**ロールバック**: 不要（破壊的変更なし）
