## Context

現在の eme パッケージでは thing bounds 検出が以下のように分散している：

- **eme-thing.el**: delimiter 検出（`syntax-ppss`）
- **eme-treesit.el**: tree-sitter ベースの AST ノード検出
- **eme-expand.el**: word, symbol, sexp-chain, string, list, defun の bounds 検出
- **eme-delimiter.el**: delimiter pair の検出（`syntax-ppss`）
- **eme-selection.el**: 移動後の word, sexp, line 等の bounds 計算

この分散により：
- 同じ検出ロジックの重複（delimiter 検出が 2 箇所）
- tree-sitter 統合の一貫性欠如
- 新しい thing 追加時の変更箇所の散在

## Goals / Non-Goals

**Goals:**
- 全ての thing bounds 検出を eme-thing.el に集約
- 統一された API (`eme-thing-bounds`, `eme-thing-all-bounds`) の提供
- tree-sitter 統合の一元化
- ユーザーによるカスタム thing の追加を可能に
- 既存 API との後方互換性維持

**Non-Goals:**
- パフォーマンスの大幅な改善（現状維持で十分）
- 新しい thing type の追加（この変更では行わない）
- キーバインドの変更

## Decisions

### Decision 1: Provider Registry パターンの採用

**選択**: alist ベースの provider registry

```elisp
(defvar eme-thing-providers
  '((word      . eme-thing--word)
    (symbol    . eme-thing--symbol)
    (sexp      . eme-thing--sexp)
    (line      . eme-thing--line)
    (delimiter . eme-thing--delimiter)
    (defun     . eme-thing--defun)
    ...)
  "Alist mapping thing types to provider functions.")
```

**理由**:
- シンプルで理解しやすい
- ユーザーが `add-to-list` で簡単に拡張可能
- cl-lib 不要

**代替案**:
- plist: キー検索が O(n) で alist と同等だが、拡張性で劣る
- hash-table: 少数の thing type では過剰、メモリ効率が悪い
- defcustom + :type: 複雑すぎる

### Decision 2: 返り値の構造

**選択**: plist 形式

```elisp
;; delimiter の場合
(:type delimiter :char ?\( :bounds (10 . 50) :inner (11 . 49))

;; word の場合（inner 概念なし）
(:type word :bounds (10 . 15) :inner nil)
```

**理由**:
- 将来の拡張に対応（metadata 追加可能）
- デバッグ時に情報が明確
- inner/bounds 両方を一度に返せる（計算コストほぼゼロ）

**代替案**:
- cons cell `(start . end)`: シンプルだが inner/bounds の区別ができない
- 複数値 `(values start end inner-start inner-end)`: Emacs Lisp では扱いにくい

### Decision 3: tree-sitter 統合方式

**選択**: provider 関数内で条件分岐

```elisp
(defun eme-thing--delimiter (&optional char)
  "Get delimiter bounds, using tree-sitter when available."
  (if (eme-thing--treesit-available-p)
      (eme-thing--delimiter-treesit char)
    (eme-thing--delimiter-syntax char)))
```

**理由**:
- 各 thing で tree-sitter の使い方が異なる
- フォールバックロジックを thing ごとに制御可能
- eme-treesit.el の関数を内部関数として移植

**代替案**:
- 別レイヤーとして分離: 抽象化の複雑さが増す
- tree-sitter 専用 provider: 重複コードが増える

### Decision 4: eme-treesit.el の扱い

**選択**: eme-thing.el に統合し、eme-treesit.el は削除

**理由**:
- tree-sitter は thing 検出の実装詳細
- 外部から直接呼び出す必要がない
- ファイル数削減によるメンテナンス性向上

**代替案**:
- 独立ファイルとして維持: 現状の分散問題が残る
- eme-thing-treesit.el にリネーム: 中途半端

### Decision 5: 後方互換性の維持

**選択**: 既存関数をラッパーとして残す

```elisp
;; 既存 API（後方互換）
(defun eme-thing-inner ()
  "Select inner of nearest delimiter."
  (interactive)
  (let ((result (eme-thing-bounds 'delimiter)))
    (when result
      (eme--select-bounds (plist-get result :inner)))))
```

**理由**:
- 既存のキーバインドがそのまま動作
- 段階的な移行が可能

## ファイル構成

```
eme/
├── eme-thing.el      # 中央 thing provider（tree-sitter 統合含む）
├── eme-expand.el     # v/V コマンド（thing API 使用）
├── eme-delimiter.el  # m/M コマンド（thing API 使用）
├── eme-selection.el  # 移動コマンド（thing API 使用）
├── eme-actions.el    # アクション（thing API 使用）
└── [eme-treesit.el]  # 削除
```

## パフォーマンスクリティカルな部分

- `eme-thing-all-bounds`: expand-region で毎回呼び出される
  - 対策: 結果をキャッシュしない（point 移動で無効化が複雑）
  - 許容: 現状の eme-expand.el と同等のコスト

- `eme-thing--treesit-available-p`: 頻繁に呼び出される
  - 対策: buffer-local 変数でキャッシュ

## Risks / Trade-offs

### Risk 1: tree-sitter フォールバックの不整合
**リスク**: tree-sitter あり/なしで微妙に異なる bounds を返す可能性
**緩和**: 既存の eme-treesit.el のロジックをそのまま移植、テストで検証

### Risk 2: 循環依存
**リスク**: eme-thing.el が他のファイルに依存し、循環が発生
**緩和**: eme-thing.el は eme-core.el のみに依存、他は eme-thing.el に依存する一方向に

### Risk 3: API 変更による既存コードの破損
**リスク**: 内部 API を使用しているユーザーコードが動作しなくなる
**緩和**: 既存の公開関数をラッパーとして維持、deprecation warning は出さない

## 他パッケージとの統合ポイント

- **which-key**: 影響なし（キーバインド変更なし）
- **expand-region (magnars)**: 競合する可能性があるが、独自実装のため問題なし
- **tree-sitter 関連パッケージ**: `treesit-*` API のみ使用、外部パッケージへの依存なし
