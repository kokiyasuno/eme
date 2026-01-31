## Why

Emacsには多くのモーダル編集パッケージ（Evil, Meow, Xah-fly-keys等）が存在するが、いずれもVimのキーバインドを踏襲するか、独自の体系を持つ。Emacsユーザーが直感的に使える「Emacsキーバインドをモーダル化した」パッケージが存在しない。本パッケージは、Kakoune/Helixの「選択 → アクション」パラダイムをEmacsの慣習（n/p/f/b等）と融合させ、hjklに依存しない新しいモーダル編集体験を提供する。

## What Changes

- **新規パッケージ作成**: `emacs-modal-editing.el` を新規作成
- **Selection Mode**: 移動と選択を統合したモード。Emacsの `n/p/f/b` をそのままモーダル化
- **Match Mode**: Text Objects と Wrap 操作を `m` プレフィックスで提供
- **Insert Mode**: `i` キーで挿入モードへ遷移、`C-g` で戻る
- **Actions**: 選択に対する操作（d/w/y/c等）を即時実行
- **拡張機構**: アンカー（`.`）による選択範囲の拡張
- **検索統合**: `/` で検索、`s/r` で次/前のマッチへ移動
- **Goto/View/Scroll**: Emacs標準を尊重しつつモーダルキーを提供
- **マクロ**: `q/Q` で記録・再生
- **レジスタ**: `'` プレフィックスでEmacs標準レジスタ操作

## Capabilities

### New Capabilities

- `mode-system`: モード切り替え（Selection/Insert）の基盤とキーマップ管理
- `selection-mode`: 移動・選択操作（n/p/f/b系、粒度変更、アンカー拡張）
- `match-mode`: Text Objects（inner/around）とWrap操作（m prefix）
- `actions`: 選択に対する操作（delete, copy, yank, change, replace等）
- `search-integration`: isearch連携と検索ナビゲーション（/, s, r）
- `goto-view`: Goto操作（goto-line, xref連携）とView/Scroll操作（v/V/l）
- `macro-register`: マクロ記録・再生とレジスタ操作

### Modified Capabilities

(なし - 新規パッケージのため)

## Impact

### コード

- 新規ファイル: `emacs-modal-editing.el`（メインパッケージ）
- テストファイル: `test/emacs-modal-editing-test.el`（buttercup）

### 依存関係

- **必須**: なし（外部依存ゼロ）
- **オプション**: tree-sitter（構文ベースのText Objects用、Emacs 29+で組み込み）

### フェーズ計画

| Phase | 内容 |
|-------|------|
| Phase 1 | modal-editing-core（本変更） |
| Phase 2 | leader-key（リーダーキー、Window/Buffer操作） |
| Phase 3 | jump（avy風ジャンプ機能、自前実装） |
| Phase 4 | multi-select（マルチセレクション） |

### 既存パッケージとの差別化

| 特徴 | Evil | Meow | 本パッケージ |
|------|------|------|-------------|
| キーバインド体系 | Vim | 独自 | Emacs (n/p/f/b) |
| パラダイム | Action → Motion | 選択 → Action | 選択 → Action |
| hjkl依存 | あり | 選択可能 | なし |
| 設計複雑度 | 高 | 中 | 低 |

### パフォーマンス

- モード切り替えはキーマップの切り替えのみ、オーバーヘッドは最小
- Emacs組み込み機能を活用するため追加の処理負荷なし

### ロールバック手順

```elisp
;; アンインストール
(emacs-modal-editing-mode -1)  ; グローバルモードを無効化
;; package.el経由の場合
(package-delete 'emacs-modal-editing)
```

### MELPA登録計画

1. 基本機能（Phase 1）完成後、GitHubでv0.1.0をリリース
2. READMEとドキュメント整備
3. MELPAにPR提出
