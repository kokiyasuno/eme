## REMOVED Requirements

### Requirement: 行内容テキストオブジェクト (inner line)
**Reason**: Match Mode の inner/around モデルを廃止し、thing システムに統合するため。`m i l` は3ストローク以上で2ストローク原則に反する。行選択は `n`/`p` の行移動で代用可能。

**Migration**:
- 行内容の選択は `n` または `p` で行を選択後、必要に応じて調整
- インデントを除いた行内容は `a` で行頭に移動後、`e` で行末まで選択
- thing システムの将来的な拡張で `t i l` として復活の可能性あり

### Requirement: 行全体テキストオブジェクト (around line)
**Reason**: Match Mode の inner/around モデルを廃止し、thing システムに統合するため。`m a l` は3ストローク以上で2ストローク原則に反する。

**Migration**:
- 行全体の選択は `n` または `p` で行を選択
- 複数行の選択は `n`/`p` を繰り返すか、数値引数を使用
- thing システムの将来的な拡張で `t b l` として復活の可能性あり

### Requirement: 行テキストオブジェクトのキーマップ統合
**Reason**: Match Mode の inner/around キーマップ（`eme-match-inner-map`、`eme-match-around-map`）自体を廃止するため。

**Migration**:
- `m` キーは surround 操作専用に変更（`m w` = wrap, `m d` = delete, `m r` = replace）
- inner/around の概念は delimiter 限定で thing システム（`t i`/`t b`）に移行

### Requirement: 行テキストオブジェクトは全 major mode で動作する
**Reason**: 上記 requirement の削除に伴い、この requirement も不要。

**Migration**: 行選択機能（`n`/`p`）は全 major-mode で動作する。
