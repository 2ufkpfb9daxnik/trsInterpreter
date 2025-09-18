# trsInterpreter

[北陸先端科学技術大学院大学計算理論研究室](https://www.jaist.ac.jp/is/labs/toc/)でインターンをさせていただいたときに作成したhaskell製.trs形式/.fp形式を読んでrewritingTool.hsのmainの正規形を出力するインタプリタ

作ったのはTRS.hsで、TRSParserは提供された

ルートのA?.hsやl?.hsなどは事前課題(haskellの学習)

## 環境

ghcupでcabalを入れ、vscodeの拡張としてsimple ghc (haskell) integrationを入れて作成

基本simple ghc integrationで開発し、cabalは.trs形式/.fp形式のファイルを読みこんでrewritingTool.hsからちゃんと全部動かすときに使った
