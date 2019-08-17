# Gauche-effects

## 概要
- https://github.com/ayatoy/racket-effects  
  を、Gauche で動作するように改造したものです。


## 内容
- https://www.eff-lang.org/  
  の algebraic effects を scheme で実装したものになります。
- examples フォルダのサンプルは、  
  https://www.eff-lang.org/try/  
  の項目に対応しています。


## その他 注意事項等
1. 現状の Gauche は、限定継続と動的環境の組み合わせ時の動作が、Racket とは異なります。  
   ( https://github.com/shirok/Gauche/issues/477 )  
   このため、emu-dynamic.scm というモジュールを作って、動作をエミュレートしています。  
   (このモジュールを使用する場合、Gauche 本体の継続や動的環境処理と組み合わせると、  
   正常に動作しない場合があるため、注意してください)

2. オリジナルの effects では、prompt-at/control-at を使用していますが、  
   Gauche には存在しないため、reset/shift に置き換えています。  
   このため、複数の機能を組み合わせた場合に、動作しないケースがあるかもしれません。


## 環境等
- OS
  - Windows 8.1 (64bit)
- 言語
  - Gauche v0.9.8

## 履歴
- 2019-8-17  v1.00 (初版)
- 2019-8-17  v1.01 emu-dynamic.scmをv3.15に更新
- 2019-8-17  v1.02 プログラムの整理  
  emu-dynamic.scmをv3.16に更新
- 2019-8-18  v1.03 コメント追加等


(2019-8-18)
