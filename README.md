```
make
./tapir samples/foobar.pi
```


サブプロセス同士で型環境を共有しないI/O typeで
 ch!(...)型をもつチャネル引数を、変数そのままに変換
 それ以外のチャネルは、regionに変換

* non-replicated inputの扱い
    * アノテーションをつけて、関数定義に変換できるようにした
        * ds-ex5-1-sim.pi
        * Web版のMoCHiで失敗したが、最新版では成功
    * 次回までに、アノテーションをつけずに変換できるように
        * そのまま変数に変換されるチャネル変数ができるだけ束縛されるようにヒューリスティックを決めて、変換
            *x?c. y?d. (P_{xのfunbody} | P_{yのfunbody})
              P_{xのfunbody} に c が出現
              P_{yのfunbody} に d が出現


TODO

* MoCHiとの連携
* nested inputの制約, non-replicated inputの扱い
* 実験

* x?y. x!1. (P1 | P2)の扱い
* fold_left vs. fold_right
* OCaml実装では、linesなどのannotationはどうなっているか?
* TyPiCalでは、どこにIO情報、型環境をのせているか?
