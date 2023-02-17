```
make
./tapir samples/foobar.pi
```


サブプロセス同士で型環境を共有しないI/O typeで
 ch!(...)型をもつチャネル引数を、変数そのままに変換
 それ以外のチャネルは、regionに変換

TODO
* MoCHiとの連携
* nested inputの制約, non-replicated inputの扱い
* 実験
