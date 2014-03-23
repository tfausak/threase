# [Threase][1]

[![Build Status][2]][3]

Plays [Threes][4] with ease.

``` sh
make install
make repl
```

``` hs
λ let g = Game {board = Board {vectors = [Vector {tiles = [Just (Tile {number = 2}),Nothing,Just (Tile {number = 1}),Just (Tile {number = 2})]},Vector {tiles = [Nothing,Nothing,Just (Tile {number = 2}),Just (Tile {number = 1})]},Vector {tiles = [Nothing,Just (Tile {number = 2}),Just (Tile {number = 3}),Just (Tile {number = 3})]},Vector {tiles = [Nothing,Nothing,Nothing,Just (Tile {number = 3})]}]}, next = Tile {number = 1}}
λ putStr (render g)
Score: 9
Next: 1
2   -   1   2
-   -   2   1
-   2   3   3
-   -   -   3
Moves: ← ↓ → ↑
Quality: 26
```

[1]: https://github.com/tfausak/threase
[2]: https://travis-ci.org/tfausak/threase.svg?branch=master
[3]: https://travis-ci.org/tfausak/threase
[4]: http://asherv.com/threes/
