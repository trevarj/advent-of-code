# aoc-haskell

Spent a good amount of time learning the current Haskell build system/environment. 
- Downloaded `ghcup`
- Used `ghcup` to select a GHC / HLS version that was also compatible with a formatter (`fourmolu`)
- Ran `stack new aoc-haskell` to create this mess!

I wrote a rudamentary CLI to run solutions by day and part:

```shell
stack run [day] [part]
```
It will look for an input file in `inputs/` named `day[n]_part[n].txt`.