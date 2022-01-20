
* https://jakewheat.github.io/intro_to_parsing/
* https://hackage.haskell.org/package/parsec-3.1.15.0/docs/Text-Parsec.html

```bash
alias g='ghc enriched2model.hs && ghc enriched2latex.hs && ghc enriched2svg.hs && rm -f *.hi *.o'
alias c='rm -rf out; rm -f *.hi *.o enriched2model enriched2latex enriched2svg'
alias p='rm -rf out; rm -f *.hi *.o'
alias d='mv enriched2model enriched2latex enriched2svg ~/bin'
```
