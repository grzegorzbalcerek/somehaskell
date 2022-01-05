
* https://jakewheat.github.io/intro_to_parsing/
* https://hackage.haskell.org/package/parsec-3.1.15.0/docs/Text-Parsec.html

```bash
export E=~/projects/kata/katamarkon.txt
export E=enriched2.txt
export E=enriched1.txt
alias g='ghc enriched2model.hs && ghc enriched2latex.hs && ghc enriched2svg.hs && rm -f *.hi *.o'
alias r='./enriched2model $E out/$(basename $E) && ./enriched2latex $E out/$(basename -s .txt $E).tex && ./enriched2svg $E out/$(basename -s .txt $E).svg'
alias m='cat out/$(basename $E)'
alias t='cat out/$(basename -s .txt $E).tex'
alias x='(cd out; xelatex $(basename -s .txt $E).tex)'
alias e='(evince out/$(basename -s .txt $E).pdf)'
alias s='cat out/$(basename -s .txt $E).svg'
alias i='inkscape out/$(basename -s .txt $E).svg'
alias c='rm -rf out; rm -f *.hi *.o enriched2model enriched2latex enriched2svg'
alias p='rm -rf out; rm -f *.hi *.o'
alias d='cp enriched2model enriched2latex enriched2svg ~/bin'
```
