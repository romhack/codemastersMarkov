# codemastersMarkov. 
Markov graphics compression tool for a variety of Codemasters games on NES.


Synopsis:
```
codemastersMarkov [-d | -c] inFileName outFileName
```
  
Description:
```

codemastersMarkov -d <inFile> <offset> <outFile> - Decompress block from given ROM file.

codemastersMarkov -c <inFile> <outFile> - Compress given plain block.

-h - Display help

-v - Output version information
```

Compression scheme was initially reverse engineered at nesdev forums by tokumaru in 2009. He has also found a defect in original scheme and implemented a development compression tool with modified algorithm, which compressed better, than original, but, unfortunately, not optimal. In 2016 Bisqwit has also written his own tool with slight modifications and fully optimal. Interested one can find more information in [this](https://wiki.nesdev.com/w/index.php/Tile_compression#Codemasters) nesdev wiki article. 
 
This actual tool handles original compression scheme, which compressed tiles in some of actual Codemasters games on NES. And can be used in romhacking/translation purposes.  
This is a list of games which are known to use the supported compression method, or are assumed to, based on a binary search of the games' ROMs:

* Bee 52
* Fire Hawk
* Quattro Adventure
* Quattro Arcade
* Quattro Sports
* Super Robin Hood
* Big Nose Freaks Out
* Dizzy The Adventurer
* Linus Spacehead's Cosmic Crusade
* MiG 29 - Soviet Fighter (Unl)

Also note, that it's impossible to automatically detect or locate compressed data, so you have to debug these yourselves. See additional files in [release](https://github.com/romhack/codemastersMarkov/releases/latest) archive for examples of usage.

The compressor is optimal, so ratios are the same or better, compared to the original packer's ratios.

Build with [Haskell Stack](https://haskellstack.org) tool.
