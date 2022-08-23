## 3110dle                                                                                                                                                    

This document gives the instructions to install and play our project, 3110dle 

### Installation Instructions (Linux, Mac OS X, and Windows)

1. After unzipping open terminal in the directory where it was unzipped

2. Create a new opam environment

   ```opam switch create 3110dle ocaml-base-compiler.4.12.0```

3. Install the required packages

   ```opam install dune```

   ```opam install ANSITerminal```

   ```opam install ounit2```
   
   ```opam install progress``

4. Type the following commands to play the game

   ```make build```

   ```make play```

### How to play 

Wordle gives players six chances to guess a randomly selected five-letter word. As shown above, if you have the right letter in the right spot, it shows up green. A correct letter in the wrong spot shows up yellow. A letter that isn't in the word in *any* spot shows up gray (Cnet.com).





