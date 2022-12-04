## Solutions for [Advent Of Code](adventofcode.com)

### Repo structure:

```
├── <year>
│   ├── <day>
│   │   ├── input.txt
│   │   ├── sample.txt (optional
│   │   └── <file(s) with solution>
│   └── auxiliary files holding project(s) metadata, i.e. foo.cabal for Haskell code (optional)
│   ...
```

### I/O format

A solution program tend to accept the input through STDIN, process it, then
output the answers to STDOUT, denoting # of the Part it belongs to
I.e.:

```bash
$ python ./2021/13/main.py < ./2021/13/input.txt
Part 1: 842
Part 2: 
######    ########  ##    ##  ######      ####        ####  ########  ##    ##
##    ##  ##        ##  ##    ##    ##  ##    ##        ##        ##  ##    ##
######    ######    ####      ##    ##  ##              ##      ##    ##    ##
##    ##  ##        ##  ##    ######    ##              ##    ##      ##    ##
##    ##  ##        ##  ##    ##  ##    ##    ##  ##    ##  ##        ##    ##
######    ##        ##    ##  ##    ##    ####      ####    ########    ####  
```
