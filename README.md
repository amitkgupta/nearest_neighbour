# k-NN Algorithm in Go

This is a quick and dirty implementation of the [k-NN algorithm](http://en.wikipedia.org/wiki/K-nearest_neighbors_algorithm) in [Go](http://golang.org/) (for k = 1).

## Blog Chain

This was inspired by a chain of blog posts, the [last one](http://re-factor.blogspot.ca/2014/06/comparing-k-nn-in-factor.html) implemented it in [Factor](http://factorcode.org/).  Before that, [this one](http://huonw.github.io/2014/06/10/knn-rust.html) did it in [Rust](http://www.rust-lang.org/).  That one was inspired by [a blog post](http://philtomson.github.io/blog/2014/05/29/comparing-a-machine-learning-algorithm-implemented-in-f-number-and-ocaml/) which did it in [F#](http://fsharp.org/) and [OCaml](http://ocaml.org/), and [a follow-up](http://philtomson.github.io/blog/2014/05/30/stop-the-presses-ocaml-wins/) which improves the first OCaml implementation.

## Comparison

I spun up a c3.xlarge on AWS, installed Golang, Rust, OCaml, F#, and downloaded Factor.  I've copy-pasta'd the code in all those blog posts, (compiled executable binaries for Rust, OCaml and F#) and run them on my machine.  Here's a performance comparison summary, from fastest to slowest:

* Golang: 3.467s
* Factor: 6.358s
* OCaml: 12.757s
* F#: 23.507s
* Rust: 78.138s

They're all the same algorithm, but by some luck, the Golang implementation does a bit better in terms of accuracy (95% vs. 94.4% for all the others).  The Go implementation takes each member of the test sample, and in case there's a tie amongst members of the training sample vying to be the test member's nearest neighbour, it picks the first one ("first" meaning "occurring higher up in 'validationsample.csv'").  The other languages aren't transparent enough for me to tell immediately how they're picking, but maybe they don't pick as deterministically?

#### Golang
```
$ time go run main.go

475

real	0m3.467s
user	0m3.367s
sys	0m0.111s
```

#### Rust
```
$ time ./foo

Percentage correct: 94.4%

real	1m18.138s
user	1m17.980s
sys	0m0.155s
```

#### OCaml
```
$ time ./classifyDigitsArray

Percentage correct:94.400000

real	0m12.757s
user	0m12.500s
sys	0m0.257s
```

#### F#
```
$ time ./foo.exe

start...
Percentage correct:94.400000

real	0m23.507s
user	0m22.751s
sys	0m0.798s
```

#### Factor
```
$ mkdir -p $FACTOR_HOME/work/k-nn
$ cp k-nn.factor $FACTOR_HOME/work/k-nn
$ cp *.csv $FACTOR_HOME/work/k-nn
$ $FACTOR_HOME/factor

IN: scratchpad USE: k-nn
Loading resource:work/k-nn/k-nn.factor
Loading resource:basis/formatting/formatting.factor
Loading resource:basis/formatting/formatting-docs.factor

IN: scratchpad gc [ k-nn ] time
Percentage correct: 94.400000
Running time: 6.357621145 seconds
```

## TODO

* See if it can be sped up with goroutines
* Stop calling everything "foo"
* Update this README since you no longer call everything "foo"
* Write a blog post
* Do it in C++
* Do it in Python with [scikit-learn](http://scipy-lectures.github.io/advanced/scikit-learn/)

