# k-NN Algorithm in Golang, Haskell, Rust, F\#, OCaml, and Factor

This repository contains naive implementations of the [k-NN algorithm](http://en.wikipedia.org/wiki/K-nearest_neighbors_algorithm) in several languages (for k = 1), and an extra one in Golang with some optimizations.

## Blog Chain

This was inspired by a chain of blog posts:
* [the last one](http://re-factor.blogspot.ca/2014/06/comparing-k-nn-in-factor.html) implemented it in [Factor](http://factorcode.org/)
* [the one before that](http://huonw.github.io/2014/06/10/knn-rust.html) did it in [Rust](http://www.rust-lang.org/).  
* that one was inspired by [a blog post](http://philtomson.github.io/blog/2014/05/29/comparing-a-machine-learning-algorithm-implemented-in-f-number-and-ocaml/) which had it in [F#](http://fsharp.org/) and [OCaml](http://ocaml.org/), and [a follow-up](http://philtomson.github.io/blog/2014/05/30/stop-the-presses-ocaml-wins/) which improves the first OCaml implementation.

This repository adds the naive implementation in [Golang](http://golang.org) and [Haskell](http://www.haskell.org/), and an additional implementation in Golang with some optimizations.

## Comparison

Performance comparisons between the naive implementations in each language were performed on a freshly spun up c3.xlarge EC2 instance as follows:

1. Install Golang, Haskell, Rust, F#, and OCaml. Download Factor.
2. Write the (naive) code for Golang and Haskell. Copy-paste the code for Rust, F#, OCaml, and Factor.
3. Compile executable binaries for Haskell, Rust, F#, and OCaml.  Run the Factor code in the `scratchpad` REPL.  Run the Golang code with `go run`.

### Results

1. Golang: 3.467s
1. Factor: 6.358s
1. OCaml: 12.757s
1. F#: 23.507s
1. Rust: 78.138s
1. Haskell: 91.581s

The naive implementations are all essentially the same algorithm, but the Golang implementation does a bit better in terms of how accurately it classifies memebers of the validation sample (95% vs. 94.4% for all the others).

#### Golang
```
$ time go run golang-k-nn.go

0.95

real	0m3.467s
user	0m3.367s
sys	0m0.111s
```

#### Haskell
```
$ time ./haskell-k-nn

Percentage correct: 472

real  1m31.581s
user  1m29.191s
sys 0m2.384s
```

#### Rust
```
$ time ./rust-k-nn

Percentage correct: 94.4%

real	1m18.138s
user	1m17.980s
sys	0m0.155s
```

#### F\# 
```
$ time ./fsharp-k-nn.exe

start...
Percentage correct:94.400000

real	0m23.507s
user	0m22.751s
sys	0m0.798s
```

#### OCaml
```
$ time ./ocaml-k-nn

Percentage correct:94.400000

real	0m12.757s
user	0m12.500s
sys	0m0.257s
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

## Optimized implementation in Golang

For Golang, an additional implementation is given which is signficantly faster, but suffers no loss in accuracy.  It involves two optimizations:

1. Short-circuit distance calculations between a test case and a training case that are necessarily suboptimal.  In other words, if you know the distance to one potential nearest neighbour is 100, and half-way through calculating the distance to another potential nearest neighbour you already have a distance-so-far of 105, stop calculating and move on to the next candidate for nearest neighbour.
2. Use goroutines to parallelize the computations.  The way this was done was not ideal, because the parallelism isn't in the classification algorithm itself, instead it parellelizes the classification of the members of the validation sample.  However, that is easy enough to do, and what's currently there is good enough to see how significant the gains are when firing on all your cores.

```
$ time go run golang-k-nn-speedup.go

0.95

real	0m1.247s
user	0m2.789s
sys	0m0.116s
```

## Contributing

1. Tell me if I should use special compiler flags to improve performance for some of the languages.
2. Tell my why this experiment is invalid.
2. Improve naive implementations without changing the spirit of the algorithm (e.g. use eager evaluation in Haskell).
3. Add optimized implementations of k-NN which improve performance at no cost to accuracy.
4. Add implementations for other languages (with compilation and/or run instructions).

## TODO

* Write a blog post
* Do it in C
* Make the Go stuff a useable package
* Explore Accuracy vs Time tradeoffs of only considering some of the training set or some of the pixels when classifying a test case
* Make it easy to experiment with a matrix of different options
* Do it in Python with [scikit-learn](http://scipy-lectures.github.io/advanced/scikit-learn/)

