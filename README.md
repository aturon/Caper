Caper: concurrent and parallel extensions to Racket

Directory structure

- `core`: an implementation of [reagents](http://www.ccs.neu.edu/home/turon/reagents.pdf) both as combinators and an embedded DSL

- `data`: concurrent data structures built from reagents

- `sync`: synchronization primitives built from reagents

- `tests`: all testing code

- `bench`: benchmarking infrastructure and specific benchmarks

- `by-hand`: hand-coded versions of data structures and synchronizers, for comparison purposes