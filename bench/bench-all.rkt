#lang racket/base

(require "benchmarks.rkt")

(displayln (bench-treiber-stack))