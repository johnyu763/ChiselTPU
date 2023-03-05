# ChiselTPU

This is a module for [Chisel](chisel-lang.org/) that mimics a TPU that allows the ability to load in an activation and systolic matrix and outputs the matrix multiplication result staggered over time.

> A TPU is a Tensor Processing Unit, meaning it can act as a matrix multiplication unit, which is useful for neural networks where the output from the last layer acts as the input for the next layer. https://en.wikipedia.org/wiki/Tensor_Processing_Unit

## Authors

* Alex Lee <alee156@ucsc.edu>
* John Yu <jyu229@ucsc.edu>

## What is implemented?
* Scala model for activation matrix
* Scala model for TPU which computes systolic matrix
* Chisel model for systolic matrix
* Chisel model for activation matrix
* Basic TPU state machine for taking in input and multiplying

## What remains?
* Use output from last matrix multiplication as activation matrix for next multiplication

## How to?

### Test the code
```console
sbt test
```