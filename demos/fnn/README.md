<!-- TITLE/ -->

## Forward-Feeding Neural Network

<!-- /TITLE -->

This is a forward-feeding neural network algorithm implemented in APL. The code here is adapted from [a series of tutorial videos](https://www.youtube.com/playlist?list=PLgTqamKi1MS3p-O0QAgjv5vt4NY5OgpiM) created by Rodrigo Girão Serrão. The network implemented in this demo is one of the simplest and earliest types of neural net but it can still do useful work.

### Background

This demo features a use case where the network is trained to recognize handwritten digits. The images of these digits come from a database compiled by the National Institute of Standards and Technology under the United States Department of Commerce. This database, called the MNIST database, contains thousands of images of handwritten digits divided into a training set of 60,000 images and a test set of 10,000 images. The code in this package builds a neural network network, teaches it to recognize the digits by training it on the training set, and then feeds the images from the test set to the network to find out how well it has been trained to recognize the digits. This digit recognition use case is considered a classic test for machine learning algorithms.

### Installation

To install the demo, evaluate `(ql:quickload 'april-demo.fnn)`. If this doesn't work, make sure that the top-level `/april` directory is present or linked within your `~/quicklisp/local-projects` directory.

### Getting started

First, download the four files `train-images.idx3-ubyte`, `train-labels.idx1-ubyte`, `t10k-images.idx3-ubyte` and `t10k-labels.idx1-ubyte` into the `input` directory and extract them. The files can be found at [the MNIST database website](http://yann.lecun.com/exdb/mnist/).

The file `demo.lisp` contains implementations of a series of functions that implement the neural network along with test functions you can use to verify that each one is working properly. The comments contain function invocations that you can evaluate if you're using an interactive Lisp system like Slime or Sly. If you'd like to understand the details of how this neural net implementation works, it's best to go through `demo.lisp` and read each section, and you can also watch Serrão's video tutorial series linked above for further details.

If you'd like to cut right to the chase and train and test the network, you can do so as follows:

```lisp
* (load-digit-data)
Loading MNIST training and test images...
"Loaded 60000 MNIST training images and 10000 test-images with labels."

* (build-digit-network)
"Built MNIST digit recognition network with shape 784 16 16 10."

* (train-digit-network)
...

* (test-digit-network)
...

* (analyze-network-output)
...
```

Note that the code above will use the entire set of 60,000 training dights and 10,000 testing digits. This may take quite some time to run, so you can limit the functions by passing a number of digits to use. For example, evaluate `(train-digit-network 10000)` to train with just 10,000 digits and `(test-digit-network 1000)` to test with just 10,000 digits. The more digits you train with, the more accurate the network is likely to be. Evaluating `(analyze-network-output)` will print a summary of information on the network's performance in the last test, including the number of successful and failed attempts (referred to as hits and misses) to guess each number image tested against the network.