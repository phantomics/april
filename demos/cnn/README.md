<!-- TITLE/ -->

## Convolutional Neural Network

<!-- /TITLE -->

---

This is a convolutional neural network algorithm implemented in APL. The code here is adapted from the original CNN in APL program written by Artem Shinkarov.

### Getting started

Download the four files `train-images.idx3-ubyte`, `train-labels.idx1-ubyte`, `t10k-images.idx3-ubyte` and `t10k-labels.idx1-ubyte` into the `input` directory.

Evaluate:

```lisp
* (asdf:load-system 'april-demo.cnn)
...

* (in-package "APRIL-DEMO.CNN")
#<PACKAGE "APRIL-DEMO-CNN">

* (load-idx-files)
"Data loaded."

* (train)
Running Zhang with 10 epochs, batchSize 1,
5 training images, 10000 tests and a rate of 0.05
  
--

...

```

This program is set to run 1000 trainings in 10 epochs. This will take some time to run and give the CPU a workout. If you'd like to try it with a shorter runtime, find the line in `demo.lisp` where the number of trainings is set: `trainings ← 1000` and change it to a lower number like `trainings ← 5`.