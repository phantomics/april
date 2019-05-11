<!-- TITLE/ -->

# April

<!-- /TITLE -->

#### Array Programming Re-Imagined in Lisp

---

Ken Iverson's masterpiece reflected in the medium of Lisp.

April compiles a subset of the APL programming language into Common Lisp. Leveraging Lisp's powerful macros and numeric processing faculties, it brings APL's expressive potential to bear for Lisp developers. Replace hundreds of lines of number-crunching code with a single line of APL.

## Why April?

APL veritably hums with algorithmic power. As a handful of characters run past the lexer, vast fields of data grow, morph and distil to reveal their secrets. However, APL has hitherto dwelt in an ivory tower, secluded inside monolithic runtime environments. If you have a store of data you'd like to use with APL, getting it there can be an ordeal. Like hauling tons of cargo on donkeys' backs through a narrow mountain pass, it's not fun, and the prospect of it has ended many discussions of APL before they could begin.

But no longer. Lisp is the great connector of the software world, digesting and transforming semantic patterns in much the same way that APL transforms numeric patterns. With APL inside of Lisp, databases, streams, binary files and other media are just a few lines of code away from processing with APL.

## Automatic Installation

April is supplied by the Quicklisp library manager, so the easiest way to install April is through Quicklisp. To do so:

```lisp
(ql:quickload 'april)
```

## Manual Installation

If you'd like to install April manually from this repository, you can follow these instructions. April depends on Common Lisp, ASDF and Quicklisp. It has been tested with Steel Bank Common Lisp (SBCL), Clozure Common Lisp (CCL), Embeddable Common Lisp (ECL), Armed Bear Common Lisp (ABCL) and LispWorks.

**Note:** Some special configuration may be needed to use April with the LispWorks IDE due to the use of UTF-8 characters. [Click here for a guide to configuring LispWorks for compatibility with April.](./lispworks-howto.md) As of now, April can only be used with the LispWorks IDE, not the CLI environment, as the LispWorks CLI does not support UTF-8.

### Cloning the Repository

First, clone the repository to a location on your system. For this example, let's say you cloned it to the directory ~/mystuff/april.

### Preparing Quicklisp

Enter your Quicklisp local-projects directory (usually ~/quicklisp/local-projects) and create a symbolic link to the directory where you cloned the April repository. For example, if you cloned the repo to ~/mystuff/april and your Quicklisp directory is ~/quicklisp/, enter:

```
cd ~/quicklisp/local-projects
ln -s ~/mystuff/april
```

### Installing Dependencies

To complete the installation, just start a Common Lisp REPL and enter:

```lisp
(ql:quickload 'april)
```

This will download and install April's dependencies, and with that the package will be built and ready.

## APL Functions and Operators

The APL language uses single characters to represent its primitive functions and operators. Most of these symbols are not part of the standard ASCII character set but are unique to APL. To see a list of the glyphs that are supported by April, visit the link below.

#### [See the complete April APL lexicon here.](./lexicon.md)

Some APL functions and operators won't be added to April since they don't make sense for April's design as a compiler from APL to Lisp. Others may be added in the future. [See the list of features not implemented here.](#whats-not-planned-for-implementation)

## Getting to Know APL

A full guide to the APL language is far beyond the scope of this file, but here are links to some good sources.

[A high-level introduction to APL.](http://vector.org.uk/art10011550)

[This is a detailed language tutorial covering most of the functions and operators in April.](http://microapl.com/APL/tutorial_contents.html)

[The original paper by Ken Iverson, creator of APL, detailing the language's underlying philosophy.](http://www.eecg.toronto.edu/~jzhu/csc326/readings/iverson.pdf)

If you would like a quick tour of the language, April includes a function that will print demos of all the commands and many APL syntax features. To see the demos, enter:

```lisp
* (april (demo))
```

The * indicates a REPL prompt. Prepare for a long read. The demo content that gets printed will tell you the name(s) of the operations that correspond to each symbol and will hopefully give you some idea of what each one does.

### How to Enter APL Characters

In order to write APL programs you'll need a way to use the language's special character set.

[Click here for information on enabling APL input within Emacs.](#enabling-apl-input-in-emacs)


## Basic Evaluation: (april) and (april-p)

Evaluating an APL expression is as simple as:

```lisp
* (april-p "1+2 3 4")
3 4 5
#(3 4 5)
```

As above, the * indicates a REPL prompt and the text below is the expression's output.

The macro `(april-p)` will evaluate any APL string passed to it as the sole argument, returning the final result. Using `(april-p)` will also produce a printout of the output in APL's traditional array printing style, which appears before the actual output value. You can see above how the `3 4 5` is printed out before the value `#(3 4 5)`. APL-style printed arrays are easier to read than Lisp's style of printing arrays; APL can use a simpler style to express its output because it doesn't have as many different data types and structures as Lisp.

If you don't need to see the printout, you can use the plain `(april)` macro. Like this:

```lisp
* (april "1+2 3 4")
#(3 4 5)
```

You should use `(april)` if you're using April to do calculations inside of a larger program and don't need the printout. Otherwise, especially if you're working with large data sets, the system may consume significant resources printing out the results of calculations.

Also note that if the output of an April expression is a single number, `(april-p)` will not print it since the Lisp representation of the number will look the same or very similar. For example:

```lisp
* (april-p "1+2")
3
```

Since the result of 1+2 is 3, a single number, no value printout is provided.

Setting state properties for the APL instance can be done like this:

```lisp
* (april-p (with (:state :count-from 0)) "⍳9")
0 1 2 3 4 5 6 7 8
#(0 1 2 3 4 5 6 7 8)
```

Instead of an APL string, the first argument to `(april)` or `(april-p)` may be a list of parameters for the APL environment. The APL expression is then passed in the second argument.

For example, you can use the `:count-from` parameter to determine whether the APL instance will start counting from 0 or 1. We'll get into more detail on how these parameters work later.

```lisp
* (april-p (with (:state :count-from 1)) "⍳9")
1 2 3 4 5 6 7 8 9
#(1 2 3 4 5 6 7 8 9)

* (april-p (with (:state :count-from 0)) "⍳9")
0 1 2 3 4 5 6 7 8
#(0 1 2 3 4 5 6 7 8)
```

More APL expressions:

```lisp
* (april-p "⍳12")
1 2 3 4 5 6 7 8 9 10 11 12
#(1 2 3 4 5 6 7 8 9 10 11 12)

* (april-p "3 4⍴⍳12")
1  2  3  4
5  6  7  8
9 10 11 12
#2A((1 2 3 4) (5 6 7 8) (9 10 11 12))

* (april-p "+/3 4⍴⍳12")
10 26 42
#(10 26 42)

* (april-p "+⌿3 4⍴⍳12")
15 18 21 24
#(15 18 21 24)

* (april-p "+/[1]3 4⍴⍳12")
15 18 21 24
#(15 18 21 24)

* (april-p "⌽3 4⍴⍳12")
 4  3  2 1
 8  7  6 5
12 11 10 9
#2A((4 3 2 1) (8 7 6 5) (12 11 10 9))

* (april-p "1⌽3 4⍴⍳12")
 2  3  4 1
 6  7  8 5
10 11 12 9
#2A((2 3 4 1) (6 7 8 5) (10 11 12 9))
```

## Parameter reference

When `(april)` or `(april-p)` is called, you may pass it either a single text string:

```lisp
* (april-p "1+1 2 3")
```

Or a parameter object followed by a text string:

```lisp
* (april-p (with (:state :count-from 0)) "⍳9")
```

This section details the parameters you can pass to April.

### (test)

To run April's test suite, just enter:

```lisp
* (april (test))
```

### (demo)

As mentioned before, you can see demos of April's functions with:

```lisp
* (april (demo))
```

### (with)

`(with)` is the workhorse of April parameters, allowing you to configure your April instance in many ways. The most common sub-parameter passed via `(with)` is `(:state)`. To wit:

```lisp
* (april (with (:state :count-from 1
                       :in ((a 1) (b 2))
                       :out (a c)))
         "c←a+b×11")
1
23
```

### (:state) sub-parameters

Let's learn some more about what's going on in that code. The sub-parameters of `(:state)` are:

#### :count-from

Sets the index from which April counts. Almost always set to 0 or 1. The default value is 1.

#### :in

Passes variables into the April instance that may be used when evaluating the subsequent expressions. In the example above, the variables `a` and `b` are set in the code, with values 1 and 2 respectively. You can use `:in` to pass values from Lisp into the April instance.

```lisp
* (april-p (with (:state :in ((a 5) (b 10))))
           "1+2+a×b")
53
```

Please note that April variables follow a stricter naming convention than Lisp variables. When naming the input variables, only alphanumeric characters, underscores and dashes may be used. In keeping with APL tradition, the delta/triangle characters ∆ and ⍙ can be used in variable names as well. Punctuation marks like ?, >, . and ! may not be used as they have separate meanings in April.

These characters are allowed in variable names within April:
```
0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_∆⍙
```

These variable names are ok for use with the `:in` parameter:
```
a var my_var another-var
```

These are not ok:
```
true! this->that pass/fail? var.name
```

If you use dashes in the names of Lisp variables you pass into April, note that inside April they will be converted to camel case. For example:

```lisp
* (april-p (with (:state :in ((my-var 2)
                              (other-var 5))))
           "myVar×otherVar+5")
20
```

The dash character `-` is used to denote the subtraction function inside April, so you may not use dashes in variable names within the language.

One more caveat: it's best to avoid using input variable names with a dash before a number or other non-letter symbol. The dash will be removed and the character following it will cannot be capitalized so information will have been lost. For example:

```
my-var-2 → myVar2
my-var-∆ → myVar∆
```

#### :out

Lists variables to be output when the code has finished evaluating. By default, the value of the last evaluated expression is passed back after an April evaluation is finished. For example:

```lisp
* (april-p "1+2
            2+3
            3+4")
7
```

The last value calculated is displayed. The `:out` sub-parameter allows you to list a set of variables that whose values will be returned once evaluation is complete. For example:

```lisp
* (april-p (with (:state :out (a b c)))
           "a←9+2
            b←5+3
            c←2×9")
11
8
18
```

#### :index-origin

This is another, more technical name for the `:count-from` sub-parameter. You can use it instead of `:count-from`:

```lisp
* (april-p (with (:state :index-origin 0)) "⍳9")
0 1 2 3 4 5 6 7 8
#(0 1 2 3 4 5 6 7 8)
```

#### :disclose-output

In APL, there's really no such thing as a value outside an array. Every piece of data used within an April instance is an array. When you enter something like 1+1, you're actually adding two arrays containing a single value, 1, and outputting another array containing the value 2. When April returns arrays like this, its default behavior is to disclose them like this:

```lisp
* (april-p "1+1")
2
```

But if you set the `:disclose-output` option to nil, you can change this:

```lisp
* (april-p (with (:state :disclose-output nil)) "1+1")
#(2)
```

With `:disclose-output` set to nil, unitary vectors will be passed directly back without having their values disclosed.

#### :print-precision

This controls the maximal precision at which April prints floating point numbers. Its default value is 10. For example:

```lisp
* (april-p "○1 2 3")
3.141592654 6.283185307 9.424777961	
#(3.141592653589793d0 6.283185307179586d0 9.42477796076938d0)

* (april-p (with (:state :print-precision 6)) "○1 2 3")
3.14159 6.28319 9.42478
#(3.141592653589793d0 6.283185307179586d0 9.42477796076938d0)

* (april-p (with (:state :print-precision 3)) "○1 2 3")
3.14 6.28 9.42
#(3.141592653589793d0 6.283185307179586d0 9.42477796076938d0)
```

Note that `:print-precision` doesn't affect the Lisp values output by April, only the printed output.

#### :print-to

When using `(april-p)`, the formatted array content is output to the `*standard-output*` stream. When using `(april)`, no formatted output is printed. You can change this using the `:print-to` sub-parameter. For example:

```lisp
* (april (with (:state :print-to *standard-output*)) "2 3⍴⍳9")
1 2 3
4 5 6
#2A((1 2 3) (4 5 6))

* (april-p (with (:state :print-to nil)) "2 3⍴⍳9")
#2A((1 2 3) (4 5 6))
```

Using the `:print-to` parameter effectively erases the distinction between `(april)` and `(april-p)`. The two different macros are provided as a courtesy so you don't need to pass a `:print-to` parameter to get printed output. You can also pass a different stream than `*standard-output*` to `:print-to` to have the printed output directed there.

#### :output-printed

When the `:output-printed` sub-parameter is passed, the string of APL-formatted data that gets printed will also be returned as the last output value by the April invocation. For example:

```lisp
* (april (with (:state :output-printed t)) "2 3⍴⍳9")
#2A((1 2 3) (4 5 6))
"1 2 3
4 5 6
"
```

If you don't want to receive the Lisp value output by April and only want the formatted string as output, you can pass the `:only` option to `:output-printed`, like this:

```lisp
* (april (with (:state :output-printed :only)) "2 3⍴⍳9")
"1 2 3
4 5 6
"
```

This way, the formatted string will be the only returned value.

### (:space) sub-parameter

If you want to create a persistent workspace where the functions and variables you've created are stored and can be used in multiple calls to April, use the `(:space)` parameter. For example:

```lisp
* (april-p (with (:space *space1*)) "a←5+2 ⋄ b←3×9")
27

* (april-p (with (:space *space1*)) "c←{⍵+2}")
#<FUNCTION ... >

* (april-p (with (:space *space1*)) "c a+b")
36
```

In the above example, a workspace called `*space1*` is created, two variables and a function are stored within it, and then the function is called on the sum of the variables. When you invoke the `(:space)` parameter followed by a symbol that is not defined, the symbol is set to point to a dynamic variable containing a hash table that stores the workspace data.

### (:state-persistent) sub-parameters

You can use the `(:state-persistent)` parameter to set state values within the workspace. It works like `(:state)`, but the difference is that when you change the state using `(:state-persistent)`, those changes will stay saved in the workspace until you reverse them, whereas the changes you make with `:state` are lost once the following code is done evaluating.

For example:

```lisp
* (april-p (with (:state-persistent :count-from 0) (:space *space1*)) "⍳7")
0 1 2 3 4 5 6
#(0 1 2 3 4 5 6)

* (april-p (with (:space *space1*)) "⍳7")
0 1 2 3 4 5 6
#(0 1 2 3 4 5 6)

* (april-p (with (:space *space2*)) "⍳7")
1 2 3 4 5 6 7
#(1 2 3 4 5 6 7)
```

Did you notice that when switching to a different space, in this case `*space2*`, the customized values are lost? Custom state settings affect only the specific workspace where they are set.

You can use `(:state-persistent)` to set persistent input variables that will stay available for each piece of code you run in your April instance. If these input variables refer to external Lisp variables, changing the external variables will change the values available to April. For example:

```lisp
* (defvar *dynamic-var* 2)
*DYNAMIC-VAR*

* (april-p (with (:state-persistent :in ((dyn-var *dynamic-var*)))
                 (:space *space1*))
           "dynVar⍟512")
9.0

* (setq *dynamic-var* 8)
8

* (april-p (with (:space *space1*)) "dynVar⍟512")
3.0
```

### (:compile-only) parameter

If you just want to compile the code you enter into April without running it, use this option. For example:

```lisp
* (april (with (:compile-only)) "1+1 2 3")
(LET* ((INDEX-ORIGIN 1) (PRINT-PRECISION 10))
  (DECLARE (IGNORABLE INDEX-ORIGIN PRINT-PRECISION))
  (APL-OUTPUT
   (DISCLOSE-ATOM (APL-CALL + (SCALAR-FUNCTION +) (AVECTOR 1 2 3) (AVECTOR 1)))
   :PRINT-PRECISION PRINT-PRECISION))
```

### (:restore-defaults) parameter

You can use this parameter to clear a workspace and return it to its default state. For example, to clear a workspace called `*space1*` enter:

```lisp
* (april (with (:restore-defaults) (:space *space1*)))
```

All `:in` and `:out` values will be nullified, `:count-from` will return to its default setting, etc.

## Sharing Scope: The (with-april-context) Macro

Perhaps you'd like to make multiple calls to April using the same workspace and other parameters and you don't want to have to enter the same parameters over and over again. The `(with-april-context)` macro can help. For example:

```lisp
* (with-april-context ((:space *space1*) (:state :index-origin 0))
    (april "g←5")
    (april "g×3+⍳9"))
#(15 20 25 30 35 40 45 50 55)
```

Inside the body of the `(with-april-context)` macro, each of the `(april)` invocations act as if they were passed the options `(with (:space *space1*) (:state :index-origin 0))`. 

```lisp
* (with-april-context ((:space *space1*) (:state :index-origin 0))
    (april "x←⍳3")
    (april (with (:state :index-origin 1)) "x,⍳5"))
#(0 1 2 1 2 3 4 5)
```

Options passed for one of the `(april)` invocations inside the context will override the options for the context. Here, the second `(april)` invocation has its index origin set to 1 which overrides the context's 0 value.

## Console Output Using the Quad Character

The `(april-p)` macro is one way to view the printed output of APL expressions. What if you want to see the result of an evaluation that occurs in the middle of your code instead of the end, or if you want to print the contents of multiple arrays within a single expression? At times like these, you can use the `⎕` character, also called "quad." In APL, console output can be produced by "assigning" values to `⎕` like this:

```lisp
* (april "a←1 2 3 ⋄ b←3+⎕←2+a ⋄ ⎕←c←4+b ⋄ c+5")
3 4 5
10 11 12
#(15 16 17)
```

Both of the values assigned to `⎕` are printed in order before the expression's final result is output. Because `(april)` is used instead of `(april-p)`, no formatted values are printed by default; only the values assigned to `⎕` are printed. Using `⎕`, it's easy to debug complex functions.

## APL System Variables and Functions in April

April makes available the following APL system variables and functions:

```
⎕IO ⎕TS ⎕PP ⎕AV ⎕A ⎕D
```

Additionally, April exposes this special system variable not found in other APL implementations:

```
⎕OST
```

[Click here to read the names and descriptions of these symbols.](./environmental-symbols.md)

## Setting a Custom Output Stream

April has a special system variable called `⎕ost` that you can use to set a custom destination for printed output. Normally, data output using `(april-p)` or values assigned to the quad character like `⎕←1 2 3` are sent to the `*standard-output*` stream. You can change this as follows:

```lisp
* (let* ((out-str (make-string-output-stream))
	 (vector (april-p "a←1 2 3 ⋄ ⎕ost←'OUT-STR' ⋄ ⎕←a+5 ⋄ ⎕←3 4 5 ⋄ ⎕ost←'*STANDARD-OUTPUT*' ⋄ 2+a")))
    (print (get-output-stream-string out-str))
    vector)
3 4 5

"6 7 8
3 4 5
" 
#(3 4 5)
```

Within the APL expression, the output stream is set to `OUT-STR`, two vectors are output to that stream, and then the stream is reset to `*STANDARD-OUTPUT*` before the expression ends and prints its final output. When the code runs, first the APL-formatted output from the `(april-p)` expression is displayed. Then, the two APL-formatted strings output to the `out-str` stream are printed. Finally, the Lisp vector that resulted from the `(april-p)` expression is printed.

Remember to use all caps when setting the `⎕ost` variable, unless your desired output stream is referenced by a literal lowercase symbol like `|output-stream|`.

## What's Not Planned for Implementation

#### Functions:

```
→ Branch
⍇ File read
⍈ File write
⍐ File hold
⍗ File drop
⎕ Evaluated input
⎕ Output with newline
⍞ Character input
⍞ Bare output
```

#### Operators:

```
& Spawn
⌶ I-Beam
```

[(Click here to see the functions and operators that have been implemented.)](./lexicon.md)

See a pattern? The functions not planned for implentation are all those that manifest low-level interactions between the APL instance and the underlying computer system. Common Lisp already has powerful tools for system interaction, so it's presumed that developers will do things like this outside of April.

## Also Not Implemented

APL's function editor system and control flow statements are not implemented; this type of functionality is also readily accessible through standard Common Lisp.

## Tests and Demo

If you missed it earlier, you can run tests for the implemented APL functions and operators by entering:

```lisp
* (april (test))
```

And you can see a demonstration of April language features by entering:

```lisp
* (april (demo))
```

## Enabling APL Input in Emacs

Most Lisp developers interact with the language through Emacs, so Emacs is also the most convenient tool to write April programs. The best way to input APL characters in Emacs is using the gnu-apl-mode Emacs plugin. You can get it [from the repository here](https://github.com/lokedhs/gnu-apl-mode) or install it directly via the MELPA Emacs package repository.

[Click here for information on using MELPA.](http://ergoemacs.org/emacs/emacs_package_system.html)

Once gnu-apl-mode is installed, you can switch to the APL input mode by typing `M-x toggle-input-method` or `C-\`. You will be prompted to enter the input mode to use, so enter `APL-Z` and then you'll be able to toggle APL input on and off by typing `C-\`. While in APL-Z input mode, you can enter APL characters by prefixing the key with a `.` period character.

## Thanks to:

Tamas K. Papp, creator of [array-operations](https://github.com/tpapp/array-operations), of which April makes heavy use.

Max Rottenkolber, creator of [MaxPC](https://github.com/eugeneia/maxpc), the heart of April's parsing engine.