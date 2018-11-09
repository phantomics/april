<!-- TITLE/ -->

# April

<!-- /TITLE -->

Ken Iverson's masterpiece reflected in the medium of Lisp.

April compiles a subset of the APL programming language into Common Lisp. Leveraging Lisp's powerful macros and numerical processing faculties, it brings APL's expressive potential to bear for Lisp developers. Replace hundreds of lines of number-crunching code with a single line of APL.

## Why April?

APL veritably hums with algorithmic power. As a handful of characters run past the lexer, vast fields of data grow, morph and distil to reveal their secrets. However, APL has hitherto dwelt in an ivory tower, secluded inside monolithic runtime environments. If you have a store of data you'd like to use with APL, getting it there can be an ordeal. Like hauling tons of cargo on donkeys' backs through a narrow mountain pass, it's not fun, and the prospect of it has ended many discussions of APL before they could begin.

But no longer. Lisp is the great connector of the software world, digesting and transforming semantic patterns in much the same way that APL transforms numeric patterns. With APL inside of Lisp, databases, streams, binary files and other media are just a few lines of code away from processing with APL.

## Installation

April depends on Common Lisp, ASDF and Quicklisp. The only Common Lisp implementation tested so far has been Steel Bank Common Lisp (SBCL).

### Preparing Quicklisp

Enter your Quicklisp local-projects directory (usually ~/quicklisp/local-projects) and create a symbolic link to the directory where you cloned the April repository. For example, if you cloned the repo to ~/mystuff/april and your Quicklisp directory is ~/quicklisp/, enter:

```
cd ~/quicklisp/local-projects
ln -s ~/mystuff/april
```

### Installing April

To complete the installation, just start a Common Lisp REPL and enter:

```
(ql:quickload 'april)
```

And the system will be built and ready.

## APL Functions and Operators

The APL language uses single characters to represent its primitive functions and operators. Most of these symbols are not part of the standard ASCII character set but are unique to APL. To see a list of the glyphs that are supported by April, visit the link below.

#### [See the complete April APL lexicon here.](./lexicon.md)

Some APL functions and operators won't be added to April since they don't make sense for April's design as a compiler from APL to Lisp. Others may be added in the future. [See the list of features not implemented here.](#whats-not-implemented-and-may-be)

## Examples

Evaluating an APL expression is as simple as:

```
* (april "1+2 3 4")

#(3 4 5)
```

The * indicates a REPL prompt. The text two lines down is the expression's output.

The macro (april) will evaluate any APL string passed to it as the sole argument, returning the final result.

Setting state properties for the APL instance can be done like this:

```
* (april (set (:state :count-from 0)) "⍳9")

#(0 1 2 3 4 5 6 7 8)
```

Instead of an APL string, the first argument to (april) may be a list of specifications for the APL environment. The APL expression is then passed in the second argument.

For example, you can use this configuration setting to determine whether the APL instance will start counting from 0 or 1.

```
* (april (set (:state :count-from 1)) "⍳9")

#(1 2 3 4 5 6 7 8 9)

* (april (set (:state :count-from 0)) "⍳9")

#(0 1 2 3 4 5 6 7 8)
```

More APL expressions:

```
* (april "⍳12")

#(1 2 3 4 5 6 7 8 9 10 11 12)

* (april "3 4⍴⍳12")

#2A((1 2 3 4) (5 6 7 8) (9 10 11 12))

* (april "+/3 4⍴⍳12")

#(10 26 42)

* (april "+⌿3 4⍴⍳12")

#(15 18 21 24)

* (april "+/[1]3 4⍴⍳12")

#(15 18 21 24)

* (april "⌽3 4⍴⍳12")

#2A((4 3 2 1) (8 7 6 5) (12 11 10 9))

* (april "1⌽3 4⍴⍳12")

#2A((2 3 4 1) (6 7 8 5) (10 11 12 9))
```

## Parameter reference

When the (april) macro is called, you may pass it either a single text string:

```
* (april "1+1 2 3")
```

Or a parameter object followed by a text string:

```
* (april (set (:state :count-from 0)) "⍳9")
```

This section details the parameters you can pass to April.

### (test)

To run April's test suite, just enter:

```
* (april (test))
```

### (set)

(set) is the workhorse of April parameters, allowing you to configure your April instance in many ways. The most common sub-parameter passed via (set) is (:state). To wit:

```
* (april (set (:state :count-from 1
                      :in ((a 1) (b 2))
                      :out (a c)))
         "c←a+b×11")

1
23
```

### (:state) sub-parameters

Let's learn some more about what's going on in that code. The sub-parameters of (:state) are:

#### :count-from

Sets the index from which April counts. Almost always set to 0 or 1. The default value is 1.

#### :in

Passes variables into the April instance that may be used when evaluating the subsequent expressions. In the example above, the variables "a" and "b" are set in the code, with values 1 and 2 respectively. You can use :in to pass values from Lisp into the April instance.

Please note that April variables follow a stricter naming convention than Lisp variables. When naming variables, only alphanumeric characters, periods and dashes may be used. Punctuation marks like ?, > and ! must not be used as they have separate meanings in April.

These characters may be used in April variable names:
```
0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz.∆⍙
```

These variable names are ok:
```
a var my-var my-var.testing
```

These are not ok:
```
true! this->that pass/fail?
```

Note also that variables are converted from Lisp-style dash-separated format into camel case for use within April code. For example:

```
* (april (set (:state :in ((my-var 2)
                          (other-var 5))))
         "myVar×otherVar+5")

20
```

#### :out

Lists variables to be output when the code has finished evaluating. By default, the value of the last evaluated expression is passed back after an April evaluation is finished. For example:

```
* (april "1+2
          2+3
          3+4")

7
```

The last value calculated is displayed. The :out sub-parameter allows you to list a set of variables that whose values will be returned once evaluation is complete. For example:

```
* (april (set (:state :out (a b c)))
         "a←9+2
          b←5+3
          c←2×9")

11
8
18
```

#### :disclose-output

In APL, there's really no such thing as a value outside an array. Every piece of data used within an April instance is an array. When you enter something like 1+1, you're actually adding two arrays containing a single value, 1, and outputting another array containing the value 2. When April returns arrays like this, its default behavior is to disclose them like this:

```
* (april "1+1")

2
```

But if you set the :disclose-output option to nil, you can change this:
```
* (april (set (:state :disclose-output nil)) "1+1")

#(2)
```

With :disclose-output set to nil, unitary vectors will be passed directly back without having their values disclosed.

### (:space) sub-parameter

If you want to create a persistent workspace where the functions and variables you've created are stored and can be used in multiple calls to April, use the (:space) parameter. For example:

```
* (april (set (:space *space1*)) "a←5+2 ⋄ b←3×9")

27

* (april (set (:space *space1*)) "c←{⍵+2}")

#<FUNCTION ... >

* (april (set (:space *space1*)) "c a+b")

36
```

In the above example, a workspace called *space1* is created, two variables and a function are stored within it, and then the function is called on the sum of the variables. When you invoke the (:space) parameter followed by a symbol that is not defined, the symbol is set to point to a dynamic variable containing a hash table that stores the workspace data.

### (:state-persistent) sub-parameters

You can use the :state-persistent parameter to set state values within the workspace. It works like :state, but the difference is that when you change the state using :state-persistent, those changes will stay saved in the workspace until you reverse them, whereas the changes you make with :state are lost once the following code is done evaluating.

For example:

```
* (april (set (:state-persistent :count-from 0) (:space *space1*)) "⍳7")

#(0 1 2 3 4 5 6)

* (april (set (:space *space1*)) "⍳7")

#(0 1 2 3 4 5 6)

* (april (set (:space *space2*)) "⍳7")

#(1 2 3 4 5 6 7)
```

Did you notice that when switching to a different space, in this case *space2*, the customized values are lost? Custom state settings affect only the specific workspace where they are set.

You can use :state-persistent to set persistent input variables that will stay available for each piece of code you run in your April instance. If these input variables refer to external Lisp variables, changing the external variables will change the values available to April. Like this:

```
* (defvar *dynamic-var* 2)

*DYNAMIC-VAR*

* (april (set (:state-persistent :in ((dyn-var *dynamic-var*)))
              (:space *space1*))
        "dynVar⍟512")

9.0

* (setq *dynamic-var* 8)

8

* (april (set (:space *space1*)) "dynVar⍟512")

3.0
```

### (:compile-only) parameter

If you just want to compile the code you enter into April without running it, use this option. For example:

```
* (april (set (:compile-only)) "1+1 2 3")

(PROGN
 (DISCLOSE	
  (FUNCALL #'APPLY-SCALAR-DYADIC #<FUNCTION +> (VECTOR 1 2 3) (VECTOR 1))))
```

### (restore-defaults)

To restore all of April's state variables to the default values, enter:

```
* (april (restore-defaults))
```

All :in and :out values will be nullified, :count-from will return to its default setting, etc.

## What's Not Implemented and May Be

#### Operators:

```
@ At
⍤ Rank
⌸ Key
⌺ Stencil
```

#### Functions:

```
⍕ Format
⍕ Format by specification
⍕ Format by example
```

## What's Not Implemented And Won't Be

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

See a pattern? The functions not planned for implentation are all those that manifest low-level interactions between the APL instance and the underlying computer system. Common Lisp already has powerful tools for system interaction, so it's presumed that developers will do things like this outside of April.

## Also Not Implemented

System functions and variables within APL are not implemented, along with APL's control flow statements. This type of functionality is also readily accessible through standard Common Lisp.

## Tests

If you missed it earlier, you can run tests for the implemented APL functions and operators by entering:

```
(april (test))
```

## Thanks to:

Tamas K. Papp, creator of [array-operations](https://github.com/tpapp/array-operations), of which April makes heavy use.

Max Rottenkolber, creator of [MaxPC](https://github.com/eugeneia/maxpc), the heart of April's parsing engine.