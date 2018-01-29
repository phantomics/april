<!-- TITLE/ -->

# Apex

<!-- /TITLE -->

Ken Iverson's masterpiece reflected in the medium of Lisp.

Apex compiles a subset of the APL programming language into Common Lisp. Leveraging Lisp's powerful macros and numerical processing faculties, it brings APL's expressive potential to bear for Lisp developers. Replace hundreds of lines of number-crunching code with a single line of APL.

## Why Apex?

APL veritably hums with semantic power. As a handful of characters run through the lexer, vast fields of data grow, morph and distil to reveal their secrets. However, APL has hitherto dwelt in an ivory tower, secluded inside monolithic (and often costly) runtime environments. If you have a store of data you'd like to use with APL, getting it there can be an ordeal. Like hauling tons of cargo on donkeys' backs through a narrow mountain pass, it's not fun, and the prospect of it has ended many conversations about APL before they could begin.

But no longer. Lisp is the great connector of the software world, digesting and transforming semantic patterns in much the same way that APL transforms numeric patterns. With APL inside of Lisp, databases, streams, binary files and other expressive media are just a few lines of code away from processing with APL.

## Installation

Apex depends on Common Lisp, ASDF and Quicklisp. The only Common Lisp implementation tested so far has been Steel Bank Common Lisp (SBCL).

### Preparing Quicklisp

Enter your Quicklisp local-projects directory (usually ~/quicklisp/local-projects) and create a symbolic link to the directory where you cloned the Apex repository. For example, if you cloned the repo to ~/mystuff/apex and your Quicklisp directory is ~/quicklisp/, enter:

```
cd ~/quicklisp/local-projects
ln -s ~/mystuff/apex
```

### Installing Apex

To complete the installation, just start a Common Lisp REPL and enter:

```
(ql:quickload 'apex)
```

And the system will be built and ready.

## APL Functions and Operators

The APL language uses single characters to represent its primitive functions and operators. Most of these symbols are not part of the standard ASCII character set but are unique to APL. To see a list of the glyphs that are supported by Apex, visit the link below.

#### [The complete Apex lexicon](./apex/blob/master/lexicon.md)

Some APL functions and operators won't be added to Apex since they don't make sense for Apex's design as a compiler from APL to Lisp. [See the list of features not planned for implementation here.](#whats-not-implemented-and-wont-be)

## Examples

Evaluating an APL expression is as simple as:

```
* (apex "1+2 3 4")

#(3 4 5)
```

The * indicates a REPL prompt. The following line contains the expression's output.

The macro (apex) will evaluate any APL string passed to it as the sole argument, returning the final result.

Setting state properties for the APL instance can be done like this:

```
* (apex (set (:state :count-from 0)) "⍳9")

#(0 1 2 3 4 5 6 7 8)
```

Instead of an APL string, the first argument to (apex) may be a property list containing an :env variable corresponding to a property list specifying features of the APL environment. The APL expression is then passed in the second argument.

For example, you can use this configuration setting to determine whether the APL instance will start counting from 0 or 1.

```
* (apex (set (:state :count-from 1)) "⍳9")

#(1 2 3 4 5 6 7 8 9)
```

More APL expressions:

```
* (apex "⍳12")

#(1 2 3 4 5 6 7 8 9 10 11 12)

* (apex "3 4⍴⍳12")

#2A((1 2 3 4) (5 6 7 8) (9 10 11 12))

* (apex "+/3 4⍴⍳12")

#(10 26 42)

* (apex "+⌿3 4⍴⍳12")

#(15 18 21 24)

* (apex "+/[1]3 4⍴⍳12")

#(15 18 21 24)

* (apex "⌽3 4⍴⍳12")

#2A((4 3 2 1) (8 7 6 5) (12 11 10 9))

* (apex "1⌽3 4⍴⍳12")

#2A((2 3 4 1) (6 7 8 5) (10 11 12 9))
```

## Parameter reference

When the (apex) macro is called, you may pass it either a single text string:

```
* (apex "1+1 2 3")
```

Or a parameter object followed by a text string:

```
* (apex (set (:state :count-from 0)) "⍳9")
```

This section details the parameters you can pass to Apex.

### (test)

To run Apex's test suite, just enter:

```
* (apex (test))
```

### (set)

(set) is the workhorse of Apex parameters, allowing you to configure your Apex instance in many ways. The most common sub-parameter passed via (set) is (:state). To wit:

```
* (apex (set (:state :count-from 1
                     :in ((a 1) (b 2))
                     :out (a c)))
        "c←a+b×11")

1
23
```

### (:state) parameters

Let's learn some more about what's going on in that code. The sub-parameters of (:state) are:

#### :count-from

Sets the index from which Apex counts. Almost always set to 0 or 1. The default value is 1.

#### :in

Passes variables into the Apex instance that may be used when evaluating the subsequent expressions. In the example above, the variables "a" and "b" are set in the code, with values 1 and 2 respectively. You can use :in to pass values from Lisp into the Apex instance.

Please note that Apex variables follow a stricter naming convention than Lisp variables. When naming variables, only alphanumeric characters, periods and dashes may be used. Punctuation marks like ?, > and ! must not be used as they have separate meanings in Apex.

Ok:
```
a var my-var my-var.testing
```

Not ok:
```
true! this->that pass/fail?
```

Note also that variables are converted from Lisp-style dash-separated format into camel case for use within Apex code. For example:

```
* (apex (set (:state :in ((my-var 2)
                          (other-var 5))))
        "myVar×otherVar+5")

20
```

#### :out

Lists variables to be output when the code has finished evaluating. By default, the value of the last evaluated expression is passed back after an Apex evaluation is finished. For example:

```
* (apex "1+2
         2+3
         3+4")

7
```

The last value calculated is displayed. The :out sub-parameter allows you to list a set of variables that whose values will be returned once evaluation is complete. For example:

```
* (apex (set (:state :out (a b c)))
        "a←9+2
         b←5+3
         c←2×9")

11
8
18
```

#### :disclose-output

In APL, there's really no such thing as a value outside an array. Every piece of data used within an Apex instance is an array. When you enter something like 1+1, you're actually adding two arrays containing a single value, 1, and outputting another array containing the value 2. When Apex returns arrays like this, its default behavior is to disclose them like this:

```
* (apex "1+1")

2
```
But if you set the :disclose-output option to nil, you can change this:
```
* (apex (set (:state :disclose-output nil)) "1+1")

#(2)
```

With :disclose-output set to nil, unitary vectors will be passed directly back without having their values disclosed.

### (:state-persistent) parameters

You can use the :state-persistent parameter to set state values within the Apex instance, just like :state. The difference is that when you change the state using :state-persistent, those changes will stay until you reverse them, whereas the changes you make with :state are reverted once the following code is done evaluating.

For example:

```
* (apex (set (:state-persistent :count-from 0)) "⍳7")

#(0 1 2 3 4 5 6)

* (apex "⍳7")

#(0 1 2 3 4 5 6)
```

You can use :state-persistent to set persistent input variables that will stay avaialable for each piece of code you run in your Apex instance. If these input variables refer to external Lisp variables, changing the external variables will change the values available to Apex. Like this:

```
* (defvar *dynamic-var* 2)

*DYNAMIC-VAR*

* (apex (set (:state-persistent :in ((dyn-var *dynamic-var*))))
        "dynVar⍟512")

#(9.0)

* (setq *dynamic-var* 8)

8

* (apex "dynVar⍟512")

#(3.0)
```

### (:space) parameter

If you want to create a persistent workspace where the functions and variables you've created are stored and can be used in multiple calls to Apex, use the (:space) parameter. For example:

```
* (apex (set (:space *space1*)) "a←5+2 ◊ b←3×9")

27

* (apex (set (:space *space1*)) "c←{⍵+2}")

#<FUNCTION ... >

* (apex (set (:space *space1*)) "c a+b")

36
```

In the above example, a workspace called *space1* is created, two variables and a function are stored within it, and then the function is called on the sum of the variables. When you invoke the (:space) parameter followed by a symbol that is not defined, the symbol is set to point to a dynamic variable containing a hash table that stores the workspace data.

### (:compile-only) parameter

If you just want to compile the code you enter into Apex without running it, use this option. For example:

```
* (apex (set (:compile-only)) "1+1 2 3")

(PROGN
 (FUNCALL #'APPLY-SCALAR-DYADIC #<FUNCTION +>
          (MAKE-ARRAY (LIST 1) :INITIAL-CONTENTS (LIST 1))
          (MAKE-ARRAY (LIST 3) :INITIAL-CONTENTS (LIST 1 2 3))))
```

### (restore-defaults)

To restore all of Apex's state variables to the default values, enter:

```
* (apex (restore-defaults))
```

All :in and :out values will be nullified, :count-from will return to its default setting, etc.

## What's Not Implemented And Won't Be

#### Functions:

```
→ Branch
⍺ Picture format
⍕ Format
⍕ Format by specification
⍕ Format by example
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

See a pattern? The functions not planned for implentation are all those that manifest low-level interactions between the APL instance and the underlying computer system. Common Lisp already has powerful tools for system interaction, so it's presumed that developers will do things like this outside of Apex.

## Also Not Implemented

System functions and variables within APL are not implemented, along with APL's control flow statements. This type of functionality is also readily accessible through standard Common Lisp.

## Tests

If you missed it earlier, you can run tests for the implemented APL functions and operators by entering:

```
(apex (test))
```

## Thanks to:

Tamas K. Papp, creator of [array-operations](https://github.com/tpapp/array-operations), of which Apex makes heavy use.

Max Rottenkolber, creator of [MaxPC](https://github.com/eugeneia/maxpc), the heart of Apex's parsing engine.