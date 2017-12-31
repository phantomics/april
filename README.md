<!-- TITLE/ -->

# Apex

<!-- /TITLE -->

Ken Iverson's masterpiece reflected in the medium of Lisp.

Apex compiles a subset of the APL programming language into Common Lisp. Leveraging Lisp's powerful macros and numerical processing faculties, it brings APL's expressive power to bear for Lisp developers. Replace hundreds of lines of number-crunching code with a single line of APL.

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

## Examples

Evaluating an APL expression is as simple as:

```
* (apex "1+2 3 4")

#(3 4 5)
```

The * indicates a REPL prompt. The following line contains the expression's output.

The macro (apex) will evaluate any APL string passed to it as the sole argument, returning the final result.

Setting environmental properties for the APL instance can be done like this:

```
* (apex (:env (:count-from 0)) "⍳9")

#(0 1 2 3 4 5 6 7 8)
```

Instead of an APL string, the first argument to (apex) may be a property list containing an :env variable corresponding to a property list specifying features of the APL environment. The APL expression is then passed in the second argument.

For example, you can use this configuration setting to determine whether the APL instance will start counting from 0 or 1.

```
* (apex (:env (:count-from 1)) "⍳9")

#(1 2 3 4 5 6 7 8 9)
```

More APL operations:

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

## What's Implemented

Actually, it's easier to list what's -not- implemented.

## What's Not Implemented But Will Be Soon

#### Operators:

```
⍣ Power
⍠ Variant
```

## What's Not Implemented And Won't Be

#### Functions:

```
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

See a pattern? The functions not planned for implentation are all those that manifest interactions between an APL instance and computer I/O devices. In Apex, it's intended for functionality like this to be accessed through the host Common Lisp instance.

## Also not implemented

System functions and variables within APL are not implemented, along with APL's control flow statements. This type of functionality is also readily accessible through standard Common Lisp.

## Thanks to:

Tamas K. Papp, creator of [array-operations](https://github.com/tpapp/array-operations), of which Apex makes heavy use.

Max Rottenkolber, creator of [MaxPC](https://github.com/eugeneia/maxpc), the heard of Apex's parsing engine.