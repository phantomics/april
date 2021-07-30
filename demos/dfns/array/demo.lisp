;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:AprilDemo.Dfns.Array -*-
;;;; demo.lisp

(in-package :april-demo.dfns.array)

(april-load (with (:space array-demo-space))
            (asdf:system-relative-pathname (intern (package-name *package*) "KEYWORD") "array.apl"))

(specify-demo
 "April array demo"
 (with :space array-demo-space
       :description "Implements array processing functions from Dyalog's dfns.")
 (:tests (provision "found ← ('milly' 'molly' 'may') ('star' 'thing' 'stone')")
         (is "found alget 'may'" "stone")
         (is "found alpop 'molly'" #("thing" #(#("milly" "may") #("star" "stone"))))
         (is "found alset 'may' 'pebble'" #(#("milly" "molly" "may") #("star" "thing" "pebble")))
         (is "found alpush 'may' 'rock'" #(#("may" "milly" "molly" "may") #("rock" "star" "thing" "stone")))
         (is ",acc ⍳4" #(#(1 2 3 4) #(2 3 4) #(3 4) 4))
         (is "+acc ⍳4" #(10 9 7 4))
         (is ",acc 2/¨⍳4" #(#(1 1 2 2 3 3 4 4) #(2 2 3 3 4 4) #(3 3 4 4) #(4 4)))
         (is "{⍺,'f',⍵}acc'abcd'" #("afbfcfd" "bfcfd" "cfd" #\d))
         (is "⍲acc 1 0 0" #*010)
         (provision "vecs←(('hello' 'world')('bonjour' 'monde'))(('good' 'night')('bon' 'soir'))")
         (is "↓disp ⍳2 2" #("┌───┬───┐"
                            "│1 1│1 2│"
                            "├───┼───┤"
                            "│2 1│2 2│"
                            "└───┴───┘"))
         (is "↓1 disp 8 (,8) 88 (,88)" #("┌→┬─┬──┬──┐"
                                         "│8│8│88│88│"
                                         "└─┴→┴~─┴~→┘"))
         (provision "ditty←2 2⍴'Tea'(2 1⍴4 2)'&'(2 40)")
         (is "↓disp ditty" #("┌───┬────┐"
                             "│Tea│4   │"
                             "│   │2   │"
                             "├───┼────┤"
                             "│&  │2 40│"
                             "└───┴────┘"))
         (is "↓1 disp ditty" #("┌→──┬────┐"
                               "↓Tea│4   │"
                               "│   │2   ↓"
                               "├#─→┼~──→┤"
                               "│&  │2 40│"
                               "└#──┴~──→┘"))
         (is "+/∘∊¨∊∘'→↓~'¨1∘disp {(⍺⍺ ⍵)(⍵⍵ ⍵)} display ditty" #(8 8))
         (is "↓disp'%' 'Eye Poke' 'Kumquat'⍪↑('Guys' 60 40)('Gals' 20 80)" #("┌────┬────────┬───────┐"
                                                                             "│%   │Eye Poke│Kumquat│"
                                                                             "├────┼────────┼───────┤"
                                                                             "│Guys│60      │40     │"
                                                                             "├────┼────────┼───────┤"
                                                                             "│Gals│20      │80     │"
                                                                             "└────┴────────┴───────┘"))
         (is "↓display 1 'a' 'abc' (2 3⍴⍳6)" #("┌→──────────────────┐"
                                               "│     ┌→──┐ ┌→────┐ │"
                                               "│ 1 a │abc│ ↓1 2 3│ │"
                                               "│   - └#──┘ │4 5 6│ │"
                                               "│           └~────┘ │"
                                               "└∊──────────────────┘"))
         (is "↓display 2 2⍴'Tea'(2 1⍴4 2)'&'(2 40)" #("┌→─────────────┐"
                                                      "↓ ┌→──┐ ┌→┐    │"
                                                      "│ │Tea│ ↓4│    │"
                                                      "│ └#──┘ │2│    │"
                                                      "│       └~┘    │"
                                                      "│       ┌→───┐ │"
                                                      "│ &     │2 40│ │"
                                                      "│ -     └~───┘ │"
                                                      "└∊─────────────┘"))
         (is "↓display 'ABC'(1 4⍴1 2 3 4)(0 1 0⍴0)('88',99)" #("┌→─────────────────────────────┐"
                                                               "│ ┌→──┐ ┌→──────┐ ┌┌⊖┐ ┌→────┐ │"
                                                               "│ │ABC│ ↓1 2 3 4│ ⌽↓0│ │88 99│ │"
                                                               "│ └#──┘ └~──────┘ └└~┘ └~────┘ │"
                                                               "└∊─────────────────────────────┘"))
         (is "↓display (⊂'ab'),¨1⍴⊂⊂,'c'" #("┌→────────────┐"
                                            "│ ┌→────────┐ │"
                                            "│ │     ┌→┐ │ │"
                                            "│ │ a b │c│ │ │"
                                            "│ │ - - └#┘ │ │"
                                            "│ └∊────────┘ │"
                                            "└∊────────────┘"))
         (is "↓displays 1 'a' 'abc' (2 3⍴⍳6)" #("┌→─4────────────────┐"
                                                "│     ┌→─3┐ ┌→─2 3┐ │"
                                                "│ 1 a │abc│ ↓1 2 3│ │"
                                                "│   - └#──┘ │4 5 6│ │"
                                                "│           └~────┘ │"
                                                "└∊──────────────────┘"))
         (is "↓displayr 1 'a' 'abc' (2 3⍴⍳6)" #("┌4────────────────────┐"
                                                "│     ┌3───┐ ┌3─────┐ │"
                                                "│ 1 a │ abc│ 2 1 2 3│ │"
                                                "│   - └#───┘ │ 4 5 6│ │"
                                                "│            └~─────┘ │"
                                                "└¯2───────────────────┘"))
         (is "'Sunday' dist 'Saturday'" 3)
         (is "'sitting' dist 'kitten'" 3)
         (is "'April' dist 'Compiler'" 6)
         (provision "months ← 'January' 'February' 'March' 'April' 'May' 'June' 'July' 'August' 'September' 'October' 'November' 'December'")
         (is "∘.dist⍨months"
             #2A((0 4 6 7 5 5 4 6 9 7 8 8) (4 0 7 7 6 7 6 7 8 7 8 7) (6 7 0 4 3 5 5 6 9 7 8 8)
                 (7 7 4 0 5 5 5 5 8 7 8 8) (5 6 3 5 0 4 3 6 9 7 8 8) (5 7 5 5 4 0 2 5 8 6 7 7)
                 (4 6 5 5 3 2 0 5 9 7 8 8) (6 7 6 5 6 5 5 0 9 7 8 8) (9 8 9 8 9 8 9 9 0 5 4 3)
                 (7 7 7 7 7 6 7 7 5 0 5 4) (8 8 8 8 8 7 8 8 4 5 0 3) (8 7 8 8 8 7 8 8 3 4 3 0)))
         (is "fuzzy∘months¨'dcmbr' 'marching' 'febury'" #("December" "March" "February"))
         (is "↓dsp 'hello' 'world'" #("───────────"
                                      "hello│world"))
         (is "↓0 dsp Rgt⍣5 ⊢2 Tape 2/¨12↑⎕A" #("────────────────│FF│GG│────────────────"
                                               "─────────────│EE       HH│─────────────"
                                               "──────────│DD             II│──────────"
                                               "───────│CC                   JJ│───────"
                                               "────│BB                         KK│────"
                                               "∘│AA                               LL│∘"))
         (is "↓dsp 3 3⍴⊂2 2⍴⍳4" #("───────────"
                                  "1 2│1 2│1 2"
                                  "3 4│3 4│3 4"
                                  "───┼───┼───"
                                  "1 2│1 2│1 2"
                                  "3 4│3 4│3 4"
                                  "───┼───┼───"
                                  "1 2│1 2│1 2"
                                  "3 4│3 4│3 4"))
         (is "0 enlist vecs" #(#\h #\e #\l #\l #\o #\w #\o #\r #\l #\d #\b #\o #\n #\j #\o #\u #\r #\m #\o
                               #\n #\d #\e #\g #\o #\o #\d #\n #\i #\g #\h #\t #\b #\o #\n #\s #\o #\i #\r))
         (is "1 enlist vecs" #("hello" "world" "bonjour" "monde" "good" "night" "bon" "soir"))
         (is "2 enlist vecs" #(#("hello" "world") #("bonjour" "monde") #("good" "night") #("bon" "soir")))
         (is "3 enlist vecs" #(#(#("hello" "world") #("bonjour" "monde"))
                               #(#("good" "night") #("bon" "soir"))))
         (provision "tea ← 2 2⍴('tea'4'two')(2'for' 'T')('me' '&' 'you')('u' 'and' 'me')")
         (is "0 enlist tea" #(#\t #\e #\a 4 #\t #\w #\o 2 #\f #\o #\r #\T #\m #\e #\& #\y #\o #\u #\u #\a
                              #\n #\d #\m #\e))
         (is "1 enlist tea" #("tea" 4 "two" 2 "for" #\T "me" #\& "you" #\u "and" "me"))
         (is "2 enlist tea" #(#("tea" 4 "two") #(2 "for" #\T) #("me" #\& "you") #(#\u "and" "me")))
         (is "'abracadabra' {⍺~⍵⊃⍺} foldl 1 2" "bcdb")
         (is "'abracadabra' {⍺~⍵⊃⍺} foldl 2 1" "rcdr")
         (is "0,∘⊂⍨ foldl 2 5⍴⍳10" #2A((5 #(4 #(3 #(2 #*10)))) (10 #(9 #(8 #(7 #(6 0)))))))
         (provision "ta1 ta2 ta3 ta4 ← {10⊥¨⍳⌽⍵}¨,\\5 4 3 2")
         (is "(⊂2 1) from ta1" #(2 1))
         (is "(2 1)(,⊂⍬) from ta2" #2A((21 22 23 24 25) (11 12 13 14 15)))
         (is "(,⊂⍬)(2 1) from ta2" #2A((12 11) (22 21) (32 31) (42 41)))
         (is "(2 1)(,⊂⍬)(,⊂⍬) from ta3" #3A(((211 212 213 214 215) (221 222 223 224 225)
                                             (231 232 233 234 235) (241 242 243 244 245))
                                            ((111 112 113 114 115) (121 122 123 124 125)
                                             (131 132 133 134 135) (141 142 143 144 145))))
         (is "(,⊂⍬)(2 1)(,⊂⍬) from ta3" #3A(((121 122 123 124 125) (111 112 113 114 115))
                                            ((221 222 223 224 225) (211 212 213 214 215))
                                            ((321 322 323 324 325) (311 312 313 314 315))))
         (is "(,⊂⍬)(,⊂⍬)(2 1) from ta3" #3A(((112 111) (122 121) (132 131) (142 141))
                                            ((212 211) (222 221) (232 231) (242 241))
                                            ((312 311) (322 321) (332 331) (342 341))))
         (is "⍴(6 7⍴1)(,⊂⍬)(,⊂⍬) from ta3" #(6 7 4 5))
         (is "⍴(,⊂⍬)(6 7⍴1)(,⊂⍬) from ta3" #(3 6 7 5))
         (is "⍴(,⊂⍬)(,⊂⍬)(6 7⍴1) from ta3" #(3 4 6 7))
         (is "⍴(6 7⍴1)(,⊂⍬)(,⊂⍬)(,⊂⍬) from ta4" #(6 7 3 4 5))
         (is "⍴(,⊂⍬)(6 7⍴1)(,⊂⍬)(,⊂⍬) from ta4" #(2 6 7 4 5))
         (is "⍴(,⊂⍬)(,⊂⍬)(6 7⍴1)(,⊂⍬) from ta4" #(2 3 6 7 5))
         (is "⍴(,⊂⍬)(,⊂⍬)(,⊂⍬)(6 7⍴1) from ta4" #(2 3 4 6 7))
         (is "⍴⍴ 1 from ta1" #(0))
         (is "⍴⍴ 1(,⊂⍬) from ta2" #(1))
         (is "⍴⍴ 1(,⊂⍬)(,⊂⍬) from ta3" #(2))
         (is "⍴⍴ 1(,⊂⍬)(,⊂⍬)(,⊂⍬) from ta4" #(3))
         (is "(2 1)(2 1)(2 1)(2 1) from ta4" #4A((((2222 2221) (2212 2211)) ((2122 2121) (2112 2111)))
                                                 (((1222 1221) (1212 1211)) ((1122 1121) (1112 1111)))))
         (is "(,⊂⍬)(2 1)(2 1) from ta3" #3A(((122 121) (112 111)) ((222 221) (212 211)) ((322 321) (312 311))))
         (is "(2 1)(,⊂⍬)(2 1) from ta3" #3A(((212 211) (222 221) (232 231) (242 241))
                                            ((112 111) (122 121) (132 131) (142 141))))
         (is "(2 1)(2 1)(,⊂⍬) from ta3" #3A(((221 222 223 224 225) (211 212 213 214 215))
                                            ((121 122 123 124 125) (111 112 113 114 115))))
         (is "{ta3[⍵;;] ≡ ⍵ (,⊂⍬) (,⊂⍬) from ta3} 2 2⍴2 1" 1)
         (is "(2 3 4⍴2 4 5) {ta3[⍵;;⍺] ≡ ⍵ (,⊂⍬) ⍺ from ta3} 2 2⍴2 1" 1)
         (is "⍴(,1)(,2)(,3) from ta3" #*111)
         (is "⍴⍴(,1)(,2)(,3) from ta3" #(3))
         (is "⍴⍴(,1)( 2)(,3) from ta3" #(2))
         (is "⍴⍴( 1)( 2)(,3) from ta3" #(1))
         (is "⍴⍴( 1)( 2)( 3) from ta3" #(0))
         (is "⍴(↑⍴⍨¨/3/⊂⍳4) from ta4" #(1 2 2 3 3 3 4 4 4 4))
         (is "⍴(↑⍴⍨¨/1 2/{⌽⍵}\\2/⊂⍳4) from ta4" #(4 4 4 4 3 3 3 2 2 1))
         (is "(⊂2 2⍴2 1 1) from 'hello' 'world'" #3A(((#\w #\o #\r #\l #\d) (#\h #\e #\l #\l #\o))
                                                     ((#\h #\e #\l #\l #\o) (#\w #\o #\r #\l #\d))))
         (is "1 in 3 1 4 1 5" #(#(2) #(4)))
         (is "'o' in 'hello' 'world'" #(#(1 5) #(2 2)))
         (is "'o'in ⍪'hello' 'world'" #(#(#*11 5) #(#(2 1) 2)))
         (is "0 in (1 1⍴⊂)⍣4⊢0" #(#(#*11 #*11 #*11 #*11)))
         (is "list 'hello'" #(#\h #(#\e #(#\l #(#\l #(#\o #\RING_OPERATOR))))))
         (is "listLength list ⎕A" 26)
         (is "vectFromList list 'hello'" "hello")
         (is "vectFromList listRmDups list 'Mississippi'" "Misisipi")
         (is "nlines 2 3 4⍴⍳24" 7)
         (is "nlines 2 2 2 2⍴2" 12)
         (is "nlines 2 0 4⍴2" 1)
         (is "nlines 2 3 0 0⍴2" 6)
         (is "nlines 1 2 3 4 5 6 7 8⍴9" 5907)
         (is "1(2 3),perv(4 5)6" #(#(#(1 4) #(1 5)) #(#(2 6) #(3 6))))
         (is "pmat 3" #2A((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1)))
         (is "2 3 3 2 +pred ⍳10" #(3 12 21 19))
         (is "{⍵[pmat⍴⍵]}'tic' 'tac' 'toe'" #2A(("tic" "tac" "toe") ("tic" "toe" "tac")("tac" "tic" "toe")
                                                ("tac" "toe" "tic") ("toe" "tic" "tac") ("toe" "tac" "tic")))
         (is "4 3 2⍴↓{⍵[pmat⍴⍵]}'abcd'" #3A((("abcd" "abdc") ("acbd" "acdb") ("adbc" "adcb"))
                                            (("bacd" "badc") ("bcad" "bcda") ("bdac" "bdca"))
                                            (("cabd" "cadb") ("cbad" "cbda") ("cdab" "cdba"))
                                            (("dabc" "dacb") ("dbac" "dbca") ("dcab" "dcba"))))
         (is "{⍵⍳⍵∘.{⍺⊃¨⊂⍵}⍵}↓pmat 3" #2A((1 2 3 4 5 6) (2 1 4 3 6 5)
                                          (3 5 1 6 2 4) (4 6 2 5 1 3)
                                          (5 3 6 1 4 2) (6 4 5 2 3 1)))
         (is "{'<',⍵,'>'} rows 'ten' 'a' 'penny'" #("<ten>" "<a>" "<penny>"))
         (provision "letterMatrices ← ⊂[2 3]3 3 5⍴↑'one' 'two' 'three' 'four' 'five' 'six' 'seven' 'eight' 'nine'")
         (is "{⍴⍵~' '} rows letterMatrices" #(#2A((3) (3) (5)) #2A((4) (4) (3)) #2A((5) (5) (4))))
         (is "{+/⍵≠' '} rows letterMatrices" #(#(3 3 5) #(4 4 3) #(5 5 4)))
         (is "{⍵[⍋⍵]} rows letterMatrices"
             #(#2A((#\  #\  #\e #\n #\o) (#\  #\  #\o #\t #\w) (#\e #\e #\h #\r #\t))
               #2A((#\  #\f #\o #\r #\u) (#\  #\e #\f #\i #\v) (#\  #\  #\i #\s #\x))
               #2A((#\e #\e #\n #\s #\v) (#\e #\g #\h #\i #\t) (#\  #\e #\i #\n #\n))))
         (is "{+/⍵÷⍴⍵} rows 3 4 5⍴⍳60" #2A((3 8 13 18) (23 28 33 38) (43 48 53 58)))
         (is "mscan ⍳10" #(1 -1 2 -2 3 -3 4 -4 5 -5))
         (is "dscan ⍳10" #(1 1/2 3/2 3/8 15/8 5/16 35/16 35/128 315/128 63/256))
         (is "+ascan⍳10" #(1 3 6 10 15 21 28 36 45 55))
         (is "-ascan⍳10" #(1 -1 -4 -8 -13 -19 -26 -34 -43 -53))
         (is "{⍺,'-',⍵} ascan ↑('one' 'two' 'three')('un' 'deux' 'trois')('yan' 'tan' 'tethera')"
             #2A(("one" "one-two" "one-two-three") ("un" "un-deux" "un-deux-trois")
                 ("yan" "yan-tan" "yan-tan-tethera")))
         (is "2 1 2 2 1 select (1 2 3 4 5)(10 20 30 40 50)" #(10 2 30 40 5))
         (is "{(⎕IO+⍵=' ')select ⍵ '.'} ↑'now is' 'the time'"
             #2A((#\n #\o #\w #\. #\i #\s #\. #\.) (#\t #\h #\e #\. #\t #\i #\m #\e)))
         (is "(2 3 4⍴⍳3) select 1 10 100×⊂2 3 4⍴⍳24"
             #3A(((1 20 300 4) (50 600 7 80) (900 10 110 1200))
                 ((13 140 1500 16) (170 1800 19 200) (2100 22 230 2400))))
         (is "1 2 select ('aaa' 'bbb')('AAA' 'BBB')" #("aaa" "BBB"))
         (is "⌈(10000,10⍴1)×⍤1⊢(shannon⍤1 , ⊣) 1+~(⍳10) ∘.> ⍳10"
             #2A((0 2 2 2 2 2 2 2 2 2 2) (4690 1 2 2 2 2 2 2 2 2 2)
                 (7220 1 1 2 2 2 2 2 2 2 2) (8813 1 1 1 2 2 2 2 2 2 2)
                 (9710 1 1 1 1 2 2 2 2 2 2) (10000 1 1 1 1 1 2 2 2 2 2)
                 (9710 1 1 1 1 1 1 2 2 2 2) (8813 1 1 1 1 1 1 1 2 2 2)
                 (7220 1 1 1 1 1 1 1 1 2 2) (4690 1 1 1 1 1 1 1 1 1 2)))
         (is "⌊10000×shannon¨'banana' 'orange'" #(14591 25849))
         (is "⌊10000×shannon ⎕A" 47004)
         (is "shannon∘{⍳2*⍵}¨⍳10" #(1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0))
         (is "'abba' subvec 'babba'" 1)
         (is "'abba' subvec 'abbas'" 1)
         (is "'abba' subvec 'baab'" 0)
         (is "'abba' subvec 'abba'" 1)
         (is "'abba'∘subvec¨'zabba' 'babba' 'abbas' 'baab'" #(1 1 1 0))
         (is "3 3 2 1 subvec 4 3 5 6 3 2 8 1 9" 1)
         (is "1 2 3 4 subvec 1 3 4 2 1 2 3 4" 1)
         (is "'abba' subvec 'ababa'" 1)
         (is "1 2 3 subvec 3 2 1" 0)
         (is "3 3 2 1 subvec 3 2 1" 0)
         (is "3 3 2 1 subvec 4 3 5 6 3 2 8 1 9" 1)
         (is "'also' 'work' subvec 'also' 'nested' 'arrays' 'work'" 1)
         (is "0 subvec ,0" 1)))
