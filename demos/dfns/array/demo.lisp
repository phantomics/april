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
                               "├──→┼~──→┤"
                               "│&  │2 40│"
                               "└───┴~──→┘"))
         (is "+/∘∊¨∊∘'→↓~'¨1∘disp {(⍺⍺ ⍵)(⍵⍵ ⍵)} display ditty" #(8 8))
         (is "↓disp'%' 'Eye Poke' 'Kumquat'⍪↑('Guys' 60 40)('Gals' 20 80)" #("┌────┬────────┬───────┐"
                                                                             "│%   │Eye Poke│Kumquat│"
                                                                             "├────┼────────┼───────┤"
                                                                             "│Guys│60      │40     │"
                                                                             "├────┼────────┼───────┤"
                                                                             "│Gals│20      │80     │"
                                                                             "└────┴────────┴───────┘"))
         (provision "solf←↑'do' ' re' '  mi' '   fa' '    sol' '     la' '      ti'")
         (is "↓display solf" #("┌→───────┐"
                               "↓do      │"
                               "│ re     │"
                               "│  mi    │"
                               "│   fa   │"
                               "│    sol │"
                               "│     la │"
                               "│      ti│"
                               "└────────┘"))
         (is "↓display 1 'a' 'abc' (2 3⍴⍳6)" #("┌→──────────────────┐"
                                               "│     ┌→──┐ ┌→────┐ │"
                                               "│ 1 a │abc│ ↓1 2 3│ │"
                                               "│   - └───┘ │4 5 6│ │"
                                               "│           └~────┘ │"
                                               "└∊──────────────────┘"))
         (is "↓display 2 2⍴'Tea'(2 1⍴4 2)'&'(2 40)" #("┌→─────────────┐"
                                                      "↓ ┌→──┐ ┌→┐    │"
                                                      "│ │Tea│ ↓4│    │"
                                                      "│ └───┘ │2│    │"
                                                      "│       └~┘    │"
                                                      "│       ┌→───┐ │"
                                                      "│ &     │2 40│ │"
                                                      "│ -     └~───┘ │"
                                                      "└∊─────────────┘"))
         (is "↓display 'ABC'(1 4⍴1 2 3 4)(0 1 0⍴0)('88',99)" #("┌→─────────────────────────────┐"
                                                               "│ ┌→──┐ ┌→──────┐ ┌┌⊖┐ ┌→────┐ │"
                                                               "│ │ABC│ ↓1 2 3 4│ ⌽↓0│ │88 99│ │"
                                                               "│ └───┘ └~──────┘ └└~┘ └+────┘ │"
                                                               "└∊─────────────────────────────┘"))
         (is "↓display (⊂'ab'),¨1⍴⊂⊂,'c'" #("┌→────────────┐"
                                            "│ ┌→────────┐ │"
                                            "│ │     ┌→┐ │ │"
                                            "│ │ a b │c│ │ │"
                                            "│ │ - - └─┘ │ │"
                                            "│ └∊────────┘ │"
                                            "└∊────────────┘"))
         (is "↓displays 1 'a' 'abc' (2 3⍴⍳6)" #("┌→─4────────────────┐"
                                                "│     ┌→─3┐ ┌→─2 3┐ │"
                                                "│ 1 a │abc│ ↓1 2 3│ │"
                                                "│   - └───┘ │4 5 6│ │"
                                                "│           └~────┘ │"
                                                "└∊──────────────────┘"))
         (is "↓displayr 1 'a' 'abc' (2 3⍴⍳6)" #("┌4────────────────────┐"
                                                "│     ┌3───┐ ┌3─────┐ │"
                                                "│ 1 a │ abc│ 2 1 2 3│ │"
                                                "│   - └────┘ │ 4 5 6│ │"
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
         (is "⍕0 enlist vecs" "helloworldbonjourmondegoodnightbonsoir")
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
         (is "⍴ (,1)(,2)(,3) from ta3" #*111)
         (is "⍴⍴(,1)(,2)(,3) from ta3" #(3))
         (is "⍴⍴(,1)( 2)(,3) from ta3" #(2))
         (is "⍴⍴( 1)( 2)(,3) from ta3" #(1))
         (is "⍴⍴( 1)( 2)( 3) from ta3" #(0))
         (is "⍴(↑⍴⍨¨/3/⊂⍳4) from ta4" #(1 2 2 3 3 3 4 4 4 4))
         (is "⍴(↑⍴⍨¨/1 2/{⌽⍵}\\2/⊂⍳4) from ta4" #(4 4 4 4 3 3 3 2 2 1))
         (is "↓(⊂2 2⍴2 1 1) from 'hello' 'world'" #2A(("world" "hello") ("hello" "world")))
         (is "(0↑⊂,⊂⍬) from 99" 99)
         (is "1 in 3 1 4 1 5" #(#(2) #(4)))
         (is "'o' in  'hello' 'world'" #(#(1 5) #(2 2)))
         (is "'o' in ⍪'hello' 'world'" #(#(#*11 5) #(#(2 1) 2)))
         (is "0 in (1 1⍴⊂)⍣4⊢0" #(#(#*11 #*11 #*11 #*11)))
         (is "list 'hello'" #(#\h #(#\e #(#\l #(#\l #(#\o #\RING_OPERATOR))))))
         (is "listLength list ⎕A" 26)
         (is "vectFromList list 'hello'" "hello")
         (is "vectFromList listRmDups list 'Mississippi'" "Misisipi")
         (is "↓'<<*>>' showmatch '<<>> <<aa>>'" #("<<>> <<aa>>"
                                                  "¯    ¯     "))
         (is "↓'<<*>>' showmatch 2 2 24⍴'<<aa>>  <<>>  <<bbb>> '" #("<<aa>>  <<>>  <<bbb>> <<"
                                                                    "¯       ¯     ¯         "
                                                                    "aa>>  <<>>  <<bbb>> <<aa"
                                                                    "      ¯     ¯           "
                                                                    ">>  <<>>  <<bbb>> <<aa>>"
                                                                    "    ¯     ¯       ¯     "
                                                                    "  <<>>  <<bbb>> <<aa>>  "
                                                                    "  ¯     ¯       ¯       "))
         (is "↓'a*b*d' showmatch 'aaaabbbccd'" #("aaaabbbccd"
                                                 "¯¯¯¯      "))
         (is "↓'a*a' showmatch 'abracadabra'" #("abracadabra"
                                                "¯  ¯ ¯ ¯   "))
         (is "↓'12*56*9' showmatch ⎕D" #("0123456789"
                                         " ¯        "))
         (is "↓⍕(2⍴¨¨'12*56*9' '*') showmatch 2⍴¨⎕D" #(" 00  11  22  33  44  55  66  77  88  99"
                                                       "     ¯                                 "))
         (is "'ban*and' match 'band'" #(0 0 0 0))
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
         (is "{⍵⍳⍵∘.{⍺⊃¨⊂⍵}⍵}↓pmat 3" #2A((1 2 3 4 5 6) (2 1 4 3 6 5) (3 5 1 6 2 4)
                                          (4 6 2 5 1 3) (5 3 6 1 4 2) (6 4 5 2 3 1)))
         (is "{'<',⍵,'>'} rows 'ten' 'a' 'penny'" #("<ten>" "<a>" "<penny>"))
         (provision "letterMatrices ← ⊂[2 3]3 3 5⍴↑'one' 'two' 'three' 'four' 'five' 'six' 'seven' 'eight' 'nine'")
         (is "{⍴⍵~' '} rows letterMatrices" #(#2A((3) (3) (5)) #2A((4) (4) (3)) #2A((5) (5) (4))))
         (is "{+/⍵≠' '} rows letterMatrices" #(#(3 3 5) #(4 4 3) #(5 5 4)))
         (is "↓¨{⍵[⍋⍵]} rows letterMatrices" #(#("  eno" "  otw" "eehrt")
                                               #(" foru" " efiv" "  isx")
                                               #("eensv" "eghit" " einn")))
         (is "{+/⍵÷⍴⍵} rows 3 4 5⍴⍳60" #2A((3 8 13 18) (23 28 33 38) (43 48 53 58)))
         (provision "vex←('one' 'two' 'three')('alpha' 'beta' 'gamma')('red' 'blue' 'green')")
         (is "↓disp 2 ↑sam⌽  vex" #("┌──────────────────┬───────────────┬────────────────┐"
                                    "│┌─────┬────┬─────┐│┌───┬───┬─────┐│┌───┬────┬─────┐│"
                                    "││alpha│beta│gamma│││one│two│three│││red│blue│green││"
                                    "│└─────┴────┴─────┘│└───┴───┴─────┘│└───┴────┴─────┘│"
                                    "└──────────────────┴───────────────┴────────────────┘"))
         (is "↓disp 2 ↑sam⌽¨ vex" #("┌───────────────┬──────────────────┬────────────────┐"
                                    "│┌───┬───┬─────┐│┌────┬─────┬─────┐│┌────┬───┬─────┐│"
                                    "││two│one│three│││beta│alpha│gamma│││blue│red│green││"
                                    "│└───┴───┴─────┘│└────┴─────┴─────┘│└────┴───┴─────┘│"
                                    "└───────────────┴──────────────────┴────────────────┘"))
         (is "↓disp 2 ↑sam⌽¨¨vex" #("┌───────────────┬──────────────────┬────────────────┐"
                                    "│┌───┬───┬─────┐│┌─────┬────┬─────┐│┌───┬────┬─────┐│"
                                    "││noe│wto│htree│││lapha│ebta│agmma│││erd│lbue│rgeen││"
                                    "│└───┴───┴─────┘│└─────┴────┴─────┘│└───┴────┴─────┘│"
                                    "└───────────────┴──────────────────┴────────────────┘"))
         (is "↓disp ⊃sam ⍪vex" #("┌─────┬──────────────────┬────────────────┐"
                                 "│┌───┐│┌─────┬────┬─────┐│┌───┬────┬─────┐│"
                                 "││one│││alpha│beta│gamma│││red│blue│green││"
                                 "│├───┤│└─────┴────┴─────┘│└───┴────┴─────┘│"
                                 "││two││                  │                │"
                                 "│├───┤│                  │                │"
                                 "││thr││                  │                │"
                                 "│└───┘│                  │                │"
                                 "└─────┴──────────────────┴────────────────┘"))
         (is "↓disp 2⊃sam ⍪vex" #("┌───────────────┬───────┬────────────────┐"
                                  "│┌───┬───┬─────┐│┌─────┐│┌───┬────┬─────┐│"
                                  "││one│two│three│││alpha│││red│blue│green││"
                                  "│└───┴───┴─────┘│├─────┤│└───┴────┴─────┘│"
                                  "│               ││beta ││                │"
                                  "│               │├─────┤│                │"
                                  "│               ││gamma││                │"
                                  "│               │└─────┘│                │"
                                  "└───────────────┴───────┴────────────────┘"))
         (is "↓disp 1 2 3 ⊃sam⍪¨vex" #("┌─────────────┬───────────────┬────────────┐"
                                       "│┌─┬───┬─────┐│┌─────┬─┬─────┐│┌───┬────┬─┐│"
                                       "││o│two│three│││alpha│b│gamma│││red│blue│g││"
                                       "││n│   │     │││     │e│     │││   │    │r││"
                                       "││e│   │     │││     │t│     │││   │    │e││"
                                       "│└─┴───┴─────┘││     │a│     │││   │    │e││"
                                       "│             │└─────┴─┴─────┘││   │    │n││"
                                       "│             │               │└───┴────┴─┘│"
                                       "└─────────────┴───────────────┴────────────┘"))
         (is "↓disp 1 2 3 ⊃sam{'<',⍵,'>'}¨vex"
             #("┌─────────────────┬────────────────────┬──────────────────┐"
               "│┌─────┬───┬─────┐│┌─────┬──────┬─────┐│┌───┬────┬───────┐│"
               "││<one>│two│three│││alpha│<beta>│gamma│││red│blue│<green>││"
               "│└─────┴───┴─────┘│└─────┴──────┴─────┘│└───┴────┴───────┘│"
               "└─────────────────┴────────────────────┴──────────────────┘"))
         (is "↓disp 1 0 1 /sam{⊂'---'} vex" #("┌───┬──────────────────┬───┐"
                                              "│---│┌─────┬────┬─────┐│---│"
                                              "│   ││alpha│beta│gamma││   │"
                                              "│   │└─────┴────┴─────┘│   │"
                                              "└───┴──────────────────┴───┘"))
         (is "↓disp (⊂1 0 1) /sam{⊂'---'}¨vex"
             #("┌─────────────┬──────────────┬──────────────┐"
               "│┌───┬───┬───┐│┌───┬────┬───┐│┌───┬────┬───┐│"
               "││---│two│---│││---│beta│---│││---│blue│---││"
               "│└───┴───┴───┘│└───┴────┴───┘│└───┴────┴───┘│"
               "└─────────────┴──────────────┴──────────────┘"))
         (is "↓disp 1 0 1 /sam{⊂'---'}¨ vex" #("┌─────────────┬──────────────────┬─────────────┐"
                                               "│┌───┬───┬───┐│┌─────┬────┬─────┐│┌───┬───┬───┐│"
                                               "││---│---│---│││alpha│beta│gamma│││---│---│---││"
                                               "│└───┴───┴───┘│└─────┴────┴─────┘│└───┴───┴───┘│"
                                               "└─────────────┴──────────────────┴─────────────┘"))
         (is "↓disp 1 1 ⍉sam⌽ ↑vex" #("┌─────┬────┬─────┐"
                                      "│green│two │three│"
                                      "├─────┼────┼─────┤"
                                      "│alpha│beta│gamma│"
                                      "├─────┼────┼─────┤"
                                      "│red  │blue│one  │"
                                      "└─────┴────┴─────┘"))
         (is "↓disp 1 1 ⍉sam(⌽¨) ↑vex" #("┌─────┬────┬─────┐"
                                         "│eno  │two │three│"
                                         "├─────┼────┼─────┤"
                                         "│alpha│ateb│gamma│"
                                         "├─────┼────┼─────┤"
                                         "│red  │blue│neerg│"
                                         "└─────┴────┴─────┘"))
         (is "↓disp 1 1 ⍉sam(ucase¨) ↑vex" #("┌─────┬────┬─────┐"
                                             "│ONE  │two │three│"
                                             "├─────┼────┼─────┤"
                                             "│alpha│BETA│gamma│"
                                             "├─────┼────┼─────┤"
                                             "│red  │blue│GREEN│"
                                             "└─────┴────┴─────┘"))
         (is "↓disp (⊂1 1) ⍉sam{ucase ⍵}¨ ↑¨vex" #("┌─────┬─────┬─────┐"
                                                   "│One  │Alpha│Red  │"
                                                   "│tWo  │bEta │bLue │"
                                                   "│thRee│gaMma│grEen│"
                                                   "└─────┴─────┴─────┘"))
         (is "↓disp (⊂1 1) ⍉sam{'·'}¨ ↑¨vex" #("┌─────┬─────┬─────┐"
                                               "│·ne  │·lpha│·ed  │"
                                               "│t·o  │b·ta │b·ue │"
                                               "│th·ee│ga·ma│gr·en│"
                                               "└─────┴─────┴─────┘"))
         (provision "eng←'One' '' '' '' 'Five'")
         (provision "esp←'Uno' 'Dos' 'Tres' '' ''")
         (is "⌽saw eng esp" #(#("enO" "" "" "" "eviF") #("onU" "soD" "serT" "" "")))
         (is "eng {⍺,'=',⍵}saw esp" #("One=Uno" "=Dos" "=Tres" "=" "Five="))
         (is "eng {⍺{' '^.=⍺:⍵ ⋄ ⍺}saw ⍵} esp" #("One" "Dos" "Tres" "" "Five"))
         (is "↑{⍺{' '^.=⍺:⍵ ⋄ ⍺}saw ⍵}/ eng esp '¿?'" #("One" "Dos" "Tres" "¿?" "Five"))
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
         (is "⌈(10000,10⍴1)×⍤1⊢(shannon⍤1 , ⊣) 1+~(⍳10) ∘.> ⍳10" #2A((0     2 2 2 2 2 2 2 2 2 2)
                                                                     (4690  1 2 2 2 2 2 2 2 2 2)
                                                                     (7220  1 1 2 2 2 2 2 2 2 2)
                                                                     (8813  1 1 1 2 2 2 2 2 2 2)
                                                                     (9710  1 1 1 1 2 2 2 2 2 2)
                                                                     (10001 1 1 1 1 1 2 2 2 2 2)
                                                                     (9710  1 1 1 1 1 1 2 2 2 2)
                                                                     (8813  1 1 1 1 1 1 1 2 2 2)
                                                                     (7220  1 1 1 1 1 1 1 1 2 2)
                                                                     (4690  1 1 1 1 1 1 1 1 1 2)))
         (is "⌊10000×shannon¨'banana' 'orange'" #(14591 25849))
         (is "⌊10000×shannon ⎕A" 47004)
         (is "⌊shannon∘{⍳2*⍵}¨⍳10" #(1 2 3 4 5 6 7 8 9 10))
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
         (is "0 subvec ,0" 1)
         (is "↓⍕' -'∘subs 2 12⍴'Many a time and oft.    '" #("Many-a-time-" "and-oft.----"))
         (is "(6 7)(666 777)subs 2 3 4⍴⍳12" #3A(((1 2 3 4) (5 666 777 8) (9 10 11 12))
                                                ((1 2 3 4) (5 666 777 8) (9 10 11 12))))
         (is "↓⍕(7 8 9) 'repl' subs 3 5⍴⍳15" #(" 1  2  3  4  5  0"
                                               " 6  r  e  p  l 10"
                                               "11 12 13 14 15  0"))
         (is "↓⍕(7 8 9) 'r'    subs 3 5⍴⍳15" #(" 1  2  3  4  5"
                                               " 6  r 10  0  0"
                                               "11 12 13 14 15"))
         (is "⍕↑subs/('Mr' 'Mrs')('his' 'her')'Mr Green and his daughter, Theresa.'"
             "Mrs Green and her daughter, Theresa.")
         (is "↓⍕(3,¨2 3 4)('peek' 'a' 'boo') subs ⍳5 5" #(" 1 1  1 2   1 3  1 4  1 5"
                                                          " 2 1  2 2   2 3  2 4  2 5"
                                                          " 3 1  peek  a    boo  3 5"
                                                          " 4 1  4 2   4 3  4 4  4 5"
                                                          " 5 1  5 2   5 3  5 4  5 5"))
         (is "(1 1)(2 3)subs 0 1 1 1 0" #(0 2 3 2 3 0))
         (is "↓display ¯1 0 1 just¨⊂'  hello world  '"
             #("┌→──────────────────────────────────────────────────────┐"
               "│ ┌→──────────────┐ ┌→──────────────┐ ┌→──────────────┐ │"
               "│ │hello world    │ │  hello world  │ │    hello world│ │"
               "│ └───────────────┘ └───────────────┘ └───────────────┘ │"
               "└∊──────────────────────────────────────────────────────┘"))
         (is "↓display just solf" #("┌→───────┐"
                                    "↓do      │"
                                    "│re      │"
                                    "│mi      │"
                                    "│fa      │"
                                    "│sol     │"
                                    "│la      │"
                                    "│ti      │"
                                    "└────────┘"))
         (is "↓display ¯1 just solf" #("┌→───────┐"
                                       "↓do      │"
                                       "│re      │"
                                       "│mi      │"
                                       "│fa      │"
                                       "│sol     │"
                                       "│la      │"
                                       "│ti      │"
                                       "└────────┘"))
         (is "↓display 0 just solf" #("┌→───────┐"
                                      "↓   do   │"
                                      "│   re   │"
                                      "│   mi   │"
                                      "│   fa   │"
                                      "│  sol   │"
                                      "│   la   │"
                                      "│   ti   │"
                                      "└────────┘"))
         (is "↓display 1 just solf" #("┌→───────┐"
                                      "↓      do│"
                                      "│      re│"
                                      "│      mi│"
                                      "│      fa│"
                                      "│     sol│"
                                      "│      la│"
                                      "│      ti│"
                                      "└────────┘"))
         (is "↓display ¯1 0 1 just¨⊂2 4 7⍴solf" #("┌→─────────────────────────────────┐"
                                                  "│ ┌┌→──────┐ ┌┌→──────┐ ┌┌→──────┐ │"
                                                  "│ ↓↓do     │ ↓↓  do   │ ↓↓     do│ │"
                                                  "│ ││re     │ ││  re   │ ││     re│ │"
                                                  "│ ││mi     │ ││  mi   │ ││     mi│ │"
                                                  "│ ││f      │ ││   f   │ ││      f│ │"
                                                  "│ ││       │ ││       │ ││       │ │"
                                                  "│ ││a      │ ││   a   │ ││      a│ │"
                                                  "│ ││sol    │ ││  sol  │ ││    sol│ │"
                                                  "│ ││la     │ ││  la   │ ││     la│ │"
                                                  "│ ││ti     │ ││  ti   │ ││     ti│ │"
                                                  "│ └└───────┘ └└───────┘ └└───────┘ │"
                                                  "└∊─────────────────────────────────┘"))
         (is "↓⍕lcase 2 7⍴'PLEASE WHISPER'" #("please " "whisper"))
         (is "↓⍕ucase 2 8⍴'please, speak up'" #("PLEASE, " "SPEAK UP"))
         (is "↓display {(⊂⍵[⍋lcase ⍵;]),⊂⍵[⍋⍵;]} 5 7⍴'Baker  Fox    able   Dog    charlie'"
             #("┌→────────────────────┐"
               "│ ┌→──────┐ ┌→──────┐ │"
               "│ ↓able   │ ↓Baker  │ │"
               "│ │Baker  │ │Dog    │ │"
               "│ │charlie│ │Fox    │ │"
               "│ │Dog    │ │able   │ │"
               "│ │Fox    │ │charlie│ │"
               "│ └───────┘ └───────┘ │"
               "└∊────────────────────┘"))
         (is "↓display {(⊂⍵),⊂mtrim ⍵} 4 10⍴'It        little    profits   that      '"
             #("┌→───────────────────────┐"
               "│ ┌→─────────┐ ┌→──────┐ │"
               "│ ↓It        │ ↓It     │ │"
               "│ │little    │ │little │ │"
               "│ │profits   │ │profits│ │"
               "│ │that      │ │that   │ │"
               "│ └──────────┘ └───────┘ │"
               "└∊───────────────────────┘"))
         (is "⍕ss'Banana' 'an' 'AN'" "BANANa")
         (is "⍕ss'Banana' 'ana' 'ANA'" "BANAANA")
         (is " ss(⍳10) (3 4 5) (88 99)" #(1 2 88 99 6 7 8 9 10))
         (is "⍕ss'b.bb' 'bb' 'zz'" "b.zz")
         (is "↓disp ssmat(2 12⍴'Is you is oris you aint?')'is' 'was'" #("Is you was or"
                                                                        "was you aint?"))
         (is "ssmat (3 5⍴⍳15) (7 8 9) (70 80 90)" #2A((1 2 3 4 5) (6 70 80 90 10) (11 12 13 14 15)))
         (is "↓⍕ssmat (3 5⍴⍳15) (7 8 9) 'repl'" #(" 1  2  3  4  5  0"
                                                  " 6  r  e  p  l 10"
                                                  "11 12 13 14 15  0"))
         (is "↓⍕ssmat (3 5⍴⍳15) (7 8 9) 'r'" #(" 1  2  3  4  5"
                                               " 6  r 10  0  0"
                                               "11 12 13 14 15"))
         (is "squeeze '   oranges    and     lemons'" " oranges and lemons")
         (is "2003 12 25 13 30 0 timestamp 'Christmas Pudding'" "2003-12-25 13:30:00 Christmas Pudding")
         (provision "newl←⎕ucs 13
    htm←,'<html>                                                    ',newl
    htm,←'  <body>                                                  ',newl
    htm,←'    <table>                                               ',newl
    htm,←'      <tr><td>%</td><td>Eye Poke</td><td>Kumquat</td></tr>',newl
    htm,←'      <tr><td>Guys</td><td>60</td><td>40</td></tr>        ',newl
    htm,←'      <tr><td>Dolls</td><td>20</td><td>80</td></tr>       ',newl
    htm,←'    </table>                                              ',newl
    htm,←' </body>                                                  ',newl
    htm,←'</html>                                                   ',newl")
         (is "'table' htx htm" #(" <tr><td>%</td><td>Eye Poke</td><td>Kumquat</td></tr> <tr><td>Guys</td><td>60</td><td>40</td></tr> <tr><td>Dolls</td><td>20</td><td>80</td></tr> "))
         (is "'<table' htx htm" #("<table> <tr><td>%</td><td>Eye Poke</td><td>Kumquat</td></tr> <tr><td>Guys</td><td>60</td><td>40</td></tr> <tr><td>Dolls</td><td>20</td><td>80</td></tr> </table>"))
         (is "'<tr' htx htm" #("<tr><td>%</td><td>Eye Poke</td><td>Kumquat</td></tr>"
                               "<tr><td>Guys</td><td>60</td><td>40</td></tr>"
                               "<tr><td>Dolls</td><td>20</td><td>80</td></tr>"))
         (is "'<td' htx htm" #("<td>%</td>" "<td>Eye Poke</td>"
                               "<td>Kumquat</td>" "<td>Guys</td>"
                               "<td>60</td>" "<td>40</td>" "<td>Dolls</td>"
                               "<td>20</td>" "<td>80</td>"))
         (is "'<td'∘htx¨'<tr'htx htm" #(#("<td>%</td>" "<td>Eye Poke</td>" "<td>Kumquat</td>")
                                        #("<td>Guys</td>" "<td>60</td>" "<td>40</td>")
                                        #("<td>Dolls</td>" "<td>20</td>" "<td>80</td>")))
         (is "'td'htx htm" #("%" "Eye Poke" "Kumquat" "Guys" "60" "40" "Dolls" "20" "80"))
         (is "↑'td'∘htx¨'tr'htx htm" #2A(("%" "Eye Poke" "Kumquat")
                                         ("Guys" "60" "40")
                                         ("Dolls" "20" "80")))
         (is "⍕¨'td'htx ltov htm" #("%" "Eye Poke" "Kumquat" "Guys" "60" "40" "Dolls" "20" "80"))
         (is "'td'htx ⎕fmt htm" #("%" "Eye Poke" "Kumquat" "Guys" "60" "40" "Dolls" "20" "80"))
         (is "'<jj'htx'<jj>aaa</jj><jj>bbb</jj>'" #("<jj>aaa</jj>" "<jj>bbb</jj>"))
         (is "'<JJ'htx'<jj>aaa</jj><jj>bbb</jj>'" #())
         (provision "lvec←{'fooling around', ⍵, 'with barrels', ⍵, 'in alleys'} ⎕ucs 10")
         (is "⍕¨ltov lvec" #("fooling around" "with barrels" "in alleys"))
         (is "(ltov lvec)≡ltov lvec,⎕ucs 10" 1)
         (is "⍸(⎕UCS 10)=vtol∘ltov⍣≡ lvec" #(15 28 38))
         (is "(' ',⍨⎕UCS 10)ltov lvec" #("fooling" "around" "with" "barrels" "in" "alleys"))
         (is "0 ltov 1 2 3 0 4 5 6 0 7 8 9" #(#(1 2 3) #(4 5 6) #(7 8 9)))
         (is "0 vtol 0 ltov 1 2 3 0 4 5 6 0 7 8 9" #(1 2 3 0 4 5 6 0 7 8 9 0))
         (is "(⊂'and')ltov 'red' 'and' 'yellow' ',' 'pink' 'and' 'green'"
             #(#("red") #("yellow" #\, "pink") #("green")))
         (is "(⊂'and')vtol (⊂'and')ltov 'red' 'and' 'yellow' ',' 'pink' 'and' 'green'"
             #("red" "and" "yellow" #\, "pink" "and" "green" "and"))
         (is "'and' ',' ltov 'red' 'and' 'yellow' ',' 'pink' 'and' 'green'"
             #(#("red") #("yellow") #("pink") #("green")))
         (is "vtol 'fooling around' 'with barrels' 'in alleys'" "fooling around
with barrels
in alleys
")
         (is "⍸(⎕ucs 10)=vtol 'fooling around' 'with barrels' 'in alleys'" #(15 28 38))
         (is "↓⍕1 disp 0 ltov 1 2 3 0 4 5 6 0 7 8 9" #("┌→────┬─────┬─────┐"
                                                       "│1 2 3│4 5 6│7 8 9│"
                                                       "└~───→┴~───→┴~───→┘"))
         (is "26 wrap ⎕a" "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
         (is "26 wrap,'A'" "A")
         (is "⍕' ·'subs 20 wrap 20⍴'tick '" "tick·tick·tick·tick·")
         (is "⍕' ·'subs 19 wrap 20⍴'tock '" "tock·tock·tock·tock
")
         (is "⍕' ·'subs 18 wrap 20⍴'tuck '" "tuck·tuck·tuck
tuck·")
         (is "⍕' ·'subs 10 wrap ⎕a" "ABCDEFGHIJ
KLMNOPQRST
UVWXYZ")
         (is "⍕10 wrap 10 ¯1 10\\'AB'" "AAAAAAAAAA
BBBBBBBBBB")
         (is "⍕10 wrap 10 ¯5 5\\'AB'" "AAAAAAAAAA
    BBBBB")
         (is "⍕' ·'subs 20 wrap(1↓,⍉↑⍬(⍳26))\\⎕a" "A·BB·CCC·DDDD·EEEEE
FFFFFF·GGGGGGG
HHHHHHHH·IIIIIIIII
JJJJJJJJJJ
KKKKKKKKKKK
LLLLLLLLLLLL
MMMMMMMMMMMMM
NNNNNNNNNNNNNN
OOOOOOOOOOOOOOO
PPPPPPPPPPPPPPPP
QQQQQQQQQQQQQQQQQ
RRRRRRRRRRRRRRRRRR
SSSSSSSSSSSSSSSSSSS
TTTTTTTTTTTTTTTTTTTT
UUUUUUUUUUUUUUUUUUUU
U
VVVVVVVVVVVVVVVVVVVV
VV
WWWWWWWWWWWWWWWWWWWW
WWW
XXXXXXXXXXXXXXXXXXXX
XXXX
YYYYYYYYYYYYYYYYYYYY
YYYYY
ZZZZZZZZZZZZZZZZZZZZ
ZZZZZZ")
         (is "⍕' ·'subs 24 wrap2 'Say can I have some of your purple berries? Yes, I''ve been eating them for six or seven weeks now; haven''t got sick once. Prob''ly keep us both alive.'
"
             "Say·can·I·have·some·of
your·purple·berries?
Yes,·I've·been·eating
them·for·six·or·seven
weeks·now;·haven't·got
sick·once.·Prob'ly·keep
us·both·alive.")
         (is "↓⍕' ·'subs 1 disp 4 2⍴24 wrap3 'Say can I have some of your purple berries? Yes, I''ve been eating them for six or seven weeks now; haven''t got sick once. Prob''ly keep us both alive.'"
             #("┌→──────────────────────┬────────────────────────┐"
               "↓Say·can·I·have·some·of·│your·purple·berries?····│"
               "├──────────────────────→┼───────────────────────→┤"
               "│Yes,·I've·been·eating··│them·for·six·or·seven···│"
               "├──────────────────────→┼───────────────────────→┤"
               "│weeks·now;·haven't·got·│sick·once.·Prob'ly·keep·│"
               "├──────────────────────→┼───────────────────────→┤"
               "│us·both·alive.·········│Say·can·I·have·some·of··│"
               "└──────────────────────→┴───────────────────────→┘"))
         (is "⍕justify (⊃'\\ ')(⎕ucs 10)∘subs⊢'We''re all going on a\\summer holiday;\\no more working for a\\week or two.'"
             "We're  all going on a
summer       holiday;
no more working for a
week or two.")
         (provision "text←(⎕ucs 10) {↑⍶{⍺,⍶,⍵}/⍵} 'Where Alph, the sacred river, ran  ' 'Through caverns measureless to man    ' '  Down to a sunless sea.           '")
         (provision "show←' ·'∘subs")
         (is "⍕(show text,⎕UCS 10),show vtrim text"
             "Where·Alph,·the·sacred·river,·ran··
Through·caverns·measureless·to·man····
··Down·to·a·sunless·sea.···········
Where·Alph,·the·sacred·river,·ran
Through·caverns·measureless·to·man
··Down·to·a·sunless·sea.")
         (is "⍕{(show ⍵),(⎕UCS 10),show vtrim ⍵} (⎕UCS 10) {↑⍶{⍺,⍶,⍵}/⍵} ' ' 'ok'"
             "·
ok

ok")
         (is "''∘≡∘⍕∘vtrim¨ '' ' ' '  '" #(1 1 1))
         (is "(⎕UCS 10)=vtrim 0 1 0 1 1 0 0 1 0 0\\⎕UCS 10" #(1 1 1 1))))
