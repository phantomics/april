;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:AprilDemo.Dfns.String -*-
;;;; demo.lisp

(in-package :april-demo.dfns.string)

(april-load (with (:space string-demo-space))
            (asdf:system-relative-pathname (intern (package-name *package*) "KEYWORD") "string.apl"))

(april (with (:space string-demo-space) ;; sample text for the wrapnote function tests
             (:store-val (note-sample "
Paragraphs  of  text  in  notes  such as these, may be wrapped to be more easily
readable  in an edit window of a particular size. Such wrapping might be advant-
ageous for example, on a \"phablet\", where edit windows are much narrower.

\"Flowing\" text paragraphs are identified as having lines that are left and right
justified.  That is, they extend from the _first_ to the _last_ column, possibly
with  some  extra  padding between words. The value of _last_ is taken to be the
width of the first non-blank line following the title sequence.

To avoid mutilating pictures,  rows containing box-drawing characters are exempt
from wrapping:

┌──────────────────────────────────────────────────────────────────────────────┐
│ This picture will not be wrapped, as each row contains at least one of the   │
│ special box-drawing characters. ┌────────────────────────────────────────────┘
└─────────────────────────────────┘

Notice  in  the example below, that [wrapnote] recombines words split by hyphen-
ation.
")
                         (note-sample-output "
Paragraphs  of  text  in notes
such  as these, may be wrapped
to  be more easily readable in
an edit window of a particular
size.  Such  wrapping might be
advantageous for example, on a
\"phablet\",  where edit windows
are much narrower.

\"Flowing\"  text paragraphs are
identified   as  having  lines
that   are   left   and  right
justified.   That   is,   they
extend from the _first_ to the
_last_  column,  possibly with
some   extra  padding  between
words.  The value of _last_ is
taken  to  be the width of the
first non-blank line following
the title sequence.

To  avoid mutilating pictures,
rows   containing  box-drawing
characters   are  exempt  from
wrapping:

┌──────────────────────────────────────────────────────────────────────────────┐
│ This picture will not be wrapped, as each row contains at least one of the   │
│ special box-drawing characters. ┌────────────────────────────────────────────┘
└─────────────────────────────────┘

Notice  in  the example below,
that   [wrapnote]   recombines
words split by hyphenation.
"))) "")

(specify-demo
 "April string demo"
 (with :space string-demo-space
       :description "Implements string processing functions from Dyalog's dfns.")
 (:tests (is "↓display ¯1 0 1 just¨⊂'  hello world  '"
             #("┌→──────────────────────────────────────────────────────┐"
               "│ ┌→──────────────┐ ┌→──────────────┐ ┌→──────────────┐ │"
               "│ │hello world    │ │  hello world  │ │    hello world│ │"
               "│ └───────────────┘ └───────────────┘ └───────────────┘ │"
               "└∊──────────────────────────────────────────────────────┘"))
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
         (is "(⎕UCS 10)=vtrim 0 1 0 1 1 0 0 1 0 0\\⎕UCS 10" #(1 1 1 1))
         (is "noteSampleOutput≡⍕30 wrapnote noteSample" 1)
         (provision "tabText←'whistles        far     and wee'")
         (is "⍕((⎕UCS 9)'→'∘subs) ¯8 xtabs tabText" "whistles→far→and wee")
         (is "⍕((⎕UCS 9)'→'∘subs) ¯4 xtabs tabText" "whistles→→far→→and→wee")
         (is "tabTrips tabText" 1)
         (is "tabTrips ,(6 35⍴↑,/(⍳20)↑¨'⎕'),⊃⌽⎕UCS 10" 1)
         (is "(2/(⎕UCS 9) ' ')≡¯4 xtabs 10↑''" 1)
         (is "∧/tabTrips¨1↓,\\10↑''" 1)
         (provision "show←' ·'∘subs")
         (provision "cvec←'  twas  ever  thus  '")
         (is "⍕show cvec" "··twas··ever··thus··")
         (is "⍕show dlb cvec" "twas··ever··thus··")
         (is "⍕show dtb cvec" "··twas··ever··thus")
         (is "⍕show deb cvec" "twas··ever··thus")
         (is "⍕show dmb cvec" "·twas·ever·thus·")
         (is "⍕show dxb cvec" "twas·ever·thus")
         (is "⍕show dab cvec" "twaseverthus")
         (provision "cmat←2 20⍴'  twas  ever  thus    heart with  clay  '")
         (is "↓⍕show cmat" #("··twas··ever··thus··"
                             "··heart·with··clay··"))
         (is "↓⍕show dlb cmat" #("twas··ever··thus··"
                                 "heart·with··clay··"))
         (is "↓⍕show deb cmat" #("twas··ever··thus"
                                 "heart·with··clay"))
         (is "↓⍕show dxb cmat" #("twas··ever·thus"
                                 "heart·with·clay"))
         (is "↓⍕show¨ dxb↓cmat" #(" twas·ever·thus  heart·with·clay"))
         (is "↓⍕show ↑dxb↓cmat" #("twas·ever·thus·"
                                  "heart·with·clay"))
         (is "⍕'s'dmb'Mississippi'"  "Misisippi")
         (is "⍕'sp'dmb'Mississippi'" "Misisipi")
         (is "⍕'is'dxb'Mississippi'" "Mipp")
         (is "1 dmb 8 1 1 1 2 5" #(8 1 2 5))))
