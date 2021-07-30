⍝ Ported from http://dfns.dyalog.com/n_contents.htm into April APL

⍝ From http://dfns.dyalog.com/n_alists.htm

alpush ← {                                   ⍝ Association list ⍺ prefixed with (key value) pair ⍵.
  ⍺,⍨∘⊂¨⍵                                    ⍝ :: list ← list ∇ key val
}

alpop ← {                                    ⍝ Leftmost value for key ⍵ from list ⍺
  keys vals←⍺                                ⍝ keys vector and corresponding values
  indx←keys⍳⊂⍵                               ⍝ index of first key ⍵ in list ⍺
  val←indx⊃vals                              ⍝ value for key ⍵
  list←(⊂indx≠⍳⍴keys)/¨⍺                     ⍝ reduced list
  val list                                   ⍝ :: val list ← list ∇ key
}

alget ← {                                    ⍝ Value for key ⍵ in association list ⍺.
  keys vals←⍺                                ⍝ keys vector and corresponding values
  (keys⍳⊂⍵)⊃vals                             ⍝ :: val ← list ∇ key
}

alset ← {                                    ⍝ Assoc list ⍺ with (key value) pair ⍵ replaced.
  key val←⍵                                  ⍝ key and new value
  {(⊂val)@(⍺⍳⊂key)⊢⍵}\⍺                      ⍝ :: list ← list ∇ key value
}

⍝ From http://dfns.dyalog.com/c_acc.htm

acc ← { ⊃⍺⍺{(⊂⍺ ⍺⍺⊃⍬⍴⍵),⍵}/1↓{⍵,⊂⍬⍴⍵}¯1⌽⍵ }  ⍝ Accumulating reduction.

⍝ From http://dfns.dyalog.com/c_disp.htm

disp ← { ⎕IO←0                               ⍝ Boxed sketch of nested array.

  ⍺←⍬ ⋄ dec ctd←2↑⍺                          ⍝ 1:decorated, 1:centred.

  matr←{↑,↓⍵}                                ⍝ matrix from non-scalar array.
  sepr←{+/¨1⊂↑⍵}                             ⍝ vec-of-mats from mat-of-vecs.
  ⍝ open←{16::(1⌈⍴⍵)⍴⊂'[ref]' ⋄ (⍺⌈⍴⍵)⍴⍵}    ⍝ stretched to expose nulls.
  open←{(⍺⌈⍴⍵)⍴⍵}                            ⍝ stretched to expose nulls.
  isor←{1 ⍬≡(≡⍵)(⍴⍵)}                        ⍝ is ⎕or of object?
  glue←{$[0=⍴⍵;⍵;↑⍺{⍺,⍶,⍵}/⍵]}               ⍝ ⍵ interspersed with ⍺s.

  aligned←{                                  ⍝ Alignment and centring.
    Rows cols←sepr⍴¨⍵                        ⍝ subarray dimensions.
    sizes←(⌈/Rows)∘.,⌈⌿cols                  ⍝ aligned subarray sizes.
    $[ctd=0;sizes↑¨⍵;                        ⍝ top-left alignment.
      v h←sepr⌈0.5×↑(⍴¨⍵)-sizes              ⍝ vertical and horizontal rotation.
      v⊖¨h⌽¨sizes↑¨⍵                         ⍝ centred aligned subarrays.
     ]
  }

  gaps←{                                     ⍝ Gap-separated sub-planes.
    $[⍺≤2;⍵;                                 ⍝ lowish rank: done.
      subs←(⍺-1)∇¨⍵                          ⍝ sub-hyperplanes.
      width←⊃⌽⍴⊃subs                         ⍝ width of inter-plane gap.
      fill←(⍺ width-3 0)⍴' '                 ⍝ inter-plane gap.
      ↑{⍺⍪fill⍪⍵}/1 open subs                ⍝ gap-separated planes.
     ]
  }

  outer←{                                    ⍝ Outer decoration.
    sizes←1 0{⊃↓(⍉⍣⍺)⍵}¨sepr⍴¨⍵              ⍝ row and col sizes of subarrays.
    sides←sizes/¨¨'│─'                       ⍝ vert and horiz cell sides.
    bords←dec↓¨'├┬'glue¨sides                ⍝ joined up outer borders.
    ↑,¨/('┌' '')⍺ bords'└┐'                  ⍝ vertical and horizontal borders.
  }

  right←{                                    ⍝ Border right each subarray.
    types←2⊥¨(⍳⍴⍵)=⊂¯1+⍴⍵                    ⍝ right border lower corner types.
    chars←'┼┤┴┘'[types]                      ⍝    ..     ..      ..      chars.
    rgt←{⍵,(-≢⍵)↑(≢⍵)1 1/'│',⍺}             ⍝ form right border.
    ((matr 1 open ⍺),¨chars)rgt¨⍵           ⍝ cells bordered right.
  }

  lower←{                                    ⍝ Border below each subarray.
    split←{((¯2+1⊃⍴⍵)/'─')glue ⍺}            ⍝ decorators split with horiz line.
    bot←{⍵⍪(-1⊃⍴⍵)↑⍺ split ⍵}                ⍝ lower border.
    (matr↑,¨/⍺)bot¨matr ⍵                    ⍝ cells bordered below.
  }

  collect←{                                  ⍝ Collected subarrays.
    lft top tt vv hh←⍺                       ⍝ array and subarray decorations.
    cells←vv right 1 open tt hh lower ⍵      ⍝ cells boxed right and below.
    boxes←(dec∨0∊⍴⍵)open cells               ⍝ opened to avoid ,/⍬ problem.
    lft,top⍪↑⍪⌿,/boxes                       ⍝ completed collection.
  }

  plane←{⍺+⍵} ⍝ placeholder

  join←{                                     ⍝ Join of gap-separated sub-planes.
    sep←(≢⍵)÷1⌈≢⍺                            ⍝ sub plane separation.
    split←(0=sep|⍳≢⍵)⊂[0]⍵                   ⍝ separation along first axis.
    (⊂⍤¯1⊢⍺)plane¨split                      ⍝ sub-plane join.
  }

  shape←{                                    ⍝ Row and column shape decorators.
    $[dec≤0=⍴⍴⍵;⍺/¨'│─';                     ⍝ no decoration or scalar.
      cols←(×¯1↑⍴⍵)⊃'⊖→'                     ⍝ zero or more cols.
      rsig←(××/¯1↓⍴⍵)⊃'⌽↓'                   ⍝ zero or more rows.
      Rows←(¯1+3⌊⍴⍴⍵)⊃'│'rsig'⍒'             ⍝ high rank decorator overrides.
      Rows cols                              ⍝ shape decorators.
     ]
  }

  type←{                                     ⍝ Type decoration char.
    $[dec<|≡⍵;'─';                           ⍝ nested: '─'
      isor ⍵;'∇';                            ⍝ ⎕or:    '∇'
      sst←{                                  ⍝ simple scalar type.
        $[0=dec×⍴⍴⍵;'─';                     ⍝ undecorated or scalar ⍕⍵: char,
          (⊃⍵∊'¯',⎕D)⊃'#~'                   ⍝ otherwise, number or space ref.
         ]
      }∘⍕                                    ⍝ ⍕ distinguishes type of scalar.
      $[0=≡⍵;sst ⍵;                          ⍝ simple scalar: type.
        {(1=⍴⍵)⊃'+'⍵}∪,sst¨dec open ⍵        ⍝ array: mixed or uniform type.
     ]]
  }

  inner←{                                    ⍝ Inner subarray decorations.
    deco←{(type ⍵),1 shape ⍵}                ⍝ type and shape decorators.
    sepr deco¨matr dec open ⍵                ⍝ decorators: tt vv hh .
  }

  plane←{                                    ⍝ Boxed rank-2 plane.
    $[2<⍴⍴⍺;⍺ join ⍵;                        ⍝ gap-separated sub-planes.
      odec←(dec shape ⍺)outer ⍵              ⍝ outer type and shape decoration.
      idec←inner ⍺                           ⍝ inner type and shape decorations.
      (odec,idec)collect ⍵                   ⍝ collected, formatted subarrays.
     ]
  }

  box←{                                      ⍝ Recursive boxing of nested array.
    $[isor ⍵;⎕FMT⊂⍵;                         ⍝ ⎕or: '∇name'.
      1=≡,⍵;dec open ⎕FMT dec open ⍵;        ⍝ simple array: format.
      mat←matr 1/dec open ⍵                  ⍝ matrix of opened subarrays.
      r c←×⍴mat                              ⍝ non-null rows/cols.
      $[dec<0∊r c;c/r⌿∇ 1 open mat;          ⍝ undecorated null: empty result.
        subs←aligned ∇¨mat                   ⍝ aligned boxed subarrays.
        (≢⍴⍵)gaps ⍵ plane subs               ⍝ collection into single plane.
     ]]
  }

  $[isor ⍵;⎕FMT⊂⍵;                           ⍝ simple ⎕OR: done.
    1=≡,⍵;⎕FMT ⍵;                            ⍝ simple array: done.
    box ⍵                                    ⍝ recursive boxing of array.
   ]
}

⍝ From http://dfns.dyalog.com/c_display.htm

display ← { ⎕IO←0                            ⍝ Boxed display of array.
  box←{                                      ⍝ box with type and axes
    vrt hrz←(¯1+⍴⍵)⍴¨'│─'                    ⍝ vert. and horiz. lines
    top←'─⊖→'[¯1↑⍺],hrz                      ⍝ upper border with axis
    bot←(⊃⍺),hrz                             ⍝ lower border with type
    rgt←'┐│',vrt,'┘'                         ⍝ right side with corners
    lax←'│⌽↓'[¯1↓1↓⍺],¨⊂vrt                  ⍝ left side(s) with axes,
    lft←⍉'┌',(↑lax),'└'                      ⍝ ... and corners
    lft,(top⍪⍵⍪bot),rgt                      ⍝ fully boxed array
  }

  open←{(1⌈⍴⍵)⍴⍵}                            ⍝ exposure of null axes
  trim←{(~1 1⍷∧⌿⍵=' ')/⍵}                    ⍝ removal of extra blank cols
  char←{$[⍬≡⍴⍵;'─';(⊃⍵∊'¯',⎕D)⊃'#~']}∘⍕      ⍝ simple scalar type
  type←{{(1=⍴⍵)⊃'+'⍵}∪,char¨⍵}               ⍝ simple array type
  axes←{(-2⌈⍴⍴⍵)↑1+×⍴⍵}                      ⍝ array axis types
  deco←{⍺←type open ⍵ ⋄ ⍺,axes ⍵}            ⍝ type and axes vector
  line←{(49=⎕DT 1⍴⍵)⊃' -'}                   ⍝ underline for atom

  {                                          ⍝ recursive boxing of arrays:
    $[0=≡⍵;' '⍪(open ⎕FMT ⍵)⍪line ⍵;         ⍝ simple scalar
      1=≡⍵;(deco ⍵)box open ⎕FMT open ⍵;     ⍝ simple array
      ('∊'deco ⍵)box trim ⎕FMT ∇¨open ⍵      ⍝ nested array
     ]
  }⍵
}

⍝ From http://dfns.dyalog.com/c_displays.htm

displays ← { ⎕IO←0                           ⍝ Boxed display of array.
  open←{(1⌈⍴⍵)⍴⍵}                            ⍝ Expose null axes.
  box←{                                      ⍝ Box with type and axes.
    shp w←open\⍵
    vrt hrz←(¯1+⍴w)⍴¨'│─'                    ⍝ Vert. and horiz. lines.
    top←('─⊖→')[¯1↑⍺],hrz                    ⍝ Upper border with axis.
    ok←(⍴shp)<⍴hrz
    top←(⍴top)↑(2↑top),(ok/shp),(2+ok×⍴shp)↓top
    bot←(⊃⍺),hrz                             ⍝ Lower border with type.
    rgt←'┐│',vrt,'┘'                         ⍝ Right side with corners.
    lax←('│⌽↓')[¯1↓1↓⍺],¨⊂vrt                ⍝ Left side(s) with axes,
    lft←⍉'┌',(↑lax),'└'                      ⍝ ... and corners.
    lft,(top⍪w⍪bot),rgt                      ⍝ Fully boxed array.
  }

  trim←{(0⊃⍵)((~1 1⍷∧⌿(1⊃⍵)=' ')/(1⊃⍵))}     ⍝ Remove extra blank cols.
  char←{$[⍬≡⍴⍵;'─';(⊃⍵∊'¯',⎕D)⊃'#~']}∘⍕      ⍝ Simple scalar type.
  type←{{(1=⍴⍵)⊃'+'⍵}∪,char¨⍵}               ⍝ Simple array type.
  axes←{(-2⌈⍴⍴⍵)↑1+×⍴⍵}                      ⍝ Array axis types.
  deco←{⍺←type open ⍵ ⋄ ⍺,axes ⍵}            ⍝ Type and axes vector.
  qfmt←{(⍕⍴⍺)(⎕FMT open ⍵)}
  {                                          ⍝ Recursively box arrays:
    $[0=≡⍵;' '⍪(⎕FMT ⍵)⍪(' '≡⊃0⍴⍵)⊃' -';     ⍝ Simple scalar.
      1 ⍬≡(≡⍵)(⍴⍵);'∇' 0 0 box(,'─')(⎕FMT ⍵);⍝ Object rep: ⎕OR.
      1=≡⍵;(deco ⍵)box open ⍵ qfmt ⍵;        ⍝ Simple array.
      ('∊'deco ⍵)box trim ⍵ qfmt ∇¨open ⍵    ⍝ Nested array.
     ]
  }⍵
}

⍝ From http://dfns.dyalog.com/c_displayr.htm

displayr ← { ⎕IO←0                           ⍝ Boxed display of array
  box←{                                      ⍝ box with type and axes
    vrt hrz←(¯1+⍴⍵)⍴¨'│─'                    ⍝ vert. and horiz. lines
    top←(1+⍴hrz)↑(⊃(¯1↑⍺)⌷'─⊖',⊂⍕¯1↑1⊃⍺),hrz ⍝ upper border with axis
    bot←(⍴top)↑(⊃2↓⍺),hrz                    ⍝ lower border with type
    rgt←'┐│',vrt,'┘'                         ⍝ right side with corners
    lax←(⊃¨(¯1↓3↓⍺)⌷¨(-1⌈¯1+⍴1⊃⍺)↑(⊂'│⌽'),¨⊂∘⍕¨¯1↓0,1⊃⍺),¨⊂vrt     ⍝ left side(s) with axes,
    lax←(⊂1+⍴vrt)↑¨(lax~¨⊂' '),¨'│'          ⍝ pad and trim
    lft←⍉'┌',(↑lax),'└'                      ⍝ ... and corners
    lft,(top⍪⍵⍪bot),rgt                      ⍝ fully boxed array
  }

  open←{(1⌈⍴⍵)⍴⍵}                            ⍝ exposed null axes
  trim←{(~1 1⍷∧⌿⍵=' ')/⍵}                    ⍝ removal of extra blank cols
  char←{$[⍬≡⍴⍵;'─';(⊃⍵∊'¯',⎕D)⊃'#~']}∘⍕      ⍝ simple scalar type
  type←{{(1=⍴⍵)⊃'+'⍵}∪,char¨⍵}               ⍝ simple array type
  axes←{(-2⌈⍴⍴⍵)↑1+×⍴⍵}                      ⍝ array axis types
  deco←{⍺←type open ⍵ ⋄ (⍴⍴⍵),(⊂⍴⍵),⍺,axes ⍵}⍝ type and axes vector

  {                                          ⍝ recursively boxed arrays:
    $[0=≡⍵;' '⍪(open ⎕FMT ⍵)⍪(' '=⊃0⍴⍵)⊃' -';⍝ simple scalar
      1 ⍬≡(≡⍵)(⍴⍵);''(0 0)'∇' 0 0 box ⎕FMT ⍵;⍝ object rep: ⎕OR
      1=≡⍵;(deco ⍵)box open' ',⎕FMT open ⍵;  ⍝ simple array
      ((⊂⍕≡⍵)deco ⍵)box trim' ',⎕FMT ∇¨open ⍵⍝ nested array
     ]
  }⍵
}

⍝ From http://dfns.dyalog.com/c_dist.htm

dist ← {                                     ⍝ Levenshtein distance.
  a←(n+1)⍴(⍴⍺)+n←⍴⍵                          ⍝ first row of matrix
  f←⍵{⌊\⍵⌊(⊃⍵),(¯1↓⍵)-1+⍺=⍶}                 ⍝ iteration step
  z←⊃f/(⌽⍺),⊂a                               ⍝ last row of matrix
  ⊃⌽z
}

⍝ From http://dfns.dyalog.com/s_dist.htm

lcase ← {                                    ⍝ Lower-casification,
  lc←'abcdefghijklmnopqrstuvwxyzåäöàæéñøü'   ⍝ (lower case alphabet)
  uc←'ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖÀÆÉÑØÜ'   ⍝ (upper case alphabet)
  (⍴⍵)⍴(lc,,⍵)[(uc,,⍵)⍳⍵]                    ⍝ ... of simple array.
}

fuzzy ← {({⍵⍳⌊/⍵}(lcase ⍺)∘dist∘lcase¨⍵)⊃⍵}

⍝ From http://dfns.dyalog.com/n_dsp.htm

dsp ← { ⎕IO←1                                ⍝ Reduced version of disp.
  $[(1=≡,⍵)∨0∊⍴⍵;⎕FMT ⍵;                     ⍝ simple or empty: char matrix
    ⍺←1 ⋄ top←'─'∘⍪⍣⍺                        ⍝ top '─' bar if ⍺
    $[1≥⍴⍴⍵;{                                ⍝ vector or scalar:
        bars←2{⍪(⌊/≢¨⍺ ⍵)/'│'}/⍵,0           ⍝ inter-item '│' separators
        join←{↑,/(⌈/≢¨⍵)↑¨⍵}                 ⍝ equalised rows cell-join
        0 ¯1↓join top¨join¨↓⍉↑⍵ bars         ⍝ join with top and separators
      }1 ∇¨⍵;                                ⍝ vector: formatted items
      subs←⍺ ∇¨⍵                             ⍝ higher rank: formatted items
      rs cs←+/¨1⊂↑⍴¨subs                     ⍝ numbers of rows and cols
      dims←(mrs←⌈/rs)∘.,mcs←⌈/⍪⍉cs           ⍝ max dimensions per row & col
      join←{↑⍺{⍺,⍶,⍵}/⍵}                     ⍝ ⍺-join of vector ⍵
      Rows←(mrs/¨'│')join¨↓dims↑¨subs        ⍝ complete rows with '│'-separated items
      hzs←'┼'join mcs/¨'─'                   ⍝ inter-row horizontal '─┼─' separators
      cells←{⍺⍪hzs⍪⍵}/Rows                   ⍝ joined rows: array of 2D planes
      gaps←(⌽⍳¯2+⍴⍴⍵)/¨' '                   ⍝ increasing cell gaps for higher ranks
      cjoin←{↑⍪/(⊂⍺),⍶,⊂⍵}                   ⍝ vertical cell join with ⍺⍺ gap
      top⊃↑{⍺ cjoin⌿⍵}/gaps,⊂cells           ⍝ cell-joining with increasing gaps
   ]]
}

⍝ From http://dfns.dyalog.com/s_dsp.htm

Tape ← { '∘',(⍺↑⍵),{⍺ ⍵}/⍺↓⍵,'∘' }           ⍝ ⍺-window tape
Rgt ← { (⊂2↑⍵),(2↓¯1↓⍵),⊃⌽⍵ }                ⍝ tape-head moves right 1 item

⍝ From http://dfns.dyalog.com/c_enlist.htm

enlist ← {                                   ⍝ List ⍺-leaves of nested array.
  $[⍺≥¯1+|≡⍵;,⍵;                             ⍝ all shallow leaves: finished.
    1↓↑,/(⊂⊂⊃⊃⍵),⍺ ∇¨,⍵                      ⍝ otherwise: concatenate sublists.
   ]
}

⍝ From http://dfns.dyalog.com/n_foldl.htm

foldl ← { ↑⍺⍺⍨/(⌽⍵),⊂⍺ }                     ⍝ Fold (reduce) from the left.

⍝ From http://dfns.dyalog.com/c_from.htm

from ← {                                     ⍝ Select (1↓⍴⍵)-cells from array ⍵.
  ⍝ ~(≢⍺)≡≢⍴⍵:⎕SIGNAL 4                      ⍝ check index length.
  indx←⍺-⎕IO ⋄ ⎕IO←0                         ⍝ easier in origin 0.
  axes←+\0,¯1↓{⊃⍴⍴⍵}¨⍺                       ⍝ cumulative selection axes.
  testPrint axes
  ↑{                                         ⍝ selection using simple index.
    indx axis←⍺                              ⍝ index and axis for selection.
    $[indx≡,⊂⍬;⍵;                            ⍝ skip: select all items.
      vec←⊂[(⍳⍴⍴⍵)~axis]⍵                    ⍝ vector along given axis.
      sel←↑indx⊃¨⊂vec                        ⍝ selection from vector.
      pos←axis+⍳⍴⍴indx                       ⍝ selection axis positions.
      (pos,(⍳⍴⍴sel)~pos)⍉sel                 ⍝ simple selection.
     ]
  }/⌽(⊂⍵),↓⍉↑indx axes                       ⍝ select along leading axes.
}

⍝ From http://dfns.dyalog.com/c_in.htm

in ← {                                       ⍝ Locations of item ⍺ in array ⍵.
  D←|≡item←⍺                                 ⍝ (depth of) sought item
  ⍬{                                         ⍝ ⍺ is pick-path
    $[item≡⍵;,⊂⍺;                            ⍝ match: path
      D≥|≡⍵;⍬;                               ⍝ give up
      paths←⍺∘,∘⊂¨⍳⍴⍵                        ⍝ extended paths
      ⊃,/,paths ∇¨⍵                          ⍝ paths in subarrays of ⍵
     ]
  }⍵                                         ⍝ ⍵ is searched-in array
}

⍝ From http://dfns.dyalog.com/c_list.htm

list ← { ↑{⍺ ⍵}/⍵,'∘' }                      ⍝ List from vector ⍵, with '∘' as null.

⍝ From http://dfns.dyalog.com/c_ltrav.htm

ltrav ← {                                    ⍝ List traversal.
  $['∘'≡head tail←⍵;⍺;                       ⍝ head and tail of list, else accumulator
    (⍺ ⍺⍺ head)∇ tail                        ⍝ accumulated result with tail.
   ]
}

⍝ From http://dfns.dyalog.com/s_list.htm

listLength ← 0∘({⍺+1} ltrav)

vectFromList ← ⍬∘({⍺,⊂⍵} ltrav)

revl ← '∘'∘({⍺ ⍵}⍨ ltrav)

listRmDups ← {                               ⍝ remove adjacent duplicates.
  ⍺←'∘'                                      ⍝ null accumulator.
  (a(b tail))←⍵                              ⍝ first two items.
  $[b≡'∘'; revl a ⍺;                         ⍝ b null: list expired.
    a≡b;⍺ ∇ b tail;                          ⍝ two items match: drop first one.
    a ⍺ ∇ b tail                             ⍝ accumulate first, continue.
   ]
}

⍝ From http://dfns.dyalog.com/n_nlines.htm

nlines ← {                                   ⍝ Number of display lines for simple array.
  {                       
    (×/⍵)+{                                  ⍝ # of lines of real data +
      +/+\0⌈⍵-1,¯1↓⍵                         ⍝ # of blank lines separating different planes
    }×\¯1↓⍵                                  ⍝ of the array
  }¯1↓⍴⍵                                     ⍝ last axis affects only width of display.
}

⍝ From http://dfns.dyalog.com/s_perv.htm

perv ← {                                     ⍝ Scalar pervasion
  ⍝ ⍺←⊢                                      ⍝ (⍺ and) ⍵ depth 0: operand fn application
  $[1=≡⍺ ⍵ ⍵;⍺ ⍺⍺ ⍵;⍺ ∇¨⍵]                   ⍝ (⍺ or) ⍵ deeper: recursive traversal.
}

⍝ From http://dfns.dyalog.com/c_pmat.htm

pmat ← {                                     ⍝ Permutation matrix of ⍳⍵.
  {$[1≥⍴⍵;↑,↓⍵;↑⍪/⍵,∘∇¨⍵∘~¨⍵]}⍳⍵             ⍝ short vector: done, else items prefix sub-perms of remainder.
}                                            ⍝ permutations of identity perm.

⍝ From http://dfns.dyalog.com/c_pred.htm

pred ← { ↑⍺⍺/¨(⍺/⍳⍴⍺)⊆⍵ }                    ⍝ Partitioned reduction.

⍝ From http://dfns.dyalog.com/c_rows.htm

rows ← {                                     ⍝ Operand function applied to argument rows.
  $[1<|≡⍵;∇¨⍵;                               ⍝ nested: item-wise application
    ⍺⍺⍤1⊢⍵                                   ⍝ simple: vector-wise application
   ]
}

⍝ From http://dfns.dyalog.com/c_mscan.htm

mscan ← {                                    ⍝ Minus scan.
  ⍺←⍬⍴⌽⍳⍴⍴⍵                                  ⍝ ⍺ is axis (default last).
  +\[⍺]⍵×[⍺](⍺⊃⍴⍵)⍴1,-1
}

⍝ From http://dfns.dyalog.com/c_dscan.htm

dscan ← {                                    ⍝ Divide scan
  ⍺←⍬⍴⌽⍳⍴⍴⍵                                  ⍝ ⍺ is axis (default last).
  ×\[⍺]⍵*[⍺](⍺⊃⍴⍵)⍴1,-1
}

⍝ From http://dfns.dyalog.com/c_ascan.htm

ascan ← {                                    ⍝ Associative scan.
  $[2>0⊥⍴⍵;⍵;                                ⍝ few items: done.
    ⌽↑⍺⍺{(⊂(⊃⍵)⍺⍺ ⍺),⍵}/⌽(⊂∘⊃¨↓⍵),↑1↓¨↓⍵
   ]
}

⍝ From http://dfns.dyalog.com/c_ascana.htm

ascana ← {                                   ⍝ Higher rank asscociative scan.
  ⍺←⍬⍴⌽⍳⍴⍴⍵                                  ⍝ default last axis.
  ↑[⍺-0.1](⍺⍺ ascan)¨↓[⍺]⍵
}

⍝ From http://dfns.dyalog.com/c_select.htm

select ← { ⍺⊃¨↑,¨/⊂¨¨⍵ }                     ⍝ ⍺-selection of items of vector ⍵.

⍝ From http://dfns.dyalog.com/c_shannon.htm

shannon ← { -+/(2∘⍟×⊣)¨({≢⍵}⌸÷≢)⍵ }          ⍝ Shannon entropy of message ⍵.

⍝ From http://dfns.dyalog.com/c_subvec.htm

subvec ← {                                   ⍝ Is ⍺ a subvector of ⍵?
  $[0∊⍴⍺;1;0∊⍴⍵;0;(1↓⍺)∇(⍵⍳1↑⍺)↓⍵]           ⍝ null ⍺: success, null ⍵: failure.
}                                            ⍝ otherwise, check remaining items.
