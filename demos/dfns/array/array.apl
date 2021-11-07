⍝ Ported from Dyalog's dfns at http://dfns.dyalog.com/n_contents.htm into April APL


⍝⍝ Array processing

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

  box←{                                      ⍝ Recursive boxing of nested array.
    isor ⍵:⎕FMT⊂⍵                            ⍝ ⎕or: '∇name'.
    1=≡,⍵:dec open ⎕FMT dec open ⍵           ⍝ simple array: format.
    mat←matr 1/dec open ⍵                    ⍝ matrix of opened subarrays.
    r c←×⍴mat                                ⍝ non-null rows/cols.
    dec<0∊r c:c/r⌿∇ 1 open mat               ⍝ undecorated null: empty result.
    subs←aligned ∇¨mat                       ⍝ aligned boxed subarrays.
    (≢⍴⍵)gaps ⍵ plane subs                   ⍝ collection into single plane.
  }

  aligned←{                                  ⍝ Alignment and centring.
    rows cols←sepr⍴¨⍵                        ⍝ subarray dimensions.
    sizes←(⌈/rows)∘.,⌈⌿cols                  ⍝ aligned subarray sizes.
    ctd=0:sizes↑¨⍵                           ⍝ top-left alignment.
    v h←sepr⌈0.5×↑(⍴¨⍵)-sizes                ⍝ vertical and horizontal rotation.
    v⊖¨h⌽¨sizes↑¨⍵                           ⍝ centred aligned subarrays.
  }

  gaps←{                                     ⍝ Gap-separated sub-planes.
    ⍺≤2:⍵                                    ⍝ lowish rank: done.
    subs←(⍺-1)∇¨⍵                            ⍝ sub-hyperplanes.
    width←⊃⌽⍴⊃subs                           ⍝ width of inter-plane gap.
    fill←(⍺ width-3 0)⍴' '                   ⍝ inter-plane gap.
    ↑{⍺⍪fill⍪⍵}/1 open subs                  ⍝ gap-separated planes.
  }

  plane←{                                    ⍝ Boxed rank-2 plane.
    2<⍴⍴⍺:⍺ join ⍵                           ⍝ gap-separated sub-planes.
    odec←(dec shape ⍺)outer ⍵                ⍝ outer type and shape decoration.
    idec←inner ⍺                             ⍝ inner type and shape decorations.
    (odec,idec)collect ⍵                     ⍝ collected, formatted subarrays.
  }

  join←{                                     ⍝ Join of gap-separated sub-planes.
    sep←(≢⍵)÷1⌈≢⍺                            ⍝ sub plane separation.
    split←(0=sep|⍳≢⍵)⊂[0]⍵                   ⍝ separation along first axis.
    (⊂⍤¯1⊢⍺)plane¨split                      ⍝ sub-plane join.
  }

  outer←{                                    ⍝ Outer decoration.
    sizes←1 0{⊃↓(⍉⍣⍺)⍵}¨sepr⍴¨⍵              ⍝ row and col sizes of subarrays.
    sides←sizes/¨¨'│─'                       ⍝ vert and horiz cell sides.
    bords←dec↓¨'├┬'glue¨sides                ⍝ joined up outer borders.
    ↑,¨/('┌' '')⍺ bords'└┐'                  ⍝ vertical and horizontal borders.
  }

  inner←{                                    ⍝ Inner subarray decorations.
    deco←{(type ⍵),1 shape ⍵}                ⍝ type and shape decorators.
    sepr deco¨matr dec open ⍵                ⍝ decorators: tt vv hh .
  }

  collect←{                                  ⍝ Collected subarrays.
    lft top tt vv hh←⍺                       ⍝ array and subarray decorations.
    cells←vv right 1 open tt hh lower ⍵      ⍝ cells boxed right and below.
    boxes←(dec∨0∊⍴⍵)open cells               ⍝ opened to avoid ,/⍬ problem.
    lft,top⍪↑⍪⌿,/boxes                       ⍝ completed collection.
  }

  right←{                                    ⍝ Border right each subarray.
    types←2⊥¨(⍳⍴⍵)=⊂¯1+⍴⍵                    ⍝ right border lower corner types.
    chars←'┼┤┴┘'[types]                      ⍝    ..     ..      ..      chars.
    rgt←{⍵,(-≢⍵)↑(≢⍵)1 1/'│',⍺}              ⍝ form right border.
    ((matr 1 open ⍺),¨chars)rgt¨⍵            ⍝ cells bordered right.
  }

  lower←{                                    ⍝ Border below each subarray.
    split←{((¯2+1⊃⍴⍵)/'─')glue ⍺}            ⍝ decorators split with horiz line.
    bot←{⍵⍪(-1⊃⍴⍵)↑⍺ split ⍵}                ⍝ lower border.
    (matr↑,¨/⍺)bot¨matr ⍵                    ⍝ cells bordered below.
  }

  type←{                                     ⍝ Type decoration char.
    dec<|≡⍵:'─'                              ⍝ nested: '─'
    isor ⍵:'∇'                               ⍝ ⎕or:    '∇'
    sst←{                                    ⍝ simple scalar type.
      0=dec×⍴⍴⍵:'─'                          ⍝ undecorated or scalar ⍕⍵: char,
      (⊃⍵∊'¯',⎕D)⊃'#~'                       ⍝ otherwise, number or space ref.
    }∘⍕                                      ⍝ ⍕ distinguishes type of scalar.
    0=≡⍵:sst ⍵                               ⍝ simple scalar: type.
    {(1=⍴⍵)⊃'+'⍵}∪,sst¨dec open ⍵            ⍝ array: mixed or uniform type.
  }

  shape←{                                    ⍝ Row and column shape decorators.
    dec≤0=⍴⍴⍵:⍺/¨'│─'                        ⍝ no decoration or scalar.
    cols←(×¯1↑⍴⍵)⊃'⊖→'                       ⍝ zero or more cols.
    rsig←(××/¯1↓⍴⍵)⊃'⌽↓'                     ⍝ zero or more rows.
    rows←(¯1+3⌊⍴⍴⍵)⊃'│'rsig'⍒'               ⍝ high rank decorator overrides.
    rows cols                                ⍝ shape decorators.
  }

  matr←{↑,↓⍵}                                ⍝ matrix from non-scalar array.
  sepr←{+/¨1⊂↑⍵}                             ⍝ vec-of-mats from mat-of-vecs.
  ⍝ open←{16::(1⌈⍴⍵)⍴⊂'[ref]' ⋄ (⍺⌈⍴⍵)⍴⍵}    ⍝ stretched to expose nulls.
  open←{(⍺⌈⍴⍵)⍴⍵}                            ⍝ stretched to expose nulls.
  isor←{1 ⍬≡(≡⍵)(⍴⍵)}                        ⍝ is ⎕or of object?
  glue←{0=⍴⍵ : ⍵ ⋄ ↑⍺{⍺,⍶,⍵}/⍵}              ⍝ ⍵ interspersed with ⍺s.

  isor ⍵:⎕FMT⊂⍵                              ⍝ simple ⎕OR: done.
  1=≡,⍵:⎕FMT ⍵                               ⍝ simple array: done.
  box ⍵                                      ⍝ recursive boxing of array.
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

  deco←{⍺←type open ⍵ ⋄ ⍺,axes ⍵}            ⍝ type and axes vector
  axes←{(-2⌈⍴⍴⍵)↑1+×⍴⍵}                      ⍝ array axis types
  open←{(1⌈⍴⍵)⍴⍵}                            ⍝ exposure of null axes
  trim←{(~1 1⍷∧⌿⍵=' ')/⍵}                    ⍝ removal of extra blank cols
  char←{⍬≡⍴⍵:'─' ⋄ (⊃⍵∊'¯',⎕D)⊃'#~'}∘⍕       ⍝ simple scalar type
  type←{{(1=⍴⍵)⊃'+'⍵}∪,char¨⍵}               ⍝ simple array type
  line←{(49=⎕DT 1⍴⍵)⊃' -'}                   ⍝ underline for atom

  {                                          ⍝ recursive boxing of arrays:
    0=≡⍵:' '⍪(open ⎕FMT ⍵)⍪line ⍵            ⍝ simple scalar
    1 ⍬≡(≡⍵)(⍴⍵):'∇' 0 0 box ⎕FMT ⍵          ⍝ object rep: ⎕OR
    1=≡⍵:(deco ⍵)box open ⎕FMT open ⍵        ⍝ simple array
    ('∊'deco ⍵)box trim ⎕FMT ∇¨open ⍵        ⍝ nested array
  }⍵
}

⍝ From http://dfns.dyalog.com/c_displays.htm

displays ← { ⎕IO←0                           ⍝ Boxed display of array.
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

  deco←{⍺←type open ⍵ ⋄ ⍺,axes ⍵}            ⍝ Type and axes vector.
  axes←{(-2⌈⍴⍴⍵)↑1+×⍴⍵}                      ⍝ Array axis types.
  open←{(1⌈⍴⍵)⍴⍵}                            ⍝ Expose null axes.
  trim←{(0⊃⍵)((~1 1⍷∧⌿(1⊃⍵)=' ')/(1⊃⍵))}     ⍝ Remove extra blank cols.
  char←{⍬≡⍴⍵:'─' ⋄ (⊃⍵∊'¯',⎕D)⊃'#~'}∘⍕       ⍝ Simple scalar type.
  type←{{(1=⍴⍵)⊃'+'⍵}∪,char¨⍵}               ⍝ Simple array type.
  qfmt←{(⍕⍴⍺)(⎕FMT open ⍵)}

  {                                          ⍝ Recursively box arrays:
    0=≡⍵:' '⍪(⎕FMT ⍵)⍪(' '≡⊃0⍴⍵)⊃' -'        ⍝ Simple scalar.
    1 ⍬≡(≡⍵)(⍴⍵):'∇' 0 0 box(,'─')(⎕FMT ⍵)   ⍝ Object rep: ⎕OR.
    1=≡⍵:(deco ⍵)box open ⍵ qfmt ⍵           ⍝ Simple array.
    ('∊'deco ⍵)box trim ⍵ qfmt ∇¨open ⍵      ⍝ Nested array.
  }⍵
}

⍝ From http://dfns.dyalog.com/c_displayr.htm

displayr ← { ⎕IO←0                           ⍝ Boxed display of array
  box←{                                      ⍝ box with type and axes
    vrt hrz←(¯1+⍴⍵)⍴¨'│─'                    ⍝ vert. and horiz. lines
    top←(1+⍴hrz)↑(⊃(¯1↑⍺)⌷'─⊖',⊂⍕¯1↑1⊃⍺),hrz ⍝ upper border with axis
    bot←(⍴top)↑(⊃2↓⍺),hrz                    ⍝ lower border with type
    rgt←'┐│',vrt,'┘'                         ⍝ right side with corners
    lax←(⊃¨(¯1↓3↓⍺)⌷¨(-1⌈¯1+⍴1⊃⍺)↑(⊂'│⌽'),¨⊂∘⍕¨¯1↓0,1⊃⍺),¨⊂vrt ⍝ left side(s) with axes,
    lax←(⊂1+⍴vrt)↑¨(lax~¨⊂' '),¨'│'          ⍝ pad and trim
    lft←⍉'┌',(↑lax),'└'                      ⍝ ... and corners
    lft,(top⍪⍵⍪bot),rgt                      ⍝ fully boxed array
  }

  deco←{⍺←type open ⍵ ⋄ (⍴⍴⍵),(⊂⍴⍵),⍺,axes ⍵}⍝ type and axes vector
  axes←{(-2⌈⍴⍴⍵)↑1+×⍴⍵}                      ⍝ array axis types
  open←{(1⌈⍴⍵)⍴⍵}                            ⍝ exposed null axes
  trim←{(~1 1⍷∧⌿⍵=' ')/⍵}                    ⍝ removal of extra blank cols
  char←{⍬≡⍴⍵:'─' ⋄ (⊃⍵∊'¯',⎕D)⊃'#~'}∘⍕       ⍝ simple scalar type
  type←{{(1=⍴⍵)⊃'+'⍵}∪,char¨⍵}               ⍝ simple array type

  {                                          ⍝ recursively boxed arrays:
    0=≡⍵:' '⍪(open ⎕FMT ⍵)⍪(' '=⊃0⍴⍵)⊃' -'   ⍝ simple scalar
    1 ⍬≡(≡⍵)(⍴⍵):''(0 0)'∇' 0 0 box ⎕FMT ⍵   ⍝ object rep: ⎕OR
    1=≡⍵:(deco ⍵)box open' ',⎕FMT open ⍵     ⍝ simple array
    ((⊂⍕≡⍵)deco ⍵)box trim' ',⎕FMT ∇¨open ⍵  ⍝ nested array
  }⍵
}

⍝ From http://dfns.dyalog.com/c_dist.htm

dist ← {                                     ⍝ Levenshtein distance.
  a←(n+1)⍴(⍴⍺)+n←⍴⍵                          ⍝ first row of matrix
  f←⍵{⌊\⍵⌊(⊃⍵),(¯1↓⍵)-1+⍺=⍶}                 ⍝ iteration step
  z←⊃f/(⌽⍺),⊂a                               ⍝ last row of matrix
  ⊃⌽z
}

fuzzy ← {({⍵⍳⌊/⍵}(lcase ⍺)∘dist∘lcase¨⍵)⊃⍵}

⍝ From http://dfns.dyalog.com/n_dsp.htm

dsp ← { ⎕IO←1                                ⍝ Reduced version of disp.
  (1=≡,⍵)∨0∊⍴⍵:⎕FMT ⍵                        ⍝ simple or empty: char matrix
  ⍺←1 ⋄ top←'─'∘⍪⍣⍺                          ⍝ top '─' bar if ⍺
  1≥⍴⍴⍵:{                                    ⍝ vector or scalar:
    bars←2{⍪(⌊/≢¨⍺ ⍵)/'│'}/⍵,0               ⍝ inter-item '│' separators
    join←{↑,/(⌈/≢¨⍵)↑¨⍵}                     ⍝ equalised rows cell-join
    0 ¯1↓join top¨join¨↓⍉↑⍵ bars             ⍝ join with top and separators
  }1 ∇¨⍵                                     ⍝ vector: formatted items
  subs←⍺ ∇¨⍵                                 ⍝ higher rank: formatted items
  rs cs←+/¨1⊂↑⍴¨subs                         ⍝ numbers of rows and cols
  dims←(mrs←⌈/rs)∘.,mcs←⌈/⍪⍉cs               ⍝ max dimensions per row & col
  join←{↑⍺{⍺,⍶,⍵}/⍵}                         ⍝ ⍺-join of vector ⍵
  rows←(mrs/¨'│')join¨↓dims↑¨subs            ⍝ complete rows with '│'-separated items
  hzs←'┼'join mcs/¨'─'                       ⍝ inter-row horizontal '─┼─' separators
  cells←{⍺⍪hzs⍪⍵}/rows                       ⍝ joined rows: array of 2D planes
  gaps←(⌽⍳¯2+⍴⍴⍵)/¨' '                       ⍝ increasing cell gaps for higher ranks
  cjoin←{↑⍪/(⊂⍺),⍶,⊂⍵}                       ⍝ vertical cell join with ⍶ gap
  top⊃↑{⍺ cjoin⌿⍵}/gaps,⊂cells               ⍝ cell-joining with increasing gaps
}

⍝ From http://dfns.dyalog.com/s_dsp.htm

Tape ← { '∘',(⍺↑⍵),{⍺ ⍵}/⍺↓⍵,'∘' }           ⍝ ⍺-window tape
Rgt ← { (⊂2↑⍵),(2↓¯1↓⍵),⊃⌽⍵ }                ⍝ tape-head moves right 1 item

⍝ From http://dfns.dyalog.com/c_enlist.htm

enlist ← {                                   ⍝ List ⍺-leaves of nested array.
  ⍺←0                                        ⍝ default: list 0-leaves.
  ⍺≥¯1+|≡⍵:,⍵                                ⍝ all shallow leaves: finished.
  1↓↑,/(⊂⊂⊃⊃⍵),⍺ ∇¨,⍵                        ⍝ otherwise: concatenate sublists.
}

⍝ From http://dfns.dyalog.com/c_from.htm

from ← {                                     ⍝ Select (1↓⍴⍵)-cells from array ⍵.
  ~(≢⍺)≡≢⍴⍵:'error'                          ⍝ check index length.
  indx←⍺-⎕IO ⋄ ⎕IO←0                         ⍝ easier in origin 0.
  axes←+\0,¯1↓{⊃⍴⍴⍵}¨⍺                       ⍝ cumulative selection axes.
  ↑{                                         ⍝ selection using simple index.
    indx axis←⍺                              ⍝ index and axis for selection.
    indx≡,⊂⍬:⍵                               ⍝ skip: select all items.
    vec←⊂[(⍳⍴⍴⍵)~axis]⍵                      ⍝ vector along given axis.
    sel←↑indx⊃¨⊂vec                          ⍝ selection from vector.
    pos←axis+⍳⍴⍴indx                         ⍝ selection axis positions.
    (pos,(⍳⍴⍴sel)~pos)⍉sel                   ⍝ simple selection.
  }/⌽(⊂⍵),↓⍉↑indx axes                       ⍝ select along leading axes.
}

⍝ From http://dfns.dyalog.com/n_foldl.htm

foldl ← { ↑⍺⍺⍨/(⌽⍵),⊂⍺ }                     ⍝ Fold (reduce) from the left.

⍝ From http://dfns.dyalog.com/c_in.htm

in ← {                                       ⍝ Locations of item ⍺ in array ⍵.
  D←|≡item←⍺                                 ⍝ (depth of) sought item
  ⍬{                                         ⍝ ⍺ is pick-path
    item≡⍵:,⊂⍺                               ⍝ match: path
    D≥|≡⍵:⍬                                  ⍝ give up
    paths←⍺∘,∘⊂¨⍳⍴⍵                          ⍝ extended paths
    ⊃,/,paths ∇¨⍵                            ⍝ paths in subarrays of ⍵
  }⍵                                         ⍝ ⍵ is searched-in array
}

⍝ From http://dfns.dyalog.com/c_list.htm

list ← { ↑{⍺ ⍵}/⍵,'∘' }                      ⍝ List from vector ⍵, with '∘' as null.

⍝ From http://dfns.dyalog.com/c_ltrav.htm

ltrav ← {                                    ⍝ List traversal.
  '∘'≡head tail←⍵:⍺                          ⍝ head and tail of list, else accumulator
  (⍺ ⍺⍺ head)∇ tail                          ⍝ accumulated result with tail.
}

⍝ From http://dfns.dyalog.com/s_list.htm

listLength ← 0∘({⍺+1} ltrav)

vectFromList ← ⍬∘({⍺,⊂⍵} ltrav)

revl ← '∘'∘({⍺ ⍵}⍨ ltrav)

listRmDups ← {                               ⍝ remove adjacent duplicates.
  ⍺←'∘'                                      ⍝ null accumulator.
  (a(b tail))←⍵                              ⍝ first two items.
  b≡'∘': revl a ⍺                            ⍝ b null: list expired.
  a≡b:⍺ ∇ b tail                             ⍝ two items match: drop first one.
  a ⍺ ∇ b tail                               ⍝ accumulate first, continue.
}

⍝ From http://dfns.dyalog.com/c_match.htm

match ← {                                    ⍝ Wildcard match.
  p x←{⍵'*'}⍣(1=≡,⍺),⍺                       ⍝ pattern and wildcard.
  v←1↓¨{(x≡¨⍵)⊂⍵}x,p                         ⍝ wildcard-separated segments.
  h←↑v⍷¨⊂⍵                                   ⍝ hits, one row (leading sub-array) per segment.
  r←0,¯1↓,↑⍴¨v                               ⍝ hit-segment rotations.
  sl←{                                       ⍝ array shifted left.
    a←¯1↓⍴⍵                                  ⍝ leading axes.
    x←⌈/⍺                                    ⍝ maximum shift.
    p←⍵,(a,x)⍴0                              ⍝ 0-padded on right.
    s←⍉a⍴⍺                                   ⍝ sub-array of vector rotations.
    (-(0×a),x)↓s⌽p                           ⍝ shifted array.
  }                                          ⍝ :: r ∇ a → a
  m←⌽∨\⌽r sl h                               ⍝ shifted mask 1 .. 1 0 .. 0
  ↑⊃¨{                                       ⍝ filtered hits.
    (lh lm)(und rm)←⍺ ⍵                      ⍝ left and right hits and masks.
    (lh∧rm)lm                                ⍝ right-masked left hits.
  }/↓⍉↑⊂⍤¯1¨h m                              ⍝ hits and masks.
}

⍝ From http://dfns.dyalog.com/s_match.htm

showmatch ← {,[⍳⍴⍴⍵]⍵,[¯0.5+⍴⍴⍵](' ¯'[⎕IO+⍺ match ⍵])}

⍝ From http://dfns.dyalog.com/n_nlines.htm

nlines ← {                                   ⍝ Number of display lines for simple array.
  {                       
    (×/⍵)+{                                  ⍝ # of lines of real data +
      +/+\0⌈⍵-1,¯1↓⍵                         ⍝ # of blank lines separating different planes
    }×\¯1↓⍵                                  ⍝ of the array
  }¯1↓⍴⍵                                     ⍝ last axis affects only width of display.
}

⍝ From http://dfns.dyalog.com/s_perv.htm

perv ← { ⍺←⊢                                 ⍝ Scalar pervasion
  1=≡⍺ ⍵ ⍵:⍺ ⍺⍺ ⍵                            ⍝ (⍺ and) ⍵ depth 0: operand fn application
           ⍺ ∇¨⍵                             ⍝ (⍺ or) ⍵ deeper: recursive traversal.
}

⍝ From http://dfns.dyalog.com/c_pmat.htm

pmat ← {                                     ⍝ Permutation matrix of ⍳⍵.
  {                                          ⍝ perms of ⍳⍵:
    1≥⍴⍵:↑,↓⍵ ⋄ ↑⍪/⍵,∘∇¨⍵∘~¨⍵                ⍝ short vector: done, else items prefix sub-perms of remainder.
  }⍳⍵                                        ⍝ permutations of identity perm.
}

⍝ From http://dfns.dyalog.com/c_pred.htm

pred ← { ↑⍺⍺/¨(⍺/⍳⍴⍺)⊆⍵ }                    ⍝ Partitioned reduction.

⍝ From http://dfns.dyalog.com/c_rows.htm

rows ← {                                     ⍝ Operand function applied to argument rows.
  1<|≡⍵:∇¨⍵                                  ⍝ nested: item-wise application
  ⍺⍺⍤1⊢⍵                                     ⍝ simple: vector-wise application
}

⍝ From http://dfns.dyalog.com/c_sam.htm

sam ← {                                      ⍝ Select and modify.
  ⍺←⊢                                        ⍝ id function for missing ⍺.
  array←⍵                                    ⍝ 'name' array argument.
  (⍺ ⍺⍺ array)←⍵⍵ ⍺ ⍺⍺ array                 ⍝ modify selected part of array.
  array                                      ⍝ return updated value.
}

⍝ From http://dfns.dyalog.com/c_saw.htm

saw ← {                                      ⍝ Function operand applied Simple-Array-Wise.
  ⍺←⊢                                        ⍝ default left arg.
  2≥|≡⍺ ⍵ ⍵:⍺ ⍺⍺ ⍵                           ⍝ Both simple: apply operand.
  1≥|≡⍵:⍺ ∇¨⊂⍵                               ⍝ ⍵ simple: traverse ⍺.
  2≥|≡⍺ 1:⍺∘∇¨⍵                              ⍝ ⍺ simple: traverse ⍵.
  ⍺ ∇¨⍵                                      ⍝ Both nested: traverse both.
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
  2>0⊥⍴⍵:⍵                                   ⍝ few items: done.
  ⌽↑⍺⍺{(⊂(⊃⍵)⍺⍺ ⍺),⍵}/⌽(⊂∘⊃¨↓⍵),↑1↓¨↓⍵
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

subvec ← { ⎕IO←1                             ⍝ Is ⍺ a subvector of ⍵?
  0∊⍴⍺:1                                     ⍝ null ⍺: success.
  0∊⍴⍵:0                                     ⍝ null ⍵: failure.
  (1↓⍺)∇(⍵⍳1↑⍺)↓⍵                            ⍝ otherwise, check remaining items.
}

⍝ From http://dfns.dyalog.com/c_subs.htm

subs ← {                                     ⍝ Vector substitution.
  fs ts←≢¨fm to←⍺                            ⍝ old and new vectors and sizes
  1≡≡⍺:to@(fm∘=)⍵                            ⍝ special case: simple scalar subs
  0=⍴⍴⍵:⊃(⍵≡fm)⌽⍵(⊂to)                       ⍝ special case: scalar argt
  lead←fs↑1                                  ⍝ leading mask
  (fm⍷⍵){                                    ⍝ hits mask
    ~1∊⍺:⍵                                   ⍝ early out if no match
    ts↓↑,/{to,fs↓⍵}¨(lead,⍺)⊂fm,⍵            ⍝ cut and pasted new for old
  }⍤1⊢⍵                                      ⍝ apply to vectors
}


⍝⍝ String processing

⍝ From http://dfns.dyalog.com/c_just.htm

just ← {                                     ⍝ Justify text array.
  ⍺←¯1                                       ⍝ left justify by default.
  ⍺=¯1: ( +/∧\' '= ⍵)            ⌽⍵          ⍝ │left        │
  ⍺= 1: (-+/∧\' '=⌽⍵)            ⌽⍵          ⍝ │       right│
  (⌈0.5×( +/∧\' '= ⍵)-+/∧\' '=⌽⍵)⌽⍵          ⍝ │   centre   │
}

⍝ From http://dfns.dyalog.com/c_lcase.htm

lcase ← {                                    ⍝ Lower-casification,
  lc←'abcdefghijklmnopqrstuvwxyzåäöàæéñøü'   ⍝ (lower case alphabet)
  uc←'ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖÀÆÉÑØÜ'   ⍝ (upper case alphabet)
  (⍴⍵)⍴(lc,,⍵)[(uc,,⍵)⍳⍵]                    ⍝ ... of simple array.
}

⍝ From http://dfns.dyalog.com/c_ucase.htm

ucase ← {                                    ⍝ Upper-casification,
  lc←'abcdefghijklmnopqrstuvwxyzåäöàæéñøü'   ⍝ (lower case alphabet)
  uc←'ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖÀÆÉÑØÜ'   ⍝ (upper case alphabet)
  (⍴⍵)⍴(uc,,⍵)[(lc,,⍵)⍳⍵]                    ⍝ ... of simple array.
}

⍝ From http://dfns.dyalog.com/n_mtrim.htm

mtrim ← { (⌽∨\⌽∨⌿⍵≠' ')/⍵ }                  ⍝ Trim trailing blank cols from simple character matrix.

⍝ From http://dfns.dyalog.com/c_ss.htm

ss ← {                                       ⍝ Approx alternative to xutils' ss.
  srce find repl←,¨⍵                         ⍝ source, find and replace vectors.
  mask←find⍷srce                             ⍝ mask of matching strings.
  prem←(⍴find)↑1                             ⍝ leading pre-mask.
  cvex←(prem,mask)⊂find,srce                 ⍝ partitioned at find points.
  (⍴repl)↓∊{repl,(⍴find)↓⍵}¨cvex             ⍝ collected with replacements.
}

⍝ From http://dfns.dyalog.com/s_ssmat.htm

ssmat ← {                                    ⍝ Matrix search/replace.
  cmat find repl←⍵                           ⍝ Char matrix, find & replace vectors.
  ↑{ss ⍵ find repl}¨↓cmat                    ⍝ Using ss for each row.
}

⍝ From http://dfns.dyalog.com/c_squeeze.htm

squeeze ← { (~'  '⍷⍵)/⍵ }                    ⍝ Compress multiple blanks.

⍝ From http://dfns.dyalog.com/c_timestamp.htm

timestamp ← { ⍺←⎕TS                          ⍝ Time-stamped message.
  join←{↑⍶{⍺,⍶,⍵}/⍵}                         ⍝ join vector ⍵ with ⍺'s.
  fmt←{(-2⌈⍴⍕⍵)↑'0',⍕⍵}¨                     ⍝ format number with leading zeros.
  date←'-'join fmt 3↑⍺                       ⍝ date: YYYY-MM-DD
  time←':'join fmt 3↑3↓⍺                     ⍝ time: HH:MM:SS
  ' 'join date time ⍵                        ⍝ blank-separated date, time and message.
}

⍝ From http://dfns.dyalog.com/c_htx.htm

htx ← {                                      ⍝ Extract html segments.
  1≠≡,⍵:⍺ ∇↑{⍺,' ',⍵}/⍵                      ⍝ accept vectors of vectors.

  xtags←{seg sep cmb vec ⍵}                  ⍝ extract tags, where:
  seg←{(⎕IO=2|⍳⍴⍵)/⍵}                        ⍝ segment with leading tag.
  sep←{((fm⍷⍵)∨to⍷⍵)⊂⍵}                      ⍝ html separated at tags.
  cmb←{(~'  '⍷⍵)/⍵}                          ⍝ compressed multiple blanks.
  vec←{(~⍵∊⎕UCS 8 10 13){⍺\⍺/⍵}⍵}            ⍝ blanks for newlines.

  rlt←{(1++/∧\'>'≠⍵)↓⍵}                      ⍝ remove leading tag.
  att←{⍵,to,'>'}                             ⍝ append trailing tag.

  fm to←'<' '</',¨⊂⍺~'<>'                    ⍝ opening and closing html tags.

  '<'=⊃⍺:att¨xtags,⍵                         ⍝ ··tagged segments.
         rlt¨xtags,⍵                         ⍝ untagged segments.
}


⍝⍝ Line vector processing

⍝ From http://dfns.dyalog.com/c_ltov.htm

ltov ← {                                     ⍝ Lines to nested vector.
  ⍺←⎕UCS 10 13 133                           ⍝ version-proof newlines.
  1↓¨⍺{                                      ⍝ remove leading separators.
    (⍵∊⍺)⊂⍵                                  ⍝ split at separator.
  }¯1⌽⍵,(~(¯1↑⍵)∊⍺)/1↑⍺                      ⍝ ensure trailing separator.
}

⍝ From http://dfns.dyalog.com/c_vtol.htm

vtol ← {                                     ⍝ Nested vector to lines.
  ⍺←⎕UCS 10                                  ⍝ default separator: new-line.
  ↑,/⍵,¨⊂⍺                                   ⍝ ⍺-separated vector items.
}

⍝ From http://dfns.dyalog.com/c_wrap.htm

wrap ← { ⎕IO←0                               ⍝ Wrap word vector at ⍺ cols.
  ⍺←102                                      ⍝ default 102-wrap.
  ⍺≥⍴⍵:⍵                                     ⍝ short enough vector: finished.
  gaps←⍸' '=(⍺+1)↑⍵                          ⍝ indices of blanks.
  take←¯1↑⍺,(⍺≥gaps)/gaps                    ⍝ chars to take.
  drop←take+' '=take⊃⍵                       ⍝ chars to drop.
  head←(take↑⍵),⎕UCS 10                      ⍝ first line and <LF> then,
  head,⍺∇drop↓⍵                              ⍝ wrapped following lines.
}

⍝ From http://dfns.dyalog.com/s_wrap.htm

wrap2 ← { ⎕IO←1                              ⍝ ⍺-wrap (Bob Smith).
  ⍺←102                                      ⍝ default 102-wrap
  v←' ',⍵,' '                                ⍝ blanks required at start and end
  j←(v=' ')/⍳⍴v                              ⍝ indices of blanks
  p←(j+⍺+1)∘.<j                              ⍝ 1s mark blanks past the cutoff
  m←p<1⌽p                                    ⍝ mark last blank that fits on the line
  i←(⍴m)⍴1,(1↓⍴m)⍴0                          ⍝ identity matrix
  c←⌹i-m                                     ⍝ compute transitive closure of m
  v[c[1;]/j]←⎕UCS 10                         ⍝ insert the line breaks
  1↓¯1↓v                                     ⍝ drop the extra blanks
}

⍝ From http://dfns.dyalog.com/s_wrap.htm

wrap3 ← { ⎕IO←1                              ⍝ ⍺-wrap (John Daintree).
  ⍺←102                                      ⍝ initialize
  ⍺≥⍴,⍵:,⊂⍵                                  ⍝ out if short enough
  sze←(⍵∊'-?., ')/⍳⍴⍵                        ⍝ length of each choice
  len←⊃⌽(⍺≥sze)/sze                          ⍝ longest length
  len←⊃(len∊sze)⌽⍺ len                       ⍝ middle of word?
  (⊂len↑⍵),⍺ ∇ len↓⍵                         ⍝ at valid wrap point
}

⍝ From http://dfns.dyalog.com/c_unwrap.htm

unwrap ← { (~⍵∊⎕UCS 10 13 133){⍺\⍺/⍵}⍵ }     ⍝ Replace <LF> with blanks.

⍝ From http://dfns.dyalog.com/c_justify.htm

justify ← { ⎕IO←0                            ⍝ Justify line-vector to width ⍺.
  segs←{¯1+⍵{(⍵,⍴⍺)-¯1,⍵}⍸⍵}                 ⍝ 1-separated segment sizes.
  split←{((⍵|⍺)>⍳⍵)+⌊⍺÷1⌈⍵}                  ⍝ ⍺ split up ⍵ ways.
  lf sp←(⎕UCS 10 32)=⊂⍵                      ⍝ <LF> and <SP> chars in line-vector.
  sizes←segs lf                              ⍝ line sizes.
  ⍺←⌈/sizes                                  ⍝ default width is longest line.
  blanks←segs~(lf∨sp)/sp                     ⍝ original number of blanks per line.
  required←blanks+⍺-sizes                    ⍝ required      ..      ..      ..
  breps←required split¨blanks                ⍝ blank replication vectors.
  last←1⌈¯1+⍴sizes                           ⍝ last line split point.
  brep←∊(last↑breps),×last↓breps             ⍝ blank replication vector.
  ((~sp)+sp\∊brep)\⍵                         ⍝ ⍺-justified line-vector.
}

⍝ From http://dfns.dyalog.com/c_vtrim.htm

vtrim ← { ⎕IO←0                              ⍝ Trim trailing blanks from line-vector.
  lf sp←⎕UCS 10 32                           ⍝ linefeed and space characters.
  1↓¯1↓{                                     ⍝ without linefeeds brackets,
    types←lf sp⍳⍵                            ⍝ 0:lf, 1:sp, 2:ch.
    mask←~1 1⍷types                          ⍝ mask of non-(duplicate blanks).
    comp←mask/types                          ⍝ ignoring duplicate blanks,
    csl←2 1 0⍷comp                           ⍝ sequence: ch, sp, ··· sp, lf.
    lsl←0 1 0⍷comp                           ⍝ sequence: lf, sp, ··· sp, lf.
    from←mask\csl∨lsl                        ⍝ sequence: xx, sp, ··· sp, lf.
    upto←1 0⍷types                           ⍝ sequence: ··········· sp, lf.
    (~¯1⌽≠\from∨upto)/⍵                      ⍝ compressed out: <sp, ··· sp>, lf.
  }lf,⍵,lf                                   ⍝ bracketed with linefeeds chars.
}

⍝ From http://dfns.dyalog.com/c_wrapnote.htm

wrapnote ← {                                 ⍝ Wrap text paragraphs in note vector.
  ⍺←102                                      ⍝ by default: wrap to print width.
  ⍝ 0::'bad note'⎕SIGNAL 11                  ⍝ catchall for unexpected note format.
  split←{{1↓¨(1,1↓⍵∊nls)⊂⍵}' ',⍵}     ⍝ partition at newline.
  join←{¯1↓↑⍺{⍺,⍺⍺,⍵}/⍵,⊂''}          ⍝ collect with ⍺-separator.
  jrgt←{fm=≢⍵}                        ⍝ line justified right?
  jlft←{' '≠⊃⍵}                       ⍝ line justified left?
  pics←{1∊'┌┬┐├┼┤└┴┘│─'∊⍵}            ⍝ line contains box-drawing chars?
  and←{(⍺⍺ ⍵)∧⍵⍵ ⍵}                   ⍝ test combiner.
  test←~∘pics and jlft and jrgt       ⍝ test for flowtext.
  first←{(1+⍵⍳0)⊃⍵}∘(≢¨)              ⍝ length of first non-blank line.
  spill←{⍵∨¯1⌽⍵}                      ⍝ include next item to right.
  list←{squeeze dehyph' 'join ⍵}      ⍝ first enlist, then
  fold←{split to wrap ⍵}              ⍝ re-wrap paragraph.
  dehyph←{(~spill'- '⍷⍵)/⍵}           ⍝ re-join hyphen-separated words.

  nls←⎕UCS 10 133                     ⍝ version-proof newlines.
  nl←⊃nls∩⍵                           ⍝ newline separator.
  to fm←2↑⍺,first 1↓split ⍵~'─'       ⍝ to and from notes width.
  vex←split ⍵                         ⍝ line_vector notes to vec-of-vecs.
  text←0,1↓spill test¨vex             ⍝ mask of lines of flowtext.
  segs←1,1↓¯1⌽0 1⍷text                ⍝ partition at start of flowtext.

  vtrim nl join↑,/(segs⊂text){        ⍝ re-collect lines of notes.
    0=⊃⍺:⍵                          ⍝ no flowtext here: continue.
    flow rest←(1 0=⊂⍺)/¨⊂⍵          ⍝ separate flowtext from the rest.
    vex←fold list flow              ⍝ refold paragraph.
    wrp←to wrap' 'join vex          ⍝ re-re-wrap text.
    new←split to justify wrp        ⍝ justify.
    new,rest                        ⍝ re-collect segment.
  }¨segs⊂vex                          ⍝ for each flowtext segment.
}
