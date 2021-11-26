⍝ Ported from Dyalog's dfns at http://dfns.dyalog.com/n_contents.htm into April APL


⍝⍝ External dependencies

disp    ← 'ARRAY-DEMO-SPACE' ⎕XWF 'disp'
display ← 'ARRAY-DEMO-SPACE' ⎕XWF 'display'
subs    ← 'ARRAY-DEMO-SPACE' ⎕XWF 'subs'


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
  split←{{1↓¨(1,1↓⍵∊nls)⊂⍵}' ',⍵}            ⍝ partition at newline.
  join←{¯1↓↑⍺{⍺,⍶,⍵}/⍵,⊂''}                  ⍝ collect with ⍺-separator.
  jrgt←{fm=≢⍵}                               ⍝ line justified right?
  jlft←{' '≠⊃⍵}                              ⍝ line justified left?
  pics←{1∊'┌┬┐├┼┤└┴┘│─'∊⍵}                   ⍝ line contains box-drawing chars?
  and←{(⍺⍺ ⍵)∧⍵⍵ ⍵}                          ⍝ test combiner.
  test←~∘pics and jlft and jrgt              ⍝ test for flowtext.
  first←{(1+⍵⍳0)⊃⍵}∘(≢¨)                     ⍝ length of first non-blank line.
  spill←{⍵∨¯1⌽⍵}                             ⍝ include next item to right.
  list←{squeeze dehyph' 'join ⍵}             ⍝ first enlist, then
  fold←{split to wrap ⍵}                     ⍝ re-wrap paragraph.
  dehyph←{(~spill'- '⍷⍵)/⍵}                  ⍝ re-join hyphen-separated words.

  nls←⎕UCS 10 13 133                         ⍝ version-proof newlines.
  nl←⊃nls∩⍵                                  ⍝ newline separator.
  to fm←2↑⍺,first 1↓split ⍵~'─'              ⍝ to and from notes width.
  vex←split ⍵                                ⍝ line_vector notes to vec-of-vecs.
  text←0,1↓spill test¨vex                    ⍝ mask of lines of flowtext.
  segs←1,1↓¯1⌽0 1⍷text                       ⍝ partition at start of flowtext.

  vtrim nl join↑,/(segs⊂text){               ⍝ re-collect lines of notes.
    0=⊃⍺:⍵                                   ⍝ no flowtext here: continue.
    flow rest←(1 0=⊂⍺)/¨⊂⍵                   ⍝ separate flowtext from the rest.
    vex←fold list flow                       ⍝ refold paragraph.
    wrp←to wrap' 'join vex                   ⍝ re-re-wrap text.
    new←split to justify wrp                 ⍝ justify.
    new,rest                                 ⍝ re-collect segment.
  }¨segs⊂vex                                 ⍝ for each flowtext segment.
}

⍝ From http://dfns.dyalog.com/c_xtabs.htm

xtabs ← { ⎕IO←0                              ⍝ Expand/compress HT chars.
  ⍺←8                                        ⍝ default: 8-col tab stops.
  ⍺=0:⍵                                      ⍝ ⍺=0: no-op.
  chs←~⍵∊⎕UCS 10 13 133                      ⍝ characters within line.

  ⍺>0:⍺{                                     ⍝ +ive ⍺: expand tabs → blanks.
    tabs nabs←1 0=⊂⍵∊⎕UCS 9                  ⍝ masks of tabs and non-tabs.
    sync←tabs≥chs                            ⍝ sync at tab and end of line.
    segs←¯1+{⍵-¯1,¯1↓⍵}⍸sync                 ⍝ inter-tab segment lengths.
    pads←0⌈⍺-⍺|(sync/tabs)/segs              ⍝ padding lengths.
    (nabs+tabs\pads)/nabs\nabs/⍵             ⍝ padded char vector.
  }⍵

  ⍺<0:(-⍺){                                  ⍝ -ive ⍺: squeeze blanks → tabs.
    bks nks←1 0=⊂⍵=' '                       ⍝ blanks and non-blanks
    runs←{⍵{⍵-⌈\⍵×~⍺}+\⍵}                    ⍝ runs of adjacent 1s.
    tabs←bks∧chs∧0=⍺|runs chs                ⍝ tab positions.
    onoff←{(⍺≠⍵){≠\⍺\2≠/¯1,⍺/⍵}⍺-⍵}          ⍝ on where ⍺=1, off where ⍵=1.
    pretab←⌽(⌽tabs)onoff⌽nks                 ⍝ blanks that precede tabs.
    (pretab≤tabs)/(⎕UCS 9)@{tabs}⍵           ⍝ without tab-preceding blanks.
  }⍵
}

⍝ From http://dfns.dyalog.com/s_xtabs.htm

tabTrip ← {⍵≡⍕⍺ xtabs ⍕(-⍺)xtabs ⍵} ⍝ TODO: should the ⍕ be needed?

tabTrips ← {∧/(0,⍳1+⍴⍵)tabTrip¨⊂⍵}


⍝⍝ Blank removal

⍝ From http://dfns.dyalog.com/c_dlb.htm

dlb ← {                                      ⍝ Drop Leading Blanks.
  ⍺←' ' ⋄ 1<|≡⍵:(⊂⍺)∇¨⍵                      ⍝ nested?
  2<⍴⍴⍵:(¯1↓⍴⍵){(⍺,1↓⍴⍵)⍴⍵}⍺ ∇,[¯1↓⍳⍴⍴⍵]⍵    ⍝ array
  1≥⍴⍴⍵:(+/∧\⍵∊⍺)↓⍵                          ⍝ vector
  (∨\∨⌿~⍵∊⍺)/⍵                               ⍝ matrix
}

⍝ From http://dfns.dyalog.com/c_dtb.htm

dtb ← {                                      ⍝ Drop Trailing Blanks.
  ⍺←' ' ⋄ 1<|≡⍵:(⊂⍺)∇¨⍵                      ⍝ nested?
  2<⍴⍴⍵:(¯1↓⍴⍵){(⍺,1↓⍴⍵)⍴⍵}⍺ ∇,[¯1↓⍳⍴⍴⍵]⍵    ⍝ array
  1≥⍴⍴⍵:(-+/∧\⌽⍵∊⍺)↓⍵                        ⍝ vector
  (~⌽∧\⌽∧⌿⍵∊⍺)/⍵                             ⍝ matrix
}

⍝ From http://dfns.dyalog.com/c_deb.htm

deb ← {                                      ⍝ Drop Ending Blanks.
  ⍺←' ' ⋄ 1<|≡⍵:(⊂⍺)∇¨⍵                      ⍝ nested?
  2<⍴⍴⍵:(¯1↓⍴⍵){(⍺,1↓⍴⍵)⍴⍵}⍺ ∇,[¯1↓⍳⍴⍴⍵]⍵    ⍝ array
  b←⍵∊⍺                                      ⍝ mask
  1≥⍴⍴⍵:((∧\b)⍱⌽∧\⌽b)/⍵                      ⍝ vector
  b←∧⌿b ⋄ ((∧\b)⍱⌽∧\⌽b)/⍵                    ⍝ matrix
}

⍝ From http://dfns.dyalog.com/c_dmb.htm

dmb ← {                                      ⍝ Drop Multiple Blanks.
  ⍺←' ' ⋄ 1<|≡⍵:(⊂⍺)∇¨⍵                      ⍝ nested?
  2<⍴⍴⍵:(¯1↓⍴⍵){(⍺,1↓⍴⍵)⍴⍵}⍺ ∇,[¯1↓⍳⍴⍴⍵]⍵    ⍝ array
  2>⍴⍴⍵:(2∨/(~⍵∊⍺),1)/⍵                      ⍝ vector
  (2∨/(,∨⌿~⍵∊⍺),1)/⍵                         ⍝ matrix
}

⍝ From http://dfns.dyalog.com/c_dxb.htm

dxb ← {                                      ⍝ Drop eXtraneous Blanks.
  ⍺←' ' ⋄ 1<|≡⍵:(⊂⍺)∇¨⍵                      ⍝ nested?
  2<⍴⍴⍵:(¯1↓⍴⍵){(⍺,1↓⍴⍵)⍴⍵}⍺ ∇,[¯1↓⍳⍴⍴⍵]⍵    ⍝ array
  b←⍵∊⍺                                      ⍝ mask
  1≥⍴⍴⍵:(1↑b)↓(b⍲1↓b,1)/⍵                    ⍝ vector
  b←∧⌿b ⋄ (0,1↑b)↓(b⍲1↓b,1)/⍵                ⍝ matrix
}

⍝ From http://dfns.dyalog.com/c_dab.htm

dab←{                                        ⍝ Drop All Blanks.
  ⍺←' ' ⋄ 1<≡⍵:(⊂⍺)∇¨⍵                       ⍝ nested?
  1≥⍴⍴⍵:⍵~⍺                                  ⍝ vector
  2=⍴⍴⍵:↑(↓⍵)~¨⊂⍺                            ⍝ matrix
  (¯1↓⍴⍵){(⍺,1↓⍴⍵)⍴⍵}⍺ ∇,[¯1↓⍳⍴⍴⍵]⍵          ⍝ array
}
