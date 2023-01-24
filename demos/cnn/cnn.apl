⍝ cnn.apl
⍝ A convolutional neural network implementation

⍝ Copyright (c) 2019, Artem Shinkarov <artyom.shinkaroff@gmail.com>
⍝
⍝ Permission to use, copy, modify, and/or distribute this software for any
⍝ purpose with or without fee is hereby granted, provided that the above
⍝ copyright notice and this permission notice appear in all copies.
⍝
⍝ THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
⍝ REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
⍝ AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
⍝ INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
⍝ LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
⍝ OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
⍝ PERFORMANCE OF THIS SOFTWARE.

⍝ Adapted and ported to April APL by Andrew Sengul.

⎕IO ← 0

avg           ← {(+/÷≢),⍵}
              ⍝ Get the number of elements first, and then reduce with + and divide by n.

maxpos        ← {(,⍵)⍳⌈/,⍵}
              ⍝ Maxpos of the array of shape [10,1,1,1,...] where values are either 0 or 1
              ⍝ First elements of the indices of the array sorted in the descendingn order.
              ⍝ Alternative: { (⍸,⍵)[0] }

conv          ← {s←1+(⍴⍵)-⍴⍺ ⋄ ⊃+/,⍺×(⍳⍴⍺){s↑⍺↓⍵}¨⊂⍵}
              ⍝ Convolve with an array `a` and weights `w` looks like this:
              ⍝ So `⍳⍴w`                  is a matrix of all the indices of `w`,
              ⍝     {(1+(⍴a)-⍴w)↑⍺↓⍵}     computes `a` shifted by ⍺ and then takes
              ⍝                           the remaining shape
              ⍝ Then we multiply enclosed arrays with `w`, sum them up and disclose them

⍝ conv        ← {s ← 1+(⍴⍵)-⍴⍺ ⋄ ⊃+/,⍺×(⍳⍴⍺){s↑⍺↓⍵}¨⊂⍵}
⍝ conv        ← {a←⍵ ⋄ s←1+(⍴a)-⍴⍺ ⋄ ⊃+/,⍺×{s↑⍵↓a}¨⍳⍴⍺}
⍝ conv        ← {s ← 1+(⍴⍵)-⍴⍺ ⋄ $[∧/(⍴⍺)=⍴⍵; s⍴+/,⍺×⍵; ⊃+/,⍺×(⍳⍴⍺){s↑⍺↓⍵}¨⊂⍵]}
⍝ conv        ← {a ← ⍵ ⋄ s ← 1+(⍴a)-⍴⍺ $[∧/(⍴⍺)=⍴a; s⍴+/,⍺×a; ⊃+/,⍺{⍺×s↑⍵↓a}¨⊂⍳⍴⍺]}

⍝ conv        ← {w←⍺ ⋄ a←⍵ ⋄ s ← 1+(⍴a)-⍴w ⋄ +⌿((×/⍴w),s)⍴w{⍺×⍵}⍤(0,(⍴⍴w))⊢{s↑⍵↓a}⍤1⊢⊃⍳⍴w}
              ⍝ Here is a version without the enclose/disclose and it got slower
              ⍝ and uglier.  Not sure whether there is an easy fix.

⍝ conv        ← {w←⍺ ⋄ a←⍵ ⋄ t ← ⌊2÷⍨(⍴w)-~2|⍴w ⋄ (-t)↓t↓ ({+/,w×⍵}⌺(⍴w)⊢a)}
              ⍝ Here we have an alternative version of the conv that uses the ⌺ operator
              ⍝ However, it is noticeably slower in our application, as we have to remove
              ⍝ the "shrads" around the actual stencil operation.

multiconv     ← {a ws bs←⍵ ⋄ bs{⍺+⍵ conv a}⍤(0,⍴⍴a)⊢ws}
              ⍝ Multiconv is just a rank operator:
              ⍝ For weights `ws` with dimensionality greater than the dimensionality of `a`
              ⍝ Here we assume that `bias` has the same dimensionality as the "outer" level
              ⍝ of `ws` and it contains scalars.  (If not this can be easily adapted).

⍝ multiconv   ← {(a ws bias)←⍵ ⋄ ⊃bias+{⊂⍵ conv a}⍤(⍴⍴a)⊢ws}

⍝ fcconv      ← {s ← 1+(⍴⍵)-⍴⍺ ⋄ s⍴+/,⍺×⍵}
⍝ fclayer     ← {(a ws bias)←⍵ ⋄ bias{⍺ + ⍵ fcconv a}⍤(0,(⍴⍴a))⊢ws}
              ⍝ Here we can pre-optimise conv for FC layer, but it doesn't really help

backbias      ← {+/,⍵}
              ⍝ Simply the sum

backin        ← {d w in←⍵ ⋄ ⊃+/,w{(⍴in)↑(-⍵+⍴d)↑⍺×d}¨⍳⍴w}
              ⍝ For every index iv of `w`, we compute w[iv]*d_out and shift it
              ⍝ by iv.  Then we take shape(in) elements of that and sum all such
              ⍝ arrays with `+`.

logistic      ← {÷1+*-⍵}
              ⍝ Logistic function is 1/(1+e^(-in))

meansqerr     ← {÷∘2+/,(⍺-⍵)*2}
              ⍝ Mean Squared Error

backlogistic  ← {⍺×⍵×1-⍵}
              ⍝ Backlogistic is a simple expression

avgpool       ← {x y←⍴⍵ ⋄ avg⍤2⊢(x÷2)(y÷2)2 2⍴⍉⍤2⊢(x÷2)2 y⍴⍵}⍤2
              ⍝ This looks a bit ugly as we have to account for applying avgpool
              ⍝ over the last (dim f) dimensions of a.  Therefore we use rank operator
              ⍝ twice.  Other than that we simply reshape the array into (|a|/f)++f
              ⍝ and apply avg.

⍝ avgpool     ← {a ← ⍵ ⋄ {(i j)←2×⍵ ⋄ avg ⊢ ((i (i+1))(j (j+1))) ⌷ a} ¨⍳(÷∘2⍴a)}
⍝ avgpool     ← {÷∘4{+/,⍵}⌺(2 2⍴2)⍤2⊢⍵}

⍝ backavgpool ← {(a f) ← ⍵ ⋄ ⊃⍪/{⊂⍵}⍤2⊢⊃,/{f⍴⍵÷×/f}¨a}
              ⍝ For every element in `a` compute an array of shape `f` where all the
              ⍝ elements are the current element of `a` divided (prod f).

backavgpool   ← {2⌿2/⍵÷4}⍤2
              ⍝ We are tempted to specialise average pool for the shape 2 2, as we
              ⍝ don't use anything else and the expression gets really nice

convlab       ← {10 1 1 1 1⍴⍵=⍳10}
              ⍝ Convert the label into this stupid format

⍝ backmulticonv is just a ranked application of backin, backw and backbias.
backmulticonv ← {
  d_out weights in bias ← ⍵

  d_in   ← +⌿d_out {backin ⍺ ⍵ in}⍤(,⍨⍴⍴in)⊢weights

  d_w    ← {⍵ conv in}⍤(⍴⍴in)⊢d_out

  d_bias ← backbias⍤(⍴⍴in)⊢d_out


  d_in d_w d_bias
}

trainZhang ← {
  img target k1 b1 k2 b2 fc b ← ⍵

  c1    ← logistic multiconv img k1 b1
  s1    ← avgpool c1
  c2    ← logistic multiconv s1 k2 b2
  s2    ← avgpool c2
  out   ← logistic multiconv s2 fc b
  d_out ← out-target
  err   ← out meansqerr target

  d_s2 d_fc d_b ← backmulticonv (d_out backlogistic out) fc s2 b

  d_c2 ← backavgpool d_s2
  bl1  ← d_c2 backlogistic c2

  d_s1 d_k2 d_b2 ← backmulticonv bl1 k2 s1 b2

  d_c1 ← backavgpool d_s1

  _ d_k1 d_b1 ← backmulticonv (d_c1 backlogistic c1) k1 img b1
  d_k1 d_b1 d_k2 d_b2 d_fc d_b err
}

testZhang ← {
  k1 b1 k2 b2 fc b ← ⍵

  c1  ← logistic multiconv ⍺ k1 b1
  s1  ← avgpool c1
  c2  ← logistic multiconv s1 k2 b2
  s2  ← avgpool c2
  out ← logistic multiconv s2 fc b

  maxpos out
}

train ← {
  e i k1 b1 k2 b2 fc b rate imgs labs trsz ← ⍵

  i≥trsz : (e÷trsz) k1 b1 k2 b2 fc b

  img    ← i⌷imgs
  target ← convlab⊢i⌷labs

  d_k1 d_b1 d_k2 d_b2 d_fc d_b err ← trainZhang img target k1 b1 k2 b2 fc b

  k1 ← k1-rate×d_k1
  k2 ← k2-rate×d_k2
  b1 ← b1-rate×d_b1
  b2 ← b2-rate×d_b2
  fc ← fc-rate×d_fc
  b  ← b -rate×d_b

  ∇ (e++/err) (i+1) k1 b1 k2 b2 fc b rate imgs labs trsz
}

timeFactors ← 24 60 60 1000
  
formatElapsed ← {1↓⊃,/2 2 2 3{(':.'[⍺-2]),⍺{⍵,⍨(⍺-⍴⍵)⍴'0'}⍕⍵}¨timeFactors⊤⍵-⍨timeFactors⊥¯4↑⎕ts}
