⍝⍝ Ported from Dyalog's dfns at http://dfns.dyalog.com/n_Graphs.htm into April APL


⍝⍝ External dependencies

disp ← 'ARRAY-DEMO-SPACE' ⎕XWF 'disp'


⍝ From http://dfns.dyalog.com/c_for.htm

for ← {                                      ⍝ Multiple selection of function list.
  (¯1↓⍺)⍺⍺{1≠⍴⍺:⍺ ⍺⍺ ⍵                       ⍝ next left ...
    (⍺⍺⍣(⊃⍺))⍵                               ⍝ if last call and first ⍺:⍺⍺ ⍵
  }(⍵⍵⍣(⊃⌽⍺))⍵                               ⍝ if last ⍺:⍵⍵ ⍵
}

⍝ From http://dfns.dyalog.com/c_invr.htm
⍝ TODO: why is a more generous ⎕CT needed here?
invr ← { ⎕CT←1e¯14                           ⍝ Approx inverse of real-valued function.
  ⍝ 0::'no inverse'⎕SIGNAL ⎕EN               ⍝ pass back domain error.
  ⍺←1+⎕CT+0×⍵                                ⍝ initial guess - slightly more than 1.
  ∆x←⎕CT*÷2                                  ⍝ increment delta-x.
  -∘⍵∘⍺⍺{                                    ⍝ Newton-Raphson: find root of ⍺⍺(x)-⍵ = 0.
    ⍵⍵ ⍵:⍵                                   ⍝ all items within ⎕ct of inverse: finished.
    y y∆←⍺⍺¨0 ∆x+⊂⍵                          ⍝ f(x) f(x+∆x)
    ∇ ⍵-y×∆x÷y∆-y                            ⍝ refined estimate.
  }(⍵∘≡∘⍺⍺)⍺                                 ⍝ starting from best guess.
}

⍝ From http://dfns.dyalog.com/c_limit.htm

limit ← {                                    ⍝ Function power limit (fixpoint).
  ⍵ ⍺⍺{                                      ⍝ 'old' value:
    ⍺≡⍵:⍵                                    ⍝       old matches new: finished.
    ⍵ ∇ ⍺⍺ ⍵                                 ⍝       otherwise: try new value.
  }⍺⍺ ⍵                                      ⍝ 'new' value.
}

⍝ From http://dfns.dyalog.com/s_limit.htm

⍝ AM  ← {(+/⍵)×÷⍴,⍵}                           ⍝ Arithmetic mean.
⍝ GM  ← {(×/⍵)*÷⍴,⍵}                           ⍝ Geometric  mean.
⍝ AGM ← {⍬⍴{(AM ⍵),GM ⍵}limit ⍵}               ⍝ Arithmetic-geometric mean.

ArcTan ← {
  ⍝ Inverse trigonometric tangent - gqr 19-11-2002.
  ⍝ For scalar ⍵: (ArcTan ⍵)≡¯3○⍵
  ⍝
  ⍝ Limit's operand function returns a 2-vector. Repeated application
  ⍝ of the function produces two sequences, both of which converge and
  ⍝ have the same limit:
  ⍝
  ⍝    A≡ {a(0),a(1), ... , a(n), ...   }
  ⍝    G≡ {g(0),g(1), ... , g(n), ...   }
  ⍝
  ⍝  Starting with arguments:
  ⍝       a(0)←(1+⍵*2)*-÷2  and  g(0)←1
  ⍝  the iteration produces:
  ⍝       a(n+1)← arithmetic mean of a(n)   and g(n)
  ⍝       g(n+1)← geometric  mean of a(n+1) and g(n)
  ⍝  until:
  ⍝   a(n+1)=a(n) and g(n+1)=g(n) and a(n+1)=g(n+1)

  next←{                                     ⍝ next term in sequence.
    AM←{(+/⍵)×÷⍴⍵}                           ⍝ arithmetic mean.
    GM←{(×/⍵)*÷⍴⍵}                           ⍝ geometric  mean.
    (AM ⍵),GM(AM ⍵),1↓⍵
  }

  start←⍬⍴(1+⍵*2)*-÷2
  finish←⍬⍴next limit start,1                ⍝ calculated limit
  ⍬⍴⍵×start÷finish
}

⍝ From http://dfns.dyalog.com/c_pow.htm

pow ← { (⍺⍺⍣⍺)⍵ }                            ⍝ Explicit function power.

⍝ From http://dfns.dyalog.com/s_pow.htm

rl   ← {(¯1+2*31)|⍵×7*5}                     ⍝ next random link  LCG(7*5, ¯1+2*31)

roll ← {⎕IO+⌊⍵×⍺÷¯1+2*31}                    ⍝ roll ⍵ with random link ⍺.

⍝ From http://dfns.dyalog.com/c_traj.htm

traj ← {                                     ⍝ Function limit 'trajectory'.
  ⍺←⍬                                        ⍝ Initial null history.
  (⊂⍵)∊⍺:⍺                                   ⍝ Argument in history: finished.
  (⍺,⊂⍵)∇ ⍺⍺ ⍵                               ⍝ Extended history with next argument.
} ⍝ TODO: break out into its own space with other power operators

⍝ From http://dfns.dyalog.com/s_traj.htm

⍝ nr ← {                                       ⍝ Newton-Raphson.
⍝   ⍺←⎕CT                                      ⍝ default increment.
⍝   y ∆y←⍺⍺¨⍵+0 ⍺                              ⍝ f(⍵), f(⍵+∆)
⍝   ⍵+(⍺×y)÷y-∆y                               ⍝ next estimate.
⍝ }

traj2 ← { ¯1∘↓∘(,∘⊂∘⍺⍺∘⊃∘⌽⍨⍣(∊⍨∘⊂∘⊃∘⌽⍨)∘⊂)⍵ }

⍝ From http://dfns.dyalog.com/c_while.htm

while ← {                                    ⍝ Conditional function power.
  ⍵⍵ ⍵:∇ ⍺⍺ ⍵                                ⍝ While ⍵⍵ ⍵: apply ⍺⍺ ⍺⍺ ··· ⍵.
  ⍵                                          ⍝ Otherwise: finished.
}

⍝ From http://dfns.dyalog.com/c_until.htm

until ← {                                    ⍝ Conditional function power.
  ⍺⍺{                                        ⍝ sub-operator for initial application:
    ⍵⍵ ⍵:⍵                                   ⍝ Until ⍵⍵,
    ∇ ⍺⍺ ⍵                                   ⍝ Apply ⍺⍺ ⍺⍺ ...
  }⍵⍵ ⍺⍺ ⍵                                   ⍝ initial application.
}
