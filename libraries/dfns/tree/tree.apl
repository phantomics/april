⍝⍝ Ported from Dyalog's dfns at http://dfns.dyalog.com/n_Trees.htm into April APL


⍝⍝ External dependencies

disp display pmat ← 'ARRAY-LIB-SPACE' ⎕XWF 'disp' 'display' 'pmat'
foldl             ← 'ARRAY-LIB-SPACE' ⎕XWO 'foldl'
traj              ← 'POWER-LIB-SPACE' ⎕XWO 'traj'


⍝ From http://dfns.dyalog.com/c_avl.htm

avl ← { ⎕IO←0                                ⍝ Adelson-Velskii, Landis trees.

  get←{                                      ⍝ value for key ⍵ from AVL tree ⍺.
    ⍺≡0:                                     ⍝ key not in tree: no value.
    (k v)_ subs←⍺                            ⍝ next key, value and subtrees.
    k≡⍵:v                                    ⍝ match: value from tree.
    dir←-/⍋↑⍵ k                              ⍝ natch: search direction.
    sub sib←dir wise subs                    ⍝ subtree to search and its sibling.
    sub ∇ ⍵                                  ⍝ value from subtree.
  }                                          ⍝ :: t ∇ k → v

  put←{                                      ⍝ tree ⍺ with key=value ⍵.
    ⍺≡0:(⍵ 0(0 0))1                          ⍝ null tree: new leaf, height incr.
    (kv obal subs)(key val)←⍺ ⍵              ⍝ next node and key=val pair.
    key≡⊃kv:(⍵ obal subs)0                   ⍝ match: just replace value.
    dir obv←1 ¯1×-/⍋↑key(⊃kv)                ⍝ natch: search direction and obverse.
    sub sib←dir wise subs                    ⍝ subtree to search and its sibling.
    nsub inc←sub ∇ ⍵                         ⍝ new subtree and height increment.
    new←obv proj kv obal(nsub sib)           ⍝ tree with new node.
    inc=0:new 0                              ⍝ no height increment: done.
    (dir balance new)(obal=0)                ⍝ height increment if OLD balance is 0.
  }                                          ⍝ :: t ∇ k v → t i

  rem←{                                      ⍝ tree ⍺ without key ⍵.
    ⍺≡0:0 0                                  ⍝ key not in tree: done.
    ⍵≡⊃⊃⍺:rmnode ⍺                           ⍝ match: remove node.
    dir obv←1 ¯1×-/⍋↑⍵(⊃⊃⍺)                  ⍝ natch: search direction and obverse.
    kv obal(sub sib)←obv proj ⍺              ⍝ subtree to search and its sibling.
    nsub inc←sub ∇ ⍵                         ⍝ new subtree and height increment.
    new←obv proj kv obal(nsub sib)           ⍝ tree with node removed.
    inc=0:new 0                              ⍝ no height decrement: done.
    nkv nbal nsubs←obv balance new           ⍝ balanced tree.
    (nkv nbal nsubs)(-nbal=0)                ⍝ height decrement if NEW balance is 0.
  }                                          ⍝ :: t ∇ k → t i

  ⍝ TODO: rm must be used in place of rem because rem is considered a function inside the closure
  rmnode←{                                   ⍝ node ⍵ removed.
    kv obal subs←⍵                           ⍝ subnodes.
    0∊subs:((subs⍳0)⊃⌽subs)¯1                ⍝ either sub null: the other.
    lft rgt←subs                             ⍝ left and right non-null subtrees.
    (sk sv)_ _←rgt limt ¯1                   ⍝ successor key=val.
    rm inc←rgt rem sk                        ⍝ right subtree with successor removed.
    new←(sk sv)obal(lft rm)                  ⍝ target node replaced with successor.
    inc=0:new 0                              ⍝ no height decrement: done.
    nkv nbal nsubs←¯1 balance new            ⍝ new balanced node.
    (nkv nbal nsubs)(-nbal=0)                ⍝ height decrement if NEW balance is 0.
  }                                          ⍝ :: ∇ t → t i

  limt←{                                     ⍝ ⍵-most node of (sub)tree ⍺.
    sub←⊃⍵ wise⊃⌽⍺                           ⍝ ⍵-sub.
    sub≡0:⍺                                  ⍝ null: this.
    sub ∇ ⍵                                  ⍝ else: ⍵-most of sub.
  }                                          ⍝ :: t ∇ d → t

  balance←{                                  ⍝ tree ⍵ with balancing moment ⍺.
    kv obal subs←⍵                           ⍝ original tree.
    new←⍺+obal                               ⍝ new balance.
    0∊obal new:kv new subs                   ⍝ balance bits absorb moment: done.
    (_ Bbal _)_←obal wise subs               ⍝ otherwise:
    Bbal≠-obal:(-obal)rot1 ⍵                 ⍝   single or
               (-obal)rot2 ⍵                 ⍝     double rotation.
  }                                          ⍝ :: m ∇ t → t

  rot1←{                                     ⍝ single ⍺-rotation of tree ⍵.
    Akv Abal(B r)←⍺ proj ⍵                   ⍝    <<A         yB>
    Bkv Bbal(p q)←⍺ proj B                   ⍝     / \        / \      where x y z
    AAbal←-⍺×Bbal=0                          ⍝   <Bx  r  =>  p  <Az    are such that:
    BBbal←+⍺×Bbal=0                          ⍝   / \            / \    <B> →  B> <A
    AA←⍺ proj Akv AAbal(q r)                 ⍝  p   q          q   r   <B  → <B> <A>
    ⍺ proj Bkv BBbal(p AA)                   ⍝ aaa
  }                                          ⍝ :: d ∇ t → t

  rot2←{                                     ⍝ double ⍺-rotation of tree ⍵.
    Akv Abal(B s)←⍺ proj ⍵                   ⍝    <<A         <<A          <C>
    Bkv Bbal(p C)←⍺ proj B                   ⍝     / \         ⌿ \         / \
    Ckv Cbal(q r)←⍺ proj C                   ⍝    B>  s  =>  <C.  s  =>  <By xA>
    AAbal←⍺×Cbal=-⍺                          ⍝   / ⍀         / \         / \ / \
    BBbal←-⍺×Cbal=⍺                          ⍝  p  xCy     <B.  r       p  q r  s
    BB←⍺ proj Bkv BBbal(p q)                 ⍝     / \     / \
    AA←⍺ proj Akv AAbal(r s)                 ⍝    q   r   p   q
    ⍺ proj Ckv 0(BB AA)                      ⍝                   where:
  }                                          ⍝ :: d ∇ t → t      x y∊'<>' '< ' ' >'

  vec←{                                      ⍝ enlist of tree ⍵.
    0≡⍵:⍬                                    ⍝ null tree: null vector.
    key_val bal(lft rgt)←⍵                   ⍝ node info and subtrees.
    (∇ lft),(⊂key_val),∇ rgt                 ⍝ left_vec, key=val, right_vec.
  }                                          ⍝ :: ∇ t → [k v]

  chk←{                                      ⍝ tree stats / integrity check.
    0=≡⍵:(⍵≡0)0 0 0 ⍬                        ⍝ null: ok maxbal=0 height=0 key-range.
    (key _)bal subs←⍵                        ⍝ key, balance and subtrees.
    stats←(⍺+1)∇¨subs                        ⍝ subtrees stats.
    oks szs dps hts krs←↓⍉↑stats             ⍝ oks sizes depths heights key-ranges.
    keys←↑key{⍺,(⊂⍶),⍵}/krs                  ⍝ subtree key ranges.
    okkey←{⍵≡⍳⍴⍵}⍋↑keys                      ⍝ left keys << key >> right keys.
    okhgt←bal=--/hts                         ⍝ balance is height difference.
    okbal←bal∊¯1 0 1                         ⍝ balance is in range.
    ok←okkey∧okbal∧okhgt∧∧/oks               ⍝ subtree is good.
    sz←1++/szs                               ⍝ subtree size.
    dp←⍺++/dps                               ⍝ total node depth.
    ht←1+⌈/hts                               ⍝ subtree height.
    kr←⌽2⍴¯1⌽keys                            ⍝ key range for subtree.
    ⍺>0:ok sz dp ht kr                       ⍝ subtree: ok size tot_dep height range.
    ok sz(⌊0.5+dp÷sz)ht                      ⍝ root: ok size mean_depth height.
  }                                          ⍝ :: ∇ t → y s d h {r}

  fmt←{                                      ⍝ formatted tree ⍵.
    null←0 0⍴''                              ⍝ format of null tree.
    ⍵≡0:null                                 ⍝ null tree: null format.
    (key val)bal subs←⍵                      ⍝ node info.
    key_val←⍺,↑,/⍕¨key'='val                 ⍝ formatted >>key=value
    deco←(1+bal)⊃'><' '─' '<>'               ⍝ balance decorators.
    fmts←{⊖⍵}\'┌└'{                          ⍝ hang subtrees by ┌─ ─┐ branches.
      0 0≡⍴⍵:⍵                               ⍝ null: done.
      mask←∧\' '=⊃↓⌽⍉⍵                       ⍝ mask of leading blanks.
      ⍉⌽↑(⊂⌽⍺,mask/'│'),↓⌽⍉⍵                 ⍝ subtree suspended by branch.
    }¨{⊖⍵}\deco ∇¨subs                       ⍝ formatted subtrees.
    case←~null null≡¨fmts                    ⍝ non-null subtree cases.
    join←(2⊥case)⊃'∘┐┘┤'                     ⍝ subtree joining char.
    join≡'∘':↑,↓key_val                      ⍝ leaf: done.
    dent←' '⊣¨key_val                        ⍝ subtree padding.
    pads←{↓↑,/dent,⊂⍵}¨fmts                  ⍝ left-padded subtrees.
    ↑↑{⍺,(↓key_val,join),⍵}/pads             ⍝ formatted tree.
  }                                          ⍝ :: ∇ t → [-;]

  proj←{(⍺=0 0 ¯1)⌽¨⍵}                       ⍝ ⍺-projection of node ⍵.
  wise←{(⍺=1)⌽⍵}                             ⍝ subtrees ⍵ in -⍺, +⍺ order.

  op←⍶ ⍝ op←⍺⍺{f←⍺⍺ ⋄ ⊃⎕CR'f'}0              ⍝ operand label.

  '∪'≡op:⊃⍺ put ⍵                            ⍝ tree ⍺ with key=value pair ⍵.
  '⍎'≡op:⍺ get ⍵                             ⍝ value for key ⍺ in tree ⍵.
  '~'≡op:⊃⍺ rem ⍵                            ⍝ tree ⍺ without key ⍵.
  '⍕'≡op:''fmt ⍵                             ⍝ formatted tree ⍵.
  '∊'≡op:vec ⍵                               ⍝ vector of key=value pairs for tree ⍵.
  '?'≡op:4↑0 chk ⍵                           ⍝ stats for tree ⍵: ok size dpth height.
}

⍝ From http://dfns.dyalog.com/s_avl.htm

fibtree ← {                                  ⍝ Depth-⍵ worst-case fibonacci tree.
  ⊃⍬⍴⎕IO{                                    ⍝ first item is tree.
    ⍵=0:0 ⍺                                  ⍝ f 0 → []
    ⍵=1:(⍺ 0(0 0))(⍺+1)                      ⍝ f 1 → ⍺ [] []
    l m←⍺ ∇ ⍵-2                              ⍝ left subtree and next value
    r n←(m+1)∇ ⍵-1                           ⍝ right ..  ..  ..  ..  ..
    (m 1(l r))n                              ⍝ f ⍵+1 → ⍺ [f ⍵-2] [f ⍵-1]
  }⍵                                         ⍝ :: tree next ← next ∇ depth
}
  
⍝ From http://dfns.dyalog.com/c_sbst.htm

sbst ← { ⎕IO←0                               ⍝ Simple Binary Search Trees.

  put←{                                      ⍝ tree ⍺ with key=value ⍵.
    ⍺≡0:(⍵(0 0))0                            ⍝ null: new node.
    ((nxt _)subs)(key _)←⍺ ⍵                 ⍝ node and search key/val.
    nxt≡key:(⍵ subs)0                        ⍝ match: new value.
    ⍺ ∇ search ⍵                             ⍝ natch: try subtrees.
  }                                          ⍝ :: t ∇ k v → t _

  get←{                                      ⍝ value for key ⍵ from tree ⍺.
    ⍺≡0:⍺'?'                                 ⍝ null: key not in tree: no value.
    ((nxt val)_)(key _)←⍺ ⍵                  ⍝ node and search key.
    nxt≡key:⍺ val                            ⍝ match: tree & value.
    ⍺ ∇ search ⍵                             ⍝ natch: try subtrees.
  }                                          ⍝ :: t ∇ k _ → _ v

  rem←{                                      ⍝ tree ⍺ without key ⍵.
    ⍺≡0:⍺ 0                                  ⍝ null: key not in tree: no change.
    ((nxt _)subs)(key _)←⍺ ⍵                 ⍝ node and key.
    ~nxt≡key:⍺ ∇ search ⍵                    ⍝ natch: value.
    0 0≡subs:0 0                             ⍝ leaf: done.              X~X → Y
    0∊subs:((subs⍳0)⊃⌽subs)0                 ⍝ one null: use other.    /       \
    (rrot ⍺)∇ ⍵                              ⍝ remove from rgt-rotn:  Y         X~X
  }                                          ⍝ :: t ∇ k _ → t _
                                             ⍝                       B   →   A
  rrot←{                                     ⍝ right rotation.      / \     / \
    B((A(p q))r)←⍵                           ⍝ unpack nodes.       A   r   p   B
    A(p(B(q r)))                             ⍝ repack nodes.      / \         / \
  }                                          ⍝ :: ∇ t → t        p   q       q   r

  search←{                                   ⍝ search subtree ⍺ for key ⊃⍵.
    inf(lft rgt)←⍺                           ⍝ parts of node.
    dir←1-2×>/⍋↑⊃¨inf ⍵                      ⍝ search direction: ¯1 1
    wise←{(2×⍺)↑3⍴⍵}                         ⍝ parameterise direction.
    _ nxt←dir wise lft rgt                   ⍝ nxt subtree to search.
    sub val←nxt ⍺⍺ ⍵                         ⍝ new subtree.
    subs←dir wise lft sub rgt                ⍝ lft and rgth subtrees.
    (inf subs)val                            ⍝ new node and value.
  }                                          ⍝ :: t (t ∇ k v → t v) ∇∇ k v → t v

  fmt←{                                      ⍝ formatted tree ⍵.
    null←0 0⍴''                              ⍝ format of null tree.
    ⍵≡0:null                                 ⍝ null tree: null format.
    (key val)subs←⍵                          ⍝ node info.
    key_val←↑,/⍕¨key'='val                   ⍝ formatted key=value
    fmts←{⊖⍵}\'┌└'{                          ⍝ hang subtrees by ┌─ ─┐ branches.
      0 0≡⍴⍵:⍵                               ⍝ null: done.
      mask←∧\' '=⊃↓⌽⍉⍵                       ⍝ mask of leading blanks.
      ⍉⌽↑(⊂⌽⍺,mask/'│'),↓⌽⍉⍵                 ⍝ subtree suspended by branch.
    }¨{⊖⍵}\∇¨subs                            ⍝ formatted subtrees.
    case←~null null≡¨fmts                    ⍝ non-null subtree cases.
    join←(2⊥case)⊃'∘┐┘┤'                     ⍝ subtree joining char.
    join≡'∘':↑,↓key_val                      ⍝ leaf: done.
    dent←' '⊣¨key_val                        ⍝ subtree padding.
    pads←{↓↑,/dent,⊂⍵}¨fmts                  ⍝ left-padded subtrees.
    ↑↑{⍺,(↓key_val,join),⍵}/pads             ⍝ formatted tree.
  }                                          ⍝ :: ∇ t → [-;]

  vec←{                                      ⍝ vector of key=value pairs.
    ⍵≡0:⍬                                    ⍝ null tree: null vector.
    key_val(lft rgt)←⍵                       ⍝ key=val and subtrees.
    (∇ lft),(⊂key_val),∇ rgt                 ⍝ left_vec, key=val, right_vec.
  }                                          ⍝ :: ∇ t → [k v]

  chk←{                                      ⍝ tree stats / integrity check.
    0=≡⍵:(0≡⍵)0 0 0 ⍬                        ⍝ null: ok ht=0 sz=0 depth=0 range=⍬.
    (key _)subs←⍵                            ⍝ node info and subtrees.
    stats←(⍺+1)∇¨subs                        ⍝ subtree stats.
    oks szs dps hts krs←↓⍉↑stats             ⍝ individual stats.
    keys←↑key{⍺,(⊂⍶),⍵}/krs                  ⍝ subtree key ranges.
    okkey←{⍵≡⍳⍴⍵}⍋↑keys                      ⍝ left keys << key >> right keys.
    okstr←2 2≡(⍴⍵),⍴⊃⌽⍵                      ⍝ good node struct.
    ok←okkey∧okstr∧∧/oks                     ⍝ good tree.
    sz←1++/szs                               ⍝ subtree size.
    dp←⍺++/dps                               ⍝ total node depth.
    ht←1+⌈/hts                               ⍝ node height.
    kr←⌽2⍴¯1⌽keys                            ⍝ key range for subtree.
    ⍺>0:ok sz dp ht kr                       ⍝ subtree: ok size tot_dep height range.
    ok sz(⌊0.5+dp÷sz)ht                      ⍝ root: ok size mean_depth height.
  }                                          ⍝ :: ∇ t → y s d h {r}

  bal←{                                      ⍝ dsw-balancing.
    vine size←0 0 list ⍵                     ⍝ vine of 0-leaves and size.
    log←⌊2⍟size+1                            ⍝ largest complete tree ≤ ⍵.
    rem←1+size-2*log                         ⍝ no of surplus nodes.
    cmps←¯2+2*1+⍳log                         ⍝ compression vector.
    ↑cmp/(1↓cmps,2×rem),⊂vine                ⍝ compression reduction → balanced tree.
  }                                          ⍝ :: ∇ t → t

  cmp←{                                      ⍝ compress of alternate vine sections.
    ⍺=0:⍵                                    ⍝ far enough: terminal leaf.
    inf(lft rgt)←⍵                           ⍝ parts of node.
    lev←(⍺-1)∇ lft                           ⍝ leftmost vine leaf.
    2|⍺:inf(lev rgt)                         ⍝ copying of alternate vine sections.
    rrot inf(lev rgt)                        ⍝ rotation of alternate vine sections.
  }                                          ⍝ :: n ∇ v → t

  list←{                                     ⍝ list (0-vine) from tree ⍵.         /
    0≡⍵:⍺                                    ⍝ null: accumlated vine.    /       C
    inf(lft rgt)←⍵                           ⍝ node info & subtrees.    B   →   /
    lev s←⍺ ∇ lft                            ⍝ left vine & size,       / \     B
    (inf(lev 0))(s+1)∇ rgt                   ⍝ ++ right vine.         A   C   /
  }                                          ⍝ :: v ∇ t → v s                A

  op←⍶                                       ⍝ operand label.

  '∪'≡op:⊃⍺ put ⍵                            ⍝ insert/replace value in tree.
  '⍎'≡op:⊃⌽⍵ get ⍺ 0                         ⍝ search for value for key.
  '~'≡op:⊃⍺ rem ⍵ 0                          ⍝ remove key=value from tree.
  '⍕'≡op:fmt ⍵                               ⍝ formatted tree.
  '∊'≡op:vec ⍵                               ⍝ vector of key=value pairs.
  '?'≡op:4↑0 chk ⍵                           ⍝ tree stats and integrity check.
  '='≡op:bal ⍵                               ⍝ balanced tree ⍵.
}

⍝ From http://dfns.dyalog.com/c_redblack.htm
  
redblack ← { ⎕IO←0                           ⍝ Red-black trees.

  ins←{                                      ⍝ tree ⍺ with key=val ⍵.        [ins]
    ⍺≡0:base ⍵ 1(0 0)                        ⍝ null: new <red> node and path.
    ((nxt _)red subs)(key _)←⍺ ⍵             ⍝ parts of node.
    nxt≡key:done ⍵ red subs                  ⍝ match: new value, long path.
    node path←⍺ ∇ search ⍵                   ⍝ natch: search subtrees.
    2≠⍴path:node path                        ⍝ not grandparent: continue.
    p c←path                                 ⍝ parent and child sides.
    ~node isred p:node path                  ⍝ parent black [insB]: no change.
    node isred-p:node insRR p                ⍝ uncle red [insRR]:
    p=c:node insRBo p                        ⍝ C is P's outer child [insRBo]:
    (node rot∘p sub p)insRBo p               ⍝ C is P's inner child [insRBi]:
  }                                          ⍝ t p ← t ∇ k v

  insRR←{                                    ⍝ red uncle.                    [insRR]
    g←flip ⍺                                 ⍝   [G] → <G>
    u←g flip sub ⍵                           ⍝   <U> → [U]
    base u flip sub-⍵                        ⍝   <P> → [P]
  }                                          ⍝ :: t ∇ p → t p

  insRBo←{                                   ⍝ C is outer child.             [insRBo]
    g←flip ⍺                                 ⍝   [G] → <G>
    p←g rot-⍵                                ⍝       ⌽ G-P
    done flip p                              ⍝   <P> → [P]
  }                                          ⍝ :: t ∇ p → t p

  get←{                                      ⍝ value for key ⍵ from tree ⍺.
    ⍺≡0:                                     ⍝ key not in tree: no value.
    (nxt val)_ subs←⍺                        ⍝ next key, value and subtrees.
    nxt≡⍵:val                                ⍝ match: value from tree.
    ((>/⍋↑⍵ nxt)⊃subs)∇ ⍵                    ⍝ search subtree.
  }                                          ⍝ v ← t ∇ k

  rem←{                                      ⍝ remove key ⍵ from tree ⍺.
    ⍺≡0:done 0                               ⍝ null: key not in tree.
    (nxt _)red(lft rgt)←⍺                    ⍝ node key and subtrees.
    ~nxt≡⊃⍵:bal ⍺ ∇ search ⍵                 ⍝ no match: search subtrees.
    0≡lft:bal rgt rep red                    ⍝ no left subtree: replace with right.
    0≡rgt:bal lft rep red                    ⍝ no right subtree: replace with left.
    kv←left rgt                              ⍝ two subtrees: right-successor node.
    sub path←rgt ∇ kv                        ⍝ right subtree with successor removed.
    bal(kv red(lft sub))(1,path)             ⍝ replaced with successor's key=val.
  }                                          ⍝ :: t ∇ k _ → t p

  left←{                                     ⍝ left-most key=val in subtree ⍵.
    kv _(lft _)←⍵                            ⍝ key=val and left subtree.
    0≡lft:kv                                 ⍝ no left node: successor key.
    ∇ lft                                    ⍝ examine left subtree.
  }                                          ⍝ :: ∇ t → kv

  rep←{                                      ⍝ repaint deleted node's child. [rep]
    ⍵:done ⍺                                 ⍝ red X: [C]                    [repR]
    ⍺≡0:dblk 0                               ⍝ child null: [[∘]]             [repBB]
    inf red subs←⍺                           ⍝ child non-null.
    red:done inf 0 subs                      ⍝ child red: <C> →  [C]         [repBR]
    dblk ⍺                                   ⍝ child blk: [C] → [[C]]        [repBB]
  }                                          ⍝ :: t ∇ r → t p

  ⍝ TODO: ddblk should just be named dblk, see why this causes a conflict
  bal←{                                      ⍝ double-black balancing.       [bal]
    sub path←⍵                               ⍝ subtree and path to new child.
    2≠⍴path:⍵                                ⍝ not parent: continue.
    dir ddblk←path×¯1 1                       ⍝ direction and 0 => double-black.
    ddblk≠0:⍵                                 ⍝ not double black: continue.
    sub isred dir:sub balR dir               ⍝ red sibling: [balR]
    sub balB dir                             ⍝ blk sibling: [balB]
  }                                          ⍝ :: ∇ t p → t p

  balR←{                                     ⍝ sibling red                   [balR]
    p0←flip ⍺                                ⍝   [P] → <P>
    p1←p0 rot-⍵                              ⍝       ⌽ P-S
    p2←flip p1                               ⍝   <S> → [S]
    Pinf Pred Psubs←p2                       ⍝ new parent node.
    N S←⍵ wise Psubs                         ⍝ [[N]] and [S]
    N_ path←N balB ⍵                         ⍝ sibling now black: [balB]
    P_subs←⍵ wise N_ S                       ⍝ new subtrees.
    (Pinf Pred P_subs)path                   ⍝ balanced tree.
  }                                          ⍝ :: t ∇ r → t p

  balB←{                                     ⍝ sibling black                 [balB]
    near far←⍺∘isred¨⍵×1,¨¯1 1               ⍝ nephew colours.
    near⍱far:⍺ balBbb ⍵                      ⍝ both nephews black: [balBbb]
    far:⍺ balB_r ⍵                           ⍝ far nephew red: [balB_r]
    ⍺ balBrb ⍵                               ⍝ far nephew black: [balBrb]
  }                                          ⍝ :: t ∇ r → t p

  balBbb←{                                   ⍝ both nephews black            [balBbb]
    pred←⍺ isred ⍬                           ⍝ parent colour.
    p0←⍺ flip sub ⍵                          ⍝   [S] → <S>
    pred:done flip p0                        ⍝   <P> → [P]
    dblk p0                                  ⍝   [P] → [[P]]
  }                                          ⍝ :: t ∇ r → t p

  balB_r←{                                   ⍝ far nephew red.               [balB_r]
    colr←{kv _ lr←⍺ ⋄ kv ⍵ lr}               ⍝ ⍵-coloured tree ⍺.
    pred←⍺ isred ⍬                           ⍝ parent colour.
    p0←⍺ colr 0                              ⍝   (P) → [P]
    p1←p0 rot-⍵                              ⍝       ⌽ P-S
    p2←p1 flip sub ⍵                         ⍝   <f> → [f]
    p3←p2 colr pred                          ⍝   (S) → (P)
    done p3                                  ⍝ balance resolved.
  }                                          ⍝ :: t ∇ r → t p

  balBrb←{                                   ⍝ far nephew black              [balBrb]
    p0←⍺ flip sub ⍵                          ⍝ [S] → <S>
    p1←p0 rot∘⍵ sub ⍵                        ⍝     ⌽ S-n
    p2←p1 flip sub ⍵                         ⍝ <n> → [n]
    p2 balB_r ⍵                              ⍝       [balB_r]
  }                                          ⍝ :: t ∇ r → t p

  root←{                                     ⍝ root:
    0≡⊃⍵:0                                   ⍝ null tree: done.
    (inf _ subs)_←⍵                          ⍝ tree and path.
    inf 0 subs                               ⍝ root: black - rule [I].
  }                                          ⍝ t ← ∇ t p

  fmt←{                                      ⍝ formatted tree ⍵.
    null←↑,↓'[∘]'                            ⍝ format of null.
    ⍵≡0:null                                 ⍝ null tree: null format.
    (key val)red subs←⍵                      ⍝ node info.
    l r←red⊃'[]' '<>'                        ⍝ colour: [black] <red>
    key_val←↑,/⍕¨l key'='val r               ⍝ formatted key=value
    fmts←{⊖⍵}\'┌└'{                          ⍝ hang subtrees by ┌─ ─┐ branches.
      0 0≡⍴⍵:⍵                               ⍝ null: done.
      mask←∧\' '=⊃↓⌽⍉⍵                       ⍝ mask of leading blanks.
      ⍉⌽↑(⊂⌽⍺,mask/'│'),↓⌽⍉⍵                 ⍝ subtree suspended by branch.
    }¨{⊖⍵}\∇¨subs                            ⍝ formatted subtrees.
    dent←' '⊣¨key_val                        ⍝ subtree padding.
    pads←{↓↑,/dent,⊂⍵}¨fmts                  ⍝ left-padded subtrees.
    ↑↑{⍺,(↓key_val,'┤'),⍵}/pads              ⍝ formatted tree.
  }                                          ⍝ :: ∇ t → [-;]

  vec←{                                      ⍝ vector of key=value pairs.
    0≡⍵:⍬                                    ⍝ null tree: null vector.
    key_val bal(lft rgt)←⍵                   ⍝ node info and subtrees.
    (∇ lft),(⊂key_val),∇ rgt                 ⍝ left_vec, key=val, right_vec.
  }

  chk←{                                      ⍝ tree statistics.
    0=≡⍵:(0≡⍵)0 0 0,⍬ 1 1                    ⍝ null: ok size dep ht range blks isblk.
    (key _)red subs←⍵                        ⍝ parts of node.
    blk←~red                                 ⍝ black node.
    stats←(⍺+1)∇¨subs                        ⍝ subtree stats.
    oks ss ds hs ks bs bks←↓⍉↑stats          ⍝ oks sizes deps hghts keys blks reds.
    keys←↑key{⍺,(⊂⍶),⍵}/ks                   ⍝ subtree key ranges.
    okK←{⍵≡⍳⍴⍵}⍋↑keys                        ⍝ left keys << key >> right keys.
    okR←blk∨∧/bks                            ⍝ check rule [R].
    okB←=/bs                                 ⍝ check rule [B].
    ok←okK∧okR∧okB∧∧/oks                     ⍝ subtree ok.
    kr←⌽2⍴¯1⌽keys                            ⍝ key range.
    blks←blk+⌈/bs                            ⍝ black count increment.
    ht sz←1+(⌈/hs),+/ss                      ⍝ tree height and size.
    dp←⍺++/ds                                ⍝ total depths.
    ⍺>0:ok sz dp ht,kr blks blk              ⍝ subtree stats.
    ok sz(⌊0.5+dp÷sz)ht                      ⍝ root: ok size mean_depth height.
  }                                          ⍝ ok size tot_dep ht rng blks blk ← ∇ t

  search←{                                   ⍝ search subtree ⍺ for key ⊃⍵.
    inf red(lft rgt)←⍺                       ⍝ parts of node.
    dir←1-2×>/⍋↑⊃¨inf ⍵                      ⍝ search direction: ¯1 1
    _ nxt←dir wise lft rgt                   ⍝ nxt subtree to search.
    sub path←nxt ⍺⍺ ⍵                        ⍝ new subtree & path.
    subs←dir wise lft sub rgt                ⍝ new subtrees.
    (inf red subs)(dir,path)                 ⍝ new node and extended path.
  }                                          ⍝ :: t (t ∇ k v → t p) ∇∇ k v → t p

  rot←{                                      ⍝ ⍵-rotation of node ⍺.
    Ninf Nred Nsubs←⍺                        ⍝        N   →   L_
    (Linf Lred Lsubs)R←⍵ wise Nsubs          ⍝       ⌿ \     / \
    ll lr←⍵ wise Lsubs                       ⍝      L   R  ll   N_
    N_←Ninf Nred(⍵ wise lr R)                ⍝     / \         / \
    Linf Lred(⍵ wise ll N_)                  ⍝   ll   lr     lr   R
  }                                          ⍝ :: t ∇ r → t

  sub←{                                      ⍝ apply ⍺⍺ to subtree ⍵ of tree ⍺.
    inf red subs←⍺                           ⍝ node info and subs.
    lft rgt←⍵ wise subs                      ⍝ right subtree is target.
    sub←⍺⍺ rgt                               ⍝ operation on right subtree.
    inf red(⍵ wise lft sub)                  ⍝ reassembled tree.
    }

  isred←{                                    ⍝ colour for subtree ⍺, path ⍵.
    inf col(lft rgt)←⍺                       ⍝ (possibly null) node colour and subs.
    ⍵≡⍬:col                                  ⍝ colour of (possibly null) node.
    (⊃⌽(⊃⍵)wise lft rgt)∇ 1↓⍵                ⍝ examine left or right subtree.
  }                                          ⍝ :: t ∇ p → red

  wise←{(2×⍺)↑3⍴⍵}                           ⍝ parameterise direction.
  flip←{kv b lr←⍵ ⋄ kv(~b)lr}                ⍝ flip colour of node ⍵.
  done←{⍺ ⍵}∘0 0 0                           ⍝ node with long path.
  base←{⍺ ⍵}∘⍬                               ⍝ new child: node with null path.
  dblk←{⍺ ⍵}∘(,0)                            ⍝ double black node.

  op←⍶                                       ⍝ operand label.
  
  '∪'≡op:root ⍺ ins ⍵                        ⍝ insert/replace value in tree.
  '~'≡op:root ⍺ rem ⍵ 0                      ⍝ remove key=value from tree.
  '⍎'≡op:⍵ get ⍺                             ⍝ search for value for key.
  '⍕'≡op:fmt ⍵                               ⍝ formatted tree.
  '∊'≡op:vec ⍵                               ⍝ vector of key=value pairs.
  '?'≡op:4↑0 chk ⍵                           ⍝ tree stats: ok size mean_depth height.
}
  
⍝ From http://dfns.dyalog.com/c_splay.htm

splay ← { ⎕IO←0                              ⍝ Splay trees.

  wise←{(2×⍶)↑3⍴⍵}                           ⍝ parameterise direction.
  
  put←{                                      ⍝ tree ⍺ with key=value ⍵.
    ⍺≡0:⍵(0 0)                               ⍝ null: new leaf.
    ((nxt _)subs)(key _)←⍺ ⍵                 ⍝ node info and subtrees.
    nxt≡key:⍵ subs                           ⍝ match: new value.
    ⍺ ∇ search ⍵                             ⍝ natch: search subtrees.
  }                                          ⍝ :: t ∇ k v → t

  rem←{                                      ⍝ tree ⍺ without key ⍵.
    ⍺≡0:0                                    ⍝ null: key not in tree: no change.
    ((nxt _)subs)(key _)←⍺ ⍵                 ⍝ node and search key.
    ~nxt≡key:⍺ ∇ search ⍵                    ⍝ natch: remove from subtree.
    0 0≡subs:0                               ⍝ leaf: replace with null.
    0∊subs:(subs⍳0)⊃⌽subs                    ⍝ one null: use the other.
    (⍺ rot 1)∇ ⍵                             ⍝ neither: remove from rotated tree.
  }                                          ⍝ :: t ∇ k → t

  search←{                                   ⍝ search subtree ⍺ for key ⊃⍵.
    inf(lft rgt)←⍺                           ⍝ parts of node.
    dir←1-2×>/⍋↑⊃¨inf ⍵                      ⍝ search direction: ¯1 1
    _ nxt←dir wise lft rgt                   ⍝ nxt subtree to search.
    sub←nxt ⍺⍺ ⍵                             ⍝ new subtree.
    inf(dir wise lft sub rgt)                ⍝ new node.
  }                                          ⍝ :: t (t ∇ k _ → t) ∇∇ k _ → t

  get←{                                      ⍝ value for key ⍵ from tree ⍺.
    ⍺≡0:0 0 0                                ⍝ null: no value.
    (key val)(lft rgt)←⍺                     ⍝ parts of node.
    key≡⍵:val ⍺ ⍬                            ⍝ match: value tree path.
    dir←1-2×>/⍋↑key ⍵                        ⍝ search direction: ¯1 1
    _ nxt←dir wise lft rgt                   ⍝ next subtree to search.
    rslt sub path←nxt ∇ ⍵                    ⍝ value, subtree and path to value.
    ∆path←dir,path                           ⍝ extended path to target.
    cand←(key val)(lft sub rgt)              ⍝ node info and subtree candidates.
    tree←(dir wise\cand)bal ∆path            ⍝ possibly rebalanced tree.
    rslt tree ∆path                          ⍝ value, tree and path to value.
  }                                          ⍝ :: t ∇ k → v t p

  bal←{                                      ⍝ improve balance of tree.
    2≠⍴⍵:⍺                                   ⍝ not grandchild: continue.
    pos neg←1 ¯1×⊃⍵                          ⍝ +/- rotation direction.
    =/⍵:(⍺ rot neg)rot neg                   ⍝ same dirns: double ¯⍵-rotation.
    C(BpA s)←neg wise\⍺                      ⍝ diff dirns:
    ∆C←neg wise\C((BpA rot pos)s)            ⍝ aaa
    ∆C rot neg                               ⍝   ¯⍵-⍵-rotation.       (see notes)
  }                                          ⍝ :: t ∇ p → t

  rot←{                                      ⍝ single ⍵-wise rotation of tree ⍺.
    B(Apq r)←⍵ wise\⍺                        ⍝       B       A
    A(p q)←⍵ wise\Apq                        ⍝      / \  →  / \
                                             ⍝     A   r   p   B
    Bqr←⍵ wise\B(q r)                        ⍝    / \         / \
    ⍵ wise\A(p Bqr)                          ⍝   p   q       q   r
  }                                          ⍝ :: t ∇ r → t

  vec←{                                      ⍝ vector of key=value pairs.
    ⍵≡0:⍬                                    ⍝ null tree: null vector.
    key_val(lft rgt)←⍵                       ⍝ key=val and subtrees.
    (∇ lft),(⊂key_val),∇ rgt                 ⍝ left_vec, key=val, right_vec.
  }                                          ⍝ :: ∇ t → [k v]

  lift←{                                     ⍝ lift child of root.
    val root path←⍵                          ⍝ val and revised tree.
    0∊path:                                  ⍝ no value: quit.
    1≠⍴path:val root                         ⍝ val not child of root: done.
    val(root rot-⊃path)                      ⍝ rotate root.
  }                                          ⍝ :: ∇ v t p → v t

  fmt←{                                      ⍝ formatted tree ⍵.
    null←0 0⍴''                              ⍝ format of null tree.
    ⍵≡0:null                                 ⍝ null tree: null format.
    (key val)subs←⍵                          ⍝ node info.
    key_val←↑,/⍕¨key'='val                   ⍝ formatted key=value
    fmts←{⊖⍵}\'┌└'{                          ⍝ hang subtrees by ┌─ ─┐ branches.
      0 0≡⍴⍵:⍵                               ⍝ null: done.
      mask←∧\' '=⊃↓⌽⍉⍵                       ⍝ mask of leading blanks.
      ⍉⌽↑(⊂⌽⍺,mask/'│'),↓⌽⍉⍵                 ⍝ subtree suspended by branch.
    }¨{⊖⍵}\∇¨subs                            ⍝ formatted subtrees.
    case←~null null≡¨fmts                    ⍝ non-null subtree cases.
    join←(2⊥case)⊃'∘┐┘┤'                     ⍝ subtree joining char.
    join≡'∘':↑,↓key_val                      ⍝ leaf: done.
    dent←' '⊣¨key_val                        ⍝ subtree padding.
    pads←{↓↑,/dent,⊂⍵}¨fmts                  ⍝ left-padded subtrees.
    ↑↑{⍺,(↓key_val,join),⍵}/pads             ⍝ formatted tree.
  }                                          ⍝ :: ∇ t → [-;]

  dep←{                                      ⍝ depth of key ⍵ in tree ⍺.
    ⍺≡0:0                                    ⍝ key not found: failure.
    (key val)subs←⍺                          ⍝ parts of tree.
    key≡⍵:1                                  ⍝ key found: at depth 1.
    dir←1-2×>/⍋↑key ⍵                        ⍝ search direction: ¯1 1
    _ sub←dir wise subs                      ⍝ next subtree to search.
    {⍵+×⍵}sub ∇ ⍵                            ⍝ incremental depth.
  }                                          ⍝ :: t ∇ k → d

  chk←{                                      ⍝ tree stats / integrity check.
    0=≡⍵:(0≡⍵)0 0 0 ⍬                        ⍝ null: ok ht=0 sz=0 depth=0 range=⍬.
    (key _)subs←⍵                            ⍝ node info and subtrees.
    stats←(⍺+1)∇¨subs                        ⍝ subtree stats.
    oks szs dps hts krs←↓⍉↑stats             ⍝ individual stats.
    keys←↑key{⍺,(⊂⍶),⍵}/krs                  ⍝ subtree key ranges.
    okkey←{⍵≡⍳⍴⍵}⍋↑keys                      ⍝ left keys << key >> right keys.
    okstr←2 2≡(⍴⍵),⍴⊃⌽⍵                      ⍝ node struct is ok.
    ok←okkey∧okstr∧∧/oks                     ⍝ good tree.
    sz←1++/szs                               ⍝ subtree size.
    dp←⍺++/dps                               ⍝ total node depth.
    ht←1+⌈/hts                               ⍝ node height.
    kr←⌽2⍴¯1⌽keys                            ⍝ key range for subtree.
    ⍺>0:ok sz dp ht kr                       ⍝ subtree: ok size tot_dep height range.
    ok sz(⌊0.5+dp÷sz)ht                      ⍝ root: ok size mean_depth height.
  }                                          ⍝ :: ∇ t → y s d h {r}

  op←⍶ ⍝ op←⍺⍺{f←⍺⍺ ⋄ ⊃⎕CR'f'}0              ⍝ operand label.

  '∪'≡op:⍺ put ⍵                             ⍝ insert/replace value in tree.
  '⍎'≡op:lift ⍵ get ⍺                        ⍝ search for value for key.
  '~'≡op:⍺ rem ⍵ 0                           ⍝ remove key=value from tree.
  '?'≡op:4↑0 chk ⍵                           ⍝ tree stats: ok size depth height.
  '⍕'≡op:fmt ⍵                               ⍝ formatted tree.
  '∊'≡op:vec ⍵                               ⍝ list of key=value pairs.
  '≡'≡op:⍵ dep ⍺                             ⍝ depth of key ⍺ in tree ⍵.
}
  
⍝ From http://dfns.dyalog.com/c_tfmt.htm

tfmt ← {                                     ⍝ Char matrix from tree.
  ⍺←''                                       ⍝ default: no indentation.
  1=≡,⍵:↑,↓⍺,⍵                               ⍝ atom: indented leaf node.
  node←⍺,⊃⍵                                  ⍝ tree: node value,
  subs←(⍺,4↑'·')∘∇¨1↓⍵                       ⍝ subtrees.
  ↑(⊂node),↑,/↓¨subs                         ⍝ node followed by subtree rows.
}

⍝ From http://dfns.dyalog.com/c_tnest.htm

tnest ← {                                    ⍝ Array from TreeView style tree.
  dvec ivec←⍵                                ⍝ depth and items vectors.
  1=≢dvec:⊃ivec                              ⍝ atom: leaf value.
  node←1↑ivec                                ⍝ tree: node value.
  dsub isub←(1=dvec)∘⊂¨⍵                     ⍝ sub treeviews.
  node,∇¨↓⍉↑(dsub-1)isub                     ⍝ joined nested subtrees.
}

⍝ From http://dfns.dyalog.com/c_trav.htm

trav ← {                                     ⍝ Generic depth-first tree traversal.
  ⊃∇⍨/⌽(⊂⍺ ⍺⍺ ⍵),⍺ ⍵⍵ ⍵                      ⍝ visits parent before children.
}

⍝ From http://dfns.dyalog.com/c_ravt.htm

ravt ← {                                     ⍝ Generic depth-first tree traversal.
  (⊃∇⍨/⌽(⊂⍺),⍺ ⍵⍵ ⍵)⍺⍺ ⍵                     ⍝ visits children before parent.
}

⍝ From http://dfns.dyalog.com/c_tview.htm

tview ← {                                    ⍝ TreeView style tree from nested array.
  ⍺←0                                        ⍝ default depth 0.
  1=≡,⍵:⍺,⊂,⊂⍵                               ⍝ atom: leaf node depth and value.
  node←⍺,⊂1↑⍵                                ⍝ tree:      node depth and value.
  subs←(⍺+1)∇¨1↓⍵                            ⍝ sub-trees.
  ,⌿node⍪↑subs                               ⍝ node together with subtrees.
}
