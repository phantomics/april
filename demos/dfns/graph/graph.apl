⍝⍝ Ported from Dyalog's dfns at http://dfns.dyalog.com/n_Graphs.htm into April APL


⍝⍝ Graph processing

⍝ From http://dfns.dyalog.com/c_assign.htm

assign ← {                                   ⍝ Hungarian method cost assignment.

  step0←{step1(⌽⌈\⌽⍴⍵)↑⍵}                    ⍝ 0: at least as many rows as cols.

  step1←{step2↑(↓⍵)-⌊/⍵}                     ⍝ 1: reduce rows by minimum value.

  step2←{                                    ⍝ 2: mark independent zeros.
    stars←{                                  ⍝ independent zeros.
      ~1∊⍵:⍺                                 ⍝ no more zeros: done.
      next←<\<⍀⍵                             ⍝ more independent zeros.
      mask←(rows next)∨cols next             ⍝ mask of dependent rows and cols.
      (⍺∨next)∇ ⍵>mask                       ⍝ ⍺-accumulated star matrix.
    }                                        ⍝
    zeros←{⍵+0 stars ⍵}⍵=0                   ⍝ 1=>zero, 2=>independent zero.
    step3 ⍵ zeros                            ⍝ next step: 3.
  }

  step3←{costs zeros←⍵                       ⍝ 3: cover cols with starred zeros.
    stars←zeros=2                            ⍝ starred zeros.
    covers←2×cols stars                      ⍝ covered cols.
    ~0∊covers:stars                          ⍝ all cols covered: solution.
    step4 costs zeros covers                 ⍝ next step: 4.
  }

  step4←{costs zeros covers←⍵                ⍝ 4: adjust covering lines.
    mask←covers=0                            ⍝ mask of uncovered elements.
    open←1=mask×zeros                        ⍝ uncovered zeros.
    ~1∊open:(⌊/(,mask)/,costs)step6 ⍵        ⍝ no uncovered zeros, next step :6.
    prime←first open                         ⍝ choose first uncovered zero.
    prow←rows prime                          ⍝ row containing prime.
    star←2=zeros×prow                        ⍝ star in row containing prime.
    ~1∊star:prime step5{                     ⍝ no star in row, next step :5,
      costs ⍵ prime                          ⍝ adjusted zeros matrix,
    }zeros+2×prime                           ⍝ new primed zero (3).
    cnext←covers+prow-2×cols star            ⍝ adjusted covers.
    znext←zeros⌈3×prime                      ⍝ primed zero.
    ∇ costs znext cnext                      ⍝ adjusted zeros and covers
  }

  step5←{costs zeros prime←⍵                 ⍝ 5: exchange starred zeros.
    star←(cols prime)∧zeros=2                ⍝ next star.
    ~1∊star:step3 ⍺{                         ⍝ no stars: next step :3.
      {costs ⍵}{⍵-2×⍵=3}⍵-⍺∧⍵>1              ⍝ unstarred stars; starred primes.
    }zeros                                   ⍝ adjusted zero markers.
    pnext←(rows star)∧zeros=3                ⍝ next prime.
    (⍺∨pnext∨star)∇ costs zeros pnext        ⍝ ⍺-accumulated prime-star-··· path.
  }

  step6←{costs zeros covers←⍵                ⍝ 6: adjust cost matrix.
    cnext←costs+⍺×¯1 1+.×0 3∘.=covers        ⍝ add and subtract minimum value.
    znext←zeros+(×costs)-×cnext              ⍝ amended zeros marker.
    step4 cnext znext covers                 ⍝ next step: 4.
  }

  rows←{(⍴⍵)⍴(⊃⌽⍴⍵)/∨/⍵}                     ⍝ row propagation.
  cols←{(⍴⍵)⍴∨⌿⍵}                            ⍝ column propagation.
  first←{(⍴⍵)⍴<\,⍵}                          ⍝ first 1 in bool matrix.

  (⍴⍵)↑step0 ⍵                               ⍝ start with step 0.
}

⍝ From http://dfns.dyalog.com/n_alists.htm

gperm ← { (⊂⍵)⍳¨⍺[⍵] }                       ⍝ ⍵-permutation of vertices of graph ⍺.

⍝ From http://dfns.dyalog.com/n_insnode.htm

insnode ← {                                  ⍝ Insert vertex ⍵ in graph ⍺.
  (⍵⌈⍴⍺)↑⍺,⍵⍴⊂⍬                              ⍝ extend graph with sufficient nulls.
}

⍝ From http://dfns.dyalog.com/n_remnode.htm

remnode ← {                                  ⍝ Remove vertex ⍵ from graph ⍺.
  new←(⍵≠⍳⍴⍺)/⍺~¨⍵                           ⍝ graph with vertex ⍵ removed,
  new-new>⍵                                  ⍝ and edges adjusted.
}

⍝ From http://dfns.dyalog.com/n_inslink.htm

inslink ← {                                  ⍝ Graph ⍺ with new edge ⍵.
  fm to←⍵                                    ⍝ edge
  ∪∘to¨@fm⊢⍺                                 ⍝ graph with new edge ⍵.
}

⍝ From http://dfns.dyalog.com/n_remlink.htm

remlink ← {                                  ⍝ Graph ⍺ without edge ⍵.
  fm to←⍵                                    ⍝ edge
  ~∘to¨@fm⊢⍺                                 ⍝ graph without edge ⍵.
}

⍝ From http://dfns.dyalog.com/n_search.htm

search ← {                                   ⍝ Breadth-first search of graph ⍺.
  graph←⍺                                    ⍝ ⍺ is graph vector.
  ⍵{                                         ⍝ from starting vertex.
    ⍵≡⍬:⍺                                    ⍝ no unvisited vertices: done.
    adjv←⍵⊃¨⊂graph                           ⍝ adjacent vertices.
    next←∪(↑,/adjv)~⍺                        ⍝ unvisited vertices.
    (⍺,next)∇ next                           ⍝ advance wave of visited vertices.
  }⍵                                         ⍝ from starting vertex.
}

⍝ From http://dfns.dyalog.com/n_path.htm

path ← {                                       ⍝ Shortest path from/to ⍵ in graph ⍺.
  graph(fm to)←⍺ ⍵                             ⍝ graph and entry/exit vertex vectors
  fm{                                          ⍝ fm is the starting-from vertex
    ⍺≡⍬:⍬                                      ⍝ no vertices left: no path
    ∨/to∊⍺:⍬(⊃∘⍵){                             ⍝ found target: path from tree:
      ⍵<0:⍺                                    ⍝ root: finished
      (⍵,⍺)∇ ⍺⍺ ⍵                              ⍝ accumulated path to next vertex
    }1↑⍺∩to                                    ⍝ found vertex ⍺
    next←graph[⍺]∩¨⊂⍸⍵=¯2                      ⍝ next vertices to visit
    back←⊃,/⍺+0×next                           ⍝ back links
    wave←⊃,/next                               ⍝ vertex wave front
    (∪wave)∇ back@wave⊢⍵                       ⍝ advanced wave front
  }¯2+(⍳⍴⍺)∊fm                                 ⍝ null spanning tree
}

⍝ From http://dfns.dyalog.com/n_span.htm

span ← {                                       ⍝ Breadth-first spanning tree for graph ⍺.
  graph←⍺                                      ⍝ ⍺ is graph vector.
  (¯2+(⍳⍴⍺)∊⍵){                                ⍝ ⍺: partial spanning tree.
    ⍵≡⍬:⍺                                      ⍝ no vertices: done.
    next←graph[⍵]∩¨⊂⍸⍺=¯2                      ⍝ untravelled edges
    back←⍵+0×next                              ⍝ back link per edge
    tree←(∊back)@(∊next)⊢⍺                     ⍝ partial spanning tree
    tree ∇∪∊next                               ⍝ advanced wave front
  }⍵                                           ⍝ ⍵: next wave of vertices to visit.
}

⍝ From http://dfns.dyalog.com/c_dfspan.htm

dfspan ← {                                   ⍝ Depth-first spanning tree: graph ⍺ from vertex ⍵.
  graph←⍺                                    ⍝ ⍺ is graph vector.
  trav←{                                     ⍝ initial vertex and parent
    ¯2≠⍺⊃⍵:⍵                                 ⍝ vertex visited: backtrack
    next←⌽⍺⊃graph                            ⍝ edges from vertex ⍺
    tree←⍶@⍺⊢⍵                               ⍝ ⍺⍺ is ⍺'s parent
    ⊃⍺ ∇∇/next,⊂tree                         ⍝ visiting each edge in order
  }                                          ⍝ :: tree ← vtx (vtx ∇∇) tree
  ⍵(¯1 trav)¯2⊣¨⍺                            ⍝ depth-first traversal of graph ⍵
}

⍝ From http://dfns.dyalog.com/c_dsp.htm

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
      rows←(mrs/¨'│')join¨↓dims↑¨subs        ⍝ complete rows with '│'-separated items
      hzs←'┼'join mcs/¨'─'                   ⍝ inter-row horizontal '─┼─' separators
      cells←{⍺⍪hzs⍪⍵}/rows                   ⍝ joined rows: array of 2D planes
      gaps←(⌽⍳¯2+⍴⍴⍵)/¨' '                   ⍝ increasing cell gaps for higher ranks
      cjoin←{↑⍪/(⊂⍺),⍶,⊂⍵}                   ⍝ vertical cell join with ⍺⍺ gap
      top⊃↑{⍺ cjoin⌿⍵}/gaps,⊂cells           ⍝ cell-joining with increasing gaps
   ]]
}

⍝ From http://dfns.dyalog.com/s_scc.htm

show ← {↑(⍕¨⍳⍴⍵),¨' → '∘,¨⍕¨⍵}

pmat ← {                                     ⍝ Permutation matrix of ⍳⍵.
  {1≥⍴⍵ : ↑,↓⍵ ⋄ ↑⍪/⍵,∘∇¨⍵∘~¨⍵}⍳⍵            ⍝ short vector: done, else items prefix sub-perms of remainder.
}     

⍝ From http://dfns.dyalog.com/c_scc.htm

scc ← {                                      ⍝ Strongly connected components.
                                             ⍝ (Tarjan)
  T←(3/⊂0⊣¨G←⍵),1 ⍬                          ⍝ state tuple T :: C L X x S
  C L X x S←⍳⍴T                              ⍝ access names for items of tuple T

  ⍝ put←{(⍹⊃⍵)⊣@(⊂⍶ ⍺)⊢⍵}                    ⍝ ⍹ at ⍺ in field ⍶ of ⍵
  put←{(⍹⊃⍵)@(⊂⍶ ⍺)⊢⍵}                       ⍝ ⍹ at ⍺ in field ⍶ of ⍵
  Lx←L put x                                 ⍝ ⍺ at x in lowlink vec :: T ← ⍺ ∇ T
  Xx←X put x                                 ⍝ ⍺ at x in indices vec :: T ← ⍺ ∇ T
  x1←{1+@x⊢⍵}                                ⍝ successor of index x  :: T ←   ∇ T
  push←,¨@S                                  ⍝ ⍺ pushed onto stack   :: T ← ⍺ ∇ T
  
  ⍺←0 ⋄ trace←{⍵⊣⎕←0 dsp ⍺,⍵}⍣(⍺≢0)          ⍝ ⍺: optional tracing   :: T ←   ∇ T  

  comp←{ v←⍺                                 ⍝ strongly connected component
    pops←1++/∧\v≠stk←S⊃⍵                     ⍝ number of connected comps on stack
    C∆←((1+⌈/C⊃⍵))@(pops↑stk)⊢C⊃⍵            ⍝ extended strongly connected comps
    ((pops↓stk)C∆)@S C⊢⍵                     ⍝ reduced stack; extended comps
  }                                          ⍝ :: T ← v ∇ T

  conn←{ v←⍺                                 ⍝ connection of vertex v
    T0←v trace ⍵                             ⍝ optional tracing
    T1←x1 v push v Lx v Xx T0                ⍝ successor state for x S L and X
    T2←↑{w←⍺                                 ⍝ edge v → w
      min_L←{(⍺ w⊃⍵)⌊@(⊂L v)⊢⍵}              ⍝ L[v] ⌊← ⍺[w]
      0=X w⊃⍵:L min_L w conn ⍵               ⍝ w not connected: depth-first trav
      X min_L⍣(w∊S⊃⍵)⊢⍵                      ⍝ low-link if w on stack
    }/(⌽v⊃G),⊂T1                             ⍝ for each edge from vertex v
    root←(L v⊃T2)=X v⊃T2                     ⍝ is a root vertex?
    v comp⍣root⊢T2                           ⍝ new component if root
  }                                          ⍝ :: T ← v ∇ T

  loop←{                                     ⍝ for each vertex in graph G
    vert←{⍺ conn⍣(0=X ⍺⊃⍵)⊢⍵}                ⍝ connection of unvisited vertex ⍺
    ⊃vert/(⌽⍳⍴G),⊂⍵                          ⍝   for each vertex in G
  }                                          ⍝ :: T ← ∇ T

  (∪⍳⊢)C⊃loop T                              ⍝ for each vertex

  ⍝ T :: C L X x S                           ⍝ state tuple
  ⍝ C :: [x]                                 ⍝ connected components vector
  ⍝ L :: [x]                                 ⍝ low-link vector
  ⍝ X :: [x]                                 ⍝ indices vector
  ⍝ x ::  #                                  ⍝ index of vertex in graph G
  ⍝ S :: [x]                                 ⍝ stack of vertices
}

⍝ From http://dfns.dyalog.com/s_scc.htm

cond ← {                                     ⍝ Condensation of graph ⍵.
  c←scc ⍵                                    ⍝ strongly connected components
  v←{⊂⍵}⌸c                                   ⍝ component-grouped vertex indices
  e←c{⊂⍵}⌸⍵                                  ⍝   ..      ..      edges
  x←∪¨(∊¨e)~¨v                               ⍝ out-of-component edges
  m←↓∨/¨x∘.∊v                                ⍝ masks of remote vertices
  g←m/¨⊂⍳⍴v                                  ⍝ condensed DAG
  g v                                        ⍝ ... and contracted vertices
}

⍝ From http://dfns.dyalog.com/c_stdists.htm

stdists ← {                                  ⍝ Spanning-tree path lengths.
  tree←⍵                                     ⍝ spanning tree
  0{                                         ⍝ distance from root
    next dvec←⍵                              ⍝ chldren and distance vector
    next≡⍬:dvec                              ⍝ no children: finished
    ∆dvec←⍺@next⊢dvec                        ⍝ extended distance vector
    ∆next←⍸tree∊next                         ⍝ grandchildren
    (⍺+1)∇ ∆next ∆dvec                       ⍝ examine rest of tree
  }(⍵⍳¯1)(⍵⊢¨¯1)                             ⍝ starting vertex and initial distances
}

⍝ From http://dfns.dyalog.com/n_stpath.htm

stpath ← {                                   ⍝ Path through spanning tree ⍺ to vertex ⍵.
  tree←⍺                                     ⍝ (partial) spanning tree.
  ⍬{                                         ⍝ path accumulator.
    ⍵<0:(⍵=¯2)↓⍺                             ⍝ root or unvisited vertex: finished.
    (⍵,⍺)∇ ⍵⊃tree                            ⍝ otherwise: prefix previous (parent) vertex.
  }⍵
}

⍝ From http://dfns.dyalog.com/c_stpaths.htm

stpaths ← {                                  ⍝ Spanning tree paths.
  tree←⍵                                     ⍝ spanning tree.
  root←⍵⍳¯1                                  ⍝ index of root vertex.
  paths←(root=⍳⍴⍵)↑¨root                     ⍝ initial path vector.
  paths{                                     ⍝ path to current vertices.
    next←(⍵=⊂tree)/¨⊂⍳⍴tree                  ⍝ vertices at next tree level.
    (⊂⍬)∧.≡next:⍺                            ⍝ all null: finished.
    exts←(⊂¨⍵⊃¨⊂⍺),¨¨next                    ⍝ paths to next tree level.
    indx←↑,/next                             ⍝ accumulated indices.
    paths←(↑,/exts)@indx⊢⍺                   ⍝ set next level paths in paths vector.
    paths ∇ indx                             ⍝ advance to next tree level.
  }root                                      ⍝ index of starting vertex.
}

⍝ From http://dfns.dyalog.com/c_X.htm

X ← {                                        ⍝ Exact cover: Knuth's Algorithm X.
  ⍺←1∨.∨⍵                                    ⍝ all cols required by default.
  x←⍳⍴⍺                                      ⍝ column indices.
  d←(x~⍺/x)∘.=x                              ⍝ dummy rows for optional columns.
  z←{                                        ⍝ cover vector.
    r c←⍴⍵                                   ⍝ number of rows and columns.
    c=0:r⍴0                                  ⍝ empty matrix: success.
    n←+⌿⍵                                    ⍝ number of covers per column.
    f←(<\n=⌊/n)/⍵                            ⍝ first column with fewest 1s.
    ⍵ ∇{                                     ⍝ ⍺ is constraint matrix.
      ~1∊⍵:0                                 ⍝ no rows: failure.
      f←<\⍵                                  ⍝ first row selector.
      c←,f⌿⍺                                 ⍝ cols selected by first row.
      r←∨/c/⍺                                ⍝ rows covered by selected cols.
      s←⍺⍺(~c)/(~r)⌿⍺                        ⍝ sub-matrix covers.
      s≡0:⍺ ∇ f<⍵                            ⍝ failure: try a different row.
      f∨(~r)\s                               ⍝ success: row f included.
    },f                                      ⍝ ⍵ is vector of marked rows.
  }⍵⍪d                                       ⍝ exact cover.
  z≡0:0                                      ⍝ failure: 0
  (-+/~⍺)↓z                                  ⍝ without dummy rows.
}

⍝ From http://dfns.dyalog.com/s_X.htm

sudokuMatrix ← {                             ⍝ Matrix for ⍵ ⍵-Sudoku puzzle.
  z←,[⍳6],[6+⍳4]⍳10⍴⌊⍵*÷2                    ⍝ cell coordinate properties.
  row←↓1 1 0 0 1 1 1 1 1 1/↑z                ⍝ each row must contain each number.
  col←↓0 0 1 1 1 1 1 1 1 1/↑z                ⍝   .. col  ..  ..  ..  ..  ..  ..
  box←↓1 0 1 0 1 1 1 1 1 1/↑z                ⍝   .. box  ..  ..  ..  ..  ..  ..
  all←↓1 1 1 1 0 0 1 1 1 1/↑z                ⍝ each cell must contain a number.
  same←≡/∘(1 0 0 0 1 0 0 0∘⊂)                ⍝ matching pairs.
  same¨row,col,box,all                       ⍝ constraints matrix for ⍵ ⍵-puzzle.
}

sudokuX ← { ⎕IO←1                            ⍝ Exact cover Sudoku solver.
  n n←⍴⍵                                     ⍝ n×n puzzle.
  ⍺←sudokuMatrix n                           ⍝ generic ⍵×⍵ constraint matrix.
  r←∊(⍵≠0)>(⊂⍳n)∊¨⍵                          ⍝ already placed rows.
  m←(~r)⌿⍺                                   ⍝ reduced matrix.
  f←X m                                      ⍝ exact cover.
  z←(~r)\f                                   ⍝ merge of placements.
  n n⍴z/(⍴z)⍴⍳n                              ⍝ solution matrix.
}

queensX ← {                                  ⍝ Exact cover N-Queens.
  m←⍳3/⍵                                     ⍝ cell coordinate properties.
  r←=/¨1 0 1∘/¨m                             ⍝ each rank must contain one queen.
  f←=/¨0 1 1∘/¨m                             ⍝  ..  file  ..     ..      ..
  
  dm←-/¨⍳2/⍵                                 ⍝ diagonals.
  du←{⍵[⍋⍵]}∪,dm                             ⍝ unique diagnonals.
  x←dm∘.=du                                  ⍝ left diagonals.
  y←(⊖dm)∘.=du                               ⍝ right diagonals.
  
  m←,[⍳2]x,y,r,f                             ⍝ constraints matrix.
  d←~(⍳1↓⍴m)∊⍳2×⍴du                          ⍝ mask of required cols.
  ⍵ ⍵⍴d X m                                  ⍝ exact cover - matrix of queens.
}


⍝⍝ Weighted graph processing

wcost ← {                                    ⍝ Cost vector for path ⍵ through weighted graph ⍺.
  graph costs←↓⍺                             ⍝ edges and edge-costs.
  2>≢⍵:0                                     ⍝ null path: no cost.
  2{                                         ⍝ ⍺:from, ⍵:to.
    node←,⍺⊃graph                            ⍝ exits from vertex.
    indx←node⍳⍵                              ⍝ index of (⍺ ⍵) vertex.
    indx⊃,⍺⊃costs                            ⍝ ... associated cost.
  }/⍵                                        ⍝ pairwise along path.
}

⍝ From http://dfns.dyalog.com/c_wpath.htm

wpath ← {                                    ⍝ Quickest path fm/to ⍵ in weighted graph ⍺.
  graph costs←↓⍺                             ⍝ graph structure and costs
  fm to←⍵                                    ⍝ start and ending vertices
  tree←¯1⊣¨graph                             ⍝ initial spanning tree
  cost←(⌊/⍬)×fm≠⍳⍴costs                      ⍝ cost to reach each vertex
  I←⊃¨∘⊂                                     ⍝ helper function: ⍺th items of ⍵
  fm{                                        ⍝ from starting vertex.
    acc to←⍵                                 ⍝ accumulator and next vertex
    to<0:(to=¯2)↓acc                         ⍝ root or unvisited vertex: finished
    ⍺ ∇(to,acc)(to⊃⍺)                        ⍝ otherwise: parent vertex prefix
  }{                                         ⍝ lowest spanning cost tree:
    tree cost←⍵                              ⍝ current tree and cost vectors
    ⍺≡⍬:tree ⍺⍺ ⍬ to                         ⍝ all vertices visited: done
    adjv←⍺ I graph                           ⍝ adjacent vertices
    accm←⍺ I cost+costs                      ⍝ costs to adjacent vertices
    best←adjv I¨⊂cost                        ⍝ costs to beat
    mask←accm<best⌊to⊃cost                   ⍝ mask of better routes
    next←mask/¨adjv                          ⍝ next vertices to visit
    back←⊃,/⍺+0×next                         ⍝ back links to parent nodes
    cvec←⊃,/mask/¨accm                       ⍝ cost vector
    decr←{(⍒cvec)I ⍵}                        ⍝ in decreasing order of cost
    wave←decr↑,/next                         ⍝ vertex wave front
    new←back cvec decr⍨@wave¨⍵               ⍝ successor tree & cost
    (∪wave)∇ new                             ⍝ wave spreads to adjacent vertices
  }tree cost                                 ⍝ initial spanning tree and cost vectors
}

⍝ From http://dfns.dyalog.com/n_wspan.htm

wspan ← {                                    ⍝ Spanning tree for weighted graph ⍺ from ⍵.
  graph costs←↓⍺                             ⍝ graph structure and costs.
  tree←¯1⊣¨graph                             ⍝ initial spanning tree.
  cost←(⌊/⍬)×⍵≠⍳⍴costs                       ⍝ cost to reach each vertex.
  I←⊃¨∘⊂                                     ⍝ helper function: ⍺th items of ⍵
  ⍵{                                         ⍝ from starting vertex.
    tree cost←⍵                              ⍝ current tree and costs
    ⍺≡⍬:tree                                 ⍝ all vertices visited: done
    adjv←⍺ I graph                           ⍝ adjacent vertices
    accm←⍺ I cost+costs                      ⍝ cumulative cost via this vertex
    mask←accm<adjv I¨⊂cost                   ⍝ mask of better routes
    cvec←⊃,/mask/¨accm                       ⍝ cost vector
    next←mask/¨adjv                          ⍝ next vertices to visit
    back←⊃,/⍺+0×next                         ⍝ back links to parent node
    decr←(⍒cvec)I⊢                           ⍝ in decreasing order of cost
    wave←decr↑,/next                         ⍝ vertex wave front
    new←back cvec decr⍨@wave¨⍵               ⍝ successor tree & cost
    (∪wave)∇ new                             ⍝ wave spreads to adjacent vertices
  }tree cost                                 ⍝ initial spanning tree and cost vectors
}

⍝ From http://dfns.dyalog.com/s_wmst.htm

wmst ← {                                     ⍝ Minimum Spanning Tree for wu-graph ⍺.
  graph costs←↓⍺                             ⍝ weighted, undirected graph
  xvec←⍳⍴graph                               ⍝ index vector for graph
  ⍵{                                         ⍝ vertices inside tree: T
    tree todo←⍵                              ⍝ partial tree and unconnected vertices
    todo≡⍬:tree                              ⍝ all vertices connected: finished
    edges←(graph∊¨⊂todo)∧xvec∊⍺              ⍝ edges from T to G~T
    min←⌊/⌊/¨edges/¨costs                    ⍝ minimum edge cost
    masks←edges∧min=costs                    ⍝ lowest cost edge masks
    fm to←{↑,/masks/¨⍵}¨xvec graph           ⍝ lowest cost edges from T to G~T
    fm≡⍬:tree                                ⍝ disjoint graph: quit
    (⍺,to)∇(fm@to⊢tree)(todo~to)             ⍝ vertices from G~T to T
  }(¯1⊣¨graph)(xvec~⍵)                       ⍝ initial tree and unconnected vertices
}
