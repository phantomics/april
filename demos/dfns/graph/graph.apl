⍝⍝ Ported from http://dfns.dyalog.com/n_Graphs.htm into April APL


⍝⍝ Graph processing

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
    $[⍵≡⍬;⍺;                                 ⍝ no unvisited vertices: done.
      adjv←⍵⊃¨⊂graph                         ⍝ adjacent vertices.
      next←∪(↑,/adjv)~⍺                      ⍝ unvisited vertices.
      (⍺,next)∇ next                         ⍝ advance wave of visited vertices.
     ]
  }⍵                                         ⍝ from starting vertex.
}

⍝ From http://dfns.dyalog.com/n_path.htm

path ← {                                     ⍝ Shortest path from/to ⍵ in graph ⍺.
  graph (fm to)←⍺ ⍵                          ⍝ graph and entry/exit vertex vectors
  fm {                                       ⍝ fm is the starting-from vertex
    $[⍺≡⍬;⍬;                                 ⍝ no vertices left: no path
      $[∨/to∊⍺;                              ⍝ found target: path from tree:
        ⍬(⊃∘⍵) { $[⍵<0;⍺;                    ⍝ root: finished
                   (⍵,⍺) ∇ ⍺⍺ ⍵]             ⍝ accumulated path to next vertex
               } 1↑⍺∩to;                     ⍝ found vertex ⍺
        next←graph[⍺]∩¨⊂⍸⍵=¯2                ⍝ next vertices to visit
        back←⊃,/⍺+0×next                     ⍝ back links
        wave←⊃,/next                         ⍝ vertex wave front
        (∪wave) ∇ back@wave⊢⍵]]              ⍝ advanced wave front
  }¯2+(⍳⍴⍺)∊fm                               ⍝ null spanning tree
}

⍝ From http://dfns.dyalog.com/n_span.htm

span ← {                                     ⍝ Breadth-first spanning tree for graph ⍺.
  graph←⍺                                    ⍝ ⍺ is graph vector.
  (¯2+(⍳⍴⍺)∊⍵) {                             ⍝ ⍺: partial spanning tree.
    $[⍵≡⍬;⍺;                                 ⍝ no vertices: done.
      next←graph[⍵]∩¨⊂⍸⍺=¯2                  ⍝ untravelled edges
      back←⍵+0×next                          ⍝ back link per edge
      tree←(∊back)@(∊next)⊢⍺                 ⍝ partial spanning tree
      tree ∇∪∊next                           ⍝ advanced wave front
     ]
  }⍵                                         ⍝ ⍵: next wave of vertices to visit.
}

⍝ From http://dfns.dyalog.com/c_dfspan.htm

dfspan ← {                                   ⍝ Depth-first spanning tree: graph ⍺ from vertex ⍵.
  graph←⍺                                    ⍝ ⍺ is graph vector.
  trav ← {                                   ⍝ initial vertex and parent
    $[¯2≠⍺⊃⍵;⍵;                              ⍝ vertex visited: backtrack
      next←⌽⍺⊃graph                          ⍝ edges from vertex ⍺
      tree←⍶@⍺⊢⍵                             ⍝ ⍶ is ⍺'s parent
      ⊃⍺ ∇∇/next,⊂tree                       ⍝ visiting each edge in order
     ]
  }                                          ⍝ :: tree ← vtx (vtx ∇∇) tree
  ⍵(¯1 trav)¯2⊣¨⍺                            ⍝ depth-first traversal of graph ⍵
}

⍝ From http://dfns.dyalog.com/c_stdists.htm

stdists ← {                                  ⍝ Spanning-tree path lengths.
  tree←⍵                                     ⍝ spanning tree
  0{                                         ⍝ distance from root
    next dvec←⍵                              ⍝ chldren and distance vector
    $[next≡⍬;dvec;                           ⍝ no children: finished
      ∆dvec←⍺@next⊢dvec                      ⍝ extended distance vector
      ∆next←⍸tree∊next                       ⍝ grandchildren
      (⍺+1)∇ ∆next ∆dvec                     ⍝ examine rest of tree
     ]
  }(⍵⍳¯1)(⍵⊢¨¯1)                             ⍝ starting vertex and initial distances
}

⍝ From http://dfns.dyalog.com/n_stpath.htm

stpath ← {                                   ⍝ Path through spanning tree ⍺ to vertex ⍵.
  tree←⍺                                     ⍝ (partial) spanning tree.
  ⍬{                                         ⍝ path accumulator.
    $[⍵<0;(⍵=¯2)↓⍺;                          ⍝ root or unvisited vertex: finished.
      (⍵,⍺)∇ ⍵⊃tree                          ⍝ otherwise: prefix previous (parent) vertex.
     ]
  }⍵
}

⍝ From http://dfns.dyalog.com/c_stpaths.htm

stpaths ← {                                  ⍝ Spanning tree paths.
  tree←⍵                                     ⍝ spanning tree.
  root←⍵⍳¯1                                  ⍝ index of root vertex.
  paths←(root=⍳⍴⍵)↑¨root                     ⍝ initial path vector.

  paths{                                     ⍝ path to current vertices.
    next←(⍵=⊂tree)/¨⊂⍳⍴tree                  ⍝ vertices at next tree level.
    $[(⊂⍬)∧.≡next;⍺;                         ⍝ all null: finished.
      exts←(⊂¨⍵⊃¨⊂⍺),¨¨next                  ⍝ paths to next tree level.
      indx←↑,/next                           ⍝ accumulated indices.
      paths←(↑,/exts)@indx⊢⍺                 ⍝ set next level paths in paths vector.
      paths ∇ indx                           ⍝ advance to next tree level.
     ]
  }root                                      ⍝ index of starting vertex.
}

⍝ From http://dfns.dyalog.com/c_X.htm

X ← {                                        ⍝ Exact cover: Knuth's Algorithm X.
  ⍺←1∨.∨⍵                                    ⍝ all cols required by default.
  x←⍳⍴⍺                                      ⍝ column indices.
  d←(x~⍺/x)∘.=x                              ⍝ dummy rows for optional columns.
  z←{                                        ⍝ cover vector.
    r c←⍴⍵                                   ⍝ number of rows and columns.
    $[c=0;r⍴0;                               ⍝ empty matrix: success.
      n←+⌿⍵                                  ⍝ number of covers per column.
      f←(<\n=⌊/n)/⍵                          ⍝ first column with fewest 1s.
      ⍵ ∇{                                   ⍝ ⍺ is constraint matrix.
           $[~1∊⍵;0;                         ⍝ no rows: failure.
             f←<\⍵                           ⍝ first row selector.
             c←,f⌿⍺                          ⍝ cols selected by first row.
             r←∨/c/⍺                         ⍝ rows covered by selected cols.
             s←⍺⍺(~c)/(~r)⌿⍺                 ⍝ sub-matrix covers.
             $[s≡0;⍺ ∇ f<⍵;                  ⍝ failure: try a different row.
               f∨(~r)\s                      ⍝ success: row f included.
              ]
            ]
         },f                                 ⍝ ⍵ is vector of marked rows.
        ]
  }⍵⍪d                                       ⍝ exact cover.
  $[z≡0;0;                                   ⍝ failure: 0
    (-+/~⍺)↓z                                ⍝ without dummy rows.
   ]
}


⍝⍝ Weighted graph processing

wcost←{                                      ⍝ Cost vector for path ⍵ through weighted graph ⍺.
  graph costs←↓⍺                             ⍝ edges and edge-costs.
  $[2>≢⍵;0;                                  ⍝ null path: no cost.
    2{                                       ⍝ ⍺:from, ⍵:to.
      node←,⍺⊃graph                          ⍝ exits from vertex.
      indx←node⍳⍵                            ⍝ index of (⍺ ⍵) vertex.
      indx⊃,⍺⊃costs                          ⍝ ... associated cost.
    }/⍵                                      ⍝ pairwise along path.
   ]
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
    $[to<0;(to=¯2)↓acc;                      ⍝ root or unvisited vertex: finished
      ⍺ ∇(to,acc)(to⊃⍺)                      ⍝ otherwise: parent vertex prefix
     ]
  }{                                         ⍝ lowest spanning cost tree:
    tree cost←⍵                              ⍝ current tree and cost vectors
    $[⍺≡⍬;tree ⍺⍺ ⍬ to;                      ⍝ all vertices visited: done
      adjv←⍺ I graph                         ⍝ adjacent vertices
      accm←⍺ I cost+costs                    ⍝ costs to adjacent vertices
      best←adjv I¨⊂cost                      ⍝ costs to beat
      mask←accm<best⌊to⊃cost                 ⍝ mask of better routes
      next←mask/¨adjv                        ⍝ next vertices to visit
      back←⊃,/⍺+0×next                       ⍝ back links to parent nodes
      cvec←⊃,/mask/¨accm                     ⍝ cost vector
      decr←{(⍒cvec)I ⍵}                      ⍝ in decreasing order of cost
      wave←decr↑,/next                       ⍝ vertex wave front
      new←back cvec decr⍨@wave¨⍵             ⍝ successor tree & cost
      (∪wave)∇ new                           ⍝ wave spreads to adjacent vertices
     ]
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
    $[⍺≡⍬;tree;                              ⍝ all vertices visited: done
      adjv←⍺ I graph                         ⍝ adjacent vertices
      accm←⍺ I cost+costs                    ⍝ cumulative cost via this vertex
      mask←accm<adjv I¨⊂cost                 ⍝ mask of better routes
      cvec←⊃,/mask/¨accm                     ⍝ cost vector
      next←mask/¨adjv                        ⍝ next vertices to visit
      back←⊃,/⍺+0×next                       ⍝ back links to parent node
      decr←{(⍒cvec)I ⍵}                      ⍝ in decreasing order of cost
      wave←decr↑,/next                       ⍝ vertex wave front
      new←back cvec decr⍨@wave¨⍵             ⍝ successor tree & cost
      (∪wave)∇ new                           ⍝ wave spreads to adjacent vertices
     ]
  }tree cost                                 ⍝ initial spanning tree and cost vectors
}

⍝ From http://dfns.dyalog.com/s_wmst.htm

wmst ← {                                     ⍝ Minimum Spanning Tree for wu-graph ⍺.
  graph costs←↓⍺                             ⍝ weighted, undirected graph
  xvec←⍳⍴graph                               ⍝ index vector for graph
  ⍵{                                         ⍝ vertices inside tree: T
    tree todo←⍵                              ⍝ partial tree and unconnected vertices
    $[todo≡⍬;tree;                           ⍝ all vertices connected: finished
      edges←(graph∊¨⊂todo)∧xvec∊⍺            ⍝ edges from T to G~T
      min←⌊/⌊/¨edges/¨costs                  ⍝ minimum edge cost
      masks←edges∧min=costs                  ⍝ lowest cost edge masks
      fm to←{↑,/masks/¨⍵}¨xvec graph         ⍝ lowest cost edges from T to G~T
      $[fm≡⍬;tree;                           ⍝ disjoint graph: quit
        (⍺,to)∇(fm@to⊢tree)(todo~to)         ⍝ vertices from G~T to T
     ]]
  }(¯1⊣¨graph)(xvec~⍵)                       ⍝ initial tree and unconnected vertices
}
