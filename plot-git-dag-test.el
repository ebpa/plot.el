(require 'buttercup)
(require 'plot-git-dag)

(describe "plot-dag-sort-in-topological-order"
  ;; (let* ((tip-nodes (list (plot-dag-node-from-list '(?a ?b))))
  ;;        (nodes (cl-loop for n iter-by (plot-dag--once (plot-dag--walk-nodes tip-nodes))
  ;;                        collect n))
  ;;        (iter-sorted-nodes (plot-dag-sort-in-topological-order nodes))
  ;;        (sorted-nodes (cl-loop for n iter-by iter-sorted-nodes
  ;;                               collect n)))
  ;;   sorted-nodes)
  )

(describe "plot-git-dag"
  (it "renders an empty DAG"
    (expect (plot-dag-show-nodes
             (plot-dag-git-graph-create)
             nil)
            :to-equal ""))
  
  (it "renders a DAG graph with one node"
    (expect (plot-dag-show-nodes
             (plot-dag-git-graph-create)
             (list (plot-dag-node-from-list '(foo))))
            :to-equal "* foo\n"))

  (it "renders a DAG graph with two connected nodes"
    (expect (plot-dag-show-nodes
             (plot-dag-git-graph-create)
             (list (plot-dag-node-from-list '(foo bar))))
            :to-equal "* foo
* bar
"))

  (it "renders a node with two parents- each with linear history"
    (expect (plot-dag-show-nodes
             (plot-dag-git-graph-create)
             (list
              (make-plot-dag-node
               :item 'foo
               :parents
               (list (plot-dag-node-from-list '(bar baz bat))
                     (plot-dag-node-from-list '(qux quux))))))
            :to-equal "*   foo
|\  
* | bar
| * qux
* | baz
| * quux
* bat"))

  (it "renders an octopus merge (t4214-log-graph-octopus.sh (1))"
    (expect
     (plot-dag-show-nodes
      (plot-dag-git-graph-create)
      (plot-dag-nodes-from-edge-list
       '((0 . 1)
         (1 . 6)
         (1 . 5)
         (0 . 2)
         (0 . 3)
         (0 . 4)
         (2 . 5)
         (3 . 5)
         (4 . 5))))
     :to-equal
     "* 6
| *---.   5
| |\ \ \
|/ / / /
| | | * 4
| | * | 3
| | |/
| * | 2
| |/
* | 1
|/
* 0"))
  
  

  (it "t4214-log-graph-octopus.sh (2)"
    "*---.   octopus-merge
|\ \ \
| | | * 4
| | * | 3
| | |/
| * | 2
| |/
* | 1
|/
* initial")

  (it "t4214-log-graph-octopus.sh (3)"
    "* after-merge
*---.   octopus-merge
|\ \ \
| | | * 4
| | * | 3
| | |/
| * | 2
| |/
* | 1
|/
* initial")

  (it "t4214-log-graph-octopus.sh (4)"
    "* left
| * after-merge
| *---.   octopus-merge
| |\ \ \
|/ / / /
| | | * 4
| | * | 3
| | |/
| * | 2
| |/
* | 1
|/
* initial")

  (it "t4214-log-graph-octopus.sh (5)"
    "* after-4
| *---.   octopus-merge
| |\ \ \
| |_|_|/
|/| | |
* | | | 4
| | | * 3
| |_|/
|/| |
| | * 2
| |/
|/|
| * 1
|/
* initial")

  (it "t4214-log-graph-octopus.sh (6)"
    "* after-4
| * after-merge
| *---.   octopus-merge
| |\ \ \
| |_|_|/
|/| | |
* | | | 4
| | | * 3
| |_|/
|/| |
| | * 2
| |/
|/|
| * 1
|/
* initial")
  (it "t4214-log-graph-octopus.sh (7)"
    "* after-initial
| *---.   octopus-merge
| |\ \ \
| | | | * 4
| |_|_|/
|/| | |
| | | * 3
| |_|/
|/| |
| | * 2
| |/
|/|
| * 1
|/
* initial")
  (it "t4214-log-graph-octopus.sh (8)"
    "* after-initial
| * after-merge
| *---.   octopus-merge
| |\ \ \
| | | | * 4
| |_|_|/
|/| | |
| | | * 3
| |_|/
|/| |
| | * 2
| |/
|/|
| * 1
|/
* initial"))


;; ----

;; @pytest.fixture
;; def graph():
;;     return Graph(use_color=False)


;; @pytest.fixture
;; def simple_nodes():
;;     return [Node.from_list(
;;         "Second",
;;         "sixth",
;;         "fifth",
;;         "fourth",
;;         "third",
;;         "second",
;;         "initial",
;;     )]


;; @pytest.fixture
;; def branched_nodes():
;;     third = Node.from_list("third", "second", "initial")
;;     tip = Node("Merge branch 'side'", parents=[
;;         Node("Second", parents=[
;;             Node("sixth", parents=[
;;                 Node("fifth", parents=[
;;                     Node("fourth", parents=[third]),
;;                 ]),
;;             ]),
;;         ]),
;;         Node("side-2", parents=[
;;             Node("side-1", parents=[third]),
;;         ]),
;;     ])
;;     return [tip]


;; @pytest.fixture
;; def tangled_nodes():
;;     second = Node.from_list("second", "initial")
;;     third = Node("third", parents=[second])
;;     fifth = Node("fifth", parents=[
;;         Node("fourth", parents=[third]),
;;     ])
;;     side1 = Node("side-1", parents=[third])
;;     tangle = Node("Merge tag 'tangle'", parents=[
;;         Node("Merge branch 'side' (early part) into tangle", parents=[
;;             Node("Merge branch 'master' (early part) into tangle", parents=[
;;                 Node("tangle-a", parents=[second]),
;;                 fifth,
;;             ]),
;;             side1,
;;         ]),
;;         Node("Merge branch 'side'", parents=[
;;             Node("side-2", parents=[side1]),
;;             Node("Second", parents=[
;;                 Node("sixth", parents=[fifth]),
;;             ])
;;         ]),
;;     ])
;;     tip = Node("Merge tag 'reach'", parents=[
;;         Node("Merge tags 'octopus-a' and 'octopus-b'", parents=[
;;             Node("seventh", parents=[tangle]),
;;             Node("octopus-b", parents=[tangle]),
;;             Node("octopus-a", parents=[tangle]),
;;         ]),
;;         Node("reach", parents=[tangle]),
;;     ])
;;     return [tip]


;; def verify_out(capfd, expected):
;;     out, _ = capfd.readouterr()
;;     print(out)
;;     assert expected == out


;; def test_linearb
(simple_nodes, capfd):
;;     graph = Graph(use_color=False)
;;     graph.show_nodes(simple_nodes)
;;     verify_out(capfd, r"""* Second
;; * sixth
;; * fifth
;; * fourth
;; * third
;; * second
;; * initial
;; """)


;; @pytest.mark.xfail
;; def test_branched(branched_nodes, capfd):
;;     graph = Graph(use_color=False)
;;     graph.show_nodes(branched_nodes)
;;     verify_out(capfd, r"""*   Merge branch 'side'
;; |\
;; | * side-2
;; | * side-1
;; * | Second
;; * | sixth
;; * | fifth
;; * | fourth
;; |/
;; * third
;; * second
;; * initial""")


;; @pytest.mark.xfail
;; def test_tangled(tangled_nodes, capfd):
;;     graph = Graph(use_color=False)
;;     graph.show_nodes(tangled_nodes)
;;     verify_out(capfd, r"""*   Merge tag 'reach'
;; |\
;; | \
;; |  \
;; *-. \   Merge tags 'octopus-a' and 'octopus-b'
;; |\ \ \
;; * | | | seventh
;; | | * | octopus-b
;; | |/ /
;; |/| |
;; | * | octopus-a
;; |/ /
;; | * reach
;; |/
;; *   Merge branch 'tangle'
;; |\
;; | *   Merge branch 'side' (early part) into tangle
;; | |\
;; | * \   Merge branch 'master' (early part) into tangle
;; | |\ \
;; | * | | tangle-a
;; * | | |   Merge branch 'side'
;; |\ \ \ \
;; | * | | | side-2
;; | | |_|/
;; | |/| |
;; | * | | side-1
;; * | | | Second
;; * | | | sixth
;; | |_|/
;; |/| |
;; * | | fifth
;; * | | fourth
;; |/ /
;; * | third
;; |/
;; * second
;; * initial""")

