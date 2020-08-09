;;; plot-dag.el --- The git graph algorithm (transcoded into elisp)           -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Erik Anderson <erik@ebpa.link>
;; Copyright (C) 2016 Sam Brightman

;; Author: Erik Anderson <erik@ebpa.link>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Forked from https://github.com/git/git/commit/0ffa31fc36ff27edaccbb565323b95dfab2202f5
;;; ~/git/git/graph.c
;;; ~/git/git/graph.h

;;; Contains additional (translated) code from https://github.com/sambrightman/asciidag/tree/bfc161e7d791c73d7af669ca5d13dcd5156b5487

;;; Code:



(cl-defun plot-dag-node-from-list ((item . rest))
  ""
  (make-plot-dag-node
   :item item
   :parents (when rest
              (list (plot-dag-node-from-list rest)))))

;; (plot-dag-node-from-list '(1 2 3 4 5))

(defun plot-dag-nodes-from-edge-list (dict)
  ""
  (message "(plot-dag-nodes-from-edge-list %s)" dict)
  (let* ((item-index (make-hash-table :test #'equal))
         (child-nodes nil)
         (parent-nodes nil))
    (mapcar
     (lambda (edge)
       (-let* (((parent . child) edge)
               (parent-node (or (gethash parent item-index)
                                (puthash parent (make-plot-dag-node :item parent) item-index)))
               (child-node (or (gethash child item-index)
                               (puthash child (make-plot-dag-node :item child) item-index)))
               (child-parents (plot-dag-node-parents child-node)))
         (add-to-list 'child-parents parent-node)
         (setf (plot-dag-node-parents child-node) child-parents)
         (add-to-list 'child-nodes child-node)
         (add-to-list 'parent-nodes parent-node)))
     dict)
    (--map (setq child-nodes (delq it child-nodes)) parent-nodes)
    ;; (list :parent-nodes parent-nodes
    ;;        :child-nodes child-nodes
    child-nodes))



(cl-defstruct plot-dag-node
  item
  parents)

(cl-defstruct plot-dag-column
  ""
  ;; The parent commit of this column.
  commit 
  ;; The color to (optionally) print this column in.  This is an
  ;; index into column_colors.
  color)

(defvar plot-dag-graph-states
  '(:GRAPH_PADDING
    :GRAPH_SKIP
    :GRAPH_PRE_COMMIT
    :GRAPH_COMMIT
    :GRAPH_POST_MERGE
    :GRAPH_COLLAPSING))

;; (defun graph-show-line-prefix (const struct diff_options *diffopt)
;;   (unless (or (not diffopt)
;;               (not diffopt->line_prefix))
;;     (fwrite diffopt->line_prefix (sizeof char) diffopt->line_prefix_length diffopt->file)))

(defvar plot-dag-column-colors ["\x1b[31m" "\x1b[32m" "\x1b[33m" "\x1b[34m" "\x1b[35m" "\x1b[36m" "\x1b[1;31m" "\x1b[1;32m" "\x1b[1;33m" "\x1b[1;34m" "\x1b[1;35m" "\x1b[1;36m" "\x1b[m"])
(defvar plot-dag-column-colors-max 0)

;; (defun parse-graph-colors-config (struct argv_array *colors, string)
;;   const char *end, *start;

;;   start = string;
;;   end = string + strlen(string);
;;   (while (< start end) 
;;     (-let* ((comma (strchrnul start ",")))
;;       char color[COLOR_MAXLEN];

;;       (if (!color_parse_mem(start, comma - start, color))
;; 	  (argv-array-push colors color)
;; 	(warning (_("ignore invalid color '%.*s' in log.graphColors"), (int)(comma - start), start)))
;;       start = comma + 1))
;;     (argv-array-push colors GIT_COLOR_RESET))

;; (defun graph-set-column-colors (colors colors-max)
;;  "Set up a custom scheme for column colors.
;; 
;;  The default column color scheme inserts ANSI color escapes to colorize
;;  the graph. The various color escapes are stored in an array of strings
;;  where each entry corresponds to a color, except for the last entry,
;;  which denotes the escape for resetting the color back to the default.
;;  When generating the graph, strings from this array are inserted before
;;  and after the various column characters.
;; 
;;  This function allows you to enable a custom array of color escapes.
;;  The 'colors_max' argument is the index of the last "reset" entry.
;; 
;;  This functions must be called BEFORE graph_init() is called.
;; 
;;  NOTE: This function isn't used in Git outside graph.c but it is used
;;  by CGit (http://git.zx2c4.com/cgit/) to use HTML for colors."
;;   column_colors = colors
;;   column_colors_max = colors_max)

;; (plot-dag-column-get-color-code :: Mixed -> Char)
(defun plot-dag-column-get-color-code (color)
  (message "(plot-dag-column-get-color-code %d)" color)
  (elt plot-dag-column-colors color))

;; (plot-dag-strbuf-write-column :: Buffer -> Mixed -> Char -> Nil)
(defun plot-dag-strbuf-write-column (buffer column col-char)
  ""
  (message "(plot-dag-strbuf-write-column %s %s %s)" buffer column col-char)
  (with-current-buffer buffer
    ;; TODO: coloring
    ;; (when (< (plot-dag-column-color column) plot-dag-column-colors-max)
    ;;   (strbuf-addstr buffer (plot-dag-column-get-color-code (plot-dag-column-color column))))
    (goto-char (point-max))
    (insert col-char)
    ;; TODO: coloring
    ;; (when (< (plot-dag-column-color column) column_colors_max)
    ;;   (strbuf-addstr buffer (plot-dag-column-get-color-code plot-dag-column-colors-max)))
    nil))

(defstruct (plot-dag-git-graph ;;(:constructor nil)
            ;;(:constructor plot-dag-git-graph-create (&key content
            ;;&aux (id (tui--new-id))))
            )
  ;; The commit currently being processed
  commit
  ;; The rev-info used for the current traversal
  revs ;; rev_info
  first-parent-only-p
  ;; The number of interesting parents that this commit has.
  ;;
  ;; Note that this is not the same as the actual number of parents.
  ;; This count excludes parents that won't be printed in the graph
  ;; output, as determined by graph_is_interesting().
  (num-parents 0)
  ;; The width of the graph output for this commit.
  ;; All rows for this commit are padded to this width, so that
  ;; messages printed after the graph output are aligned.
  width
  ;; The next expansion row to print
  ;; when state is :GRAPH_PRE_COMMIT
  (expansion-row 0)
  ;; The current output state.
  ;; This tells us what kind of line graph_next_line() should output.
  (state :GRAPH_PADDING)
  ;; The output state for the previous line of output.
  ;; This is primarily used to determine how the first merge line
  ;; should appear, based on the last line of the previous commit.
  (prev-state :GRAPH_PADDING)
  ;; The index of the column that refers to this commit.
  ;;
  ;; If none of the incoming columns refer to this commit,
  ;; this will be equal to num_columns.
  (commit-index 0)
  ;; The commit_index for the previously displayed commit.
  ;;
  ;; This is used to determine how the first line of a merge
  ;; graph output should appear, based on the last line of the
  ;; previous commit.
  (prev-commit-index 0)
  ;; The maximum number of columns that can be stored in the columns
  ;; and new_columns arrays.  This is also half the number of entries
  ;; that can be stored in the mapping and new_mapping arrays.
  (column-capacity 30)
  ;; The number of columns (also called "branch lines" in some places)
  (num-columns 0)
  ;; The number of columns in the new_columns array
  (num-new-columns 0)
  ;; The number of entries in the mapping array
  (mapping-size 0)
  ;; The column state before we output the current commit.'(
  columns
  ;; The new column state after we output the current commit.
  ;; Only valid when state is :GRAPH_COLLAPSING.)
  new-columns
  ;; An array that tracks the current state of each
  ;; character in the output line during state :GRAPH_COLLAPSING.)
  ;; Each entry is -1 if this character is empty, or a non-negative
  ;; integer if the character contains a branch line.  The value of
  ;; the integer indicates the target position for this branch line.
  ;; (I.e., this array maps the current column positions to their
  ;; desired positions.)
  ;;
  ;; The maximum capacity of this array is always
  ;; sizeof(int) * 2 * column_capacity.
  mapping;
  ;; A temporary array for computing the next mapping state
  ;; while we are outputting a mapping line.  This is stored as part
  ;; of the git_graph simply so we don't have to allocate a new
  ;; temporary array each time we have to output a collapsing line.
  new-mapping;
  ;; The current default column color being used.  This is
  ;; stored as an index into the array column_colors.
  default-column-color ;; short
  (use-color t))

;; (plot-dag-git-graph-create :: Mixed -> Mixed)
(defun plot-dag-git-graph-create ()
  ""
  (message "(plot-dag-git-graph-create)")
  (-let* ((graph (make-plot-dag-git-graph)))
    (setf (plot-dag-git-graph-columns graph) (make-vector (plot-dag-git-graph-column-capacity graph) nil)
          (plot-dag-git-graph-new-columns graph) (make-vector (plot-dag-git-graph-column-capacity graph) nil)
          (plot-dag-git-graph-mapping graph) (make-vector (* 2 (plot-dag-git-graph-column-capacity graph)) nil)
          (plot-dag-git-graph-new-mapping graph) (make-vector (* 2 (plot-dag-git-graph-column-capacity graph)) nil)
          (plot-dag-git-graph-default-column-color graph) (- (length plot-dag-column-colors) 1))
    graph))
;;(plot-dag-git-graph-create)

;; struct git-graph -> strbuf
;; (defun diff-output-prefix-callback (struct diff_options *opt, void *data)
;;   (-let* ((graph data)
;;           (static struct strbuf msgbuf = STRBUF_INIT))
;;     assert(opt);

;;     (strbuf_reset &msgbuf)
;;     (when (opt->line_prefix)
;;       (strbuf-add &msgbuf opt->line_prefix opt->line_prefix_length))
;;     (when graph
;;       (plot-dag-graph-padding-line graph &msgbuf))
;;     &msgbuf))

(defvar default_diffopt nil)

;; => void
;; (defun graph-setup-line-prefix (diffopt)
;;  "Called to setup global display of line_prefix diff option.
;; 
;;  Passed a diff_options structure which indicates the line_prefix and the
;;  file to output the prefix to. This is sort of a hack used so that the
;;  line_prefix will be honored by all flows which also honor "--graph"
;;  regardless of whether a graph has actually been setup. The normal graph
;;  flow will honor the exact diff_options passed, but a NULL graph will cause
;;  display of a line_prefix to stdout."
;;   default_diffopt = diffopt;

;;   ;; setup an output prefix callback if necessary
;;   (when (and diffopt (null diffopt->output_prefix))
;;     diffopt->output_prefix = diff_output_prefix_callback))

;; (graph-init :: Mixed -> Mixed)
(defun graph-init (opt)
  "Return git-graph."
  (message "(graph-init %s)" opt)
  ;;(-let* ((graph (make-git-graph)))
  ;; (when (null plot-dag-column-colors)
  ;;   char *string;
  ;;   (if (git-config-get-string "log.graphcolors" &string)
  ;;       ;; not configured -- use default
  ;;       (graph-set-column-colors column-colors-ansi column-colors-ansi-max)
  ;;     static struct argv_array custom_colors = ARGV_ARRAY_INIT;
  ;;     ;; argv_array_clear(&custom_colors);
  ;;     ;; parse_graph_colors_config(&custom_colors, string);
  ;;     ;; free(string);
  ;;     ;; graph_set_column_colors takes a max-index, not a count
  ;;     graph_set_column_colors(custom_colors.argv, custom_colors.argc - 1)))

                                        ;graph->commit = NULL;
  graph->revs = opt;
  ;;graph->num_parents = 0;
  ;;graph->expansion_row = 0;
  ;; graph->state = :GRAPH_PADDING;
  ;; graph->prev_state = :GRAPH_PADDING;
  ;; graph->commit_index = 0;
  ;; graph->prev_commit_index = 0;
  ;; graph->num_columns = 0;
  ;; graph->num_new_columns = 0;
  ;; graph->mapping_size = 0;
  ;; Start the column color at the maximum value, since we'll
  ;; always increment it for the first commit we output.
  ;; This way we start at 0 for the first commit.
  graph->default_column_color = column_colors_max - 1;

  ;; Allocate a reasonably large default number of columns
  ;; We'll automatically grow columns later if we need more room.
  ;; graph->column_capacity = 30;
  ;; ALLOC_ARRAY(graph->columns, graph->column_capacity);
  ;; ALLOC_ARRAY(graph->new_columns, graph->column_capacity);
  ;; ALLOC_ARRAY(graph->mapping, 2 * graph->column_capacity);
  ;; ALLOC_ARRAY(graph->new_mapping, 2 * graph->column_capacity);

  ;; The diff output prefix callback, with this we can make
  ;; all the diff output to align with the graph lines.
  opt->diffopt.output_prefix = diff_output_prefix_callback;
  opt->diffopt.output_prefix_data = graph;
  )

;; (plot-dag-graph-update-state :: Mixed -> Mixed -> Mixed)
(defun plot-dag-graph-update-state (graph new-state)
  "Replace the current GRAPH state with NEW-STATE."
  (message "(plot-dag-graph-update-state %s %s)" graph new-state)
  (setf (plot-dag-git-graph-prev-state graph) (plot-dag-git-graph-state graph)
        (plot-dag-git-graph-state graph) new-state)
  graph)

;; => void
;; (defun graph-ensure-capacity (graph, int num_columns)
;;   ""
;;   (unless (graph->column_capacity >= num_columns)
;;     (cl-loop do 
;;              graph->column_capacity *= 2;
;;              while (< graph->column_capacity num-columns))

;;     REALLOC_ARRAY(graph->columns, graph->column_capacity);
;;     REALLOC_ARRAY(graph->new_columns, graph->column_capacity);
;;     REALLOC_ARRAY(graph->mapping, graph->column_capacity * 2);
;;     REALLOC_ARRAY(graph->new_mapping, graph->column_capacity * 2)))

;; (plot-dag-graph-is-interesting-p :: Mixed -> Mixed -> Boolean)
(defun plot-dag-graph-is-interesting-p (graph commit)
  "Return t if the commit will be printed in the graph output, and return nil otherwise."
  (message "(plot-dag-graph-is-interesting-p %s %s)" graph commit)
  ;; If revs->boundary is set, commits whose children have
  ;; been shown are always interesting, even if they have the
  ;; UNINTERESTING or TREESAME flags set.
  (or (and (plot-dag-git-graph-revs graph)
           (rev-info-boundary (plot-dag-git-graph-revs graph))
	   ;; TODO: remove?: (& (commit-object commit).flags CHILD_SHOWN)
           )
      ;; Otherwise, use get_commit_action() to see if this commit is
      ;; interesting
      ;; TODO: (eq (get-commit-action (plot-dag-git-graph-revs graph) commit) commit-show)
      ))

(defun plot-dag-interesting-parents (graph)
  ""
  (message "(plot-dag-interesting-parents %s)" graph)
  (let* ((parents (plot-dag-node-parents (plot-dag-git-graph-commit graph)))) 
    (if (plot-dag-git-graph-first-parent-only-p graph)
        (first parents)
      parents)))

;; (plot-dag-next-interesting-parent :: Mixed -> Mixed -> Mixed)
(defun plot-dag-next-interesting-parent (graph orig) ;; commit list
  "Returns a commit list."
  (message "(plot-dag-next-interesting-parent %s %s)" graph orig)
  (cl-block nil
    (-let* (list) ;; commit_list
      ;; If revs->first_parent_only is set, only the first
      ;; parent is interesting.  None of the others are.
      (if (plot-dag-git-graph-first-parent-only-p graph)
          nil
        ;; Return the next interesting commit after orig
        (cl-loop with list = (commit-next orig)
                 while list
                 do
	         (when (plot-dag-graph-is-interesting-p graph (commit-item list)) ;; SUSPECT
	           (cl-return-from nil list))
                 (setq list (commit-next list)))
        nil))))

;; (plot-dag-first-interesting-parent :: Mixed -> Mixed)
(defun plot-dag-first-interesting-parent (graph)
  "Return commit_list."
  (message "(plot-dag-first-interesting-parent %s)" graph)
  (first (plot-dag-interesting-parents graph))
  ;; If this commit has no parents, ignore it
  ;; (-when-let* ((parents (plot-dag-node-parents (plot-dag-git-graph-commit graph)))) ;; commit_list
  ;;   ;; If the first parent is interesting, return it
  ;;   (if (plot-dag-graph-is-interesting-p graph (plot-dag-node-item (first parents)))
  ;;       parents
  ;;     ;; Otherwise, call next_interesting_parent() to get
  ;;     ;; the next interesting parent
  ;;     (plot-dag-next-interesting-parent graph parents)))
  )

;; (plot-dag-graph-get-current-column-color :: Mixed -> Number)
(defun plot-dag-graph-get-current-column-color (graph)
  "Return a color index."
  (message "(plot-dag-graph-get-current-column-color %s)" graph)
  (if (not (plot-dag-git-graph-use-color graph))
      nil
    (plot-dag-git-graph-default-column-color graph)))

;; (plot-dag-graph-increment-column-color :: Mixed -> Mixed)
(defun plot-dag-graph-increment-column-color (graph)
  "Update the graph's default column color."
  (message "(plot-dag-graph-increment-column-color %s)" graph)
  (setf (plot-dag-git-graph-default-column-color graph)
        (% (+ (plot-dag-git-graph-default-column-color graph) 1)
           (length plot-dag-column-colors)))
  nil)

;; (plot-dag-graph-find-commit-color :: Mixed -> Mixed -> Number)
(defun plot-dag-graph-find-commit-color (graph commit)
  "Return a color for COMMIT based on the current state of GRAPH."
  (message "(plot-dag-graph-find-commit-color %s %s)" graph commit)
  (or
   (cl-loop for i from 0 below (plot-dag-git-graph-num-columns graph)
            if (eq (plot-dag-column-commit (elt (plot-dag-git-graph-columns graph) i)) commit)
            return (plot-dag-column-color (elt (plot-dag-git-graph-columns graph) i)))
   (plot-dag-graph-get-current-column-color graph)))

;; (plot-dag-graph-insert-into-new-columns :: Mixed -> Mixed -> Mixed -> Mixed)
(defun plot-dag-graph-insert-into-new-columns (graph commit mapping-index) ;; mapping-index passed by reference
  ""
  (message "(plot-dag-graph-insert-into-new-columns %s %s %s)" graph commit mapping-index)
  ;; If the commit is already in the new_columns list, we don't need to
  ;; add it.  Just update the mapping correctly.

  (cl-block check
    (message "before")
    (message "graph-mapping: %s" (plot-dag-git-graph-mapping graph))
    (cl-loop for i from 0 below (plot-dag-git-graph-num-new-columns graph)
             do
	     (when (eq (plot-dag-column-commit (elt (plot-dag-git-graph-new-columns graph) i))
                       commit)
               (message "set %s = %s" mapping-index i)
	       (setf (elt (plot-dag-git-graph-mapping graph) mapping-index)
                     i)
	       (cl-return-from check (+ mapping-index 2))))
    (message "after")
    (message "graph-mapping: %s" (plot-dag-git-graph-mapping graph))
    
    ;; This commit isn't already in new_columns.  Add it.
    (let* ((column (make-plot-dag-column :commit commit :color (plot-dag-graph-find-commit-color graph commit))))
      (setf (elt (plot-dag-git-graph-new-columns graph) (plot-dag-git-graph-num-new-columns graph))
            column)
      (message "set mapping[%s] = %s" mapping-index (plot-dag-git-graph-num-new-columns graph))
      (setf (elt (plot-dag-git-graph-mapping graph) mapping-index)
            (plot-dag-git-graph-num-new-columns graph))
      (cl-incf (plot-dag-git-graph-num-new-columns graph))
      
      (message "graph-mapping: %s" (plot-dag-git-graph-mapping graph))
      (+ mapping-index 2))))

;; (plot-dag-graph-update-width :: Mixed -> Mixed -> Void)
(defun plot-dag-graph-update-width (graph is-commit-in-existing-columns)
  "Compute the width needed to display the graph for this commit.
This is the maximum width needed for any row.  All other rows
will be padded to this width.

Compute the number of columns in the widest row:
Count each existing column (graph->num_columns), and each new
column added by this commit."
  (message "(plot-dag-graph-update-width %s %s)" graph is-commit-in-existing-columns)
  (-let* ((max-cols (+ (plot-dag-git-graph-num-columns graph)
                       (plot-dag-git-graph-num-parents graph))))

    ;; Even if the current commit has no parents to be printed, it
    ;; still takes up a column for itself.
    (when (< (plot-dag-git-graph-num-parents graph) 1)
      (cl-incf max-cols))

    ;; We added a column for the current commit as part of
    ;; graph->num_parents.  If the current commit was already in
    ;; graph->columns, then we have double counted it.
    (when is-commit-in-existing-columns
      (cl-decf max-cols))

    ;; Each column takes up 2 spaces
    (setf (plot-dag-git-graph-width graph) (* max-cols 2))))

;; (plot-dag-graph-update-columns :: Mixed -> Void)
(defun plot-dag-graph-update-columns (graph)
  ""
  (message "(plot-dag-graph-update-columns %s)" graph)
  (-let* (max-new-columns)
    ;; Swap graph->columns with graph->new_columns
    ;; graph->columns contains the state for the previous commit,
    ;; and new_columns now contains the state for our commit.
    ;;
    ;; We'll re-use the old columns array as storage to compute the new
    ;; columns list for the commit after this one.
    (setf (plot-dag-git-graph-columns graph)
          (plot-dag-git-graph-new-columns graph))
    (setf (plot-dag-git-graph-num-columns graph)
          (plot-dag-git-graph-num-new-columns graph))
    (setf (plot-dag-git-graph-num-new-columns graph)
          0)
    (message "Num columns: %d" (plot-dag-git-graph-num-columns graph))
    (message "graph-mapping: %s" (plot-dag-git-graph-mapping graph))

    ;; Now update new_columns and mapping with the information for the
    ;; commit after this one.
    ;;
    ;; First, make sure we have enough room.  At most, there will
    ;; be graph->num_columns + graph->num_parents columns for the next
    ;; commit.
    (setq max-new-columns (+ (plot-dag-git-graph-num-columns graph) (plot-dag-git-graph-num-parents graph)));
    ;; (graph-ensure-capacity graph max-new-columns)
    (message "Max new columns: %d" max-new-columns)

    (message "graph-mapping (before clearing): %s" (plot-dag-git-graph-mapping graph))
    ;; Clear out graph->mapping
    (setf (plot-dag-git-graph-mapping-size graph) (* 2 max-new-columns))
    (cl-loop for i from 0 below (plot-dag-git-graph-mapping-size graph)
             do
	     (setf (elt (plot-dag-git-graph-mapping graph) i) -1))
    (message "graph-mapping (after clearing): %s" (plot-dag-git-graph-mapping graph))

    ;; Populate graph->new_columns and graph->mapping
    ;;
    ;; Some of the parents of this commit may already be in
    ;; graph->columns.  If so, graph->new_columns should only contain a
    ;; single entry for each such commit.  graph->mapping should
    ;; contain information about where each current branch line is
    ;; supposed to end up after the collapsing is performed.
    (-let* ((seen-this nil)
            (mapping-idx 0)
            (is-commit-in-columns-p t))
      
      (cl-loop for col-commit = nil
               for i from 0 to (plot-dag-git-graph-num-columns graph)
               do
               (message "loop: %d" i)
               (message "num graph columns: %d" (plot-dag-git-graph-num-columns graph))
	       (if (not (eq i (plot-dag-git-graph-num-columns graph)))
                   (setq col-commit (plot-dag-column-commit (elt (plot-dag-git-graph-columns graph) i)))
                 (message "seen this: %s" seen-this)
	         (when seen-this
                   (cl-return))
	         (setq is-commit-in-columns-p nil)
	         (setq col-commit (plot-dag-git-graph-commit graph)))

	       (if (eq col-commit (plot-dag-git-graph-commit graph))
                   (progn
                     (message "then")
	             (-let* ((old-mapping-idx mapping-idx))
	               (setq seen-this t)
	               (setf (plot-dag-git-graph-commit-index graph) i)
	               (cl-loop for parent in (plot-dag-interesting-parents graph)
                                do
		                ;; If this is a merge, or the start of a new
                                ;; childless column, increment the current
                                ;; color.
		                (when (or (> (plot-dag-git-graph-num-parents graph) 1)
                                          (not is-commit-in-columns-p))
			          (plot-dag-graph-increment-column-color graph))
		                (setq mapping-idx
                                      (plot-dag-graph-insert-into-new-columns graph parent mapping-idx)))
                       ;; We always need to increment mapping_idx by at
                       ;; least 2, even if it has no interesting parents.
                       ;; The current commit always takes up at least 2
                       ;; spaces.
	               (when (eq mapping-idx old-mapping-idx)
	                 (setq mapping-idx (+ mapping-idx 2)))))
                 (progn
                   (message "else")
                   (setq mapping-idx
                         (plot-dag-graph-insert-into-new-columns graph col-commit mapping-idx)))))

      (message "graph-mapping (before shrinking): %s" (plot-dag-git-graph-mapping graph))
      ;; Shrink mapping_size to be the minimum necessary
      (while (and (> (plot-dag-git-graph-mapping-size graph)
                     1)
	          (< (elt (plot-dag-git-graph-mapping graph)
                          (- (plot-dag-git-graph-mapping-size graph) 1))
                     0))
        (cl-decf (plot-dag-git-graph-mapping-size graph)))
      (message "graph-mapping (after shrinking): %s" (plot-dag-git-graph-mapping graph))

      ;; Compute graph->width for this commit
      (plot-dag-graph-update-width graph is-commit-in-columns-p))))

;; (plot-dag-graph-update :: Mixed -> Mixed -> Mixed)
(defun plot-dag-graph-update (graph commit)
  "Update a git_graph with a new commit.
This will cause the graph to begin outputting lines for the new
commit the next time graph_next_line() is called.

If graph_update() is called before graph_is_commit_finished()
returns 1, the next call to graph_next_line() will output an
ellipsis (\"...\") to indicate that a portion of the graph is
missing."
  (message "(plot-dag-graph-update %s %s)" graph commit)
  ;; Set the new commit
  (setf (plot-dag-git-graph-commit graph) commit)

  ;; Count how many interesting parents this commit has
  (setf (plot-dag-git-graph-num-parents graph) 0)
  (cl-loop for parent in (plot-dag-interesting-parents graph)
           do
	   (cl-incf (plot-dag-git-graph-num-parents graph)))

  ;; Store the old commit_index in prev_commit_index.
  ;; graph_update_columns() will update graph->commit_index for this
  ;; commit.
  (setf (plot-dag-git-graph-prev-commit-index graph) (plot-dag-git-graph-commit-index graph))

  ;; Call graph_update_columns() to update
  ;; columns, new_columns, and mapping.
  (plot-dag-graph-update-columns graph)

  (setf (plot-dag-git-graph-expansion-row graph) 0)

  ;; Update graph->state.
  ;; Note that we don't call graph_update_state() here, since
  ;; we don't want to update graph->prev_state.  No line for
  ;; graph->state was ever printed.
  ;;
  ;; If the previous commit didn't get to the :GRAPH_PADDING state,
  ;; it never finished its output.  Goto :GRAPH_SKIP, to print out
  ;; a line to indicate that portion of the graph is missing.
  ;;
  ;; If there are 3 or more parents, we may need to print extra rows
  ;; before the commit, to expand the branch lines around it and make
  ;; room for it.  We need to do this only if there is a branch row
  ;; (or more) to the right of this commit.
  ;;
  ;; If there are less than 3 parents, we can immediately print the
  ;; commit line.
  (cond
   ((not (eq (plot-dag-git-graph-state graph) :GRAPH_PADDING))
    (setf (plot-dag-git-graph-state graph) :GRAPH_SKIP))
   ((and (>= (plot-dag-git-graph-num-parents graph) 3)
         (< (plot-dag-git-graph-commit-index graph)
            (- (plot-dag-git-graph-num-columns graph) 1)))
    (setf (plot-dag-git-graph-state graph) :GRAPH_PRE_COMMIT))
   (t
    (setf (plot-dag-git-graph-state graph) :GRAPH_COMMIT))))

;; (plot-dag-graph-is-mapping-correct-p :: Mixed -> Mixed)
(defun plot-dag-graph-is-mapping-correct-p (graph)
  ""
  (message "(plot-dag-graph-is-mapping-correct-p %s)" graph)
  ;; The mapping is up to date if each entry is at its target,
  ;; or is 1 greater than its target.
  ;; (If it is 1 greater than the target, '/' will be printed, so it
  ;; will look correct on the next row.)
  (message "graph-mapping: %s" (plot-dag-git-graph-mapping graph))
  (let* ((correct-p (cl-loop for i from 0 below (plot-dag-git-graph-mapping-size graph)
                             for target = (elt (plot-dag-git-graph-mapping graph) i)
                             if (and (not (< target 0))
                                     (not (eq target (/ i 2))))
	                     return nil
                             finally return t)))
    (message "correct-p: %s" correct-p)
    correct-p))

;; (plot-dag-graph-pad-horizontally :: Mixed -> Mixed -> Mixed -> Void)
(defun plot-dag-graph-pad-horizontally (graph chars-written)
  "Add additional spaces to the end of the strbuf, so that all lines for a particular commit have the same width.
  
This way, fields printed to the right of the graph will remain aligned for the entire commit."
  (message "(plot-dag-graph-pad-horizontally %s)" graph)
  (when (< chars-written (plot-dag-git-graph-width graph))
    (--dotimes (- (plot-dag-git-graph-width graph) chars-written)
      (goto-char (point-max))
      (insert " "))))

;; (plot-dag-graph-output-padding-line :: Mixed -> Mixed -> Void)
(defun plot-dag-graph-output-padding-line (graph)
  "Output a padding line in the graph.  This is similar to graph_next_line().  However, it is guaranteed to never print the current commit line.  Instead, if the commit line is next, it will simply output a line of vertical padding, extending the branch lines downwards, but leaving them otherwise unchanged."
  (message "(plot-dag-graph-output-padding-line %s)" graph)
  ;; We could conceivable be called with a NULL commit
  ;; if our caller has a bug, and invokes graph_next_line()
  ;; immediately after graph_init(), without first calling
  ;; graph_update().  Return without outputting anything in this
  ;; case.
  (unless (null (plot-dag-git-graph-commit graph))
    ;; Output a padding row, that leaves all branch lines unchanged
    (cl-loop for i from 0 below (plot-dag-git-graph-num-new-columns graph)
             do
	     (insert (propertize "|" 'column (elt (plot-dag-git-graph-new-columns graph) i)))
	     (insert " "))

    (plot-dag-graph-pad-horizontally graph (* (plot-dag-git-graph-num-new-columns graph) 2))))

;; (plot-dag-graph-output-skip-line :: Mixed -> Mixed -> Mixed)
(defun plot-dag-graph-output-skip-line (graph)
  ""
  (message "(plot-dag-graph-output-skip-line %s)" graph)
  ;; Output an ellipsis to indicate that a portion
  ;; of the graph is missing.
  (goto-char (point-max))
  (insert "...")
  (plot-dag-graph-pad-horizontally graph 3)

  (if (and (>= (plot-dag-git-graph-num-parents graph) 3)
	   (< (plot-dag-git-graph-commit-index graph)
              (- (plot-dag-git-graph-num-columns graph) 1)))
      (plot-dag-graph-update-state graph :GRAPH_PRE_COMMIT)
    (plot-dag-graph-update-state graph :GRAPH_COMMIT)))

;; (plot-dag-graph-output-pre-commit-line :: Mixed -> Mixed -> Void)
(defun plot-dag-graph-output-pre-commit-line (graph)
  ""
  (message "(plot-dag-graph-output-pre-commit-line %s)" graph)
  ;; This function formats a row that increases the space around a commit
  ;; with multiple parents, to make room for it.  It should only be
  ;; called when there are 3 or more parents.
  ;;
  ;; We need 2 extra rows for every parent over 2.
  (assert (>= (graph-num-parents graph) 3))
  (-let* ((num-expansion-rows (* (- (graph-num-parents graph) 2) 2)))

    ;; graph->expansion_row tracks the current expansion row we are on.
    ;; It should be in the range [0, num_expansion_rows - 1]
    (assert (and (<= 0 (graph-expansion-row graph))
                 (< (graph-expansion-row graph)
                    num-expansion-rows)))
    ;; Output the row
    (cl-loop for seen-this = nil
             for chars-written = 0
             for i from 0
             for col in (graph-columns graph)
             do
	     (cond
              ((eq (plot-dag-column-commit col)
                   (plot-dag-git-graph-commit graph))
	       (setq seen-this t)
	       (insert (propertize "|" 'column col))
	       (insert (make-string (plot-dag-git-graph-expansion-row graph) ?\ ))
	       (setq chars-written (+ chars-written 1 + (graph-expansion-row graph))))
              ((and seen-this
                    (eq (plot-dag-git-graph-expansion-row graph) 0))
	       ;; This is the first line of the pre-commit output.
               ;; If the previous commit was a merge commit and
               ;; ended in the :GRAPH_POST_MERGE state, all branch
               ;; lines after graph->prev_commit_index were
               ;; printed as "\" on the previous line.  Continue
               ;; to print them as "\" on this line.  Otherwise,
               ;; print the branch lines as "|".
	       (if (and (eq graph-prev-state :GRAPH_POST_MERGE)
                        (< (plot-dag-git-graph-prev-commit-index graph) i))
		   (insert (propertize "\\" 'column col))
	         (insert (propertize "|" 'column col)))
	       (cl-incf chars-written))
              ((and seen-this (> graph->expansion_row 0))
	       (insert (propertize "\\" 'column col))
	       (cl-incf chars-written))
              (_
	       (insert (propertize "|" 'column col))
	       (cl-incf chars-written)))

	     (insert " ")
	     (cl-incf chars-written)))

  (plot-dag-graph-pad-horizontally graph chars-written)

  ;; Increment graph->expansion_row,
  ;; and move to state :GRAPH_COMMIT if necessary
  (cl-incf (graph-expansion-row graph))
  (when (>= (graph-expansion-row graph) num-expansion-rows)
    (plot-dag-graph-update-state graph :GRAPH_COMMIT)))

;; (plot-dag-graph-output-commit-char :: Mixed -> Mixed -> Void)
(defun plot-dag-graph-output-commit-char (graph)
  ""
  (message "(plot-dag-graph-output-commit-char %s)" graph)
  (cond
   ;; For boundary commits, print 'o'
   ;; (We should only see boundary commits when revs->boundary is set.)
   ;; TODO
   ;; ((graph->commit->object.flags & BOUNDARY)
   ;;  (assert (rev-info-boundary (plot-dag-git-graph-revs graph)))
   ;;  (strbuf-addch sb "o")
   ;;  nil)
   (t
    ;; get_revision_mark() handles all other cases without assert()
    (insert "*" ;; TODO (get-revision-mark (plot-dag-git-graph-revs graph) (plot-dag-git-graph-commit graph))
            ))))

;; (plot-dag-graph-draw-octopus-merge :: Mixed -> Mixed -> Number)
(defun plot-dag-graph-draw-octopus-merge (graph)
  "Draw the horizontal dashes of an octopus merge and return the number of characters written."
  (message "(plot-dag-graph-draw-octopus-merge %s)" graph)
  (goto-char (point-max))
  ;; Here dashless-parents represents the number of parents which don't
  ;; need to have dashes (the edges labeled "0" and "1").  And
  ;; dashful-parents are the remaining ones.
  ;;
  ;; | *---.
  ;; | |\ \ \
  ;; | | | | |
  ;; x 0 1 2 3
  (-let* ((dashless-parents 2)
          (dashful-parents (- (plot-dag-git-graph-num-parents graph) dashless-parents))
          ;; Usually, we add one new column for each parent (like the diagram
          ;; above) but sometimes the first parent goes into an existing column,
          ;; like this:
          ;;
          ;; | *---.
          ;; | |\ \ \
          ;; |/ / / /
          ;; x 0 1 2
          ;;
          ;; In which case the number of parents will be one greater than the
          ;; number of added columns.
          (added-cols (- (plot-dag-git-graph-num-new-columns graph) (plot-dag-git-graph-num-columns graph)))
          (parent-in-old-cols (- (plot-dag-git-graph-num-parents graph) added-cols))
          ;; In both cases, commit_index corresponds to the edge labeled "0".
          (first-col (+ (plot-dag-git-graph-commit-index graph)
                        dashless-parents
                        (- parent-in-old-cols))))
    (cl-loop for i from 0 below dashful-parents
             do
             ;; TODO: annotate w/ column (nth (+ i first-col) (plot-dag-git-graph-new-columns graph))
	     (insert "-")
	     (insert (if (eq i (- dashful-parents 1)) "." "-")))
    (* 2 dashful-parents)))

;; (plot-dag-graph-output-commit-line :: Mixed -> Mixed -> Void)
(defun plot-dag-graph-output-commit-line (graph)
  ""
  (message "(plot-dag-graph-output-commit-line %s)" graph)
  ;; Output the row containing this commit Iterate up to and including
  ;; graph->num_columns, since the current commit may not be in any of
  ;; the existing columns.  (This happens when the current commit
  ;; doesn't have any children that we have already processed.)
  (-let* ((seen-this nil)
          (chars-written 0))
    (cl-loop for i from 0 below (plot-dag-git-graph-num-columns graph)
             for col = (elt (plot-dag-git-graph-columns graph) i)
	     for col-commit = nil
             do
	     (if (eq i (plot-dag-git-graph-num-columns graph))
                 (progn
		   (when seen-this
		     (cl-return))
		   (setq col-commit (plot-dag-git-graph-commit graph)))
	       (setq col-commit (plot-dag-column-commit (elt (plot-dag-git-graph-columns graph) i))))

	     (cond
              ((eq col-commit (plot-dag-git-graph-commit graph))
	       (setq seen-this t)
	       (plot-dag-graph-output-commit-char graph)
	       (cl-incf chars-written)

	       (when (> (plot-dag-git-graph-num-parents graph) 2)
	         (cl-incf chars-written (plot-dag-graph-draw-octopus-merge graph))))
	      ((and seen-this (> (plot-dag-git-graph-num-parents graph) 2))
               (insert (propertize "\\" 'column col))
	       (cl-incf chars-written))
	      ((and seen-this (eq (plot-dag-git-graph-num-parents graph) 2))
	       ;; This is a 2-way merge commit.
               ;; There is no :GRAPH_PRE_COMMIT stage for 2-way
               ;; merges, so this is the first line of output
               ;; for this commit.  Check to see what the previous
               ;; line of output was.
               ;;
               ;; If it was :GRAPH_POST_MERGE, the branch line
               ;; coming into this commit may have been '\',
               ;; and not '|' or '/'.  If so, output the branch
               ;; line as '\' on this line, instead of '|'.  This
               ;; makes the output look nicer.
	       (if (and (eq (plot-dag-git-graph-prev-state graph) :GRAPH_POST_MERGE)
                        (< (plot-dag-git-graph-prev-commit-index graph) i))
		   (insert (propertize "\\" 'column col))
                 (insert (propertize "|" 'column col)))
	       (cl-incf chars-written))
              (t
	       (insert (propertize "|" 'column col))
	       (cl-incf chars-written)))
	     (insert " ")
	     (cl-incf chars-written))

    (plot-dag-graph-pad-horizontally graph chars-written)

    (plot-dag-graph-update-state graph 
	                     (cond
                              ((> (plot-dag-git-graph-num-parents graph) 1)
	                       :GRAPH_POST_MERGE)
	                      ((plot-dag-graph-is-mapping-correct-p graph)
                               :GRAPH_PADDING)
	                      (t
	                       :GRAPH_COLLAPSING)))))

;; (plot-dag-find-new-column-by-commit :: Mixed -> Mixed -> Mixed
(defun plot-dag-find-new-column-by-commit (graph commit)
  "Return column."
  (message "(plot-dag-find-new-column-by-commit %s %s)" graph commit)
  (cl-loop for i from 0 below (plot-dag-git-graph-num-new-columns graph)
	   if (eq (plot-dag-column-commit (elt (plot-dag-git-graph-new-columns graph) i))
                  commit)
	   return (elt (plot-dag-git-graph-new-columns graph) i)))

;; (plot-dag-graph-output-post-merge-line :: Mixed -> Buffer -> Void)
(defun plot-dag-graph-output-post-merge-line (graph)
  ""
  (message "(plot-dag-graph-output-post-merge-line %s)" graph)
  (declare (wip TODO "test"))
  (-let* ((seen-this nil)
          (chars-written 0))
    (message "num-columns: %d" (plot-dag-git-graph-num-columns graph))
    ;; Output the post-merge row
    (cl-loop for i from 0 to (plot-dag-git-graph-num-columns graph)
             for col = (elt (plot-dag-git-graph-columns graph) i)
	     for col-commit = nil
             do
             (cl-block inner
               (if (eq i (plot-dag-git-graph-num-columns graph))
                   (progn
	             (when seen-this
	               (cl-return inner))
                     (setq col-commit (plot-dag-git-graph-commit graph)))
                 (setq col-commit (plot-dag-column-commit col)))

	       (cond
                ((eq col-commit (plot-dag-git-graph-commit graph))
                 (message "A")
	         ;; Since the current commit is a merge find the columns
                 ;; for the parent commits in new_columns and use those to
                 ;; format the edges.
                 (let* (parents par-column)
	           (setq seen-this t)
	           (setq parents (plot-dag-interesting-parents graph))
	           (assert parents)
	           (setq par-column (plot-dag-find-new-column-by-commit graph (first parents)))
	           (assert par-column)

                   (insert (propertize "|" 'column par-column))
	           (cl-incf chars-written)
	           (cl-loop ;;for j from 0 below (- (plot-dag-git-graph-num-parents graph) 1)
		    for parent in (cdr parents)
                    do
		    (assert parent)
		    (setq par-column (plot-dag-find-new-column-by-commit graph parent))
		    (assert par-column)
                    (insert (propertize "\\" 'column par-column))
		    (insert " "))
	           (setq chars-written (+ chars-written (* (plot-dag-git-graph-num-parents graph) 2)))))
	        (seen-this
                 (message "B")
                 (insert (propertize "\\" 'column col))
	         (insert " ")
	         (setq chars-written (+ chars-written 2)))
                (t
                 (message "C")
                 (insert (propertize "|" 'column col))
	         (insert " ")
	         (setq chars-written (+ chars-written 2))))))

    (plot-dag-graph-pad-horizontally graph chars-written)

    ;; Update graph->state
    (if (plot-dag-graph-is-mapping-correct-p graph)
        (plot-dag-graph-update-state graph :GRAPH_PADDING)
      (plot-dag-graph-update-state graph :GRAPH_COLLAPSING))))

;; (plot-dag-graph-output-collapsing-line :: Mixed -> Buffer -> Void)
(defun plot-dag-graph-output-collapsing-line (graph)
  ""
  (message "(plot-dag-graph-output-collapsing-line %s)" graph)
  (-let* ((used-horizontal 0)
          (horizontal-edge -1)
          (horizontal-edge-target -1))

    ;; Clear out the new_mapping array
    (cl-loop for i from 0 below (plot-dag-git-graph-mapping-size graph)
             do
	     (setf (elt (plot-dag-git-graph-new-mapping graph) i) -1))

    (cl-loop for i from 0 below (plot-dag-git-graph-mapping-size graph)
             do
	     (-let* ((target (elt (plot-dag-git-graph-mapping graph) i)))
	       (unless (< target 0)
	         ;; Since update_columns() always inserts the leftmost
                 ;; column first, each branch's target location should
                 ;; always be either its current location or to the left of
                 ;; its current location.
                 ;;
                 ;; We never have to move branches to the right.  This makes
                 ;; the graph much more legible, since whenever branches
                 ;; cross, only one is moving directions.
	         (assert (<= (* target 2) i))

	         (cond
                  ((eq (* target 2) i)
		   ;; This column is already in the
                   ;; correct place
		   (assert (eq (elt (plot-dag-git-graph-new-mapping graph) i) -1))
		   (setf (elt (plot-dag-git-graph-new-mapping graph) i) target))
                  ((< (elt (plot-dag-git-graph-new-mapping graph) (- i 1)) 0)
		   ;; Nothing is to the left.
                   ;; Move to the left by one
		   (setf (elt (plot-dag-git-graph-new-mapping graph) (- i 1)) target)
		   ;; If there isn't already an edge moving horizontally
                   ;; select this one.
		   (when (eq horizontal-edge -1)
		     (setq horizontal-edge i)
		     (setq horizontal-edge-target target)
		     ;; The variable target is the index of the graph
                     ;; column, and therefore target*2+3 is the
                     ;; actual screen column of the first horizontal
                     ;; line.
		     (cl-loop with j = (+ (target * 2) 3)
                              while (< j (i - 2))
                              do
			      (setf (elt (plot-dag-git-graph-new-mapping graph) j) target)
                              (cl-incf j 2))))
		  ((eq (nth (- i 1) (plot-dag-git-graph-new-mapping graph)) target)
		   ;; There is a branch line to our left
                   ;; already, and it is our target.  We
                   ;; combine with this line, since we share
                   ;; the same parent commit.
                   ;;
                   ;; We don't have to add anything to the
                   ;; output or new_mapping, since the
                   ;; existing branch line has already taken
                   ;; care of it.
                   nil)
                  (_
		   ;; There is a branch line to our left,
                   ;; but it isn't our target.  We need to
                   ;; cross over it.
                   ;;
                   ;; The space just to the left of this
                   ;; branch should always be empty.
                   ;;
                   ;; The branch to the left of that space
                   ;; should be our eventual target.
		   (assert (> (nth (- i 1) (plot-dag-git-graph-new-mapping graph)) target))
		   (assert (< (nth (- i 2) (plot-dag-git-graph-new-mapping graph)) 0))
		   (assert (eq (nth (- i 3) (plot-dag-git-graph-new-mapping graph)) target))
		   (setf (nth (- i 2) (plot-dag-git-graph-new-mapping graph)) target)
		   ;; Mark this branch as the horizontal edge to
                   ;; prevent any other edges from moving
                   ;; horizontally.
		   (when (eq horizontal-edge -1)
		     (setq horizontal-edge i))))

	         ;; The new mapping may be 1 smaller than the old mapping
	         (when (< (elt (plot-dag-git-graph-new-mapping graph) (- (plot-dag-git-graph-mapping-size graph) 1)) 0)
	           (cl-decf (plot-dag-git-graph-mapping-size graph)))

	         ;; Output out a line based on the new mapping info
	         (cl-loop for i from 0 below (plot-dag-git-graph-mapping-size graph)
                          for target = (elt (plot-dag-git-graph-new-mapping graph) i)
                          do
		          (cond
                           ((< target 0)
		            (insert " "))
                           ((eq (* target 2) i)
		            (insert (propertize "|" 'column (elt (plot-dag-git-graph-new-columns graph) target))))
		           ((and (eq target horizontal-edge-target)
                                 (not (eq i (- horizontal-edge 1))))
		            ;; Set the mappings for all but the first
                            ;; segment to -1 so that they won't continue
                            ;; into the next line.
		            (when (not (eq i (+ (* target 2) 3)))
		              (setf (elt (plot-dag-git-graph-new-mapping graph) i) -1))
		            (setq used-horizontal 1)
		            (insert (propertize "_" 'column (elt (plot-dag-git-graph-new-columns graph) target))))
                           (t
		            (when (and used-horizontal
                                       (< i horizontal-edge))
		              (setf (elt (plot-dag-git-graph-new-mapping graph) i) -1))
		            (insert (propertize "/" 'column (elt (plot-dag-git-graph-new-columns graph) target))))))

	         (plot-dag-graph-pad-horizontally graph (plot-dag-git-graph-mapping-size graph))

	         ;; Swap mapping and new_mapping
	         (setf (plot-dag-git-graph-mapping graph) (plot-dag-git-graph-new-mapping graph))

	         ;; If graph->mapping indicates that all of the branch lines
                 ;; are already in the correct positions, we are done.
                 ;; Otherwise, we need to collapse some branch lines together.
	         (when (plot-dag-graph-is-mapping-correct-p graph)
	           (plot-dag-graph-update-state graph :GRAPH_PADDING)))))))

;; (plot-dag-graph-next-line :: Mixed -> Buffer -> Number)
(defun plot-dag-graph-next-line (graph)
  "Output the next line for a graph.
This formats the next graph line into the specified strbuf.  It
is not terminated with a newline.

Returns 1 if the line includes the current commit, and 0
otherwise.  graph_next_line() will return 1 exactly once for each
time graph_update() is called.

NOTE: This function isn't used in Git outside graph.c but it is
used by CGit (http://git.zx2c4.com/cgit/) to wrap HTML around
graph lines."
  (message "(plot-dag-graph-next-line %s)" graph)
  (message "(plot-dag-git-graph-state graph): %s" (plot-dag-git-graph-state graph))
  (pcase (plot-dag-git-graph-state graph)
    (:GRAPH_PADDING
     (plot-dag-graph-output-padding-line graph)
     nil)
    (:GRAPH_SKIP
     (plot-dag-graph-output-skip-line graph)
     nil)
    (:GRAPH_PRE_COMMIT
     (plot-dag-graph-output-pre-commit-line graph)
     nil)
    (:GRAPH_COMMIT
     (plot-dag-graph-output-commit-line graph)
     t)
    (:GRAPH_POST_MERGE
     (plot-dag-graph-output-post-merge-line graph)
     nil)
    (:GRAPH_COLLAPSING
     (plot-dag-graph-output-collapsing-line graph)
     nil)
    (_
     ;; FIXME?
     (assert nil)
     nil)))

;; (plot-dag-graph-padding-line :: Mixed -> Buffer -> Void)
(defun plot-dag-graph-padding-line (graph)
  ""
  (message "(plot-dag-graph-padding-line %s)" graph)
  (-let* ((chars-written 0))
    (if (not (eq (plot-dag-git-graph-state graph) :GRAPH_COMMIT))
        (plot-dag-graph-next-line graph)
      ;; Output the row containing this commit
      ;; Iterate up to and including graph->num_columns,
      ;; since the current commit may not be in any of the existing
      ;; columns.  (This happens when the current commit doesn't have any
      ;; children that we have already processed.)
      (cl-loop for i from 0 below (plot-dag-git-graph-num-columns graph)
	       for col = (elt (plot-dag-git-graph-columns graph) i)
               do
	       (insert (propertize "|" 'column col))
	       (cl-incf chars-written)

	       (if (and (eq (plot-dag-column-commit col)
                            (plot-dag-git-graph-commit graph))
                        (> (plot-dag-git-graph-num-parents graph) 2))
                   (progn
		     (setq len (* (- (plot-dag-git-graph-num-parents graph) 2) 2))
		     (insert (make-string len ?\ ))
		     (cl-incf chars-written len))
	         (insert " ")
	         (cl-incf chars-written)))
      (plot-dag-graph-pad-horizontally graph chars-written)

      ;; Update graph->prev_state since we have output a padding line
      (setf (plot-dag-git-graph-prev-state graph) :GRAPH_PADDING)
      nil)))

;; (plot-dag-graph-is-commit-finished-p :: Mixed -> Number)
(defun plot-dag-graph-is-commit-finished-p (graph)
  "Determine if a graph has finished outputting lines for the
current commit.

Returns 1 if graph_next_line() needs to be called again before
graph_update() should be called.  Returns 0 if no more lines are needed
for this commit.  If 0 is returned, graph_next_line() may still be
called without calling graph_update(), and it will merely output
appropriate \"vertical padding\" in the graph."
  (message "(plot-dag-graph-is-commit-finished-p _)")
  (let* ((is-finished-p (eq (plot-dag-git-graph-state graph) :GRAPH_PADDING)))
    (message "is-finished-p: %s" is-finished-p)
    is-finished-p))

;; (plot-dag-graph-show-commit :: Mixed -> Void)
(defun plot-dag-graph-show-commit (graph)
  "If the graph is non-NULL, print the history graph to stdout,
up to and including the line containing this commit.  Does not
print a terminating newline on the last line."
  (message "(plot-dag-graph-show-commit %s)" graph)
  (-let* ((shown-commit-line nil))

    ;; (graph-show-line-prefix default-diffopt)

    (unless (null graph)
      ;; When showing a diff of a merge against each of its parents, we
      ;; are called once for each parent without graph_update having been
      ;; called.  In this case, simply output a single padding line.
      (when (plot-dag-graph-is-commit-finished-p graph)
        (message "is-finished-p: t")
        (plot-dag-graph-show-padding graph)
        (setq shown-commit-line t))

      (while (and (not shown-commit-line)
                  (not (plot-dag-graph-is-commit-finished-p graph)))
        (message "loop")
        (setq shown-commit-line (plot-dag-graph-next-line graph))
        ;; (insert msgbuf ;; (diffopt-file (rev-info-diffopt (plot-dag-git-graph-revs graph)))
        ;;         )
        
        (when (not shown-commit-line)
	  (insert "\n" ;; (diffopt-file (rev-info-diffopt (plot-dag-git-graph-revs graph)))
                  )
	  ;; (graph-show-line-prefix (rev-info-diffopt (plot-dag-git-graph-revs graph)))
          )
        (- (point-max) (point-min);; strbuf-setlen msgbuf 0
           )))))

;; (plot-dag-graph-show-oneline :: Mixed -> Void)
(defun plot-dag-graph-show-oneline (graph)
  "If the graph is non-NULL, print one line of the history graph to stdout.
Does not print a terminating newline on the last line."
  (message "(plot-dag-graph-show-oneline _)")
  ;; (with-temp-buffer
  ;; (-let* ((msgbuf (current-buffer))) ;; STRBUF_INIT
  ;; (graph-show-line-prefix default-diffopt)
  (unless (null graph)
    (plot-dag-graph-next-line graph)
    ;; (insert (buffer-string) ;; msgbuf.buf (sizeof char) msgbuf.len graph->revs->diffopt.file
    ;;         )
    ))

;; (plot-dag-graph-show-padding :: Mixed -> Void)
(defun plot-dag-graph-show-padding (graph)
  "If the graph is non-NULL, print one line of vertical graph
  padding to stdout.  Does not print a terminating newline on the
  last line."
  (message "(plot-dag-graph-show-padding _)")
  (declare (wip TODO "finish transcoding"))
  ;;(-let* ((msgbuf ""))
  ;; (graph-show-line-prefix default-diffopt)
  (unless (null graph)
    (plot-dag-graph-padding-line graph)
    ;; (fwrite msgbuf.buf (sizeof char) msgbuf.len graph->revs->diffopt.file)
    ))

;; (plot-dag-graph-show-remainder :: Mixed -> Number)
(defun plot-dag-graph-show-remainder (graph)
  "If the graph is non-NULL, print the rest of the history graph
for this commit to stdout.  Does not print a terminating newline
on the last line."
  (message "(plot-dag-graph-show-remainder _)")
  (-let* (;; (msgbuf (current-buffer)) ;; STRBUF_INIT;
          (shown nil))
    ;; (graph-show-line-prefix default-diffopt)

    (if (or (null graph)
            (plot-dag-graph-is-commit-finished-p graph))
        nil
      (cl-loop while t
               do
               (plot-dag-graph-next-line graph)
               ;; (insert msgbuf.buf (sizeof char) msgbuf.len graph->revs->diffopt.file)
               ;; (strbuf-setlen msgbuf 0)
               (setq shown t)

               (if (not (plot-dag-graph-is-commit-finished-p graph))
                   (progn
	             (insert "\n")
	             ;; (graph-show-line-prefix graph->revs->diffopt)
                     )
                 (cl-return)))
      shown)))

;; (plot-dag-graph-show-strbuf :: Mixed -> Mixed -> Mixed)
(defun plot-dag-graph-show-strbuf (graph buffer)
  "Print a strbuf.  If the graph is non-NULL, all lines but the first will be prefixed with the graph output.

If the strbuf ends with a newline, the output will end after this
newline.  A new graph line will not be printed after the final
newline.  If the strbuf is empty, no output will be printed.

Since the first line will not include the graph output, the
caller is responsible for printing this line's graph (perhaps via
graph_show_commit() or graph_show_oneline()) before calling
plot-dag-graph-show-strbuf().

 TODO:
 - Limit the number of columns, similar to the way gitk does.
   If we reach more than a specified number of columns, omit
   sections of some columns."
  (message "(plot-dag-graph-show-strbuf _)")
  ;; Print the strbuf line by line, and display the graph info before each line but the first.
  (-let* ((lines (with-current-buffer buffer
                   (s-lines (buffer-string)))))
    (cl-loop for line in lines
             do
             (insert line "\n")
             (plot-dag-graph-show-oneline graph))))
;; (while p
;;   (-let* ((len nil)
;;           (next-p (strchr p "\n")))
;;     (if next-p
;;         (progn
;;           (cl-incf next-p)
;;           (setq len (- next-p p)))
;;       (setq len (- (+ buffer
;;                       (length ))
;;                    p)))
;;     (insert p)
;;     (when (and next-p
;;                (not (eq next-p "\0")))
;;       (plot-dag-graph-show-oneline graph))
;;     (setq p next-p)))))

;; (plot-dag-graph-show-commit-msg :: Mixed -> Mixed -> Void)
(defun plot-dag-graph-show-commit-msg (graph buffer)
  "Print a commit message strbuf and the remainder of the graph to BUFFER.

This is similar to graph_show_strbuf(), but it always prints the
remainder of the graph.

If the strbuf ends with a newline, the output printed by
graph_show_commit_msg() will end with a newline.  If the strbuf
is missing a terminating newline (including if it is empty), the
output printed by graph_show_commit_msg() will also be missing a
terminating newline.

Note that unlike some other graph display functions, you must
pass the file handle directly. It is assumed that this is the
same file handle as the file specified by the graph diff
options. This is necessary so that graph_show_commit_msg can be
called even with a NULL graph."
  (message "(plot-dag-graph-show-commit-msg _)")
  (with-current-buffer buffer
    (kill-region (point-min) (point-max))
    (-let* (newline-terminated)
      ;; Show the commit message
      (plot-dag-graph-show-strbuf graph buffer)

      (unless (null graph)
        (setq newline-terminated (and (> (point-min) 1)
                                      (equal (buffer-substring (- (point-max) 1) (point-max)) "\n")))
        ;; If there is more output needed for this commit, show it now
        (when (not (plot-dag-graph-is-commit-finished-p graph))
          ;; If buffer doesn't have a terminating newline, print one now,
          ;; so we can start the remainder of the graph output on a
          ;; new line.
          (when (not newline-terminated)
            (goto-char (point-max))
	    (insert "\n"))

          (plot-dag-graph-show-remainder graph)

          ;; If buffer ends with a newline, our output should too.
          (when newline-terminated
	    (insert "\n")))))
    ;; CLEANUP: remove this once finished
    (switch-to-buffer buffer)))


(defun plot-dag-show-nodes (graph nodes)
  "Taken from https://github.com/sambrightman/asciidag.

\"Show an ASCII DAG for the nodes provided.

Nodes are walked and returned without duplicates and then sorted
topologically (a requirement of the algorithm). The original Git
API is then used internally to display the graph line-by-line,
outputting the Node's content at the relevant point.

Args: tips (:obj:`list` of :obj:`Node`): tips of trees to display\""
  (message "(plot-dag-show-nodes %s %s)" graph nodes)
  (declare (wip TODO "test"
                TODO "rewrite docstring"))
  (with-temp-buffer
    (let* ((iter-nodes (plot-dag-sort-in-topological-order (cl-loop for n iter-by (plot-dag--once (plot-dag--walk-nodes nodes))
                                                                collect n)))
           (nodes (cl-loop for node iter-by iter-nodes
                           collect node)))
      (cl-loop for node in nodes
               do
               (plot-dag-graph-update graph node)
               (plot-dag-graph-show-commit graph)
               (insert (prin1-to-string (plot-dag-node-item node)))
               (when (not (plot-dag-graph-is-commit-finished-p graph))
                 (insert "\n")
                 (plot-dag-graph-show-remainder graph))
               (insert "\n")))
    (buffer-substring (point-min) (point-max))))

(iter-defun plot-dag--walk-nodes (nodes)
  "Taken from https://github.com/sambrightman/asciidag."
  (cl-loop for node in nodes
           do
           (iter-yield node))
  (cl-loop for node in nodes
           do
           (cl-loop for ancestor iter-by (plot-dag--walk-nodes (plot-dag-node-parents node))
                    do
                    (iter-yield ancestor))))

(iter-defun plot-dag--once (iter-nodes)
  "Based on https://github.com/sambrightman/asciidag."
  (let* ((seen (make-hash-table)))
    (cl-loop for node iter-by iter-nodes
             if (not (gethash node seen))
             do
             (puthash node t seen)
             (iter-yield node))))

(iter-defun plot-dag-sort-in-topological-order (nodes)
  "Taken from https://github.com/sambrightman/asciidag."
  (let* ((in-degree (make-hash-table)))

    (cl-loop for node in nodes
             do
             (puthash node 1 in-degree))

    (cl-loop for node in nodes
             do
             (cl-loop for parent in (plot-dag-node-parents node)
                      if (> (or (gethash parent in-degree) 0) 0)
                      do
                      (puthash parent
                               (1+ (or (gethash parent in-degree) 0))
                               in-degree)))

    (let* ((queue (cl-loop for node being the hash-keys of in-degree
                           using (hash-values degree)
                           if (eq degree 1)
                           collect node))) ;; CLEANUP: one point of potential reversal
      (cl-loop for node in queue
               do
               (cl-loop for parent in (plot-dag-node-parents node)
                        do
                        (when (not (eq (or (gethash parent in-degree) 0) 0))
                          (puthash
                           parent
                           (1- (or (gethash parent in-degree) 0))
                           in-degree)
                          (when (eq (or (gethash parent in-degree) 0) 1)
                            (setcdr (last queue) (list parent)))))
               (puthash node 0 in-degree)
               (iter-yield node)))))

;; (cl-loop for n iter-by (plot-dag-sort-in-topological-order (list (plot-dag-node-from-list '(1 2))))
;;          collect n)

;; (cl-loop for n iter-by (plot-dag-sort-in-topological-order (plot-dag-node-from-list '(1 2)))
;;          collect n)


(provide 'plot-git-dag)
