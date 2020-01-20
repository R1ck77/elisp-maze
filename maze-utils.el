(require 'maze-data)

(defmacro comment (&rest forms)) ; TODO/FIXME tests?

(defconst maze/random-state (cl-make-random-state t)
  "The random state used to generate the mazes")

(defvar maze/debug t
  "Print the current random state before generating each maze")

(defmacro maze/for-each-cell (maze &rest forms)
  "Executes each form with row and column bound to the current position"
  (declare (indent defun))
  (let ((index (make-symbol "index"))
        (tmp (make-symbol "tmp")))
    `(-each (maze/all-indices ,maze)
       (lambda (index)
         (let ((,tmp (maze/index-to-position ,maze index)))
           (let ((column (car ,tmp))
                 (row (cadr ,tmp)))
             ,@forms))))))

(defmacro maze/until (condition &rest forms)
  (declare (indent defun)
           (debug t))
  (let ((condition-result (make-symbol "condition-result"))
        (result (make-symbol "result")))
    `(let ((,condition-result nil))
       (let ((,result))
         (while (not ,condition-result)
           (setq ,result (progn ,@forms))
           (setq ,condition-result ,condition))
         ,result))))

(defun maze/random-choice (items)
  (if items
      (let ((size (length items)))
        (nth (cl-random size maze/random-state)
             items))))

(defun maze/utils--unoccupied-cells (maze exclusion-table)
  (--filter (not (gethash it exclusion-table)) (maze/all-indices maze)))

(defun maze/random-cell (maze &optional exclusion-table)
  (maze/random-choice
   (maze/utils--unoccupied-cells maze (or exclusion-table (make-hash-table)))))

(defun maze/left-cell (column row)
  (list (1- column) row))

(defun maze/right-cell (column row)
  (list (1+ column) row))

(defun maze/top-cell (column row)
  (list column (1- row)))

(defun maze/bottom-cell (column row)
  (list column (1+ row)))

(defun maze/valid-moves (maze column row)
  "Returns an association list of valid neighbors for a specific coordinate

The list has the :top :right :bottom :left keys and can be queried with assq"
  (--filter (apply #'maze/valid-cell-p (list maze (cdr it)))
            (list (cons :right (maze/right-cell column row))
                  (cons :bottom (maze/bottom-cell column row))
                  (cons :top (maze/top-cell column row))
                  (cons :left (maze/left-cell column row)))))

(defun maze/valid-neighbors (maze position)
  "Returns an association list of valid neighbors for a specific coordinate

position is in the form (column raw)

The list has the :top :right :bottom :left keys and can be queried with assq"
  (let ((column (car position))
        (row (cadr position)))
    (--filter (apply #'maze/valid-cell-p (list maze it))
              (list (maze/right-cell column row)
                    (maze/bottom-cell column row)
                    (maze/top-cell column row)
                    (maze/left-cell column row)))))


(defun maze/random-walk-step (maze current-index)
  "Return a random position from the current one"
  (let ((legal-moves (maze/valid-neighbors maze (maze/index-to-position maze current-index))))
    (maze/position-to-index maze (maze/random-choice legal-moves))))

(defun maze/utils--unfold-keep-going (next-value next-seed)
  "Return value for unfold after a generic non visited cell has been added"
  (cons next-value (cons next-seed nil)))

(defun maze/utils--unfold-reached-visited-cell (next-value next-seed)
  "Return value for unfold after a visited cell has been reached

The next result will be the last"
  (cons next-value (cons next-seed t)))

(defun maze/utils--unfold-stop-after-next-insertion (next-value-to-insert)
  "Add the current (visited) cell and then stop the next iteration"
  (cons next-value-to-insert '(nil . nil)))

(defun maze/utils--looped-path-unfold (maze stop-f next-cell-f current-index is-end-cell)
  "Unfold a new looped path that connects a starting cell with one of (possibly many) visited cells"
  (if current-index
      (if is-end-cell
          (maze/utils--unfold-stop-after-next-insertion current-index)
        (let* ((next-position (funcall next-cell-f maze current-index))
               (status (funcall stop-f maze current-index next-position)))
          (cond
           ((eq status :last)
            (maze/utils--unfold-reached-visited-cell current-index next-position))
           ((eq status :continue)
            (maze/utils--unfold-keep-going current-index next-position))
           ((eq status :stop) nil)
           (t (error "Unsupported result from stop-f")))))))

(defun maze/create-path (maze start stop-f next-cell-f)
  "Return a list of indices representing a random path in the maze

maze is the maze representing the path
start is the starting cell (will always be included in the path)
stop-p is a function that returns :stop :continue or :last
next-cell-f is a function that returns the new cell from the maze and the index of the current one

Returns a list of cells with at least one cell."
  (append (-unfold  (lambda (seed)
                      (maze/utils--looped-path-unfold maze
                                                      stop-f
                                                      next-cell-f
                                                      (car seed) (cdr seed)))
                    (cons start nil))))

(provide 'maze-utils)
