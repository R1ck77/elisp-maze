(require 'maze-data)

(defmacro maze/for-each-cell (maze &rest forms)
  "Executes each form with row and column bound to the current position"
  (declare (indent defun))
  (let ((index (make-symbol "index"))
        (tmp (make-symbol "tmp")))
    `(-each (number-sequence 0 (1- (* (maze-rows ,maze)
                                      (maze-columns ,maze))))
       (lambda (index)
         (let ((,tmp (maze/index-to-position ,maze index)))
           (let ((column (car ,tmp))
                 (row (cadr ,tmp)))
             ,@forms))))))

(defun maze/random-choice (items)
  "Straight from the Rosetta Code"
  (let* ((size (length items))
         (index (random size)))
    (nth index items)))

(defun maze/--unoccupied-cells (maze exclusion-table)
  (--filter (not (gethash it exclusion-table))
            (number-sequence 0 (1- (maze/get-cells-number maze)))))

(defun maze/random-cell (maze &optional exclusion-table)
  (maze/random-choice
   (maze/--unoccupied-cells maze (or exclusion-table (make-hash-table)))))

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




(provide 'maze-utils)
