(require 'dash)
(require 'maze-data)
(require 'maze-utils)

(defun maze/hk--occupied-neighbors (used-cells-table cell-index)
  (--filter (gethash it used-cells-table) (--map (maze/position-to-index maze it) (maze/valid-neighbors maze (maze/index-to-position maze cell-index)))))

(defun maze/hk--valid-starting-point (maze used-cells-table cell-index)
  (if (not (gethash cell-index used-cells-table))
   (let*  ((neighbors (maze/valid-neighbors maze (maze/index-to-position maze cell-index)))
           (occupied (length (--filter (gethash (maze/position-to-index maze it) used-cells-table) neighbors))))
     (> occupied 0))))

;;; TODO/FIXME not lazy…
(defun maze/hk--first-valid-starting-point (maze used-cells-table)
  (first
   (--filter (maze/hk--valid-starting-point maze used-cells-table it)
                                         (maze/all-indices maze))))

(defun maze/hk--hunt (maze used-cells-table)
  "Return a cons of a new starting cell followed by an occupied cell to start from"
  (let ((starting-point (maze/hk--first-valid-starting-point maze used-cells-table)))
    (when starting-point
      (cons starting-point (maze/random-choice (maze/hk--occupied-neighbors used-cells-table starting-point))))))


(defun maze/hk--stop-f (maze current-index next-candidate)
  (if (not next-candidate)
      :last
    :continue))

(defun maze/hk--is-index-unvisited? (cell-index used-cells-table path-cells-table)
  (not (or (gethash it used-cells-table)
           (gethash it path-cells-table))))

(defun maze/hk--neighbor-indexes (maze current-index)
  (--map (maze/position-to-index maze it)
         (maze/valid-neighbors maze (maze/index-to-position maze current-index))))

(defun maze/hk--valid-path-moves (maze current-index used-cells-table all-generated-cells-table)
  (let ((neighbor-indexes (maze/hk--neighbor-indexes maze current-index)))
        (--filter (maze/hk--is-index-unvisited? it used-cells-table all-generated-cells-table)
                  neighbor-indexes)))

(defun maze/hk--next-unused-cell (maze current-index used-cells-table all-generated-cells-table)
  (let* ((valid-next-indices (maze/hk--valid-path-moves maze current-index used-cells-table all-generated-cells-table))
         (selected-index (maze/random-choice valid-next-indices)))
    (when selected-index
      (puthash selected-index t all-generated-cells-table)
      selected-index)))

(defun maze/hk--next-path-cell-function-generator (used-cells-table)
  (lexical-let ((used-cells-table used-cells-table)
                (all-generated-cells-table (make-hash-table)))
    (lambda (maze current-index)
      (maze/hk--next-unused-cell maze current-index
                                 used-cells-table
                                 all-generated-cells-table))))

;;; TODO/FIXME side effects everywhere
(defun maze/hk--carve-hunt-and-kill (maze)
  (let ((new-maze maze)
        (maze-size (maze/get-cells-number maze))
        (used-cells-table (make-hash-table))
        (start-cell 0))
    (let ((next-index-f (maze/hk--next-path-cell-function-generator used-cells-table)))
      (while (< (hash-table-count used-cells-table)
                (maze/get-cells-number new-maze))
        (puthash start-cell t used-cells-table)
        ;;; TODO/FIXME this whole module is a thing of nightmare
        (let ((new-path (maze/create-path new-maze start-cell #'maze/hk--stop-f next-index-f)))
          (--each new-path (puthash it t used-cells-table))
          (setq new-maze (maze/carve-path new-maze new-path))
          (setq start-previous (maze/hk--hunt new-maze used-cells-table)) ;;; TODO/FIXME inverted condition
          (when start-previous
            (setq new-maze (maze/carve-passage-index new-maze (car start-previous) (cdr start-previous)))
            (setq start-cell (car start-previous)))
          )))
    new-maze))

(defun maze/hunt-and-kill (columns rows)
  (maze/hk--carve-hunt-and-kill (maze/create-empty columns rows)))

(provide 'maze-hunt-and-kill)
        
