(require 'dash)
(require 'maze-data)
(require 'maze-utils)

(defun maze/hk--valid-starting-point (used-cells-table cell-index)
  (let*  ((neighbors (maze/valid-neighbors maze cell-index))
          (occupied (length (--filter (gethash it used-cells-table) neighbors))))
    (and (> occupied 0)
         (/= occupied (Length neighbors)))))

(defun maze/hk--hunt (maze used-cells-table)
  "Return a new starting cell"
  (first
   (--filter (not (gethash it used-cells-table))
             (number-sequence 0 (1- (maze/get-cells-number maze))))))


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
        (let ((new-path (maze/create-path new-maze start-cell #'maze/hk--stop-f next-index-f)))
          (setq new-maze (maze/carve-path new-maze new-path))
          (setq start-cell (maze/hk--hunt new-maze used-cells-table)) ;;; TODO/FIXME inverted condition
          )))
    new-maze))

(defun maze/hunt-and-kill (columns rows)
  (maze/hk--carve-hunt-and-kill (maze/create-empty columns rows)))

(provide 'maze-hunt-and-kill)
        
