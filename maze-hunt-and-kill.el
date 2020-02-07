(require 'dash)
(require 'maze-map)
(require 'maze-data)
(require 'maze-utils)

(defun maze/hk--occupied-neighbors (used-cells-table cell-index)
  (--filter (maze/map-get it used-cells-table) (--map (maze/position-to-index maze it) (maze/valid-neighbors maze (maze/index-to-position maze cell-index)))))

(defun maze/hk--valid-starting-point (maze used-cells-table cell-index)
  (if (not (maze/map-get cell-index used-cells-table))
   (let* ((neighbors (maze/valid-neighbors maze (maze/index-to-position maze cell-index)))
           (occupied (length (--filter (maze/map-get (maze/position-to-index maze it) used-cells-table) neighbors))))
     (> occupied 0))))

(defun maze/hk--first-valid-starting-point (maze used-cells-table)
  (car
   (--drop-while (not (maze/hk--valid-starting-point maze used-cells-table it))
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
  (not (or (maze/map-get it used-cells-table)
           (maze/map-get it path-cells-table))))

(defun maze/hk--neighbor-indexes (maze current-index)
  (--map (maze/position-to-index maze it)
         (maze/valid-neighbors maze (maze/index-to-position maze current-index))))

(defun maze/hk--valid-path-moves (maze current-index used-cells-table all-generated-cells-table)
  (let ((neighbor-indexes (maze/hk--neighbor-indexes maze current-index)))
        (--filter (maze/hk--is-index-unvisited? it used-cells-table all-generated-cells-table)
                  neighbor-indexes)))

(defun maze/hk--next-unused-cell! (maze current-index used-cells-table all-generated-cells-table)
  "Changes the content of all-generated-cells-table"
  (let* ((valid-next-indices (maze/hk--valid-path-moves maze current-index used-cells-table all-generated-cells-table))
         (selected-index (maze/random-choice valid-next-indices)))
    (when selected-index
      (maze/map-put selected-index t all-generated-cells-table)
      selected-index)))

(defun maze/hk--next-path-cell-function-generator (used-cells-table)
  (lexical-let ((used-cells-table used-cells-table)
                (all-generated-cells-table (maze/map-create)))
    (lambda (maze current-index)
      (maze/hk--next-unused-cell! maze current-index
                                  used-cells-table
                                  all-generated-cells-table))))

(defun maze/hk--has-unvisited-cells? (maze visited-cells-table)
  (< (maze/map-count visited-cells-table)
     (maze/get-cells-number maze)))

(defun maze/hk--carve-path! (maze visited-cells-table new-path)
  "Modifies visited-cells-table, and returns a new maze"
  (--each new-path (maze/map-put it t visited-cells-table))
  (maze/carve-path maze new-path))

(defun maze/hk--carve-hunt-and-kill (new-maze used-cells-table start-cell)
  (let ((next-index-f (maze/hk--next-path-cell-function-generator used-cells-table)))
    (while (maze/hk--has-unvisited-cells? new-maze used-cells-table)
      (maze/map-put start-cell t used-cells-table)
      (setq new-maze (maze/hk--carve-path! new-maze used-cells-table (maze/create-path new-maze start-cell #'maze/hk--stop-f next-index-f)))
      (setq start-previous (maze/hk--hunt new-maze used-cells-table))
      (when start-previous
        (setq new-maze (maze/carve-passage-index new-maze (car start-previous) (cdr start-previous)))
        (setq start-cell (car start-previous)))))
  new-maze)

(defun maze/hunt-and-kill (columns rows)
  (let ((empty-maze (maze/create-empty columns rows))
        (empty-used-cells-table (maze/map-create)))
    (maze/hk--carve-hunt-and-kill empty-maze empty-used-cells-table 0)))

(provide 'maze-hunt-and-kill)
        
