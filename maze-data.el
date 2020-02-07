(require 'cl)
(require 'dash)
(require 'maze-map)

(cl-defstruct maze rows columns connections)

(defun maze/create-empty (columns rows)
  (make-maze :rows rows
             :columns columns
             :connections (maze/map-create :test 'equal)))

(defun maze/copy (source)
  (make-maze :rows (maze-rows source)
             :columns (maze-columns source)
             :connections (maze/map-copy (maze-connections source))))

(defun maze/error-for-cell (maze cell)
  (let ((column (car cell))
        (row (cadr cell)))
    (cond
     ((< column 0) (format "Negative column (%s, %s)" column row))
     ((< row 0) (format "Negative row (%s, %s)" column row))
     ((>= column (maze-columns maze)) (format "Out of bound column (%s, %s)" column row))
     ((>= row (maze-rows maze)) (format "Out of bound row (%s, %s)" column row)))))

(defun maze/valid-cell-p (maze cell)
  (not (maze/error-for-cell maze cell))) ()

(defun maze/check-extremes (maze column row)
  (let ((error-string (maze/error-for-cell maze (list column row))))
    (if error-string
        (error error-string))))

(defun maze/position-to-index (maze position)
  "Convers a position (column row) to an index"
  (let ((column (car position))
        (row (cadr position)))
    (maze/check-extremes maze column row)
    (+ column (* row (maze-columns maze)))))

(defun maze/check-index (maze index)
  (if (< index 0) (error "Index underflow"))
  (if (>= index (* (maze-columns maze) (maze-rows maze))) (error "Index underflow")))

(defun maze/index-to-position (maze index)
  (maze/check-index maze index)
  (let ((columns (maze-columns maze)))
    (list (mod index columns)
          (/ index columns))))

(defun maze/throw-when-not-neighbors (from to)
  (let ((distance (-reduce #'+ (--zip-with (abs (- it other)) from to))))
    (if (/= distance 1)
        (error "Non contiguous cells"))))

(defun maze/sort-passage (from to)
  (if (= from to)
      (error "invalid connection"))
  (if (> from to)
      (cons to from)
    (cons from to)))

(defun maze/carve-passage (maze from to)
  "Create a copy of the current maze with a new passage carved in

from and to are (column row) lists

Throws if the from and to cells are not neighbors"
  (maze/throw-when-not-neighbors from to)
  (let ((from-index (maze/position-to-index maze from))
        (to-index (maze/position-to-index maze to)))
    (let ((connection (maze/sort-passage from-index to-index)))
      (if (maze/map-get connection (maze-connections maze))
          maze
        (let ((new-maze (maze/copy maze)))
          (maze/map-put connection t (maze-connections new-maze))
          new-maze)))))

(defun maze/carve-passage-index (maze from-index to-index)
  (maze/carve-passage maze
                      (maze/index-to-position maze from-index)
                      (maze/index-to-position maze to-index)))

(defun maze/carve-path (maze cells)
  (let ((new-maze maze))
    (--each (-partition-in-steps 2 1 cells)
      (setq new-maze (maze/carve-passage-index new-maze (car it) (cadr it))))
    new-maze))

(defun maze/get-cells-number (maze)
  (* (maze-columns maze)
     (maze-rows maze)))

(defun maze/all-indices (maze)
  (number-sequence 0 (1- (maze/get-cells-number maze))))

(provide 'maze-data)
