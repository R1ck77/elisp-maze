(require 'cl)
(require 'dash)
;;;; missing tests

(cl-defstruct maze rows columns connections)

(defun maze/create-empty (columns rows)
  (make-maze :rows rows
             :columns columns
             :connections (make-hash-table :test 'equal)))

(defun maze/copy (source)
  (make-maze :rows (maze-rows source)
             :columns (maze-columns source)
             :connections (copy-hash-table (maze-connections source))))

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

;;; TODO/FIXME Bad interface. Change to (maze pos)
(defun maze/position-to-index (maze column row)
  (maze/check-extremes maze column row)
  (+ column (* row (maze-columns maze))))

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

;;; TODO/FIXME very slow: a mutating version of the maze is probably required
(defun maze/carve-passage (maze from to)
  "Create a copy of the current maze with a new passage carved in

from and to are (column row) lists

Throws if the from and to cells are not neighbors"
  (maze/throw-when-not-neighbors from to)
  (let ((from-index (apply #'maze/position-to-index (cons maze from)))
        (to-index (apply #'maze/position-to-index (cons maze to))))
    (let ((connection (maze/sort-passage from-index to-index)))
      (if (gethash connection (maze-connections maze))
          maze
        (let ((new-maze (maze/copy maze)))
          (puthash connection t (maze-connections new-maze))
          new-maze)))))

(defun maze/carve-passage-index (maze from-index to-index)
  (maze/carve-passage maze
                      (maze/index-to-position maze from-index)
                      (maze/index-to-position maze to-index)))

(defun maze/get-cells-number (maze)
  (* (maze-columns maze)
     (maze-rows maze)))

(provide 'maze-data)
