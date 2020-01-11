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

(defun maze/check-extremes (maze column row)
  (let ((error-string (maze/error-for-cell maze (list column row))))
    (if error-string
        (error error-string))))

(defun maze/position-to-index (maze column row)
  (maze/check-extremes maze column row)
  (+ column (* row (maze-columns maze))))

(defun maze/index-to-position (maze index)
  (maze/check-index maze index)
  (let ((columns (maze-columns maze)))
    (list (mod index columns)
          (/ index columns))))

(provide 'maze)
