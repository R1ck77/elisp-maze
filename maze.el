(require 'cl)
(require 'dash)

(cl-defstruct maze rows columns connections)

(defun maze/create-empty (columns rows)
  (make-maze :rows rows
             :columns columns
             :connections (make-hash-table :test 'equal)))

(defun maze/copy (source)
  (make-maze :rows (maze-rows source)
             :columns (maze-columns source)
             :connections (copy-hash-table (maze-connections source))))

(provide 'maze)
