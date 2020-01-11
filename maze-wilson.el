(require 'maze-data)
(require 'maze-utils)

(defun maze/--wilson-path (maze used-cells path path-cells))

(defun maze/--carve-wilson (maze)
  (let* ((maze-size (maze/get-cells-number maze))
         (first-cell-index (/ maze-size 2))
         (used-cells (make-hash-table)))
    (puthash first-cell-index t used-cells)    
    (while (< (hash-table-count used-cells) maze-size)
      )) 
  )

(defun pick-random-cell)

(defun maze/wilson (columns rows)
  (maze/--carve-wilson (maze/create-empty columns rows)))

(provide 'maze-wilson)
