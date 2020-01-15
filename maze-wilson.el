(require 'dash)
(require 'maze-data)
(require 'maze-utils)

(defun maze/wilson-erase-last-loop (path index)
  (--drop-while (/= it index) path))

(defun maze/wilson-erase-loops (path)
  "Return a path with loops erased"
  (let ((tmp))
    (--each path (if (seq-contains tmp it)
                     (setq tmp (maze/wilson-erase-last-loop tmp it))
                   (progn
                     (setq tmp (cons it tmp)))))
    (nreverse tmp)))

(defun maze/wilson--next-path-cell (maze current-index)
  (maze/random-walk-step maze current-index))

(defun maze/wilson--generate-stop-f (used-cells)
  (lexical-let ((used-cells used-cells))
    (lambda (maze current-index next-candidate)
      (if (gethash next-position used-cells)
          :last
        :continue))))

(defun maze/wilson--path (maze used-cells start)
  "Return a path starting from path and ending in a used-cell

start must not be in the used-cells list"
  (if (gethash start used-cells)
      (error "Starting from an used cell!"))
  (maze/wilson-erase-loops (maze/create-path maze start
                                             (maze/wilson--generate-stop-f used-cells)
                                             #'maze/wilson--next-path-cell)))

(defun maze/wilson--create-initial-exclusion-table (maze)
    (let ((maze-size (maze/get-cells-number maze))
        (used-cells (make-hash-table)))
    (puthash (/ maze-size 2) t used-cells)
    used-cells))

(defun maze/wilson--carve (maze)
  (let ((new-maze maze)
        (exclusion-table (maze/wilson--create-initial-exclusion-table maze))
        (maze-size (maze/get-cells-number maze)))
    (while (< (hash-table-count exclusion-table) maze-size)
      (let ((new-path (maze/wilson--path new-maze exclusion-table (maze/random-cell new-maze exclusion-table))))
        (setq new-maze (maze/carve-path new-maze new-path))
        (--each new-path (puthash it t exclusion-table))))
    new-maze))

(defun maze/wilson (columns rows)
  (maze/wilson--carve (maze/create-empty columns rows)))

(provide 'maze-wilson)
