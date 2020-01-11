(require 'maze-data)
(require 'maze-utils)

(defun maze/right-cell (column row)
  (list (1+ column) row))

(defun maze/bottom-cell (column row)
  (list column (1+ row)))

(defun maze/binary--choices (maze column row)
  (--filter (apply #'maze/valid-cell-p (list maze it))
            (list (maze/right-cell column row)
                  (maze/bottom-cell column row))))

(defun maze/carve-binary (maze)
  (let ((result maze))
    (maze/for-each-cell maze
      (let ((choice (maze/random-choice (maze/binary--choices maze column row))))
        (when choice
          (setq result (apply #'maze/carve-passage (cons result (list (list column row)
                                                                 choice)))))))
    result))

(defun maze/binary (columns rows)
  (maze/carve-binary (maze/create-empty columns rows)))

(provide 'maze-binary)
