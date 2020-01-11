(require 'maze-data)
;;; TODO/FIXME tests

(defmacro maze/for-each-cell (maze &rest forms)
  "Executes each form with row and column bound to the current position"
  (declare (indent defun))
  (let ((index (make-symbol "index"))
        (tmp (make-symbol "tmp")))
    `(-each (number-sequence 0 (1- (* (maze-rows ,maze)
                                      (maze-columns ,maze))))
       (lambda (index)
         (let ((,tmp (maze/index-to-position ,maze index)))
           (let ((column (car ,tmp))
                 (row (cadr ,tmp)))
             ,@forms))))))

(defun maze/random-choice (items)
  "Straight from the Rosetta Code"
  (let* ((size (length items))
         (index (random size)))
    (nth index items)))

(provide 'maze-utils)
