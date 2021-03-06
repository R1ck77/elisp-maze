(require 'buttercup)
(require 'maze-map)

(defun create-hash-table (&rest values)
  (let ((table (make-hash-table)))
    (--each (-partition 2 values)
      (puthash (car it) (cadr it) table))
    table))

(describe "maze-map"
  (describe "maze/hash--table-to-list"
    (it "returns nil on an empty map"
      (expect (maze/hash--table-to-list (make-hash-table))
              :to-be nil))
    (it "returns all expected elements"
      (expect (maze/hash--table-to-list (create-hash-table 1 2 3 4 10 20 5 6))
              :to-have-same-items-as '((1 . 2) (3 . 4) (5 . 6) (10 . 20))))
    (it "returns works on a singled element list"
      (expect (maze/hash--table-to-list (create-hash-table 1 2))
              :to-have-same-items-as '((1 . 2))))))
