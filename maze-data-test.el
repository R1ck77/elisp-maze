(require 'buttercup)
(require 'maze-data)

(defconst empty-maze (maze/create-empty 7 4))

(defun compose-index-transforms (maze index)
  (apply #'maze/position-to-index (cons maze (maze/index-to-position maze index))))

(describe "maze-data"
  (describe "maze/create-empty"
    (it "creates a maze with the correct number of columns"
      (expect (maze-columns (maze/create-empty 13 7))
              :to-be 13))
    (it "creates a maze with the correct number of rows"
      (expect (maze-rows (maze/create-empty 13 7))
              :to-be 7))
    (it "creates a maze with no connections"
      (expect (hash-table-count (maze-connections (maze/create-empty 100 20)))
              :to-be 0)))
  (describe "maze/index-to-position"
    (it "throws if the index is out of range"
      (expect (maze/index-to-position empty-maze -1) :to-throw 'error)
      (expect (maze/index-to-position empty-maze 28) :to-throw 'error)))
  (describe "maze/position-to-index"
    (it "is the inverse of maze/index-to-position"
      (expect (compose-index-transforms empty-maze 0) :to-be 0)
      (expect (compose-index-transforms empty-maze 6) :to-be 6)
      (expect (compose-index-transforms empty-maze 13) :to-be 13)
      (expect (compose-index-transforms empty-maze 27) :to-be 27))
    (it "converts the index as expected"
      (expect (maze/position-to-index empty-maze 0 0) :to-be 0)
      (expect (maze/position-to-index empty-maze 6 1) :to-be 13)
      (expect (maze/position-to-index empty-maze 6 3) :to-be 27))
    (it "throws an error if out of range"
      (expect (maze/position-to-index empty-maze -1 0) :to-throw 'error)
      (expect (maze/position-to-index empty-maze 0 -1) :to-throw 'error)
      (expect (maze/position-to-index empty-maze 7 0) :to-throw 'error)
      (expect (maze/position-to-index empty-maze 0 7) :to-throw 'error)))
  (describe "maze/carve-passage"
    (it "returns a different maze when the passage doesn't exist"
      (expect (maze/carve-passage empty-maze '(1 2) '(1 3)) :not :to-be empty-maze))
    (it "returns the same maze if the passage is already there"
      (let ((carved-maze (maze/carve-passage empty-maze '(1 2) '(1 3))))
        (expect (maze/carve-passage carved-maze '(1 2) '(1 3)) :to-be carved-maze)))
    (it "throws an error if the first and last cells are the same"
      (expect (maze/carve-passage empty-maze '(1 2) '(1 2)) :to-throw 'error))
    (it "throws an error if the cells are not contiguous"
      (expect (maze/carve-passage empty-maze '(0 2) '(2 2)) :to-throw 'error)
      (expect (maze/carve-passage empty-maze '(1 1) '(1 3)) :to-throw 'error)
      (expect (maze/carve-passage empty-maze '(1 2) '(2 3)) :to-throw 'error)))
  (describe "maze/copy"
    (it "creates a deep copy of a maze")))