(require 'buttercup)
(require 'maze)

(defconst empty-maze (maze/create-empty 13 7))

(describe "maze"
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
  (describe "maze/copy"
    (it "creates a deep copy of a maze"
      )))
