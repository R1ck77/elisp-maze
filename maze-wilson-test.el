(require 'buttercup)
(require 'maze-wilson)

(describe "maze-wilson"
  (describe "maze/wilson"
    (it "returns a maze without crashing"
      (expect (maze-p (maze/wilson 5 5))))))
