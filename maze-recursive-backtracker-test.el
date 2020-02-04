(require 'maze-recursive-backtracker)
(require 'maze-ascii)
(require 'buttercup)

(describe "maze-recursive-backtracker"
  (describe "maze/recursive-backtracker"
    (it "finishes, eventually"
      (with-temp-buffer
        (expect (maze-p (maze/recursive-backtracker 8 4)))))))
