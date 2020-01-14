(require 'buttercup)
(require 'maze-hunt-and-kill)

(describe "maze-hunt-and-kill"
  (describe "maze/hunt-and-kill"
    (it "returns a maze without crashing"
      (expect (maze-p (maze/hunt-and-kill 5 5))))))
