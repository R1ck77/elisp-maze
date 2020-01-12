(require 'buttercup)
(require 'maze-binary)

(describe "maze-binary"
  (describe "maze/binary"
    (it "returns a maze without crashing"
      (expect (maze-p (maze/binary 5 5))))))
