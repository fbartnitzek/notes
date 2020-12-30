-- - write a func that takes an arg x and returns a lazy seq that has every third number, starting with x
-- lazySeq 42 = 42 45 48 ...
--   - then, write a func that includes every fifth number, beginning with y
--   - combine these func through composition to return every eighth number, beginning with `x + y`

lazySeq3 x = x : (lazySeq3 (x + 3))
lazySeq5 y = y : (lazySeq5 (y + 5))
lazySeq8 z = zipWith (+) (lazySeq3 z) (lazySeq5 z)

