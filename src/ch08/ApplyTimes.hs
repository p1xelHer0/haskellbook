module ApplyTimes where

applyTimes
  :: (Eq a, Num a)
  => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f . applyTimes (n - 1) f $ b
-- 0:  applyTimes 5 (+1) 5
-- 1:  (+1) (applyTimes 4 (+1) 5)
-- 2:  (+1) (+1) (applyTimes 3 (+1) 5)
-- 3:  (+1) (+1) (+1) (applyTimes 2 (+1) 5)
-- 4:  (+1) (+1) (+1) (+1) (applyTimes 1 (+1) 5)
-- 5:  (+1) (+1) (+1) (+1) (+1) (applyTimes 0 (+1) 5)
-- 6:  (+1) (+1) (+1) (+1) (+1) 5
-- 7:  (+1) (+1) (+1) (+1) 6
-- 8:  (+1) (+1) (+1) 7
-- 9:  (+1) (+1) 8
-- 10: (+1) 9
-- 11: 10
