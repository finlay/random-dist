{-# LANGUAGE BangPatterns #-}

import Control.DeepSeq
import Control.Monad as M
import Statistics.Distribution.Random.Exponential
import Random.CRI.SPRNG as LFG

main = do
  lfg <- LFG.create 42
  M.replicateM_ 1000000 $ do
     exponential lfg  >>= return . rnf


