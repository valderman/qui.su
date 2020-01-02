{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scheduler
  ( TimeUnit (..), Interval
  , seconds, every, schedule
  ) where
import Control.Concurrent
import Data.Word

newtype Interval = Interval { seconds :: Word }
  deriving (Num, Ord, Eq, Integral, Real, Enum)

data TimeUnit
  = Seconds
  | Minutes
  | Hours
  | Days
  | Weeks
  | Months
  | Years

every :: Word -> TimeUnit -> Interval
every n Seconds = Interval n
every n Minutes = Interval (n*60)
every n Hours   = Interval (n*60*60)
every n Days    = Interval (n*60*60*24)
every n Weeks   = every n Days * 7
every n Months  = every n Days * 30
every n Years   = every n Days * 365

sleep :: Interval -> IO ()
sleep secs
  | secs <= 3600 = threadDelay (fromIntegral secs*1000000)
  | otherwise    = threadDelay (3600*1000000) >> sleep (secs-3600)

schedule :: Interval -> IO () -> IO ()
schedule secs m = forkIO go >> return ()
  where go = sleep secs >> m >> go
