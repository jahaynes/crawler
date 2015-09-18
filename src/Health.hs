module Health where

import Types

import qualified STMContainers.Map as M

{-
data Health = Health {
    getLastPingTime :: M.Map ThreadId UTCTime
    } -}
    
    
initialiseHealth :: IO Health
initialiseHealth =
    M.newIO >>= \m -> return (Health {getLastPingTime = m})