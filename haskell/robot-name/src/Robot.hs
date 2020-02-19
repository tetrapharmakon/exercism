module Robot
  ( Robot
  , initialState
  , mkRobot
  , resetName
  , robotName
  ) where

import Control.Monad.State (StateT)
import Data.IORef

type Robot = IORef String

type RunState = [String]

initialState :: RunState
initialState = error "You need to implement this function"

mkRobot :: StateT RunState IO Robot
mkRobot = error "You need to implement this function."

resetName :: Robot -> StateT RunState IO ()
resetName robot = error "You need to implement this function."

robotName :: Robot -> IO String
robotName robot = error "You need to implement this function."
