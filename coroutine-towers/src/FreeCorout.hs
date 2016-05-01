{-# LANGUAGE GADTs #-}

module FreeCorout where

import Control.Monad.Indexed.Free
import Control.Monad.Indexed
import IxCont
import Control.Monad.Identity

------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

type E = Int
type T = Bool
type ET = E -> T
type EET = E -> ET

type Tow i j a = IxFree IxCont i j a

------------------------------------------------------------------------------
-- Grammar
------------------------------------------------------------------------------

{--

-- (</>) :: Tow i j (a -> b) -> Tow j k a -> Tow i k b
m </> n = case m of
  Pure x -> imap ($x) n
  Free t  -> Free $ t >>>= \s -> ireturn (n <\> s)

-- (<\>) :: Tow i j a -> Tow j k (a -> b) -> Tow i k b
m <\> n = case m of
    Pure x -> imap ($x) n
    Free t  -> Free $ t >>>= \s -> ireturn (n </> s)


runSent :: Tow i j a -> IxCont i j a
runSent m = case m of
  Pure x -> ireturn x
  Free t -> t >>>= runSent

--}

main :: IO ()
main = return ()

