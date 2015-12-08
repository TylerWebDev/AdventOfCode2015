module Wires where

import Data.Bits
import Data.Function.Memoize
import qualified Control.Applicative as App
import qualified Data.Map as Map

data Wire k
    = Const Int
    | Link k
    | And k k
    | Or k k
    | LShift k Int
    | RShift k Int
    | Not k
    deriving (Show)

lookupEval :: Map.Map String (Wire String) -> String -> Maybe Int
lookupEval m =
    eval
        where
            e :: String -> Maybe Int
            e k =
                case Map.lookup k m of
                    Just (Const a)
                        -> Just a
                    Just (Link a)
                        -> eval a
                    Just (And a b)
                        -> App.liftA2 (.&.) (eval a) (eval b)
                    Just (Or a b)
                        -> App.liftA2 (.|.) (eval a) (eval b)
                    Just (LShift a i)
                        -> App.liftA2 shift (eval a) (Just i)
                    Just (RShift a i)
                        -> App.liftA2 shift (eval a) $ Just (negate i)
                    Just (Not a)
                        -> complement <$> eval a
                    Nothing
                        -> Just $ read k
            eval = memoize e
