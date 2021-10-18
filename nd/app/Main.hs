{-# LANGUAGE GADTs, StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
-- https://kseo.github.io/posts/2017-01-13-constraint-kinds.html

module Main where

type HasShowEq a = (Show a, Eq a)

data ProofExpr a where
    Axiom :: (HasShowEq a) => a -> ProofExpr a
    And :: (HasShowEq a, HasShowEq b) => ProofExpr a -> ProofExpr b -> ProofExpr (a,b)

deriving instance (Show a) => Show (ProofExpr a)

main :: IO ()
main = putStrLn "Hello, Haskell!"
