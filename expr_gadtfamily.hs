{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}       --For 'Measurable
{-# LANGUAGE TypeFamilies #-}    --For KMetric'
{-# LANGUAGE ConstraintKinds #-} --For KMetric t

data Line = KLine
data Point = KPoint

data Measurable = 'Measurable               --Measurable Types Constraint TypeFamily

type family KMetric' a where                --Measurable Primitives Constraint Type Family
    KMetric' Line = 'Measurable

type KMetric t = (KMetric' t ~ 'Measurable) --Measurable ConstraintKind

data KExpr a where
    K :: a -> KExpr a
    C :: ConstExpr a -> KExpr a
    KLength :: (KMetric a) => KExpr a -> KExpr Float
    KConLine :: KExpr Point -> KExpr Point -> KExpr Line
    KAdd :: (Num a) => KExpr a -> KExpr a -> KExpr a

--Used with KExpr C constructor to provide labeled and typed constants.
--Pattern found from https://stackoverflow.com/a/37359543
data ConstExpr a where
    ConstF :: String -> ConstExpr Float
