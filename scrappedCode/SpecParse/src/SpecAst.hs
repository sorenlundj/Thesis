module SpecAst where

data Tag = Tag [(String, String)] String (Maybe [Tag])
         | TagString String
        deriving (Show, Eq)

data MmodType = ShipA
              | ServiceA
              | CompanyA
              | String

--instance (Eq a) => Eq (MmodType a) where
--    ShipA    == ShipA    = ShipA    == ShipA   
--    ServiceA == ServiceA = ServiceA == ServiceA
--    CompanyA == CompanyA = CompanyA == CompanyA

--instance Show ShipA where
--    show (ShipA)    = ShipA   
--    Show (ServiceA) = ServiceA
--    Show (CompanyA) = CompanyA

type ErrMsg = String



-- instance  (Eq a) => Eq (Tree a)  where
--     (Leaf x)     == (Leaf y)        =  x == y
--     (Branch l r) == (Branch l' r')  =  l == l' && r == r'
--     _            == _               =  False