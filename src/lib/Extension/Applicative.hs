module Extension.Applicative ((<#>)) where

(<#>) :: Applicative f => f (a -> b) -> a -> f b
f <#> g = f <*> pure g
