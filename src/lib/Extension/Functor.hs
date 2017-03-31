module Extension.Functor ((<&>)) where

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
