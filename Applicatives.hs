module Applicatives where

 maybeMap :: (a -> b) -> Maybe a -> Maybe b
 maybeMap _ Nothing = Nothing
 maybeMap f (Just x) = Just (f x)

 x :: Maybe Int
 x = pure 3

 <*> :: f (a -> b) -> f a -> f b