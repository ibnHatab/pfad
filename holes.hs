{-# LANGUAGE ScopedTypeVariables #-}

module Holes where
 
-- import Control.Monad

-- data Hole = Hole
-- data Hole1 a = Hole1
-- hole :: a
-- hole = undefined


compose :: forall a b c. (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)
    where
      _ = f :: b -> c
      _ = g :: a -> b
      _ = x :: a

map' :: forall a b. (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs
    where
      _ = f :: a -> b
      _ = x :: a

apply :: forall m a b. Monad m => m(a -> b) -> m a -> m b
apply mf ma = mf >>= \f -> ma >>= \x -> return (f x)
    -- where
    --   _ = return :: b -> m b
    --   _ = (>>=) :: forall h. m h -> (h -> m b) -> m b
    --   _ = (mf >>=) :: ((a -> b) -> m b) -> m b
    --   _ = (ma >>=) :: (a -> m b) -> m b
    --   _ = mf :: m(a->b)
    --   _ = ma :: m a
    --   k f = ma >>= r
    --       where            
    --         _ = f :: (a -> b)
    --         r x = return (f x) :: m b
    --             where
    --               _ = x :: a
    --               _ = f x :: b
    --               _ = return (f x) :: m b


filterM' :: forall m a. Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' p xs = foldr f (return []) xs :: m [a]
    where
      f x r = p x >>= \c -> if c then r >>= \ys -> return (x:ys) else r
      -- _ = return :: b -> m b
      -- _ = p :: (a -> m Bool)
      -- _ = xs :: [a]
          -- where 
          --   _ = x :: a
          --   _ = r :: m [a]
          --   k c = if c then r >>= j else r :: m [a]
          --       where _ = c :: Bool
          --             j ys = return (x:xs) :: m [a]
          --                 where 
          --                   _ = ys :: [a]
          --                   _ = (x:xs) :: [a]
          --                   _ = return (x:xs) :: m [a]
 




