{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

class HasTrie a where
  infixr 2 :->:
  data a :->: b
  trie :: (a -> b) -> a :->: b
  unTrie :: (a :->: b) -> (a -> b)

instance HasTrie Bool where
  data Bool :->: b = BoolTrie b b
  trie f = BoolTrie (f True) (f False)
  unTrie (BoolTrie tv fv) True = tv
  unTrie (BoolTrie tv fv) False = fv

instance HasTrie Int where
  data Int :->: b = IntTrie b (Int :->: b) | End
  trie f = let mkIntTrie f x = IntTrie (f x) (mkIntTrie f (x + 1))
           in mkIntTrie f 0
  unTrie (IntTrie x y) 0 = x
  unTrie (IntTrie x y) n = unTrie y (n-1)

instance HasTrie => HasTrie (Maybe a) where
  data Maybe a :->: output = MaybeTrie output (a :->: output)
  -- trie f = MaybeTrie (f Nothing) (trie (f . Just))
  trie f = MaybeTrie (f Nothing) (f . Just)
  unTrie (MaybeTrie ifNothing ifJust) = maybe ifNothing (unTrie ifJust)


fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = memofib (n - 2) + memofib (n - 1)

memo :: HasTrie a => (a -> b) -> (a -> b)
memo f = unTrie . trie $ f

memofib = memo fib

