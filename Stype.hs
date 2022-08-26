data L0 a = Nil | C0 a (L0 a)

instance Show a => Show (L0 a) where
    show Nil = ""
    show (C0 x xs) = show x ++ " " ++ show xs

instance Functor L0 where
    fmap f Nil = Nil
    fmap f (C0 x xs) = C0 (f x) (fmap f xs)

instance Applicative L0 where
    pure a = C0 a Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (C0 f fs) <*> (C0 x xs) = C0 (f x) $ fs <*> xs

instance Semigroup (L0 a) where
    Nil <> xs = xs
    xs <> Nil = xs
    (C0 x xs) <> ys = C0 x (xs <> ys)

instance Monoid (L0 a) where
    mempty = Nil
    mappend = (<>)

class Header h where
    head1 :: h a -> a

data L1 a = C1 a (L0 a)

instance Show a => Show (L1 a) where
    show (C1 x xs) = show x ++ " " ++ show xs

instance Header L1 where
    head1 (C1 x _) = x

instance Functor L1 where
    fmap f (C1 x xs) = C1 (f x) (fmap f xs)

instance Applicative L1 where
    pure a = C1 a Nil
    (C1 f fs) <*> (C1 x xs) = C1 (f x) $ fs <*> xs

instance Semigroup (L1 a) where
    (C1 x xs) <> (C1 y ys) = C1 x $ xs <> pure y <> ys -- this is still an L1 but it would also fit "L2".
    -- xs <> _ = xs -- this would lose information
    -- _ <> ys = ys -- this would lose information

-- L1 can't be a monoid! There's no mempty for the mappend I've implemented!

data L2 a = C2 a (L1 a)

instance Show a => Show (L2 a) where
    show (C2 x xs) = show x ++ " " ++ show xs

instance Header L2 where
    head1 (C2 x _) = x

