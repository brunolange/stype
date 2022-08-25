data L0 a = Nil | C0 a (L0 a)

instance Show a => Show (L0 a) where
    show Nil = ""
    show (C0 x xs) = show x ++ " " ++ show xs

class Header h where
    head1 :: h a -> a

data L1 a = C1 a (L0 a)

instance Show a => Show (L1 a) where
    show (C1 x xs) = show x ++ " " ++ show xs

instance Header L1 where
    head1 (C1 x _) = x

data L2 a = C2 a (L1 a)

instance Show a => Show (L2 a) where
    show (C2 x xs) = show x ++ " " ++ show xs

instance Header L2 where
    head1 (C2 x _) = x

