-- | Module providing convience functions for working with large anonymous
-- coproducts
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}

module Data.Coproduct where

import Data.Profunctor

-- | Type synonym for easier use of large nested coproducts
type (:|:) = Either

infixr 5 :|:

alt31 :: Choice p => p a b -> p (a :|: c :|: d) (b :|: c :|: d)
alt31 = left'

alt32 :: Choice p => p a b -> p (c :|: a :|: d) (c :|: b :|: d)
alt32 = right' . left'

alt33 :: Choice p => p a b -> p (c :|: d :|: a) (c :|: d :|: b)
alt33 = right' . right'

alt43 :: Choice p => p a b -> p (c :|: d :|: a :|: e) (c :|: d :|: b :|: e)
alt43 = alt33 . left'

alt44 :: Choice p => p a b -> p (c :|: d :|: e :|: a) (c :|: d :|: e :|: b)
alt44 = alt33 . right'

alt54 ::
       Choice p
    => p a b
    -> p (c :|: d :|: e :|: a :|: f) (c :|: d :|: e :|: b :|: f)
alt54 = alt44 . left'

alt55 ::
       Choice p
    => p a b
    -> p (c :|: d :|: e :|: f :|: a) (c :|: d :|: e :|: f :|: b)
alt55 = alt44 . right'

alt65 ::
       Choice p
    => p a b
    -> p (c :|: d :|: e :|: f :|: a :|: g) (c :|: d :|: e :|: f :|: b :|: g)
alt65 = alt55 . left'

alt66 ::
       Choice p
    => p a b
    -> p (c :|: d :|: e :|: f :|: g :|: a) (c :|: d :|: e :|: f :|: g :|: b)
alt66 = alt55 . right'

alt76 ::
       Choice p
    => p a b
    -> p (c :|: d :|: e :|: f :|: g :|: a :|: h) (c :|: d :|: e :|: f :|: g :|: b :|: h)
alt76 = alt66 . left'

alt77 ::
       Choice p
    => p a b
    -> p (c :|: d :|: e :|: f :|: g :|: h :|: a) (c :|: d :|: e :|: f :|: g :|: h :|: b)
alt77 = alt66 . right'

alt87 ::
       Choice p
    => p a b
    -> p (c :|: d :|: e :|: f :|: g :|: h :|: a :|: i) (c :|: d :|: e :|: f :|: g :|: h :|: b :|: i)
alt87 = alt77 . left'

alt88 ::
       Choice p
    => p a b
    -> p (c :|: d :|: e :|: f :|: g :|: h :|: i :|: a) (c :|: d :|: e :|: f :|: g :|: h :|: i :|: b)
alt88 = alt77 . right'

alt98 ::
       Choice p
    => p a b
    -> p (c :|: d :|: e :|: f :|: g :|: h :|: i :|: a :|: j) (c :|: d :|: e :|: f :|: g :|: h :|: i :|: b :|: j)
alt98 = alt88 . left'

alt99 ::
       Choice p
    => p a b
    -> p (c :|: d :|: e :|: f :|: g :|: h :|: i :|: j :|: a) (c :|: d :|: e :|: f :|: g :|: h :|: i :|: j :|: b)
alt99 = alt88 . right'

either3 :: (a -> x) -> (b -> x) -> (c -> x) -> a :|: b :|: c -> x
either3 f g h = either f (either g h)

either4 ::
       (a -> x) -> (b -> x) -> (c -> x) -> (d -> x) -> a :|: b :|: c :|: d -> x
either4 f g h i = either f (either g (either h i))

either5 ::
       (a -> x)
    -> (b -> x)
    -> (c -> x)
    -> (d -> x)
    -> (e -> x)
    -> a :|: b :|: c :|: d :|: e -> x
either5 f g h i j = either f (either g (either h (either i j)))

either6 ::
       (a -> x)
    -> (b -> x)
    -> (c -> x)
    -> (d -> x)
    -> (e -> x)
    -> (f -> x)
    -> a :|: b :|: c :|: d :|: e :|: f -> x
either6 f g h i j k = either f (either g (either h (either i (either j k))))

either7 ::
       (a -> x)
    -> (b -> x)
    -> (c -> x)
    -> (d -> x)
    -> (e -> x)
    -> (f -> x)
    -> (g -> x)
    -> a :|: b :|: c :|: d :|: e :|: f :|: g -> x
either7 f g h i j k l =
    either f (either g (either h (either i (either j (either k l)))))

either8 ::
       (a -> x)
    -> (b -> x)
    -> (c -> x)
    -> (d -> x)
    -> (e -> x)
    -> (f -> x)
    -> (g -> x)
    -> (h -> x)
    -> a :|: b :|: c :|: d :|: e :|: f :|: g :|: h -> x
either8 f g h i j k l m =
    either f (either g (either h (either i (either j (either k (either l m))))))

either9 ::
       (a -> x)
    -> (b -> x)
    -> (c -> x)
    -> (d -> x)
    -> (e -> x)
    -> (f -> x)
    -> (g -> x)
    -> (h -> x)
    -> (i -> x)
    -> a :|: b :|: c :|: d :|: e :|: f :|: g :|: h :|: i -> x
either9 f g h i j k l m n =
    either
        f
        (either
             g
             (either h (either i (either j (either k (either l (either m n)))))))
