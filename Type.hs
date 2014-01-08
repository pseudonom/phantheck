{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Type where

type family Equals (a :: k) (b :: k) :: Bool where
  a `Equals` a = True
  a `Equals` b = False

type family Or (a :: Bool) (b :: Bool) :: Bool where
  True `Or` b = True
  a `Or` True = True
  a `Or` b = False

type family And (a :: Bool) (b :: Bool) :: Bool where
  True `And` True = True
  a `And` b = False

type family Elem (y :: k) (xs :: [k]) :: Bool where
  y `Elem` '[] = False
  y `Elem` (x ': xs) = (y `Equals` x) `Or` (y `Elem` xs)

type family Covers (xs :: [k]) (ys :: [k]) :: Bool where
  xs `Covers` '[] = True
  xs `Covers` (y ': ys) = (y `Elem` xs) `And` (xs `Covers` ys)

type family If (b :: Bool) (t :: k) (f :: k) :: k where
  If True t f = t
  If False t f = f

-- Takes a set of preconditions,
-- a set of postconditions and their preconditional requirements,
-- and returns the postconditions which
-- have their preconditional requirements satisfied
type family Postconditions (pres :: [k]) (sets :: [(k, [k])]) :: [k] where
  Postconditions pres '[] = '[]
  Postconditions pres ('(post, reqs) ': sets) =
    If (pres `Covers` reqs)
      (post ': Postconditions pres sets)
      (Postconditions pres sets)

type Precondition a props = (a `Elem` props) ~ True
