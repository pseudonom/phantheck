module Type where

type family Equals (a :: k) (b :: k) :: Bool where
  a `Equals` a = 'True
  a `Equals` b = 'False

type family Or (a :: Bool) (b :: Bool) :: Bool where
  'True `Or` b = 'True
  a `Or` 'True = 'True
  a `Or` b = 'False

type family And (a :: Bool) (b :: Bool) :: Bool where
  'True `And` 'True = 'True
  a `And` b = 'False

type family Elem (y :: k) (xs :: [k]) :: Bool where
  y `Elem` '[] = 'False
  y `Elem` (x ': xs) = (y `Equals` x) `Or` (y `Elem` xs)

type family IsSuperSetOf (xs :: [k]) (ys :: [k]) :: Bool where
  xs `IsSuperSetOf` '[] = 'True
  xs `IsSuperSetOf` (y ': ys) = (y `Elem` xs) `And` (xs `IsSuperSetOf` ys)

type family If (b :: Bool) (t :: k) (f :: k) :: k where
  If 'True t f = t
  If 'False t f = f

type family CatMaybes (xs :: [Maybe k]) :: [k] where
  CatMaybes '[] = '[]
  CatMaybes ('Just x ': xs) = x ': CatMaybes xs
  CatMaybes ('Nothing ': xs) = CatMaybes xs

type family Postconditions (preconditions :: [k]) (prereqsAndPostconditions :: [([k], k)]) :: [k] where
  Postconditions xs ys = CatMaybes (MaybePostconditions xs ys)
type family MaybePostconditions (preconditions :: [k]) (prereqsAndPostconditions :: [([k], k)]) :: [Maybe k] where
  MaybePostconditions xs '[] = '[]
  MaybePostconditions xs (y ': ys) = IfPrereqsMet xs y ': MaybePostconditions xs ys

-- Takes a set of preconditions,
-- a postcondition and its preconditional requirements,
-- and returns Just the postcondition if its preconditional requirements are satisfied
-- or Nothing if they aren't
type family IfPrereqsMet (preconditions :: [k]) (prereqsAndPostcondition :: ([k], k)) :: Maybe k where
  IfPrereqsMet pres '(reqs, post) =
    If (pres `IsSuperSetOf` reqs) ('Just post) 'Nothing

type Precondition a props = (a `Elem` props) ~ 'True
