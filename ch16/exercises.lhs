> {-# LANGUAGE FlexibleInstances #-}
>
> module Chapter16 where
>
> import GHC.Arr

Exercises: Be Kind

Given a type signature, determine the kinds of each type variable:

1. What’s the kind of 𝑎?

f :: a -> a

(->) :: * -> *, thus
a :: *

2. What are the kinds of 𝑏 and 𝑇? (The 𝑇 is capitalized on purpose!)

f :: a -> b a -> T (b a)

a, b a :: *, thus
b :: * -> *

b a, T (b a) :: *, thus
T :: * -> *

3. What’s the kind of 𝑐?

f :: c a b -> c b a

c a b :: *, thus
c :: * -> * -> *o


Wait, how does that even typecheck?

(.) :: (b -> c) -> (a -> b) -> a -> c

fmap :: Functor f => (m -> n) -> f m -> f n

fmap :: Functor g => (x -> y) -> g x -> g y

By simple substitution

fmap . fmap
    :: (Functor f, Functor g)
    => (x -> y)
    -> (f m -> f n)

with the additional constraint that
    (g x -> g y) = (m -> n)

So, we can replace this to get:

fmap . fmap
    :: (Functor f, Functor g)
    => (x -> y)
    -> f (g x)
    -> f (g y)

As we would expect, it modifies the value inside two nested functors, preserving both structures.


Exercises: Heavy Lifting

Add fmap, parentheses, and function composition to the expression as needed for the expression to typecheck and produce the expected result.
It may not always need to go in the same place, so don’t get complacent.

1.

> a = fmap (+1) $ read "[1]" :: [Int]

2.

> b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

3.

> c  =      (*2) . (\x -> x - 2)
> c' = fmap (*2)   (\x -> x - 2)

4.

> d =
>   ((return '1' ++) . show) .
>   (\x -> [x, 1..3])
> -- and d' same as above

5.

> e :: IO Integer
> e = let ioi = readIO "1" :: IO Integer
>         changed = fmap (read . ("123"++) . show) ioi
>     in fmap (*3) changed


Exercise: Possibly

Write a Functor instance for a datatype identical to Maybe. We’ll use our own datatype because Maybe already has a Functor instance and we cannot make a duplicate one.

> data Possibly a =
>   LolNope
>   | Yeppers a
>   deriving (Eq, Show)
>
> instance Functor Possibly where
>   fmap f (Yeppers a) = Yeppers (f a)
>   fmap _ LolNope = LolNope


Short Exercise

1. Write a Functor instance for a datatype identical to Either. We’ll use our own datatype because Either has a Functor instance.

> data Sum a b =
>   First a
>   | Second b
>   deriving (Eq, Show)
>
> instance Functor (Sum a) where
>   fmap _ (First a) = First a
>   fmap f (Second b) = Second (f b)

2. Why is a Functor instance that applies the function only to First, Either’s Left, impossible? We covered this earlier.

Because the "a" in First is part of "Sum a"'s structure, and you cannot define an instance on "Sum" as it has the wrong kind (* -> * -> *, as opposed to * -> *)


Chapter exercises

Determine if a valid Functor can be written for the datatype provided.

1.

> data Bool = False | True

No, Bool has the wrong kind.

2.

> data BoolAndSomethingElse a = False' a | True' a

Yes.

> instance Functor BoolAndSomethingElse where
>   fmap f (False' a) = False' (f a)
>   fmap f (True' a) = True' (f a)

3.

> data BoolAndMaybeSomethingElse a = Falsish | Truish a

Yes.

> instance Functor BoolAndMaybeSomethingElse where
>   fmap _ Falsish = Falsish
>   fmap f (Truish a) = Truish (f a)

4. Use the kinds to guide you on this one, don’t get too hung up on the details.

> newtype Mu f = InF { outF :: f (Mu f) }

No, Mu has the wrokg kind.

5. Again, follow the kinds and ignore the unfamiliar parts

> data D = D (Array Word Word) Int Int

No, D has the wrong kind.


Rearrange the arguments to the type constructor of the datatype so the Functor instance works.

1.

> data Sum' b a = First' a | Second' b
>
> instance Functor (Sum' e) where
>   fmap f (First' a) = First' (f a)
>   fmap f (Second' b) = Second' b

2.

> data Company a c b = DeepBlue a c | Something b
>
> instance Functor (Company e e') where
>   fmap f (Something b) = Something (f b)
>   fmap _ (DeepBlue a c) = DeepBlue a c

3.

> data More b a = L a b a | R b a b
>   deriving (Eq, Show)
>
> instance Functor (More x) where
>   fmap f (L a b a') = L (f a) b (f a')
>   fmap f (R b a b') = R b (f a) b'


Write Functor instances for the following datatypes.

1.

> data Quant a b = Finance | Desk a | Bloor b
>
> instance Functor (Quant a) where
>   fmap _ Finance = Finance
>   fmap _ (Desk a) = Desk a
>   fmap f (Bloor b) = Bloor (f b)

2. No, it’s not interesting by itself.

> data K a b = K a
>
> instance Functor (K a) where
>   fmap _ (K a) = K a

3.

> newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
>
> -- should remind you of an
> -- instance you've written before
> instance Functor (Flip K a) where
>   fmap f (Flip (K a)) = Flip (K (f a))

4.

> data EvilGoateeConst a b = GoatyConst b
>
> instance Functor (EvilGoateeConst a) where
>   fmap f (GoatyConst b) = GoatyConst (f b)

5. Do you need something extra to make the instance work?

> data LiftItOut f a = LiftItOut (f a)
>
> instance Functor f => Functor (LiftItOut f) where
>   fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

6.

> data Parappa f g a = DaWrappa (f a) (g a)
>
> instance (Functor f, Functor g) => Functor (Parappa f g) where
>   fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

7. Don’t ask for more type class instances than you need. You can let GHC tell you what to do.

> data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
>
> instance Functor g => Functor (IgnoreOne f g a) where
>   fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

8.

> data Notorious g o a t = Notorious (g o) (g a) (g t)
>
> instance Functor g => Functor (Notorious g o a) where
>   fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

9. You’ll need to use recursion.

> data List a = Nil | Cons a (List a)
>
> instance Functor List where
>   fmap _ Nil = Nil
>   fmap f (Cons a la) = Cons (f a) (fmap f la)

10. A tree of goats forms a Goat-Lord, fearsome poly-creature.

> data GoatLord a = NoGoat
>   | OneGoat a
>   | MoreGoats (GoatLord a)
>               (GoatLord a)
>               (GoatLord a)
>
> instance Functor GoatLord where
>   fmap _ NoGoat = NoGoat
>   fmap f (OneGoat a) = OneGoat (f a)
>   fmap f (MoreGoats l m r) = MoreGoats (fmap f l) (fmap f m) (fmap f r)

11. You’ll use an extra functor for this one, although your solution might do it monomorphically without using fmap. Keep in mind that you will probably not be able to validate this one in the usual manner. Do your best to make it work.

> data TalkToMe a = Halt
>   | Print String a
>   | Read (String -> a)
>
> instance Functor TalkToMe where
>   fmap _ Halt = Halt
>   fmap f (Print s a) = Print s (f a)
>   fmap f (Read cont) = Read (fmap f cont)
