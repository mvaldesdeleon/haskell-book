> module Chapter12 where

Chapter Exercises

Determine the kinds

1. Given

id :: a -> a

What is the kind of a?

a :: *

2. r :: a -> f a

What are the kinds of a and f?

a, f a :: *

f :: * -> *

String processing

Because this is the kind of thing linguists ahem enjoy doing in their spare time.

1. Write a recursive function named replaceThe which takes a text/string, breaks it into words and replaces each instance of “the” with “a”.

It’s intended only to replace exactly the word “the”. notThe is a suggested helper function for accomplishing this.

> notThe :: String -> Maybe String
> notThe s =
>   if s /= "the"
>       then Just s
>       else Nothing
>
> replaceThe :: String -> String
> replaceThe = unwords . go . words
>   where
>       go [] = []
>       go (w:ws) =
>           case notThe w of
>               Just w -> w : go ws
>               Nothing -> "a" : go ws

2. Write a recursive function that takes a text/string, breaks it into words, and counts the number of instances of ”the” followed by a vowel-initial word.

> countTheBeforeVowel :: String -> Integer
> countTheBeforeVowel = go . words
>   where
>       go (wa:wb:ws) =
>           case notThe wa of
>               Just _ -> go (wb:ws)
>               Nothing ->
>                   if vowelInitial wb
>                       then 1 + go (wb:ws)
>                       else go (wb:ws)
>       go _ = 0
>
> vowelInitial :: String -> Bool
> vowelInitial [] = False
> vowelInitial (x:_) = isVowel x
>
> isVowel :: Char -> Bool
> isVowel c = c `elem` vowels

3. Return the number of letters that are vowels in a word.

Hint: it’s helpful to break this into steps. Add any helper functions necessary to achieve your objectives.

a) Test for vowelhood
b) Return the vowels of a string
c) Count the number of elements returned

> countVowels :: String -> Integer
> countVowels = fromIntegral . length . filter (isVowel)

Validate the word

Use the Maybe type to write a function that counts the number of vowels in a string and the number of consonants. If the number of vowels exceeds the number of consonants, the function returns Nothing. In many human languages, vowels rarely exceed the number of consonants so when they do, it may indicate the input isn’t a word (that is, a valid input to your dataset):

> newtype Word' =
>   Word' String deriving (Eq, Show)
>
> vowels = "aeiou"
>
> countConsonants :: String -> Integer
> countConsonants = fromIntegral . length . filter (not . isVowel)
>
> mkWord :: String -> Maybe Word'
> mkWord s
>   | countVowels s > countConsonants s = Nothing
>   | otherwise = Just $ Word' s

It’s only Natural

You’ll be presented with a datatype to represent the natural numbers. The only values representable with the naturals are whole numbers from zero to infinity. Your task will be to implement functions to convert Naturals to Integers and Integers to Naturals. The conversion from Naturals to Integers won’t return Maybe because Integer is a strict superset of Natural. Any Natural can be represented by an Integer, but the same is not true of any Integer. Negative numbers are not valid natural numbers.

> -- As natural as any
> -- competitive bodybuilder
> data Nat =
>     Zero
>   | Succ Nat
>   deriving (Eq, Show)
>
> natToInteger :: Nat -> Integer
> natToInteger Zero = 0
> natToInteger (Succ n) = 1 + natToInteger n
>
> integerToNat :: Integer -> Maybe Nat
> integerToNat i =
>   if i >= 0
>       then Just $ go i
>       else Nothing
>   where
>       go 0 = Zero
>       go i = Succ (go $ i - 1)

Small library for Maybe

Write the following functions.

This may take some time.

1. Simple boolean checks for Maybe values.

> isJust :: Maybe a -> Bool
> isJust Nothing = False
> isJust _ = True
>
> isNothing :: Maybe a -> Bool
> isNothing Nothing = True
> isNothing _ = False

2. The following is the Maybe catamorphism. You can turn a Maybe value into anything else with this.

> mayybee :: b -> (a -> b) -> Maybe a -> b
> mayybee b f Nothing = b
> mayybee b f (Just a) = f a

3. In case you just want to provide a fallback value. Try writing it in terms of the maybe catamorphism.

> fromMaybe :: a -> Maybe a -> a
> fromMaybe a = mayybee a id

4. Converting between List and Maybe.

> listToMaybe :: [a] -> Maybe a
> listToMaybe [] = Nothing
> listToMaybe (a:_) = Just a
>
> maybeToList :: Maybe a -> [a]
> maybeToList Nothing = []
> maybeToList (Just a) = [a]

5. For when we want to drop the Nothing values from our list.

> catMaybes :: [Maybe a] -> [a]
> catMaybes [] = []
> catMaybes (ma:ms) =
>   case ma of
>       Just a -> a : catMaybes ms
>       Nothing -> catMaybes ms

6. You’ll see this called “sequence” later.

> flipMaybe :: [Maybe a] -> Maybe [a]
> flipMaybe [] = Just []
> flipMaybe (ma:ms) =
>   case ma of
>       Just a -> (a :) <$> flipMaybe ms
>       Nothing -> Nothing

Small library for Either

Write each of the following functions. If more than one possible unique function exists for the type, use common sense to determine what it should do.

1. Try to eventually arrive at a solution that uses foldr, even if earlier versions don’t use foldr.

> lefts' :: [Either a b] -> [a]
> lefts' = foldr f []
>   where
>       f e as =
>           case e of
>               Left a -> (a : as)
>               Right _ -> as

2. Same as the last one. Use foldr eventually.

> rights' :: [Either a b] -> [b]
> rights' = foldr f []
>   where
>       f e bs =
>           case e of
>               Left _ -> bs
>               Right b -> b : bs

3.

> partitionEithers' :: [Either a b] -> ([a], [b])
> partitionEithers' = foldr f ([], [])
>   where
>       f e (as, bs) =
>           case e of
>               Left a -> ((a : as), bs)
>               Right b -> (as, (b : bs))

4.

> eitherMaybe'
>   :: (b -> c)
>   -> Either a b
>   -> Maybe c
> eitherMaybe' f e =
>   case e of
>       Left _ -> Nothing
>       Right b -> Just $ f b

5. This is a general catamorphism for Either values.

> either' :: (a -> c) -> (b -> c) -> Either a b -> c
> either' f g e =
>   case e of
>       Left a -> f a
>       Right b -> g b

6. Same as before, but use the either' function you just wrote.

> eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
> eitherMaybe'' f = either' (const Nothing) (Just . f)

Write your own iterate and unfoldr

1. Write the function myIterate using direct recursion. Compare the behavior with the built-in iterate to gauge correctness. Do not look at the source or any examples of iterate so that you are forced to do this yourself.

> myIterate :: (a -> a) -> a -> [a]
> myIterate f a = a : iterate f (f a)

Write the function myUnfoldr using direct recursion. Compare with the built-in unfoldr to check your implementation. Again, don’t look at implementations of unfoldr so that you figure it out yourself.

> myUnfoldr
>   :: (b -> Maybe (a, b))
>   -> b
>   -> [a]
> myUnfoldr f b =
>   case f b of
>       Just (a, b) -> a : myUnfoldr f b
>       Nothing -> []

3. Rewrite myIterate into betterIterate using myUnfoldr. A hint — we used unfoldr to produce the same results as iterate earlier. Do this with different functions and see if you can abstract the structure out.

> betterIterate :: (a -> a) -> a -> [a]
> betterIterate f a = myUnfoldr next a
>   where
>       next a = Just (a, f a)

Finally something other than a list!

Given the BinaryTree from last chapter, complete the following exercises. Here’s that datatype again:

> data BinaryTree a
>     = Leaf
>     | Node (BinaryTree a) a (BinaryTree a)
>     deriving (Eq, Ord, Show)

1. Write unfold for BinaryTree.

> unfold
>   :: (a -> Maybe (a,b,a))
>   -> a
>   -> BinaryTree b
> unfold f a =
>   case f a of
>       Just (la, b, ra) -> Node (unfold f la) b (unfold f ra)
>       Nothing -> Leaf

2. Make a tree builder.

Using the unfold function you’ve made for BinaryTree, write the following function:

> treeBuild
>   :: Integer
>   -> BinaryTree Integer
> treeBuild n = unfold (f n) 0
>   where
>       f :: Integer -> Integer -> Maybe (Integer, Integer, Integer)
>       f n i
>           | n > i = Just (i + 1, i, i + 1)
>           | otherwise = Nothing

