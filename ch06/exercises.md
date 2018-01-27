# Eq Instances

Write the Eq instance for the datatype provided.

1. It’s not a typo, we’re just being cute with the name.
   
   ```
   data TisAnInteger =
       TisAn Integer
   ```

   ```
   instance Eq TisAnInteger where
       (TisAn a) == (TisAn b) = a == b
   ```

2. 

   ```
   data TwoIntegers =
       Two Integer Integer
   ```

   ```
   instance Eq TwoIntegers where
       (Two a b) == (Two c d) = a == c && b == d
   ```

3. 
   
   ```
   data StringOrInt =
       TisAnInt Int
     | TisAString String
   ```

   ```
   instance Eq StringOrInt where
       (TisAnInt a) == (TisAnInt b) = a == b
       (TisAString a) == (TisAString b) = a == b
       _ == _ = False
   ```

4. 

   ```
   data Pair a =
       Pair a a
   ```

   ```
   instance Eq a => Eq (Pair a) where
       (Pair a b) == (Pair c d) = a == c && b == d
   ```

5.

   ```
   data Tuple a b =
       Tuple a b
   ```

   ```
   instance (Eq a, Eq b) => Eq (Tuple a b) where
       (Tuple a b) == (Tuple c d) = a == c && b == d
   ```

6. 

   ```
   data Which a =
       ThisOne a
     | ThatOne a
   ```

   ```
   instance Eq a => Eq (Which a) where
       (ThisOne a) == (ThisOne b) = a == b
       (ThatOne a) == (ThatOne b) = a == b
       _ == _ = False
   ```

7.

   ```
   data EitherOr a b =
       Hello a
     | Goodbye b
   ```

   ```
   instance (Eq a, Eq b) => Eq (EitherOr a b) where
       (Hello a) == (Hello b) = a == b
       (Goodbye a) == (Goodbye b) = a == b
       _ == _ = False
   ```

# Tuple Experiment

Look at the types given for `quotRem` and `divMod`. What do you think those functions do? Test your hypotheses by playing with them in the REPL. 

```
quotRem :: Integral a => a -> a -> (a, a)
quotRem a b = (quot a b, rem a b)

divMod :: Integral a => a -> a -> (a, a)
divMod a b = (div a b, mod a b)
```

# Will They Work?

Next, take a look at the following code examples and try to decide if they will work, what result they will return if they do, and why or why not (be sure, as always, to test them in your REPL once you have decided on your answer):

1. `max (length [1, 2, 3]) (length [8, 9, 10, 11, 12])`

   `5`. `length` returns `Int` which has an `Ord` instance defined.

2. `compare (3 * 4) (3 * 5)`

   `LT`. `Num` and `Ord` instances are required, which are provided by the numeric literals, which would default to either `Integer` or `Double`

3. `compare "Julie" True`

   Will not typecheck. Applying `"Julie"` to compare forces the following argument to also be `[Char]`, instead of `Bool`.

4. `(5 + 3) > (3 + 6)`

   `False`. Same as in 2.

# Chapter Exercises

## Multiple choice

1. The `Eq` class
    
    a) includes all types in Haskell  
    b) is the same as the `Ord` class  
    c) makes equality tests possible  
    d) only includes numeric types

    C.

2. The typeclass `Ord`

    a) allows any two values to be compared  
    b) is a subclass of `Eq`  
    c) is a superclass of `Eq`  
    d) has no instance for `Bool`

    A, B.

3. Suppose the typeclass `Ord` has an operator `>`. What is the type of `>`?

    a) `Ord a => a -> a -> Bool`  
    b) `Ord a => Int -> Bool`  
    c) `Ord a => a -> Char`  
    d) `Ord a => Char -> [Char]`

    A.

4. In `x = divMod 16 12`

    a) the type of `𝑥` is `Integer`  
    b) the value of `𝑥` is undecidable  
    c) the type of `𝑥` is a tuple  
    d) `𝑥` is equal to 12 / 16

    C.

5. The typeclass `Integral` includes
    
    a) `Int` and `Integer` numbers  
    b) integral, real, and fractional numbers  
    c) Schrodinger’s cat  
    d) only positive numbers

    A.

## Does it typecheck?

Examine the following code and decide whether it will typecheck. Then load it in GHCi and see if you were correct. If it doesn’t typecheck, try to match the type error against your understanding of why it didn’t work. If you can, fix the error and re-run the code.

1. Does the following code typecheck? If not, why not?

   ```
   data Person = Person Bool

   printPerson :: Person -> IO ()
   printPerson person = putStrLn (show person)
   ```

   No, `Person` has no instance of `Show`. Fix: Add `deriving Show`.

2. Does the following typecheck? If not, why not?

   ```
   data Mood = Blah
             | Woot deriving Show

   settleDown x = if x == Woot
                    then Blah
                    else x
   ```

   No, `Mood` has no instance of `Eq`. Fix: Change current deriving clause with `deriving (Show, Eq)`.

3. If you were able to get `settleDown` to typecheck:

    a) What values are acceptable inputs to that function?

    `Blah` and `Woot`.

    b) What will happen if you try to run `settleDown 9`? Why?

    Will not work, as `Mood` does not match `Num a => a`.

    c) What will happen if you try to run `Blah > Woot`? Why?

    Wil not work, as `Mood` does not have an `Ord` instance.

4. Does the following typecheck? If not, why not?

   ```
   type Subject = String
   type Verb = String
   type Object = String

   data Sentence =
     Sentence Subject Verb Object
     deriving (Eq, Show)

   s1 = Sentence "dogs" "drool"
   s2 = Sentence "Julie" "loves" "dogs"
   ```

   Yes. `s1 :: Object -> Sentence` and `s2 :: Sentence`.

## Given a datatype declaration, what can we do?

Given the following datatype definitions:

```
data Rocks =
   Rocks String
   deriving (Eq, Show)

data Yeah =
   Yeah Bool
   deriving (Eq, Show)

data Papu =
   Papu Rocks Yeah
   deriving (Eq, Show)
```

Which of the following will typecheck? For the ones that don’t typecheck, why don’t they?

1.

   ```
   phew = Papu "chases" True
   ```

   No, is missing the data constructors for `Rocks` and `Yeah`. Fix: `phew = Papu (Rocks "chases") (Yeah True)`.

2.

   ```
   truth = Papu (Rocks "chomskydoz")
                (Yeah True)
   ```

   Yes, `truth :: Papu`.

3. 

   ```
   equalityForall :: Papu -> Papu -> Bool
   equalityForall p p' = p == p'
   ```

   Yes.

4.

   ```
   comparePapus :: Papu -> Papu -> Bool
   comparePapus p p' = p > p'
   ```

   No, `Papu` has no `Ord` instance. Fix: Add it to the `deriving` clause for the three datatypes.

## Match the types


We’re going to give you two types and their implementations. Then we’re going to ask you if you can substitute the second type for the first. You can test this by typing the first declaration and its type into a file and editing in the new one, loading to see if it fails. Don’t just guess, test all your answers!

1.
   a) 
   
   ```
   i :: Num a => a
   i = 1
   ```

   b)

   `i :: a`

   Won't work. `1` is of type `Num a => a`, we cannot drop the typeclass constraint. 

2.
   a)

   ```
   f :: Float
   f = 1.0
   ```

   b)

   `f :: Num a => a`

   Won't work. `1.0` is of type `Fractional a => a`, and we cannot relax this constraint to `Num a => a`.

3.
   a)

   ```
   f :: Float
   f = 1.0
   ```

   b)

   `f :: Fractional a => a`

   Works.

4.
   a)

   ```
   f :: Float
   f = 1.0
   ```

   b)

   `f :: RealFrac a => a`

   Works. We are making the type more specific.

5.
   a)

   ```
   freud :: a -> a
   freud x = x
   ```

   b)

   `freud :: Ord a => a -> a`

   Works. We are making the type more specific.

6.
   a)

   ```
   freud' :: a -> a
   freud' x = x
   ```

   b)

   `freud' :: Int -> Int`

   Works. We are making the type more specific.

7.
   a)

   ```
   myX = 1 :: Int

   sigmund :: Int -> Int
   sigmund x = myX
   ```

   b)

   `sigmund :: a -> a`

   Won't work. By constraining the type of `myX`, we cannot make `sigmund` any more generic.

8.
   a)

   ```
   myX = 1 :: Int

   sigmund' :: Int -> Int
   sigmund' x = myX
   ```

   b)

   `sigmund' :: Num a => a -> a`

   As stated, we cannot make this function any more generic.

9.
   a)

   ```
   jung :: Ord a => [a] -> a
   jung xs = head (sort xs)
   ```

   b)

   `jung :: [Int] -> Int`

   Works, as `Int` has an instance of `Ord`.

10.
   a)

   ```
   young :: [Char] -> Char
   young xs = head (sort xs)
   ```

   b)

   `young :: Ord a => [a] -> a`

   Works. `sort` requires `Ord a => [a]`, `head` requires `[a]`.

11.
   a)

   ```
   mySort :: [Char] -> [Char]
   mySort = sort
   
   signifier :: [Char] -> Char
   signifier xs = head (mySort xs)
   ```

   b)

   `signifier :: Ord a => [a] -> a`

   Won't work. `mySort` requires `[Char]`, which cannot be relaxed to `Ord a => [a]`.

## Type-Kwon-Do Two: Electric Typealoo

1.
    
    ```
    chk :: Eq b => (a -> b) -> a -> b -> Bool
    chk f a b = f a == b
    ```

2.

   ```
   -- Hint: use some arithmetic operation to
   -- combine values of type 'b'. Pick one.
   
   arith :: Num b => (a -> b) -> Integer -> a -> b
   arith f i a = f a + fromInteger i
   ```