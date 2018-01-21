# Mood Swing

Given the following datatype, answer the following questions:

`data Mood = Blah | Woot deriving Show`

1. What is the type constructor, or name of this type?

   `Mood`.

2. If the function requires a `Mood` value, what are the values you could possibly use there?

   `Blah` or `Woot`.

3. We are trying to write a function `changeMood` to change Chris’s mood instantaneously. It should act like not in that, given one value, it returns the other value of the same type. So far, we’ve written a type signature `changeMood :: Mood -> Woot`. What’s wrong with that?

   `Woot` is a data constructor, so it cannot be used in a type signature. Use `Mood` instead.

4. Now we want to write the function that changes his mood. Given an input mood, it gives us the other one. Fix any mistakes and complete the function:

   ```
   changeMood Mood = Woot
   changeMood    _ = Blah
   ```

   `Mood` is a type constructor, and cannot be used at the code level. Use `Blah` instead.

5. Enter all of the above — datatype (including the deriving Show bit), your corrected type signature, and the corrected function into a source file. Load it and run it in GHCi to make sure you got it right.

   See [moodswing.hs](https://github.com/mvaldesdeleon/haskell-book/blob/master/ch04/moodswing.hs)