> module Chapter15 where
>
> import Data.Monoid

Exercise: Optional Monoid

Write the Monoid instance for our Maybe type renamed to Optional.

> data Optional a =
>   Nada
>   | Only a
>   deriving (Eq, Show)
>
> instance Semigroup a => Semigroup (Optional a) where
>   (Only a) <> (Only b) = Only $ a <> b
>   (Only a) <> Nada = Only a
>   Nada <> (Only b) = Only b
>   Nada <> Nada  = Nada
>
> instance Monoid a => Monoid (Optional a) where
>   mempty = Nada
>   mappend = (<>)

Madness

> type Verb = String
> type Adjective = String
> type Adverb = String
> type Noun = String
> type Exclamation = String
>
> madlibbin' :: Exclamation
>            -> Adverb
>            -> Noun
>            -> Adjective
>            -> String
> madlibbin' e adv noun adj =
>   e <> "! he said " <>
>   adv <> " as he jumped into his car " <>
>   noun <> " and drove off with his " <>
>   adj <> " wife."
>
> madlibbinBetter' :: Exclamation
>            -> Adverb
>            -> Noun
>            -> Adjective
>            -> String
> madlibbinBetter' e adv noun adj =
>   mconcat [e, "! he said ", adv, " as he jumped into his car ", noun, " and drove off with his ", adj, " wife."]
