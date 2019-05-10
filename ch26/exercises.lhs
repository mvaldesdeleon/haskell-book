>{-# LANGUAGE InstanceSigs #-}
>
> module Chapter26 where
>
> import Control.Monad
> import Control.Applicative

IdentityT

> newtype IdentityT m a = IdentityT { runIdentityT :: m a }
>
> instance Functor m => Functor (IdentityT m) where
>   fmap f (IdentityT ma) = IdentityT $ fmap f ma
>
> instance Applicative m => Applicative (IdentityT m) where
>   pure a = IdentityT $ pure a
>   (IdentityT mf) <*> (IdentityT ma) = IdentityT $ mf <*> ma
>
> instance Monad m => Monad (IdentityT m) where
>   return = pure
>   (IdentityT mf) >>= f = IdentityT $ mf >>= runIdentityT . f

MaybeT

> newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
>
> instance (Functor m) => Functor (MaybeT m) where
>   fmap f (MaybeT mma) = MaybeT $ (fmap . fmap) f mma

> instance (Applicative m) => Applicative (MaybeT m) where
>   pure a = MaybeT $ (pure . pure) a
>   (MaybeT mmf) <*> (MaybeT mma) = MaybeT $ (<*>) <$> mmf <*> mma
>
> instance Applicative m => Alternative (MaybeT m) where
>   empty = MaybeT $ pure empty
>   (MaybeT lmma) <|> (MaybeT rmma) = MaybeT $ (<|>) <$> lmma <*> rmma
>
> instance (Monad m) => Monad (MaybeT m) where
>   return = pure
>
>   (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
>   (MaybeT mma) >>= f = MaybeT $ mma >>= \ma -> case ma of
>                                       Just a -> runMaybeT $ f a
>                                       Nothing -> return Nothing

EitherT

> newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }
>
> instance Functor m => Functor (EitherT e m) where
>   fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea
>
> instance Applicative m => Applicative (EitherT e m) where
>   pure a = EitherT $ (pure . pure) a
>
>   (EitherT mef) <*> (EitherT mea) = EitherT $ (<*>) <$> mef <*> mea
>
> instance Monad m => Monad (EitherT e m) where
>   return = pure
>
>   (EitherT mea) >>= f = EitherT $ do
>       ea <- mea
>       case ea of
>           Left e -> return $ Left e
>           Right a -> runEitherT $ f a
>
> swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
> swapEitherT (EitherT mea) = EitherT $ swapEither <$> mea
>   where
>       swapEither ea = case ea of
>                           Left e -> Right e
>                           Right a -> Left a
>
> eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
> eitherT f g (EitherT mea) = mea >>= either f g

ReaderT

> newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
>
> instance Functor m => Functor (ReaderT r m) where
>   fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma
>
> instance Applicative m => Applicative (ReaderT r m) where
>   pure a = ReaderT $ (pure . pure) a
>   (ReaderT rmf) <*> (ReaderT rma) = ReaderT $ (<*>) <$> rmf <*> rma
>
> instance Monad m => Monad (ReaderT r m) where
>   return = pure

>   (>>=) :: (ReaderT r m a) -> (a -> ReaderT r m b) -> ReaderT r m b
>   (ReaderT rma) >>= f = ReaderT $ \r -> do
>                           a <- rma r
>                           runReaderT (f a) r

StateT

> newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }
>
> instance (Functor m) => Functor (StateT s m) where
>   fmap f (StateT smas) = StateT $ (fmap . fmap) (\(a, s) -> (f a, s)) smas
>
> instance (Monad m) => Applicative (StateT s m) where
>   pure a = StateT $ \s -> pure (a, s)
>
>   (StateT smfs) <*> (StateT smas) = StateT $ \s -> do
>                                               (f, ns) <- smfs s
>                                               (a, fs) <- smas ns
>                                               return (f a, fs)
>
> instance (Monad m) => Monad (StateT s m) where
>   return = pure
>   (StateT smas) >>= f = StateT $ \s -> do
>                           (a, ns) <- smas s
>                           runStateT (f a) ns

Wrap It Up

> embedded :: MaybeT
>               (EitherT String
>                        (ReaderT () IO))
>               Int
> embedded = (MaybeT . EitherT . ReaderT) (const . return . Right . Just $ 1)
>
> test :: EitherT String (ReaderT Bool (StateT Double IO)) Int
> test = undefined
>
> tost = runStateT <$> (runReaderT . runEitherT $ test)

MonadTrans

> class MonadTrans t where
>   lift :: (Monad m) => m a -> t m a
>
> instance MonadTrans IdentityT where
>   lift :: Monad m => m a -> IdentityT m a
>   lift = IdentityT
>
> instance MonadTrans MaybeT where
>   lift :: Monad m => m a -> MaybeT m a
>   lift = MaybeT . fmap Just
>
> instance MonadTrans (EitherT e) where
>   lift :: Monad m => m a -> EitherT e m a
>   lift = EitherT . fmap Right
>
> instance MonadTrans (ReaderT r) where
>   lift :: Monad m => m a -> ReaderT r m a
>   lift = ReaderT . const
>
> instance MonadTrans (StateT s) where
>   lift :: Monad m => m a -> StateT s m a
>   lift ma = StateT $ \s -> fmap (\a -> (a, s)) ma

MonadIO

> class (Monad m) => MonadIO m where
>   liftIO :: IO a -> m a
>
> instance MonadIO IO where
>   liftIO = id
>
> instance MonadIO m => MonadIO (IdentityT m) where
>   liftIO = lift . liftIO
>
> instance MonadIO m => MonadIO (MaybeT m) where
>   liftIO = lift . liftIO
>
> instance MonadIO m => MonadIO (EitherT e m) where
>   liftIO = lift . liftIO
>
> instance MonadIO m => MonadIO (ReaderT r m) where
>   liftIO = lift . liftIO
>
> instance MonadIO m => MonadIO (StateT s m) where
>   liftIO = lift . liftIO

Chapter Exercises

> data Identity a = Identity { runIdentity :: a }
>   deriving (Eq, Ord, Show)
>
> instance Functor Identity where
>   fmap f (Identity a) = Identity $ f a
>
> instance Applicative Identity where
>   pure = Identity
>   (Identity f) <*> (Identity a) = Identity $ f a
>
> instance Monad Identity where
>   return = pure
>   (Identity a) >>= f = f a
>
> type Reader r a = ReaderT r Identity a
>
> runReader :: Reader r a -> r -> a
> runReader ra r = runIdentity $ runReaderT ra r
>
> ask :: Monad m => ReaderT r m r
> ask = ReaderT $ return

1. rDec is a function that should get its argument in the context of Reader and return a value decremented by one.

2. Once you have an rDec that works, make it and any inner lambdas pointfree if that’s not already the case.

> rDec :: Num a => Reader a a
> rDec = subtract 1 <$> ask

3. rShow is show, but in Reader.

4. Once you have an rShow that works, make it pointfree.

> rShow :: Show a => ReaderT a Identity String
> rShow = show <$> ask

5. rPrintAndInc will first print the input with a greeting, then return the input incremented by one.

> rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
> rPrintAndInc = do
>   val <- ask
>   liftIO $ putStrLn $ "Hi: " ++ show val
>   return $ val + 1

6. sPrintIncAccum first prints the input with a greeting, then puts the incremented input as the new state, and returns the original input as a String.

> get :: Monad m => StateT s m s
> get = StateT $ \s -> return (s, s)
>
> put :: Monad m => s -> StateT s m ()
> put s = StateT $ \_ -> return ((), s)

> sPrintIncAccum :: (Num a, Show a) => StateT a IO String
> sPrintIncAccum = do
>   val <- get
>   liftIO $ putStrLn $ "Hi:" ++ show val
>   put $ val + 1
>   return $ show val

Fix the code

> isValid :: String -> Bool
> isValid v = '!' `elem` v
>
> maybeExcite :: MaybeT IO String
> maybeExcite = do
>   v <- liftIO getLine
>   guard $ isValid v
>   return v
>
> doExcite :: IO ()
> doExcite = do
>   putStrLn "say something excite!"
>   excite <- runMaybeT maybeExcite
>   case excite of
>       Nothing -> putStrLn "MOAR EXCITE"
>       Just e -> putStrLn ("Good, was very excite: " ++ e)
