{-
instance Functor (Parser tok a) where
    fmap f (Parser p) = Parser $ \tok -> fmap (first (fmap f)) (p tok)

instance Applicative (Parser tok a) where
    pure x = Parser $ \tok -> return (const x, tok)


{-
newtype ParserM tok a = ParserM { runParserM :: tok -> ListT (Either RouteError) (a, tok) }

instance Monad (ParserM tok) where
    return a = ParserM $ \tok -> return (a, tok)
    (ParserM p) >>= f = 
        ParserM $ \tok ->
            do (a, tok') <- p tok
               runParserM (f a) tok

newtype Parser tok a b = Parser { runParser :: tok -> ListT (Either RouteError) (a -> b, tok) }

instance Monad (Parser tok a) where
    (Parser p) >>= k = 
        Parser $ \tok ->
            do (f, tok') <- p tok
               (b, tok'') <- runParser (k (f undefined)) tok'
               return (b, tok'')
-}

{-
   instance Monad ((->) r) where
        return = const
        f >>= k = \ r -> k (f r) r 
-}

{-
newtype Parser tok a b = Parser { runParser :: tok -> Either RouteError [(a -> b, tok)] }

instance Functor (Parser tok a) where
    fmap f (Parser p) = 
        Parser $ \s -> fmap (map (first (fmap f))) $ p s

instance Monad (Parser tok a) where
    return b = Parser $ \tok -> Right [(const b, tok)]
    (Parser p) >>= f = Parser $ \tok ->
        case p tok of
          (Left e) -> Left e
          (Right as) ->
              case partitionEithers [ (runParser (f a) tok')  | (a, tok') <- as ] of
                ([], []) -> Right []
--              partitionEithers (map f as)
  -}  
{-
-- | Build a router for a value given all the ways to parse and serialize it.
val :: (tok ->  Either e [(a, tok)]) -> (a -> [tok -> tok]) -> Router e tok r (a :- r)
val rs ss = Router
    (liftEitherP (fmap (map (first (:-))) . rs))
    (\(a :- r) -> (map (\f -> (f, r))) (ss a))
-}
liftEither :: (Error e) => Either e [a] -> ListT (StateT ErrorPos (Either e)) a
liftEither (Left e)   = ListT $ (StateT $ \s -> Left e)
liftEither (Right as) = ListT (StateT $ \s -> Right (as, s))

liftEitherP :: (tok -> Either RouteError [(a, tok)]) -> Parser RouteError tok a
liftEitherP e = Parser $ \tok -> liftEither (e tok)
