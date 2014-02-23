---------------------------------------------------------------------------------
-- |
-- Module       : Control.Monad.Free.TH.Types
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- Code to generate some of the simpler types.
--
---------------------------------------------------------------------------------
module Control.Monad.Free.TH.Types where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
    (   Dec
    ,   Name
    ,   Q
    )

-- $setup
-- >>> import Language.Haskell.TH.Lib ( stringE )
-- >>> import Language.Haskell.TH.Syntax ( runQ )

-- | Takes the name of a type class and declares a Functor for it that can
-- be made into a monad with the Free functor.
--
-- At the moment it ignores Contexts ( constraints like @(Eq a) =>@ etc..)
--
-- (Contributions welcome!)
--
-- e.g (see Control.Monad.State#MonadState for a more proper typeclass that uses dependent types.)
-- @
--     -- Not the best way to express the State Monad in a typeclass I know...
--     class MonadState m where
--         data State m
--         get :: m (State m)
--         put :: State m -> m ()
-- @
--
-- `declareFunctor` should produce something like:
--
-- @
--     data MonadStateF s a =
--             Get (s -> a)
--         |   Put a s
-- @
--
-- >>> class MonadState m where data State m; get :: m (State m); put :: State m -> m ()
--
-- $(stringE . show =<< declareFunctor ''MonadState)
--
--
-- declareFunctor :: Name -> Q Info
-- declareFunctor nm = do
--     info <- reify nm
--     case info of
--         ClassI (ClassD _ nm ) _    -> functorFromClassDec dec
--         _               -> error $ show nm ++ " is not a typeclass."
-- 
-- functorFromClassDec :: Dec -> Q Dec
-- functorFromClassDec dec


-- | Takes in a data type (which has a Functor instance) with data constructors of the following forms:
--
-- @
--     data FooFunctor x y a =
--             TerminatingAction x      -- ^ doesn't contain a at all, corresponding to cases like @ terminate :: x -> m () @ where @ terminate x y z >>= f == terminate x @, i.e it stops all further binds.. (e.g `exitFailure`)
--         |   NonTerminatingAction a x -- ^ a product type with a, corresponding to actions like @ action :: x -> m () @ where action does not stop further binding (e.g `putStr`)
--         |   WithValue (y -> a) x     -- ^ a product type with (y -> a), corresponding to monadic values like @ foo :: x -> m y @
-- @
--
-- And produces some simple declarations of the types that will be necessary:
--
-- @
--     type FooFunctorT' m a = FreeT (FooFunctor x y z) m a
-- @
