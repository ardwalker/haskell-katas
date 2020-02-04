
{-
https://downloads.haskell.org/~ghc/7.0.3/docs/html/users_guide/pragmas.html
-}

--------------------------------------------------------------------------------
-- Compiler options
-- See Language Options at https://downloads.haskell.org/~ghc/7.0.3/docs/html/users_guide/flag-reference.html
--------------------------------------------------------------------------------
{-# OPTIONS_GHC #-}
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}


--------------------------------------------------------------------------------
-- LANGUAGE pragma
--------------------------------------------------------------------------------
{-# LANGUAGE #-}
{-# LANGUAGE ForeignFunctionInterface, CPP #-}


{-# INCLUDE #-}

--------------------------------------------------------------------------------
-- WARNING and DEPRECATED pragmas
--------------------------------------------------------------------------------
module Wibble {-# DEPRECATED "Use Wobble instead" #-} where
module Wibble {-# WARNING "This is an unstable interface." #-} where  
  
-- You can attach a warning to a function, class, type, or data constructor, with the following top-level declarations:
{-# DEPRECATED f, C, T "Don't use these" #-}
{-# WARNING unsafePerformIO "This is unsafe; I hope you know what you're doing" #-}


