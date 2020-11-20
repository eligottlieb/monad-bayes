{-|
Module      : Control.Monad.Bayes.Traced
Description : Distributions on execution traces
Copyright   : (c) Adam Scibior, 2015-2020
License     : MIT
Maintainer  : leonhard.markert@tweag.io
Stability   : experimental
Portability : GHC

-}

module Control.Monad.Bayes.Traced (
  module Control.Monad.Bayes.Traced.Static
) where

import Control.Category
import Control.Monad.Bayes.Traced.Static
import Numeric.Log (Log)

type Trace = ([Double], Log Double)

extend :: Trace -> Trace -> Trace
extend (va, da) (vb, db) = (va ++ vb, da * db)

data Learner p a b = Learner {
  parameters :: p,
  implementation :: p -> a -> b,
  update :: p -> a -> b -> p,
  request :: p -> a -> b -> a
}

instance Category (Learner Trace) where
  id = Learner {parameters = ([], 1), implementation = \t a -> a,
                update = \t a b -> t, request = \t a b -> b}
  cb . ca = Learner {
    parameters = extend (parameters ca) (parameters cb),
    implementation = \t a -> implementation cb t $ implementation ca t a,
    update = \t a c -> let b = implementation ca t a
                           b' = request cb t b c in
      extend (update ca t a b') (update cb t b c),
    request = \t a c -> let b = implementation ca t a in
      request ca t a $ request cb t b c
  }
