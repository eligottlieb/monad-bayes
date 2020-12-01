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
import Data.Monoid
import Numeric.Log (Log)

-- data Learner p a b = Learner {
--   parameters :: p,
--   implementation :: p -> a -> b,
--   update :: p -> a -> b -> p,
--   request :: p -> a -> b -> a
-- }

data Trace = Trace [[Double]] [Log Double]

extend :: Trace -> Trace -> Trace
extend (Trace va da) (Trace vb db) =
  Trace (map (uncurry (++)) (zip va vb)) (map (uncurry (*)) (zip da db))

instance Semigroup Trace where
  (<>) = extend

instance Monoid Trace where
  mempty = Trace [] []
  mappend = extend

-- instance Category (Learner Trace) where
--   id = Learner {parameters = ([], [1]), implementation = \t a -> a,
--                 update = \t a b -> t, request = \t a b -> b}
--   cb . ca = Learner {
--     parameters = extend (parameters ca) (parameters cb),
--     implementation = \t a -> implementation cb t $ implementation ca t a,
--     update = \t a c -> let b = implementation ca t a
--                            b' = request cb t b c in
--       extend (update ca t a b') (update cb t b c),
--     request = \t a c -> let b = implementation ca t a in
--       request ca t a $ request cb t b c
--   }

data TracedLearner t p a b = TracedLearner {
  parameters :: p,
  implementation :: t -> p -> a -> (b, t),
  update :: p -> a -> t -> p,
  propagate :: p -> t -> a -> b -> a
}

data Tree a = Leaf a | Branch (Tree a) (Tree a)

instance Semigroup (Tree a) where
  t <> t' = Branch t t'

instance Monoid a => Monoid (Tree a) where
  mempty = Leaf mempty

instance Category (TracedLearner (Tree Trace) (Tree [Double])) where
  id = TracedLearner {
    parameters = Leaf [],
    implementation = \t p a -> (a, mempty),
    update = \p a t -> p,
    propagate = \p t a b -> a
  }
  cb . ca = TracedLearner {
    parameters = parameters ca <> parameters cb,
    implementation = impl ca cb,
    update = upd ca cb,
    propagate = propag ca cb
  }

impl :: TracedLearner (Tree Trace) (Tree [Double]) a b ->
        TracedLearner (Tree Trace) (Tree [Double]) b c -> Tree Trace ->
        Tree [Double] -> a -> (c, Tree Trace)
impl ca cb (Branch ta tb) (Branch pa pb) a =
  let (b, ta') = implementation ca ta pa a
      (c, tb') = implementation cb tb pb b in
    (c, ta' <> tb')

upd :: TracedLearner (Tree Trace) (Tree [Double]) a b ->
       TracedLearner (Tree Trace) (Tree [Double]) b c ->
       Tree [Double] -> a -> Tree Trace -> Tree [Double]
upd ca cb (Branch pa pb) a (Branch ta tb) =
  let (b, _) = implementation ca ta pa a in
    (update ca pa a ta) <> (update cb pb b tb)

propag :: TracedLearner (Tree Trace) (Tree [Double]) a b ->
          TracedLearner (Tree Trace) (Tree [Double]) b c ->
          Tree [Double] -> Tree Trace -> a -> c -> a
propag ca cb (Branch pa pb) (Branch ta tb) a c =
  let (b, _) = implementation ca ta pa a in
    propagate ca pa ta a $ propagate cb pb tb b c

-- type ProperlyWeightedLearner p a b = Learner (p, Trace) [a] ([b], Trace)
--
-- type Proposal p a b = (p, (p -> [a] -> ([b], Trace)))
-- type Target p a b = (p, p -> Trace -> [a] -> ([b], Trace))
--
-- importanceSample :: Proposal phi a b -> Target theta a b -> ProperlyWeightedLearner (theta, phi) a b
-- importanceSample (phi, q) (theta, p) = Learner {
--   parameters = (theta, phi),
--   implementation (th, ph)
-- }

data TraceType = RandomVars [String] | Sequential [TraceType] |
                 Parallel [TraceType]

(&) :: TraceType -> TraceType -> TraceType
(Parallel tys) & (Parallel tys') = Parallel (tys ++ tys')
ty & ty' = Parallel [ty, ty']

(>>) :: TraceType -> TraceType -> TraceType
(Sequential tys) >> (Sequential tys') = Sequential (tys ++ tys')
ty >> ty' = Sequential [ty, ty']
