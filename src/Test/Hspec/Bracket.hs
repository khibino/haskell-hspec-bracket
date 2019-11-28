-- |
-- Module      : Test.Hspec.Bracket
-- Copyright   : 2019 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module provides bracket definitions for Hspec Monads.
module Test.Hspec.Bracket (
  bracketSpecWith_,
  bracketSpecWith,
  ) where

import Control.Exception (bracket_, bracket)
import Test.Hspec (Spec, SpecWith, runIO, hspec, before)

-- | bracket_ definition for `SpecWith`
bracketSpecWith_ :: (Spec -> Spec) -- ^ execution strategy like `parallel`
                 -> IO a           -- ^ begin action
                 -> IO b           -- ^ end action
                 -> c              -- ^ example item
                 -> SpecWith c     -- ^ spec body
                 -> SpecWith c     -- ^ result bracketed spec
bracketSpecWith_ strategy begin end item spec =
  runIO $ bracket_ begin end $
  hspec $ strategy $ before (return item) spec

-- | bracket definition for `SpecWith`
bracketSpecWith :: (Spec -> Spec)    -- ^ execution strategy like `parallel`
                -> IO a              -- ^ open action
                -> (a -> IO b)       -- ^ close action
                -> c                 -- ^ example item
                -> (a -> SpecWith c) -- ^ spec body
                -> SpecWith c        -- ^ result bracketed spec
bracketSpecWith strategy open close item spec =
  runIO $ bracket open close $
  \r -> hspec $ strategy $ before (return item) $ spec r
