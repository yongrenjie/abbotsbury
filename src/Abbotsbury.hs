-- |
-- Module    : Abbotsbury
-- Copyright : (C) 2021 Jonathan Yong
-- License   : MIT
--
-- This top-level module (re-)exports all of the functionality that you, as an
-- end user, will likely use. Documentation is provided in the main submodules,
-- which are as follows:
--
--  * "Abbotsbury.Work" defines the 'Work' and other fundamental datatypes used
--  by @abbotsbury@.
--  * "Abbotsbury.Crossref" provides functions for creating 'Work's by fetching
--    data from Crossref.
--  * "Abbotsbury.Cite" generates citations from 'Work's.

module Abbotsbury
  ( module Abbotsbury.Work
  , module Abbotsbury.Crossref
  , module Abbotsbury.Cite
  ) where

import           Abbotsbury.Cite
import           Abbotsbury.Crossref
import           Abbotsbury.Work
