{-# LANGUAGE ViewPatterns, PatternSynonyms #-}

module Data.RiscV.Instruction where

import Clash.Prelude.Safe

newtype MajorOpcode = MajorOpcode (BitVector 5) deriving (Eq)
newtype MinorOpcode = MinorOpcode (BitVector 3) deriving (Eq)
newtype Register = Register (BitVector 5) deriving (Eq)
