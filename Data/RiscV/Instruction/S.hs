{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.RiscV.Instruction.S where

import Clash.Explicit.Prelude.Safe

import Util

import Data.RiscV.Instruction

pattern S { major, minor, rs1, rs2, imm } <-
    (split -> ((split & \ (split -> (imm1 :: BitVector 7, blah), imm0 :: BitVector 5) -> (imm1 ++# imm0, blah)) ->
               (imm,
                split -> (split -> (Register -> rs1, Register -> rs2),
                          MinorOpcode -> minor)),
               split -> (0b11 :: BitVector 2, MajorOpcode -> major))) where
    S (MajorOpcode major) (MinorOpcode minor) (Register rs1) (Register rs2) (split -> (imm1, imm0)) =
        unpack $ imm1 ++# rs2 ++# rs1 ++# minor ++# (imm0 :: BitVector 5) ++# major ++# (0b11 :: BitVector 2)
