{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.RiscV.Instruction.I where

import Clash.Explicit.Prelude.Safe

import Data.RiscV.Instruction

pattern R { major, rd, minor, rs1, imm } <-
    (split -> (split -> (split -> (split -> (imm :: BitVector 12, Register -> rs1),
                                   MinorOpcode -> minor),
                         Register -> rd),
               split -> (0b11 :: BitVector 2, MajorOpcode -> major))) where
    R (MajorOpcode major) (Register rd) (MinorOpcode minor) (Register rs1) imm =
        unpack $ imm ++# rs1 ++# minor ++# rd ++# major ++# (0b11 :: BitVector 2)
