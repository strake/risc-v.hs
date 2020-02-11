{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.RiscV.Instruction.R where

import Clash.Explicit.Prelude.Safe

import Data.RiscV.Instruction

pattern R { major, rd, minor, rs1, rs2, func } <-
    (split -> (split -> (split -> (split -> (func :: BitVector 7,
                                             split -> (Register -> rs2, Register -> rs1)),
                                   MinorOpcode -> minor),
                         Register -> rd),
               split -> (0b11 :: BitVector 2, MajorOpcode -> major))) where
    R (MajorOpcode major) (Register rd) (MinorOpcode minor) (Register rs1) (Register rs2) func =
        unpack $ func ++# rs2 ++# rs1 ++# minor ++# rd ++# major ++# (0b11 :: BitVector 2)
