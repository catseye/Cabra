--
-- cabra.hs
-- Interpreter for the Cabra Programming Language
-- Chris Pressey, Cat's Eye Technologies
--
-- $Id$
--

--
-- Copyright (c)2007 Cat's Eye Technologies.  All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--
--  1. Redistributions of source code must retain the above copyright
--     notices, this list of conditions and the following disclaimer.
--  2. Redistributions in binary form must reproduce the above copyright
--     notices, this list of conditions, and the following disclaimer in
--     the documentation and/or other materials provided with the
--     distribution.
--  3. Neither the names of the copyright holders nor the names of their
--     contributors may be used to endorse or promote products derived
--     from this software without specific prior written permission. 
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
-- FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
-- COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
-- INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
-- BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
-- LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.
--


-----------------------------------------------------------------------
-- ========================== Data types =========================== --
-----------------------------------------------------------------------

import qualified Data.Set as Set

data Cabra = Skip
           | UnSet Integer
           | Set Integer
           | Bottom
           | IfSet Integer Cabra Cabra
           | Par Cabra Cabra
           | Seq Cabra Cabra
    deriving (Show, Ord, Eq)


-----------------------------------------------------------------------
-- =========================== Execution =========================== --
-----------------------------------------------------------------------

interpret :: Cabra -> (Set.Set Integer) -> ((Set.Set Integer), Integer)

interpret (Set x) input =
    let
        cycles = if (Set.member x input) then 1 else x
    in
        (Set.union input (Set.singleton x), cycles)

interpret (UnSet x) input = (Set.difference input (Set.singleton x), 1)

interpret (IfSet x a b) input =
    if (Set.member x input) then
        interpret a input
    else
        interpret b input

interpret (Seq a b) input =
    let
        (output_a, cycles_a) = interpret a input
        (output_b, cycles_b) = interpret b output_a
    in
        (output_b, cycles_a + cycles_b)

interpret (Par a b) input =
    let
        (output_a, cycles_a) = interpret a input
        (output_b, cycles_b) = interpret b input
    in
        if (cycles_a < cycles_b) then
            (output_a, cycles_a)
        else
            if (cycles_b < cycles_a) then
                (output_b, cycles_b)
            else
                if (a < b) then  -- lexicographic tiebreaker
                    (output_a, cycles_a)
                else
                    (output_b, cycles_b)

interpret Skip input = (input, 0)

interpret Bottom input = interpret Bottom input

run prog input =
    let
        (output, cycles) = interpret prog (Set.fromList input)
    in
        Set.elems output


-----------------------------------------------------------------------
-- =========================== Test Cases ========================== --
-----------------------------------------------------------------------

test 1 =
    Seq (Set 5) (Set 23)

test 2 =
    IfSet 5 (Seq (UnSet 5) (Set 6)) Skip

test 3 =
    Par (IfSet 100 (Set 300) Skip)
        (Seq (UnSet 100) (Set 10))

--
-- Tests for right-distributivity.
--

test 4 =
    Seq
        (Par (Set 1) (Set 2))
        (IfSet 1 (IfSet 2 (Set 3) Skip) Skip)

test 5 =
    Par
        (Seq (Set 1) (IfSet 1 (IfSet 2 (Set 3) Skip) Skip))
        (Seq (Set 2) (IfSet 1 (IfSet 2 (Set 3) Skip) Skip))

test 6 =
    Seq
        (Par (Set 4) (UnSet 4))
        (IfSet 4 (Seq (UnSet 4) (Set 6)) (Set 5))

test 7 =
    Par
        (Seq (Set 4)   (IfSet 4 (Seq (UnSet 4) (Set 6)) (Set 5)))
        (Seq (UnSet 4) (IfSet 4 (Seq (UnSet 4) (Set 6)) (Set 5)))
