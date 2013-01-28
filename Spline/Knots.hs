-- Copyright (c) 2013, Yogesh Sajanikar (yogesh_sajanikar@yahoo.com)
-- All rights reserved
--
-- Redistribution and use in source and binary forms, with or without 
-- modification, are permitted provided that the following conditions are met: 

-- Redistributions of source code must retain the above copyright notice, this 
-- list of conditions and the following disclaimer. Redistributions in binary 
-- form must reproduce the above copyright notice, this list of conditions and 
-- the following disclaimer in the documentation and/or other materials 
-- provided with the distribution.   

-- Neither the name of the <ORGANIZATION> nor the names of its contributors may 
-- be used to endorse or promote products derived from this software without 
-- specific prior written permission.   

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
-- ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE 
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
-- POSSIBILITY OF SUCH DAMAGE.          

{-# LANGUAGE TypeFamilies,FlexibleContexts #-}

-- | KnotSpace implements KnotVector, and Knot
module KnotSpace where

import Data.Maybe

-- In future, we might want to provide an environment, just like Parsec, so 
-- that the environment (actually a space/metric) which provides basic 
-- facilities such as Scalar/Vector/Dimension that can be applied as a default 
-- in the library. The environment can also provide a precision that can be 
-- used for distinguishing two Knots

-- | A knot is a ordered numeral with an associated multiplicity
class (Eq k, Ord k) => Knot k where 
  type ScalarT k :: *
  type IntT    k :: *
  create :: ScalarT k -> k
  mult   :: k         -> IntT k
  value  :: k         -> ScalarT k
  
  
-- | A knot sequence is a ordered collection of Knot
class Knots ks where 
  type KnotT ks :: *
  empty :: Knot (KnotT ks) => KnotT ks -> ks
  insert :: Knot (KnotT ks) => ks -> KnotT ks -> ks
  insertS :: Knot (KnotT ks) => ks -> ScalarT (KnotT ks) -> ks
  find :: Knot (KnotT ks) => ks -> ScalarT (KnotT ks) -> Maybe ks
  

data KnotD = KnotD { valueD :: Double, multD :: Integer } deriving Show

instance Eq KnotD where
  (==) x y = (valueD x) == (valueD y)

instance Ord KnotD where
  compare x y = compare (valueD x) (valueD y)
  
instance Knot KnotD where
  type ScalarT KnotD = Double
  type IntT    KnotD = Integer
  create d = KnotD d 1
  mult k = multD k
  value k = valueD k

