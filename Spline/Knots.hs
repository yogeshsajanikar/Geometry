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
import Data.Set (Set, empty, member, insert, split)

-- In future, we might want to provide an environment, just like Parsec, so 
-- that the environment (actually a space/metric) which provides basic 
-- facilities such as Scalar/Vector/Dimension that can be applied as a default 
-- in the library. The environment can also provide a precision that can be 
-- used for distinguishing two Knots

-- | A knot is a ordered numeral with an associated multiplicity
class (Eq k, Ord k) => Knot k where 
  type ScalarT k :: *
  type IntT    k :: *
  -- | Create the knot from its scalar with multiplicity 1
  create :: ScalarT k -> k
  -- | Access the multiplicity of the knot
  mult   :: k         -> IntT k
  -- | Access the value of the knot
  value  :: k         -> ScalarT k
  
  
-- | A knot sequence is a ordered collection of Knot
class Knots ks where 
  type KnotT ks :: *
  -- | Create empty knot sequence
  empty   :: ks
  -- | Insert a given knot in the sequence
  insert  :: Knot (KnotT ks) => ks       -> KnotT ks           -> ks
  -- | Insert a given knot value (multiplicity 1) in the given knot vector 
  insertS :: Knot (KnotT ks) => ks       -> ScalarT (KnotT ks) -> ks
  -- | Find if a scalar value is present in the given knot sequence
  find    :: Knot (KnotT ks) => ks       -> ScalarT (KnotT ks) -> Bool
  
-- | KnotD represents instance of Knot (Essentially a real knot value)
data KnotD = KnotD { valueD :: Double, multD :: Integer } deriving Show

instance Eq KnotD where
  (==) x y = (valueD x) == (valueD y)

instance Ord KnotD where
  compare x y = compare (valueD x) (valueD y)
  
instance Knot KnotD where
  type ScalarT KnotD = Double
  type IntT    KnotD = Integer
  create d = KnotD d 1
  mult   k = multD k
  value  k = valueD k

data KnotV a = KnotV { valueV:: a, multV:: Integer } deriving(Show)

incrV :: KnotV a -> KnotV a
incrV k = KnotV (valueV k) (1 + (multV k))

instance Eq a => Eq (KnotV a) where
  (==) k1 k2 = (valueV k1) == (valueV k2)
  
instance Ord a => Ord (KnotV a) where
  compare k1 k2 = compare (valueV k1) (valueV k2)

instance (Eq a, Ord a) => Knot (KnotV a) where 
  type ScalarT (KnotV a) = a
  type IntT    (KnotV a) = Integer
  create d = KnotV d 1
  mult   k = multV k
  value  k = valueV k

-- Create the knot set 

data KnotsV a = KnotsV { knotSetV :: Set(KnotV a) } deriving(Show)

insertV :: (Ord a) => KnotsV a -> KnotV a -> KnotsV a 
insertV    ks          k = 
  let 
    kset = knotSetV ks
    kb   = member k kset
    dk   = if kb then incrV k else k
  in
     KnotsV (Data.Set.insert dk kset)
              


instance (Eq a, Ord a) => Knots (KnotsV a) where
  type KnotT (KnotsV a) = KnotV a
  empty  = KnotsV Data.Set.empty
  insert = insertV
  insertS ks s = KnotSpace.insert ks (KnotV s 1)
  find ks s = member (KnotV s 1) (knotSetV ks)

                 
    