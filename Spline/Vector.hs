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

{-# LANGUAGE TypeFamilies #-}

module VectorSpace where

infixl 7 <+>

class Vector k where
  type ScalarT k :: *
  -- | Zero identity
  zeroV :: k
  -- | Negate the given vector
  negateV :: k -> k
  -- | Addition of vectors
  (<+>) :: k -> k -> k
  -- | Subtraction of vectors
  (<->) :: k -> k -> k
  (<->) k1 k2 = (<+>) k1 (negateV k2)
  -- | Post multiplication of vetor with a scalar
  (<*>>) :: k -> ScalarT k -> k
  -- | Pre multiplication of vector
  (<<*>) :: ScalarT k -> k -> k
  (<<*>)   s             k   = (<*>>) k s


data Vector2D = Vector2D { x :: Double, y :: Double } deriving(Show)

instance Vector Vector2D where
  type ScalarT Vector2D = Double
  zeroV = Vector2D 0.0 0.0
  negateV v = Vector2D (- (x v)) (- (y v))
  (<+>) v1 v2 = Vector2D ((x v1) + (x v2)) ((y v1) + (y v2))
  (<*>>) v1 s = Vector2D ((x v1) * s) ((y v1) * s)
