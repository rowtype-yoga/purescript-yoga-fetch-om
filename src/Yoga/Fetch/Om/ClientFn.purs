module Yoga.Fetch.Om.ClientFn
  ( class BuildClientFn
  , buildClientFn
  , class CheckBodyIsUnit
  , IsUnit
  , IsNotUnit
  ) where

import Data.Unit (Unit, unit)
import Prim.RowList (RowList)
import Prim.RowList as RL
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)
import Yoga.Om (Om)

data IsUnit
data IsNotUnit

class CheckBodyIsUnit :: Type -> Type -> Constraint
class CheckBodyIsUnit body flag | body -> flag

instance CheckBodyIsUnit Unit IsUnit
else instance CheckBodyIsUnit body IsNotUnit

class BuildClientFn
  :: RowList Type -> RowList Type -> Type -> Type -> Row Type -> Row Type -> Row Type -> Type -> Type -> Constraint
class
  BuildClientFn pathQueryRL headersRL bodyFlag body pathQuery headers errorRow result fn
  | pathQueryRL headersRL bodyFlag body pathQuery headers errorRow result -> fn where
  buildClientFn
    :: Proxy pathQueryRL
    -> Proxy headersRL
    -> Proxy bodyFlag
    -> (Record pathQuery -> Record headers -> body -> Om {} errorRow result)
    -> fn

-- No path/query, no headers, no body
instance
  BuildClientFn RL.Nil
    RL.Nil
    IsUnit
    Unit
    pq
    h
    errorRow
    result
    (Om {} errorRow result) where
  buildClientFn _ _ _ f = f (unsafeCoerce {}) (unsafeCoerce {}) unit

-- No path/query, no headers, with body
instance
  BuildClientFn RL.Nil
    RL.Nil
    IsNotUnit
    body
    pq
    h
    errorRow
    result
    (body -> Om {} errorRow result) where
  buildClientFn _ _ _ f = \b -> f (unsafeCoerce {}) (unsafeCoerce {}) b

-- No path/query, with headers, no body
instance
  BuildClientFn RL.Nil
    (RL.Cons n t tl)
    IsUnit
    Unit
    pq
    h
    errorRow
    result
    (Record h -> Om {} errorRow result) where
  buildClientFn _ _ _ f = \hdrs -> f (unsafeCoerce {}) hdrs unit

-- No path/query, with headers, with body
instance
  BuildClientFn RL.Nil
    (RL.Cons n t tl)
    IsNotUnit
    body
    pq
    h
    errorRow
    result
    (Record h -> body -> Om {} errorRow result) where
  buildClientFn _ _ _ f = \hdrs b -> f (unsafeCoerce {}) hdrs b

-- With path/query, no headers, no body
instance
  BuildClientFn (RL.Cons n t tl)
    RL.Nil
    IsUnit
    Unit
    pq
    h
    errorRow
    result
    (Record pq -> Om {} errorRow result) where
  buildClientFn _ _ _ f = \pqr -> f pqr (unsafeCoerce {}) unit

-- With path/query, no headers, with body
instance
  BuildClientFn (RL.Cons n t tl)
    RL.Nil
    IsNotUnit
    body
    pq
    h
    errorRow
    result
    (Record pq -> body -> Om {} errorRow result) where
  buildClientFn _ _ _ f = \pqr b -> f pqr (unsafeCoerce {}) b

-- With path/query, with headers, no body
instance
  BuildClientFn (RL.Cons n t tl)
    (RL.Cons hn ht htl)
    IsUnit
    Unit
    pq
    h
    errorRow
    result
    (Record pq -> Record h -> Om {} errorRow result) where
  buildClientFn _ _ _ f = \pqr hdrs -> f pqr hdrs unit

-- With path/query, with headers, with body
instance
  BuildClientFn (RL.Cons n t tl)
    (RL.Cons hn ht htl)
    IsNotUnit
    body
    pq
    h
    errorRow
    result
    (Record pq -> Record h -> body -> Om {} errorRow result) where
  buildClientFn _ _ _ f = f
