module Yoga.Fetch.Om.ClientFunction
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
  | pathQueryRL -> pathQuery
  , headersRL -> headers
  , pathQueryRL headersRL bodyFlag body errorRow result -> fn where
  buildClientFn
    :: Proxy pathQueryRL
    -> Proxy headersRL
    -> Proxy bodyFlag
    -> (Record pathQuery -> Record headers -> body -> Om (Record ()) errorRow result)
    -> fn

-- No path/query, no headers, no body
instance
  BuildClientFn RL.Nil
    RL.Nil
    IsUnit
    Unit
    ()
    ()
    errorRow
    result
    (Om (Record ()) errorRow result) where
  buildClientFn _ _ _ f = f {} {} unit

-- No path/query, no headers, with body
instance
  BuildClientFn RL.Nil
    RL.Nil
    IsNotUnit
    body
    ()
    ()
    errorRow
    result
    (body -> Om (Record ()) errorRow result) where
  buildClientFn _ _ _ f b = f {} {} b

-- No path/query, with headers, no body
instance
  BuildClientFn RL.Nil
    (RL.Cons n t tl)
    IsUnit
    Unit
    ()
    h
    errorRow
    result
    (Record h -> Om (Record ()) errorRow result) where
  buildClientFn _ _ _ f hdrs = f {} hdrs unit

-- No path/query, with headers, with body
instance
  BuildClientFn RL.Nil
    (RL.Cons n t tl)
    IsNotUnit
    body
    ()
    h
    errorRow
    result
    (Record h -> body -> Om (Record ()) errorRow result) where
  buildClientFn _ _ _ f hdrs b = f {} hdrs b

-- With path/query, no headers, no body
instance
  BuildClientFn (RL.Cons n t tl)
    RL.Nil
    IsUnit
    Unit
    pq
    ()
    errorRow
    result
    (Record pq -> Om (Record ()) errorRow result) where
  buildClientFn _ _ _ f pqr = f pqr {} unit

-- With path/query, no headers, with body
instance
  BuildClientFn (RL.Cons n t tl)
    RL.Nil
    IsNotUnit
    body
    pq
    ()
    errorRow
    result
    (Record pq -> body -> Om (Record ()) errorRow result) where
  buildClientFn _ _ _ f pqr b = f pqr {} b

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
    (Record pq -> Record h -> Om (Record ()) errorRow result) where
  buildClientFn _ _ _ f pqr hdrs = f pqr hdrs unit

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
    (Record pq -> Record h -> body -> Om (Record ()) errorRow result) where
  buildClientFn _ _ _ f = f
