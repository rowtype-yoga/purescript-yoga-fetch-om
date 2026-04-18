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
  :: RowList Type -> RowList Type -> Type -> Type -> Row Type -> Row Type -> Row Type -> Type -> Row Type -> Type -> Type -> Constraint
class
  BuildClientFn pathQueryRL headersRL bodyFlag body pathQuery headers given ctx errorRow result fn
  | pathQueryRL -> pathQuery
  , headersRL -> headers
  , pathQueryRL headersRL bodyFlag body given ctx errorRow result -> fn where
  buildClientFn
    :: Proxy pathQueryRL
    -> Proxy headersRL
    -> Proxy bodyFlag
    -> (Record given -> Record pathQuery)
    -> (Record pathQuery -> Record headers -> body -> Om ctx errorRow result)
    -> fn

-- No path/query, no headers, no body
instance
  BuildClientFn RL.Nil
    RL.Nil
    IsUnit
    Unit
    ()
    ()
    given
    ctx
    errorRow
    result
    (Om ctx errorRow result) where
  buildClientFn _ _ _ _ f = f {} {} unit

-- No path/query, no headers, with body
instance
  BuildClientFn RL.Nil
    RL.Nil
    IsNotUnit
    body
    ()
    ()
    given
    ctx
    errorRow
    result
    (body -> Om ctx errorRow result) where
  buildClientFn _ _ _ _ f b = f {} {} b

-- No path/query, with headers, no body
instance
  BuildClientFn RL.Nil
    (RL.Cons n t tl)
    IsUnit
    Unit
    ()
    h
    given
    ctx
    errorRow
    result
    (Record h -> Om ctx errorRow result) where
  buildClientFn _ _ _ _ f hdrs = f {} hdrs unit

-- No path/query, with headers, with body
instance
  BuildClientFn RL.Nil
    (RL.Cons n t tl)
    IsNotUnit
    body
    ()
    h
    given
    ctx
    errorRow
    result
    (Record h -> body -> Om ctx errorRow result) where
  buildClientFn _ _ _ _ f hdrs b = f {} hdrs b

-- With path/query, no headers, no body
instance
  BuildClientFn (RL.Cons n t tl)
    RL.Nil
    IsUnit
    Unit
    pq
    ()
    given
    ctx
    errorRow
    result
    (Record given -> Om ctx errorRow result) where
  buildClientFn _ _ _ convert f pqr = f (convert pqr) {} unit

-- With path/query, no headers, with body
instance
  BuildClientFn (RL.Cons n t tl)
    RL.Nil
    IsNotUnit
    body
    pq
    ()
    given
    ctx
    errorRow
    result
    (Record given -> body -> Om ctx errorRow result) where
  buildClientFn _ _ _ convert f pqr b = f (convert pqr) {} b

-- With path/query, with headers, no body
instance
  BuildClientFn (RL.Cons n t tl)
    (RL.Cons hn ht htl)
    IsUnit
    Unit
    pq
    h
    given
    ctx
    errorRow
    result
    (Record given -> Record h -> Om ctx errorRow result) where
  buildClientFn _ _ _ convert f pqr hdrs = f (convert pqr) hdrs unit

-- With path/query, with headers, with body
instance
  BuildClientFn (RL.Cons n t tl)
    (RL.Cons hn ht htl)
    IsNotUnit
    body
    pq
    h
    given
    ctx
    errorRow
    result
    (Record given -> Record h -> body -> Om ctx errorRow result) where
  buildClientFn _ _ _ convert f pqr hdrs b = f (convert pqr) hdrs b
