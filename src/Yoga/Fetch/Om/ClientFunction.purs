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
  :: RowList Type -> RowList Type -> Type -> Type -> Row Type -> Row Type -> Type -> Type -> Row Type -> Type -> Type -> Constraint
class
  BuildClientFn pathQueryRL headersRL bodyFlag body pathQuery headers arg ctx errorRow result fn
  | pathQueryRL -> pathQuery
  , headersRL -> headers
  , pathQueryRL headersRL bodyFlag body arg ctx errorRow result -> fn where
  buildClientFn
    :: Proxy pathQueryRL
    -> Proxy headersRL
    -> Proxy bodyFlag
    -> (arg -> Record pathQuery)
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
    arg
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
    arg
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
    arg
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
    arg
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
    arg
    ctx
    errorRow
    result
    (arg -> Om ctx errorRow result) where
  buildClientFn _ _ _ convert f pqr = f (convert pqr) {} unit

-- With path/query, no headers, with body
instance
  BuildClientFn (RL.Cons n t tl)
    RL.Nil
    IsNotUnit
    body
    pq
    ()
    arg
    ctx
    errorRow
    result
    (arg -> body -> Om ctx errorRow result) where
  buildClientFn _ _ _ convert f pqr b = f (convert pqr) {} b

-- With path/query, with headers, no body
instance
  BuildClientFn (RL.Cons n t tl)
    (RL.Cons hn ht htl)
    IsUnit
    Unit
    pq
    h
    arg
    ctx
    errorRow
    result
    (arg -> Record h -> Om ctx errorRow result) where
  buildClientFn _ _ _ convert f pqr hdrs = f (convert pqr) hdrs unit

-- With path/query, with headers, with body
instance
  BuildClientFn (RL.Cons n t tl)
    (RL.Cons hn ht htl)
    IsNotUnit
    body
    pq
    h
    arg
    ctx
    errorRow
    result
    (arg -> Record h -> body -> Om ctx errorRow result) where
  buildClientFn _ _ _ convert f pqr hdrs b = f (convert pqr) hdrs b
