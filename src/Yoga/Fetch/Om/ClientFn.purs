module Yoga.Fetch.Om.ClientFn
  ( class BuildClientFn
  , buildClientFn
  , class CheckBodyIsUnit
  , class CheckRowEmpty
  , IsUnit
  , IsNotUnit
  , IsEmpty
  , IsNonEmpty
  ) where

import Data.Unit (Unit, unit)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)
import Yoga.Om (Om)

data IsUnit
data IsNotUnit
data IsEmpty
data IsNonEmpty

class CheckBodyIsUnit :: Type -> Type -> Constraint
class CheckBodyIsUnit body flag | body -> flag

instance CheckBodyIsUnit Unit IsUnit
else instance CheckBodyIsUnit body IsNotUnit

class CheckRowEmpty :: Row Type -> Type -> Constraint
class CheckRowEmpty row flag | row -> flag

instance RowToList row RL.Nil => CheckRowEmpty row IsEmpty
else instance CheckRowEmpty row IsNonEmpty

class BuildClientFn
  :: RowList Type -> Type -> Type -> Type -> Row Type -> Row Type -> Row Type -> Type -> Type -> Constraint
class
  BuildClientFn pathQueryRL headersFlag bodyFlag body pathQuery headers errorRow result fn
  | pathQueryRL headersFlag bodyFlag body pathQuery headers errorRow result -> fn where
  buildClientFn
    :: Proxy pathQueryRL
    -> Proxy headersFlag
    -> Proxy bodyFlag
    -> (Record pathQuery -> Record headers -> body -> Om {} errorRow result)
    -> fn

instance
  BuildClientFn RL.Nil
    IsEmpty
    IsUnit
    Unit
    pq
    h
    errorRow
    result
    (Om {} errorRow result) where
  buildClientFn _ _ _ f = f (unsafeCoerce {}) (unsafeCoerce {}) unit

instance
  BuildClientFn RL.Nil
    IsEmpty
    IsNotUnit
    body
    pq
    h
    errorRow
    result
    (body -> Om {} errorRow result) where
  buildClientFn _ _ _ f = \b -> f (unsafeCoerce {}) (unsafeCoerce {}) b

instance
  BuildClientFn RL.Nil
    IsNonEmpty
    IsUnit
    Unit
    pq
    h
    errorRow
    result
    (Record h -> Om {} errorRow result) where
  buildClientFn _ _ _ f = \hdrs -> f (unsafeCoerce {}) hdrs unit

instance
  BuildClientFn RL.Nil
    IsNonEmpty
    IsNotUnit
    body
    pq
    h
    errorRow
    result
    (Record h -> body -> Om {} errorRow result) where
  buildClientFn _ _ _ f = \hdrs b -> f (unsafeCoerce {}) hdrs b

instance
  BuildClientFn (RL.Cons n t tl)
    IsEmpty
    IsUnit
    Unit
    pq
    h
    errorRow
    result
    (Record pq -> Om {} errorRow result) where
  buildClientFn _ _ _ f = \pqr -> f pqr (unsafeCoerce {}) unit

instance
  BuildClientFn (RL.Cons n t tl)
    IsEmpty
    IsNotUnit
    body
    pq
    h
    errorRow
    result
    (Record pq -> body -> Om {} errorRow result) where
  buildClientFn _ _ _ f = \pqr b -> f pqr (unsafeCoerce {}) b

instance
  BuildClientFn (RL.Cons n t tl)
    IsNonEmpty
    IsUnit
    Unit
    pq
    h
    errorRow
    result
    (Record pq -> Record h -> Om {} errorRow result) where
  buildClientFn _ _ _ f = \pqr hdrs -> f pqr hdrs unit

instance
  BuildClientFn (RL.Cons n t tl)
    IsNonEmpty
    IsNotUnit
    body
    pq
    h
    errorRow
    result
    (Record pq -> Record h -> body -> Om {} errorRow result) where
  buildClientFn _ _ _ f = f
