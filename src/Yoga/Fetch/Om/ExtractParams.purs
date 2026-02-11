module Yoga.Fetch.Om.ExtractParams
  ( class ExtractRequestHeaders
  , class ExtractRequestBody
  , class FindBodyType
  , class UnwrapEncoding
  , class FindHeaders
  ) where

import Data.Unit (Unit)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Yoga.HTTP.API.Route.Encoding (JSON, FormData, NoBody, PlainText)
import Yoga.HTTP.API.Route.Handler (Request)

class ExtractRequestBody :: Type -> Type -> Constraint
class ExtractRequestBody request body | request -> body

instance
  ( RowToList row rl
  , FindBodyType rl encoding
  , UnwrapEncoding encoding body
  ) =>
  ExtractRequestBody (Record row) body
else instance
  ExtractRequestBody (Record row) body =>
  ExtractRequestBody (Request (Record row)) body

class ExtractRequestHeaders :: Type -> Row Type -> Constraint
class ExtractRequestHeaders request headers | request -> headers

instance
  ( RowToList row rl
  , FindHeaders rl headers
  ) =>
  ExtractRequestHeaders (Record row) headers
else instance
  ExtractRequestHeaders (Record row) headers =>
  ExtractRequestHeaders (Request (Record row)) headers

class FindBodyType :: RowList Type -> Type -> Constraint
class FindBodyType rl encoding | rl -> encoding

instance FindBodyType RL.Nil NoBody
instance FindBodyType (RL.Cons "body" encoding tail) encoding
else instance FindBodyType tail encoding => FindBodyType (RL.Cons label ty tail) encoding

class UnwrapEncoding :: Type -> Type -> Constraint
class UnwrapEncoding encoding body | encoding -> body

instance UnwrapEncoding (JSON ty) ty
instance UnwrapEncoding (FormData ty) ty
instance UnwrapEncoding NoBody Unit
instance UnwrapEncoding PlainText String

class FindHeaders :: RowList Type -> Row Type -> Constraint
class FindHeaders rl headers | rl -> headers

instance FindHeaders RL.Nil ()
instance FindHeaders (RL.Cons "headers" (Record h) tail) h
else instance FindHeaders tail headers => FindHeaders (RL.Cons label ty tail) headers
