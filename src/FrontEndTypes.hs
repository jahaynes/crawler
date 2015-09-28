module FrontEndTypes where

data Element
data Event
data TaskId
data XMLHttpRequest

type Url = String

newtype Escaped = Escaped String