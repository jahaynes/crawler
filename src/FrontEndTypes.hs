module FrontEndTypes where

import Fay.Text (Text)

data Element
data Event
data TaskId
data XMLHttpRequest

type Url = Text

newtype Escaped = Escaped Text