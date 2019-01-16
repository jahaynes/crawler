module FormTypes where

import Types

import Network.HTTP.Types               (Method)

data Form =
    Form CanonicalUrl Action [FormParameters] deriving Show

data Action =
    Action Method RelativeUrl deriving Show

data CombinedFormActions =
    CombinedFormActions Label FormParameters

data ApplicableSuppliedFormActions =
    ApplicableSuppliedFormActions Label FormParameters deriving Show
