module Emesch where

import Text.Parsec
import Text.Parsec.Char

symbol :: Char.Parser
symbol = (Char.oneOf "~!@#$%^&*:`';-=,./|")
