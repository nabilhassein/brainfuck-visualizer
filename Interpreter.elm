module Interpreter exposing (..)

import Stream exposing (Stream)


-- memory is a tape extending infinitely in both directions; this is a zipper
type alias Memory = { left : Stream Char, curr: Char, right : Stream Char }

-- TODO: change return type to deal with I/O here, plus actually implementing
runProgram : String -> Memory -> Memory
runProgram program memory = memory

