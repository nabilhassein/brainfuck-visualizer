module Interpreter exposing (..)

import Char exposing (fromCode, toCode)
import Stream exposing (Stream)
import Debug exposing (crash)


-- memory is a tape extending infinitely in both directions; this is a zipper
type alias Memory = { left : Stream Char, curr: Char, right : Stream Char }


-- commands in the brainfuck language

-- command: >
incrementDataPointer : Memory -> Memory
incrementDataPointer memory =
    case Stream.next memory.right of
        (rs, Just x) -> Memory (Stream.concat
                                    (Stream.singleton memory.curr)
                                    memory.left
                               ) x rs
        (_, Nothing) -> crash "impossible: memory is infinite"

-- command: <
decrementDataPointer : Memory -> Memory
decrementDataPointer memory =
    case Stream.next memory.left of
        (ls, Just x) -> Memory ls x (Stream.concat
                                         (Stream.singleton memory.curr)
                                         memory.right
                                    )
        (_, Nothing) -> crash "impossible: memory is infinite"

-- command: +
incrementByte : Memory -> Memory
incrementByte memory = { memory | curr = (fromCode (toCode memory.curr + 1)) }

-- command: -
decrementByte : Memory -> Memory
decrementByte memory = { memory | curr = (fromCode (toCode memory.curr - 1)) }

-- TODO: change return type to deal with I/O here, plus actually implementing
runProgram : String -> Memory -> Memory
runProgram program memory = memory

