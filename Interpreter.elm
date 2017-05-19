module Interpreter exposing (Memory, BrainfuckProgram, runProgram)

import Char exposing (fromCode, toCode)
import Debug exposing (crash)
import Stream exposing (Stream)


-- memory and programs are both examples of the zipper data structure
-- if unfamiliar see http://learnyouahaskell.com/zippers

-- memory is a tape extending infinitely in both directions
type alias Memory = { left : Stream Char, curr: Char, right : Stream Char }

-- programs are finite zippers
type alias BrainfuckProgram = { left : List Char, curr: Char, right : List Char }

goRight : BrainfuckProgram -> BrainfuckProgram
goRight program =
    case (List.head program.right, List.tail program.right) of
        (Just x, Just rs) ->
            BrainfuckProgram (program.curr :: program.left) x rs
        _ -> crash "TODO"


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

-- command: [
loopL : Memory -> BrainfuckProgram -> BrainfuckProgram
loopL memory program =
    let jumpPast count program =
        case (count, List.head program.right) of
            (0, Just ']') -> goRight program |> goRight
            (_, Just ']') -> jumpPast (count - 1) (goRight program)
            (_, Just '[') -> jumpPast (count + 1) (goRight program)
            _             -> jumpPast count (goRight program)
    in if toCode memory.curr == 0 then jumpPast 0 program else goRight program


-- TODO: change return type to deal with I/O here, plus actually implementing
runProgram : String -> Memory -> Memory
runProgram program memory = memory
