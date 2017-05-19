module Interpreter exposing (Memory, BrainfuckProgram, readProgram, runProgram)

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
        _ -> crash "bug: should not goRight in this case"

goLeft : BrainfuckProgram -> BrainfuckProgram
goLeft program =
    case (List.head program.left, List.tail program.left) of
        (Just x, Just ls) ->
            BrainfuckProgram ls x (program.curr :: program.right)
        _ -> crash "bug: should not goLeft in this case"


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

-- command: ]
loopR : Memory -> BrainfuckProgram -> BrainfuckProgram
loopR memory program =
    let jumpBack count program =
        case (count, List.head program.left) of
            (0, Just '[') -> program
            (_, Just '[') -> jumpBack (count - 1) (goLeft program)
            (_, Just ']') -> jumpBack (count + 1) (goLeft program)
            _             -> jumpBack count (goLeft program)
    in if toCode memory.curr /= 0 then jumpBack 0 program else goRight program


-- the only possible syntax errors in a brainfuck program are mismatched braces
-- all characters besides < > + - [ ] . , are ignored as whitespace
-- TODO: disambiguate error cases to point user to error
readProgram : String -> Maybe BrainfuckProgram
readProgram program =
    let countBrackets c = case c of
                              '[' -> \n -> n + 1
                              ']' -> \n -> n - 1
                              _   -> identity
        runningCount = List.scanl countBrackets 0 (String.toList program)
        anyMismatched = List.member -1 runningCount
        bracketCount = List.reverse runningCount |> List.head
    in if anyMismatched || bracketCount /= (Just 0)
       then Nothing
       else case String.uncons program of
                Nothing -> Nothing
                Just (h, t) -> Just (BrainfuckProgram [] h (String.toList t))

-- TODO: how to deal with IO? (not yet implemented)
runProgram : BrainfuckProgram -> Memory -> (BrainfuckProgram, Memory)
runProgram program memory =
    case program.curr of
        '>' -> (goRight program, incrementDataPointer memory)
        '<' -> (goRight program, decrementDataPointer memory)
        '+' -> (goRight program, incrementByte memory)
        '-' -> (goRight program, decrementByte memory)
        '[' -> (loopL memory program, memory)
        ']' -> (loopR memory program, memory)
        _   -> (goRight program, memory)
