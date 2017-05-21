module Interpreter exposing (Memory, BrainfuckProgram, readProgram, runProgram)

import Char exposing (fromCode, toCode)
import Debug exposing (crash)
import Dict exposing (Dict)
import Stream exposing (Stream)

-- in general see https://en.wikipedia.org/wiki/Brainfuck

-- memory and programs are both examples of the zipper data structure
-- if unfamiliar see http://learnyouahaskell.com/zippers

-- memory is a tape extending infinitely in both directions
type alias Memory = { left : Stream Char, curr: Char, right : Stream Char }

-- explicit datatype for our commands
type Command =
    MemTransform (Memory -> Memory)
        | ProgramTransformL (Memory -> BrainfuckProgram -> BrainfuckProgram)
        | ProgramTransformR (Memory -> BrainfuckProgram -> BrainfuckProgram)
        -- TODO: add IO

-- programs are finite zippers of commands
type alias BrainfuckProgram = { left : List Command, curr: Command, right : List Command }

-- helper functions
goRight : BrainfuckProgram -> BrainfuckProgram
goRight program =
    case program.right of
        r :: rs ->
            BrainfuckProgram (program.curr :: program.left) r rs
        _ -> crash "bug: should not goRight in this case"

goLeft : BrainfuckProgram -> BrainfuckProgram
goLeft program =
    case program.left of
        l :: ls ->
            BrainfuckProgram ls l (program.curr :: program.right)
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
incrementByte memory = { memory | curr = toCode memory.curr + 1 |> fromCode }

-- command: -
decrementByte : Memory -> Memory
decrementByte memory = { memory | curr = toCode memory.curr - 1 |> fromCode }

-- command: [
loopL : Memory -> BrainfuckProgram -> BrainfuckProgram
loopL memory program =
    let jumpPast count program =
        case (count, List.head program.right) of
            (0, Just (ProgramTransformR _)) -> goRight program |> goRight
            (_, Just (ProgramTransformR _)) -> goRight program |> jumpPast (count - 1)
            (_, Just (ProgramTransformL _)) -> goRight program |> jumpPast (count + 1)
            _                               -> goRight program |> jumpPast count
    in if toCode memory.curr == 0 then jumpPast 0 program else goRight program

-- command: ]
loopR : Memory -> BrainfuckProgram -> BrainfuckProgram
loopR memory program =
    let jumpBack count program =
        case (count, List.head program.left) of
            (0, Just (ProgramTransformL _)) -> program
            (_, Just (ProgramTransformL _)) -> goLeft program |> jumpBack (count - 1)
            (_, Just (ProgramTransformR _)) -> goLeft program |> jumpBack (count + 1)
            _                               -> goLeft program |> jumpBack count
    in if toCode memory.curr /= 0 then jumpBack 0 program else goRight program


charToCommand : Dict Char Command
charToCommand = Dict.fromList [
                 ('>', MemTransform incrementDataPointer),
                 ('<', MemTransform decrementDataPointer),
                 ('+', MemTransform incrementByte),
                 ('-', MemTransform decrementByte),
                 ('[', ProgramTransformL loopL),
                 (']', ProgramTransformR loopR)
                ]


-- the only possible syntax errors in a brainfuck program are mismatched braces
-- all characters besides < > + - [ ] . , are ignored as whitespace
-- TODO: disambiguate error cases to point user to error
readProgram : String -> Maybe BrainfuckProgram
readProgram program =
    let countBrackets c = case c of
                              '[' -> \n -> n + 1
                              ']' -> \n -> n - 1
                              _   -> identity
        runningCount = String.toList program |> List.scanl countBrackets 0
        anyMismatched = List.member -1 runningCount
        bracketCount = List.reverse runningCount |> List.head
    in if anyMismatched || bracketCount /= Just 0
       then Nothing
       else let commands = String.toList program |>
                           List.filterMap (\c -> Dict.get c charToCommand)
            in case commands of
                   []     -> Nothing
                   h :: t -> Just <| BrainfuckProgram [] h (List.append t <| List.singleton (MemTransform identity))

-- TODO: how to deal with IO? (not yet implemented)
interpret : BrainfuckProgram -> Memory -> (BrainfuckProgram, Memory)
interpret program memory =
    case program.curr of
        MemTransform cmd      -> (goRight program, cmd memory)
        ProgramTransformL cmd -> (cmd memory program, memory)
        ProgramTransformR cmd -> (cmd memory program, memory)

runProgram : BrainfuckProgram -> Memory -> Memory
runProgram program memory =
    let (newProgram, newMemory) = interpret program memory
    in if List.isEmpty newProgram.right -- last command is never executed, hence why append dummy noop command above
       then newMemory
       else runProgram newProgram newMemory
