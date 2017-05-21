import Html exposing (Html, Attribute, div, input, text, button, br)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Stream exposing (Stream)

import Interpreter exposing (Memory, BrainfuckProgram, readProgram, runProgram)


main : Program Never Model Msg
main = Html.beginnerProgram { model = model, view = view, update = update }

-- MODEL
type alias Model = { maybeCode : Maybe BrainfuckProgram
                   , memory : Memory
                   , rawProgram : String
                   }

model : Model
model = { maybeCode = Nothing, memory = initialMemory, rawProgram = "" }

-- by default, memory has the null byte in each of its infinite cells
initialMemory : Memory
initialMemory = Memory (Stream.value '0') '0' (Stream.value '0')


-- UPDATE
type Msg = Load String | Run | Clear

update : Msg -> Model -> Model
update msg model = case msg of
    Load p ->
      { model | maybeCode = readProgram p, rawProgram = p }
    Run ->
        case model.maybeCode of
            Nothing -> model
            Just code -> { model | memory = runProgram code model.memory }
    Clear ->
      { model | memory = initialMemory }


-- VIEW
preview : Int -> Memory -> String
preview n memory =
    let f chars = Stream.limit n chars |> Stream.toList |> List.intersperse '|' |> String.fromList
    in String.concat([f memory.left, "|", String.fromChar memory.curr, "|", f memory.right])


view : Model -> Html Msg
view model = div [] [
              input [ placeholder "brainfuck program", onInput Load ] []
             , button
                  [onClick Run
                  , disabled <| case model.maybeCode of
                                    Nothing -> True
                                    Just _  -> False
                  ]
                  [text <| case model.maybeCode of
                               Nothing -> "invalid program :("
                               Just _  -> "run this program!!"]
             , button [onClick Clear] [text "clear memory"]
             , br [] []
             , div [] [text (preview 10 model.memory)]
             ]
