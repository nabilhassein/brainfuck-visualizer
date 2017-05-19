import Html exposing (Html, Attribute, div, input, text, button, br)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Stream exposing (Stream)

import Interpreter exposing (Memory, BrainfuckProgram, runProgram)


main : Program Never Model Msg
main = Html.beginnerProgram { model = model, view = view, update = update }

-- MODEL
type alias Model = { program : String, memory : Memory }

initialMemory : Memory
initialMemory = Memory (Stream.value '\0') '\0' (Stream.value '\0')

model : Model
model = { program = "", memory = initialMemory }


-- UPDATE
type Msg = Load String | Run | Clear

update : Msg -> Model -> Model
update msg model = case msg of
    Load p ->
      { model | program = p }
    Run ->
      { model | memory = runProgram (model.program) (model.memory) }
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
             , button [onClick Run] [text "run program"]
             , button [onClick Clear] [text "clear memory"]
             , br [] []
             , div [] [text (preview 10 model.memory)]
             ]
