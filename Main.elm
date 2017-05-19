import Html exposing (Html, Attribute, div, input, text, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Stream exposing (Stream)


main : Program Never Model Msg
main = Html.beginnerProgram { model = model, view = view, update = update }

-- MODEL

-- memory is a tape extending infinitely in both directions; this is a zipper
type alias Memory = { left : Stream Char, curr: Char, right : Stream Char }

type alias Model = { program : String, memory : Memory }

initialMemory : Memory
initialMemory = Memory (Stream.value '\0') '\0' (Stream.value '\0')

model : Model
model = { program = "", memory = initialMemory }


-- UPDATE

-- TODO: change return type to deal with I/O here, plus actually implementing
runProgram : String -> Memory -> Memory
runProgram program memory = memory

type Msg = LoadProgram String | Run | Clear

update : Msg -> Model -> Model
update msg model = case msg of
    LoadProgram p ->
      { model | program = p }
    Run ->
      { model | memory = runProgram (model.program) (model.memory) }
    Clear ->
      { model | memory = initialMemory }


-- VIEW

view : Model -> Html Msg
view model = div [] [
              input [ placeholder "brainfuck program", onInput LoadProgram ] []
             , button [onClick Run] [text "run program"]
             , button [onClick Clear] [text "clear memory"]                  
             ]
