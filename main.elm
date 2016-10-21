import Html exposing (div, button, text, br, span)
import Html.App exposing (beginnerProgram)
import Html.Events exposing (onClick)
import Array

main =
  beginnerProgram { model = model, view = view, update = update }

type alias Model = List Int

initialModel : Model
initialModel = [1, 2, 3, 4, 5, 6, 7, 8, -1]

model : Model
model = initialModel

tileValue : Int -> String
tileValue tileNum =
  if tileNum == -1 then
    "-"
  else
    toString tileNum

renderTile : (Int, Int) -> Html.Html Msg
renderTile (index, tileNum) =
  button [ onClick (MoveTile index) ] [ text (tileValue tileNum) ]

renderTiles : List Int -> Int -> Int -> Html.Html Msg
renderTiles tiles start end =
  let
    tileItems =
      tiles
        |> Array.fromList
        |> Array.slice start end
        |> Array.toList
        |> List.indexedMap (,)
        |> List.map (\(index, val) -> (index + start, val))
        |> List.map renderTile
  in
    div [] tileItems

view : Model -> Html.Html Msg
view model =
  div [] [
    div [] [
      button [ onClick Shuffle ] [ text "Shuffle" ],
      button [ onClick Reset ] [ text "Reset" ]
    ],
    div [] [ renderTiles model 0 3 , renderTiles model 3 6 , renderTiles model 6 9 ]
  ]

type Direction = Left | Right | Up | Down

emptyTileAtIndex : List Int -> Int -> Bool
emptyTileAtIndex tiles index =
  let
    valAtIndex = Array.get index (Array.fromList tiles)
  in
    case valAtIndex of
      Just val -> val == -1
      Nothing -> False

canMove : Direction -> List Int -> Int -> Bool
canMove direction tiles index =
  let
    rowSize =
      model
        |> List.length
        |> toFloat
        |> sqrt
        |> floor
  in
    case direction of
      Left ->
        let
          tileNotInLeftColumn = (index % rowSize /= 0)
          tileToLeftEmpty = (emptyTileAtIndex tiles (index - 1))
        in
          tileNotInLeftColumn && tileToLeftEmpty
      Right ->
        let
          tileNotInRightColumn = (((index + 1) % rowSize) /= 0)
          tileToRightEmpty = (emptyTileAtIndex tiles (index + 1))
        in
          tileNotInRightColumn && tileToRightEmpty
      Down ->
        let
          tileNotInBottomRow = (index > (rowSize - 1))
          tileBelowEmpty = (emptyTileAtIndex tiles (index - rowSize))
        in
          tileNotInBottomRow && tileBelowEmpty
      Up ->
        let
          tileNotInTopRow = (index < ((List.length tiles) - rowSize))
          tileAboveEmpty = (emptyTileAtIndex tiles (index + rowSize))
        in
          tileNotInTopRow && tileAboveEmpty

swap : List Int -> Int -> Int -> List Int
swap list oldIndex newIndex =
  let
    oldIndexVal = (Array.get oldIndex (Array.fromList list))
    newIndexVal = (Array.get newIndex (Array.fromList list))
  in
    case oldIndexVal of
      Just oldVal ->
        case newIndexVal of
          Just newVal ->
            List.map (\val -> (if val == oldVal then newVal else if val == newVal then oldVal else val)) list
          Nothing -> list
      Nothing -> list

type Msg = MoveTile Int | Shuffle | Reset

update : Msg -> List Int -> List Int
update msg model =
  case msg of
    MoveTile index ->
      let
        rowSize =
          model
            |> List.length
            |> toFloat
            |> sqrt
            |> floor
      in
        if (canMove Left model index) then
          swap model index (index - 1)
        else if (canMove Right model index) then
          swap model index (index + 1)
        else if (canMove Down model index) then
          swap model index (index - rowSize)
        else if (canMove Up model index) then
          swap model index (index + rowSize)
        else
          model

    Shuffle -> model

    Reset -> initialModel
