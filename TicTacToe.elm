import Array as A
import Window
import Graphics.Input as I


main = lift2 scene Window.dimensions gameState

--------------------------------------------------------
-- INPUT

clicky : I.Input Cell 
clicky = I.input { num = 0, mark = Blank }

--------------------------------------------------------
-- MODEL 

data Mark = Blank | X | O
type Cell = { num : Int,
              mark: Mark }
data Turn = XTurn | OTurn | GameOver

markFromTurn: Turn -> Mark
markFromTurn x = if | x == XTurn -> X
                    | x == OTurn -> O
                    | otherwise  -> Blank

type Game = { marks: (A.Array Mark), turn: Turn }

defaultGame : Game
defaultGame = { marks = (A.initialize 9 <| always Blank), turn = XTurn }

--Sizes of things
squareSize = 100
(boardWidth, boardHeight) = ( 3 * squareSize, 3 * squareSize)


--------------------------------------------------------
-- UPDATE

-- These check if there are three in a row of any mark
check : A.Array Mark -> Mark -> Int -> Bool
check x c a = 
        let s = A.get a x
        in if s == Just c then True else False

itemsSame : A.Array Mark -> Mark -> [Int] -> Bool
itemsSame array mark wins = foldl (&&) True (map (check array mark) wins)

checkWon : Game -> Mark -> Bool
checkWon game mark =
      let wins = [[0, 1, 2], [3, 4, 5], [6, 7, 8], [0, 3, 6], [1, 4, 7], 
                 [2, 5, 8], [0, 4, 8], [2, 4, 6]]
      in if foldl (||) False (map (itemsSame game.marks mark) wins) then True 
            else False

checkCat : Game -> Bool
checkCat game = A.foldl (&&) True (A.map ((/=) Blank) game.marks)

-- Stepping 
stepTurn : Game -> Turn
stepTurn game = 
      if | checkWon game X || 
           checkWon game O    -> GameOver
         | game.turn == XTurn -> OTurn
         | otherwise          -> XTurn

stepGame : Cell -> Game -> Game
stepGame click game = 
      let n = click.num
          g = game
          t = game.turn
      in  { marks = A.set n (markFromTurn g.turn) g.marks, turn = stepTurn g }
      
checkGame : Cell -> Game -> Game
checkGame n game =
      let g = stepGame n game in
      if | checkWon g X || 
           checkWon g O ||
           checkCat g   -> {marks = g.marks, turn = GameOver }
         | otherwise    -> g
             

newGame = stepGame {num = 4, mark = X } defaultGame

gameState : Signal Game
gameState = foldp checkGame defaultGame clicky.signal

---------------------------------------------------------
-- VIEW

s = squareSize

--Marks
m = squareSize * (3/5)
drawX       = collage s s [ traced (solid red) (segment (-m/2, -m/2) (m/2,m/2)),
                            traced (solid red) (segment (-m/2, m/2) (m/2,-m/2)) ]
drawO       = collage s s [ outlined (solid blue) (circle ((m+8)/2))]
-- this one is clickable and takes an n so it's clear which blank was clicked
drawBlank n = collage s s [ filled lightGray (square (m+5)) ] 
                            |> I.clickable clicky.handle { num = n, mark = Blank }

-- LINES
lineVert = filled black (rect 5 boardHeight)
lineHorz = rotate (degrees 90) lineVert

lines = collage boardWidth boardHeight [ moveX (-s/2) lineVert,
                                         moveX  (s/2) lineVert,
                                         moveY (-s/2) lineHorz,
                                         moveY  (s/2) lineHorz ]

-- Next part takes an Array of Marks and turns it into a
-- list of Cells. It has to be cells because cells have a number 
-- attached that tells us which cell is clicked. Then we chop the 
-- list into three lists of three so it can be displayed nicely. 
-- Maybe there's a better way to do it?
toElement : Cell -> Element
toElement c = 
      let x = c.mark 
          n = c.num
      in if | x == X    -> drawX 
            | x == O    -> drawO  
            | otherwise -> drawBlank n

chopList : [a] -> [[a]]
chopList x = (take 3 x) :: (take 3 <| drop 3 x) :: [take 3 <| drop 6 x]

marksToCells : [Mark] -> [Cell]
marksToCells x = 
        let n = length x
        in if n > 1 then { num = 9 - n, mark = head x } :: marksToCells (tail x)
                    else [ { num = 9 - n, mark = head x } ]


marksToList : A.Array Mark -> [[Element]]
marksToList x = chopList <| map toElement (marksToCells <| A.toList x )

--The big main elements              
titleBox = collage 300 125 [ filled green (rect 300 50),
                             toForm <| centered <| toText <| "TIC-TAC-TOE" ]

toBoard : Game -> Element
toBoard game = 
        let x = marksToList game.marks
        in layers [
                container boardHeight boardWidth middle
                    (flow down (map (flow right) x)),
                lines ]

statusBox g = 
        let m = if | checkWon g X -> "X won!"
                   | checkWon g O -> "O won!"
                   | checkCat g   -> "CAT!!!!"
                   | otherwise    -> ""
            mainBox color message = 
                     collage 300 125 [ filled color (rect 300 50),
                                       toForm <| centered <| toText <| message ++ m ]
            xBox = mainBox red "X's Turn"
            oBox = mainBox blue "O's Turn"
            gameOverBox color = mainBox color "GAME OVER: "
        in if | g.turn == GameOver -> gameOverBox yellow
              | checkCat g         -> gameOverBox orange
              | g.turn == XTurn    -> xBox
              | otherwise          -> oBox


scene (w,h) game =
  layers [ 
           --flow down [asText (checkCat (testGame)) ],  
           container w h midTop (titleBox),
           container w h middle (toBoard <| game),
           container w h midBottom (statusBox game) ]

--------------------------------------------------------------
-- DEBUG

markToText : Mark -> String
markToText x = if | x == X    -> "X"
                  | x == O    -> "O"
                  | otherwise -> "."
                  
turnToText : Turn -> String
turnToText x = if | x == XTurn    -> "Xs turn -"
                  | x == OTurn    -> "Os turn -"
                  | otherwise -> "Game Over"

gameToText : Game -> String
gameToText game = 
        let c = A.toList game.marks
            listStrings = (turnToText game.turn) :: (map markToText c)
        in join " " listStrings

testGame = { marks = A.fromList [X, O, O, 
                                 O, X, X, 
                                 X, O, O], turn = XTurn }
            
