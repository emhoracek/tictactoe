import Array as A
import Window
import Graphics.Input as I


main = lift2 scene Window.dimensions gameState

--------------------------------------------------------
-- INPUT

-- this is a signal of cells clicked during which turn
clickedCell : I.Input Cell 
clickedCell = I.input { index = 0, mark = Blank }

--------------------------------------------------------
-- MODEL 

-- the mark is the visual X, O, or nothing in the cell.
data Mark = Blank | X | O
-- a cell is a numbered space with a mark in it.
type Cell = { index : Int,
              mark: Mark }
-- It's always either X's or O's turn or the game isn't
-- being played.
data Turn = XTurn | OTurn | GameOver


-- if it's x's or o's turn, that's the mark that needs
-- made during that turn.
markFromTurn: Turn -> Mark
markFromTurn turn = if | turn == XTurn -> X
                       | turn == OTurn -> O
                       | otherwise  -> Blank

type WinningCombo = [Int]
-- three in a row = a winner
-- there are only 8 winning combinations
winningCombos : [WinningCombo]
winningCombos = [[0, 1, 2], [3, 4, 5], [6, 7, 8], 
                 [0, 3, 6], [1, 4, 7], [2, 5, 8], 
                 [0, 4, 8], [2, 4, 6]]

-- the game is the array of marks and current turn
type Game = { marks: (A.Array Mark), turn: Turn }

-- a game starts with 9 blank cells and x's turn
defaultGame : Game
defaultGame = { marks = (A.initialize 9 <| always Blank), turn = XTurn }
-- initialize 9 creates an array of length 9 and sets them
-- all to "blank"

--Sizes of things
squareSize = 100
(boardWidth, boardHeight) = ( 3 * squareSize, 3 * squareSize)


--------------------------------------------------------
-- UPDATE

-- These check if there are three in a row of any mark

itemSame : a -> Int -> A.Array a -> Bool
itemSame item index array = A.get index array == Just item


itemsSame : A.Array Mark -> Mark -> WinningCombo -> Bool
itemsSame array mark combo = 
        let isSame index = itemSame mark index array
        in  foldl (&&) True <| map isSame combo
            -- if all marks of any combo are same, then win!

checkWon : Game -> Mark -> Bool
checkWon game mark =
      let isSame = itemsSame game.marks mark
          test   = map isSame winningCombos -- list of whether winning
                                            -- combos match
      in  foldl (||) False test -- if any combos match, then won!

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
      let n = click.index
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
             

newGame = stepGame {index = 4, mark = X } defaultGame

gameState : Signal Game
gameState = foldp checkGame defaultGame clickedCell.signal

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
                            |> I.clickable clickedCell.handle { index = n, mark = Blank }

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
          n = c.index
      in if | x == X    -> drawX 
            | x == O    -> drawO  
            | otherwise -> drawBlank n

chopList : [a] -> [[a]]
chopList x = (take 3 x) :: (take 3 <| drop 3 x) :: [take 3 <| drop 6 x]

marksToCells : [Mark] -> [Cell]
marksToCells x = 
        let n = length x
        in if n > 1 then { index = 9 - n, mark = head x } :: marksToCells (tail x)
                    else [ { index = 9 - n, mark = head x } ]


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
            
