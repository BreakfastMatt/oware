module Oware

type StartingPosition =
    | South
    | North


type Player = { //playerOne = South, playerTwo = North
    houses: int*int*int*int*int*int
    score: int
    numPieces: int //Determines how many pieces are currently on their board
}

type Board = {
    playerOne: Player //ReadyPlayerOne
    playerTwo: Player
    currentTurn: StartingPosition //South or North
    }


let getSeeds n board = 
    //Will return the number of seeds in house n.
    let (a,b,c,d,e,f),(a',b',c',d',e',f') = board.playerOne.houses, board.playerTwo.houses
    match n with 
    |1 -> a
    |2 -> b
    |3 -> c
    |4 -> d
    |5 -> e
    |6 -> f
    |7 -> a'
    |8 -> b'    
    |9 -> c'
    |10 -> d'
    |11 -> e'
    |12 -> f'
    |_  -> failwith "{getSeeds} Invalid choice of house"

let theChosenHouse n board = 
    //Will take the chosen house and set its own seed count to zero, 
    //which indicates that the player has taken their turn and selected house n.
    let (a,b,c,d,e,f) = board.playerOne.houses
    let (a',b',c',d',e',f') = board.playerTwo.houses
    match n with
    |1  -> {board with playerOne = {board.playerOne with houses = (0,b,c,d,e,f)} }
    |2  -> {board with playerOne = {board.playerOne with houses = (a,0,c,d,e,f)} }
    |3  -> {board with playerOne = {board.playerOne with houses = (a,b,0,d,e,f)} } 
    |4  -> {board with playerOne = {board.playerOne with houses = (a,b,c,0,e,f)} }
    |5  -> {board with playerOne = {board.playerOne with houses = (a,b,c,d,0,f)} } 
    |6  -> {board with playerOne = {board.playerOne with houses = (a,b,c,d,e,0)} } 
    |7  -> {board with playerTwo = {board.playerTwo with houses = (0,b',c',d',e',f')} }
    |8  -> {board with playerTwo = {board.playerTwo with houses = (a',0,c',d',e',f')} }
    |9  -> {board with playerTwo = {board.playerTwo with houses = (a',b',0,d',e',f')} }
    |10 -> {board with playerTwo = {board.playerTwo with houses = (a',b',c',0,e',f')} } 
    |11 -> {board with playerTwo = {board.playerTwo with houses = (a',b',c',d',0,f')} }
    |12 -> {board with playerTwo = {board.playerTwo with houses = (a',b',c',d',e',0)} }
    |_  -> failwith "{theChosenHouse} Something went wrong, house was not in the range of 1 and 12 (inclusive)."

let incrementHouseSeed n (a,b,c,d,e,f,a',b',c',d',e',f') = 
    //This function will be used to increment the number of seeds in other houses.
    match n with 
    //South Houses
    |1 -> (a+1,b,c,d,e,f,a',b',c',d',e',f')
    |2 -> (a,b+1,c,d,e,f,a',b',c',d',e',f')
    |3 -> (a,b,c+1,d,e,f,a',b',c',d',e',f')
    |4 -> (a,b,c,d+1,e,f,a',b',c',d',e',f')
    |5 -> (a,b,c,d,e+1,f,a',b',c',d',e',f')
    |6 -> (a,b,c,d,e,f+1,a',b',c',d',e',f')
    //North Houses 
    |7 -> (a,b,c,d,e,f,a'+1,b',c',d',e',f')
    |8 -> (a,b,c,d,e,f,a',b'+1,c',d',e',f')
    |9 -> (a,b,c,d,e,f,a',b',c'+1,d',e',f')
    |10 -> (a,b,c,d,e,f,a',b',c',d'+1,e',f')
    |11 -> (a,b,c,d,e,f,a',b',c',d',e'+1,f')
    |12 -> (a,b,c,d,e,f,a',b',c',d',e',f'+1)
    |_ -> failwith "{incrementHouseSeed} There aren't any houses that do not lie within the range of 1 and 12 (inclusive)."

let score board = 
    //Merely returns a tuple containing the South Score and North Score.
    let southScore,northScore = board.playerOne.score , board.playerTwo.score 
    southScore,northScore  

let gameState board = 
    //Will return the games current state
   let x,y = score board
   match x > 24 with 
   |true -> "South won"
   |false -> 
        match y > 24  with 
        |true -> "North won"
        |false -> 
            match x = 24 && y = 24 with 
            |true ->  "Game ended in a draw"
            |false ->  
                match board.currentTurn with  
                |South -> "South's turn"
                |North -> "North's turn"

let incrementScore turn board= 
  
  let rec countSeedsS n board= 
      match n  with 
      |12 -> board
      |_ -> match getSeeds n board, board.playerOne.score = 24 && board.playerTwo.score = 24  with
            |2,false -> countSeedsS  (n+1) {board with playerOne = {board.playerOne with score = board.playerOne.score + 2; houses = (theChosenHouse n board).playerOne.houses }}
            |3,false-> countSeedsS  (n+1) {board with playerOne = {board.playerOne with score = board.playerOne.score + 3; houses = (theChosenHouse n board).playerOne.houses }}
            |_,false-> countSeedsS  (n+1) board
            |_,_ -> board

  let rec countSeedsN n board= 
      match n  with 
      |7 -> board
      |_ -> match getSeeds n board, board.playerTwo.score = 24 && board.playerOne.score >= 24 with
            |2,false -> countSeedsN  (n+1) {board with playerTwo = {board.playerTwo with score = board.playerTwo.score + 2; houses = (theChosenHouse n board).playerTwo.houses }}
            |3,false -> countSeedsN  (n+1) {board with playerTwo = {board.playerTwo with score = board.playerTwo.score + 3; houses = (theChosenHouse n board).playerTwo.houses }}
            |_,false -> countSeedsN  (n+1) board
            |_,_ -> board
  match turn with 
  |South ->  countSeedsS  7 board 
  |North ->  countSeedsN  1 board 
  
let nextPlayersTurn position = 
    //Simple function that is used to alternate player turns.
    match position with
    | South -> North //this means that South (player one) just had their turn and now it is North's (player two's) turn.
    | North -> South //this means that North (player two) just had their turn and now it is South's (player one's) turn

let checkIfOwnHouse n position = 
    //This function will be used in conjunction with a match to disallow the player to manipulate their opponent's houses
    match position with
    |South -> match n with 
              |7|8|9|10|11|12 -> false
              |_ -> true
    |North -> match n with 
              |1|2|3|4|5|6 -> false
              |_ -> true

let useHouse n board = 
    //Player cannot manipulate their opponent's houses
    match (checkIfOwnHouse n board.currentTurn) with
    |false -> board
    |_ ->
    //Player cannot select an invalid house
    match getSeeds n board with
    |0 -> board //return the board as is (ie the person did not select a valid house)
    |_ -> 
    let (a,b,c,d,e,f),(a',b',c',d',e',f') = (theChosenHouse n board).playerOne.houses,(theChosenHouse n board).playerTwo.houses
    let updatedHouses = (a,b,c,d,e,f,a',b',c',d',e',f') 
    let numSeeds = getSeeds n board

    //Recursive function to distribute seeds from selected house to other houses
    let rec distributeSeeds n count updatedHouses ogN = //n = house to distribute to next, count = number of seeds remaining.
        let n = match n with //To make a loop:  if n = 13, bind n to 1.  (therefore have a circular loop of 1-12)
                | 13 -> 1 
                | _ -> n
        match  count > 0 with
        |true -> match n = ogN with //To Skip the original house
                 |false -> distributeSeeds (n+1) (count-1) (incrementHouseSeed n updatedHouses) ogN
                 |_ -> distributeSeeds (n+1) count updatedHouses ogN
        |_ -> updatedHouses
    let (a,b,c,d,e,f,a',b',c',d',e',f') =  distributeSeeds (n+1) numSeeds updatedHouses n
    //Updates the board after seed distribution
    let pl1 = {board.playerOne with houses = (a,b,c,d,e,f); score = board.playerOne.score} 
    let pl2 = {board.playerTwo with houses = (a',b',c',d',e',f'); score = board.playerTwo.score}
    let board = {board with playerOne = pl1; playerTwo = pl2; currentTurn = board.currentTurn}

    //To Update the scores
    let scoreboard = incrementScore board.currentTurn board
    let pl1 = {board.playerOne with houses = scoreboard.playerOne.houses; score = scoreboard.playerOne.score}
    let pl2 = {board.playerTwo with houses = scoreboard.playerTwo.houses; score = scoreboard.playerTwo.score}
    //To alternate player turn
    let turn = nextPlayersTurn board.currentTurn  

    //Returns board with updated score and turn (i.e. changes that occurred after player x made their move)
    {board with playerOne = pl1; playerTwo = pl2; currentTurn = turn}

let start position = 
    //Initialises the board
    let h = (4,4,4,4,4,4)
    //All houses (South & North) must be initialised to have 4 seeds each.
    let pl1 = {houses = h ; score = 0; numPieces = 24}
    let pl2 = {houses = h ; score = 0;numPieces = 24}
    {playerOne = pl1; playerTwo = pl2; currentTurn = position} 

[<EntryPoint>]
let main _ =
    printfn "Hello from F#!"
    0 // return an integer exit code
