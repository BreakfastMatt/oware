module Oware

type StartingPosition =
    | South
    | North


type Player = { //playerOne = South, playerTwo = North
    houses: int*int*int*int*int*int
    score: int
}

type Board = {
    playerOne: Player //ReadyPlayerOne
    playerTwo: Player
    currentTurn: StartingPosition //South or North
    }


let getSeeds n board = 
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
    |_  -> failwith "Invalid choice of house"

let theChosenHouse n (a,b,c,d,e,f,a',b',c',d',e',f') = function
    |1  -> (0,b,c,d,e,f,a',b',c',d',e',f') 
    |2  -> (a,0,c,d,e,f,a',b',c',d',e',f') 
    |3  -> (a,b,0,d,e,f,a',b',c',d',e',f') 
    |4  -> (a,b,c,0,e,f,a',b',c',d',e',f')
    |5  -> (a,b,c,d,0,f,a',b',c',d',e',f') 
    |6  -> (a,b,c,d,e,0,a',b',c',d',e',f') 
    |7  -> (a,b,c,d,e,f,0,b',c',d',e',f') 
    |8  -> (a,b,c,d,e,f,a',0,c',d',e',f') 
    |9  -> (a,b,c,d,e,f,a',b',0,d',e',f') 
    |10 -> (a,b,c,d,e,f,a',b',c',0,e',f') 
    |11 -> (a,b,c,d,e,f,a',b',c',d',0,f') 
    |12 -> (a,b,c,d,e,f,a',b',c',d',e',0) 
    |_  -> failwith "Someone done did a fuck up"

let incrementHouseSeed n (a,b,c,d,e,f,a',b',c',d',e',f') = 
    let n = match n with
            |13 -> 1
            |_ -> n
    match n with 
    |1 -> (a+1,b,c,d,e,f,a',b',c',d',e',f')
    |2 -> (a,b+1,c,d,e,f,a',b',c',d',e',f')
    |3 -> (a,b,c+1,d,e,f,a',b',c',d',e',f')
    |4 -> (a,b,c,d+1,e,f,a',b',c',d',e',f')
    |5 -> (a,b,c,d,e+1,f,a',b',c',d',e',f')
    |6 -> (a,b,c,d,e,f+1,a',b',c',d',e',f')
    |7 -> (a,b,c,d,e,f,a'+1,b',c',d',e',f')
    |8 -> (a,b,c,d,e,f,a',b'+1,c',d',e',f')
    |9 -> (a,b,c,d,e,f,a',b',c'+1,d',e',f')
    |10 -> (a,b,c,d,e,f,a',b',c',d'+1,e',f')
    |11 -> (a,b,c,d,e,f,a',b',c',d',e'+1,f')
    |12 -> (a,b,c,d,e,f,a',b',c',d',e',f'+1)
    |_ -> failwith "Sad cause bad"
    
let useHouse n board =
    let (a,b,c,d,e,f),(a',b',c',d',e',f') = board.playerOne.houses,board.playerTwo.houses 
    let updatedHouses = (a,b,c,d,e,f,a',b',c',d',e',f') 
    let updatedHouses = theChosenHouse n updatedHouses

    let numSeeds = getSeeds n board
    //Implement recursive function to distribute seeds from selected house to other houses
    failwith "{{{{{{{{{{Not finished yet}}}}}}}}}}"

let start position =
    let h = (4,4,4,4,4,4)
    let pl1 = {houses = h ; score = 0}
    let pl2 = {houses = h ; score = 0}
    {playerOne = pl1; playerTwo = pl2; currentTurn = position}
    
let score board = 
    let southScore,northScore = board.playerOne.score , board.playerTwo.score 
    southScore,northScore


let gameState board = 
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
  

[<EntryPoint>]
let main _ =
    printfn "Hello from F#!"
    0 // return an integer exit code
