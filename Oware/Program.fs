module Oware

type StartingPosition =
    | South
    | North


type Board = {
    houses: int*int*int*int*int*int*int*int*int*int*int*int //(A,B,C,D,E,F,a,b,c,d,e,f) [cf charOf function]
    scores: int*int //(South Score, North Score)
    currentTurn: StartingPosition //South or North
    }


let getSeeds n board = 
    let (a,b,c,d,e,f,a',b',c',d',e',f') = board.houses
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

let useHouse n board = failwith "Not implemented b"

let start position =    
    let h = (4,4,4,4,4,4,4,4,4,4,4,4)
    let s = (0,0) 
    {houses = h; scores = s; currentTurn = position}
    
let score board = failwith "Not implemented c"

let gameState board = failwith "Not implemented d"

[<EntryPoint>]
let main _ =
    printfn "Hello from F#!"
    0 // return an integer exit code
