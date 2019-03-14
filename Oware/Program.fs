module Oware

type StartingPosition =
    | South
    | North

let getSeeds n board = failwith "Not implemented"

let useHouse n board = failwith "Not implemented"

let start position = failwith "Not implemented"

let score board = failwith "Not implemented"

let gameState board = 
   let a,b,c,d,e,f,g,h,i,j,k,l = board.Houses
   let x,y = board.Scores 
   match x > (y + (a + b + c + d + e + f + g + h + i + j + k + l))  with 
   |true -> "South wins"
   |false -> 
        match x > (y + (a + b + c + d + e + f + g + h + i + j + k + l))  with 
        |true -> "South wins"
        |false -> 
   match board.CurrentTurn with
   |South -> "South's turn"
   |North -> "North's turn"
   
   (*match board with 
   |(0,0,0,0,0,0,0,0,0,0,0,0),_,_,_ ->  "score board" 
   |_,_,_,"South's turn" -> "North's turn"
   |_,_,_, "North's turn" -> "South's turn"
   //failwith "Not implemented"*)

[<EntryPoint>]
let main _ =
    printfn "Hello from F#!"
    0 // return an integer exit code
