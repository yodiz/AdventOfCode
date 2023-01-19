#if INTERACTIVE
#load "../Common.fsx"
#else
module DayT
#endif

open System
open AoC

type Vec2D = { x : int; y: int }
module Vec2D = 
    let create x y = { x = x; y = y }
    let manhattanDistance (v1:Vec2D) (v2:Vec2D) = abs(v1.x - v2.x) + abs(v1.y - v2.y)
    let apply (v1:Vec2D) (v2:Vec2D) fn = { x = fn v1.x v2.x; y = fn v1.y v2.y }
    let add v1 v2 = apply v1 v2 (+)
    let multiply v1 v2 = apply v1 v2 (*)
    let subtract v1 v2 = apply v1 v2 (-)

    let left = create -1 0
    let right = create 1 0
    let up = create 0 -1
    let down = create 0 1
let vec2D x y = { x = x; y = y }

type Piece = { Shape : Vec2D array; Loc : Vec2D; Index : int }

let shapes =  [|
                [|0,0; 1,0; 2,0; 3,0|] 
                [|1,0; 0,1; 1,1; 2,1; 1,2|]
                [|2,0; 2,1; 2,2; 1,2; 0,2|]
                [|0,0; 0,1; 0,2; 0,3|]
                [|0,0; 0,1; 1,0; 1,1|]
              |]
              |> Array.map (fun (p) -> p |> Array.map (fun (x,y) -> vec2D x y))

let folder = __SOURCE_DIRECTORY__ + "\\"

let printBoard (board:Map<Vec2D, bool>) (piece:Piece) = 
    ()
    //let toy = board |> Map.fold (fun s k v -> min s k.y) 0
    //for y = toy-5 to 0 do
    //    printf "#"
    //    for x = 0 to 6 do
    //        let isPiece = 
    //            piece.Shape |> Array.exists (fun p -> let l = Vec2D.add piece.Loc p
    //                                                  l.x = x && l.y = y)

    //        match isPiece, board |> Map.tryFind (vec2D x y) with
    //        |true, _ -> printf "@"
    //        |_, Some _ -> printf "#"
    //        |_, _ -> printf "."
    //    printfn "#"
    //printfn "#########"
    //printfn ""


//The tall, vertical chamber is exactly seven units wide. Each rock appears so that its left edge 
//is two units away from the left wall and its bottom edge is three units above the highest rock 
//in the room (or the floor, if there isn't one).
let spawnPiece shapeN (board:Map<Vec2D, bool>) = 
    let shape = shapes[shapeN%shapes.Length]
    let sx = shape |> Array.map (fun p -> p.x) |> Array.min
    let sy = shape |> Array.map (fun p -> p.y) |> Array.max

    let locX = 2 
    let locY = 
        board 
        |> Map.toArray 
        |> Array.map (fun (x,_) -> x.y)
        |> Array.fold (fun s y -> min s y) 1
        |> (fun y -> y - 3 - sy - 1) 
    //printfn "  Spawning %i at %i,%i" shapeN locX locY
    { Shape = shape; Loc = vec2D locX locY; Index = shapeN }

module Piece = 
    let canMove (l:Vec2D) board (p:Piece) = 
        p.Shape 
        |> Array.map (fun x -> Vec2D.add x p.Loc |> Vec2D.add l) 
        |> Array.forall 
            (fun x -> 
                board 
                |> Map.tryFind x 
                |> function |Some s -> false |_ -> true
                && x.x >= 0 && x.x <= 6 && x.y <= 0 
            )
let rec tick (n:int) (stopAt:int) (currentPiece:Piece) (board:Map<Vec2D, bool>) (moves:Vec2D array) = 
    //printfn "Tick %i, currentPiece %i" n currentPiece.Index
    if currentPiece.Index >= stopAt then
        board |> Map.toArray |> Array.map (fun (x,_) -> x.y) |> Array.min
    else
        let move = moves[n%moves.Length]
        printBoard board currentPiece
        //Movie piece left/right if possible
        let currentPiece = 
            if Piece.canMove move board currentPiece then
                { currentPiece with Loc = currentPiece.Loc |> Vec2D.add move }
            else 
                currentPiece
        printBoard board currentPiece
        let nInc, currentPiece, board = 
            if Piece.canMove Vec2D.down board currentPiece then
                1, { currentPiece with Loc = Vec2D.add currentPiece.Loc Vec2D.down }, board
            else
                let newBoard = 
                    currentPiece.Shape 
                    |> Array.map (fun p -> Vec2D.add p currentPiece.Loc)
                    |> Array.fold (fun s x -> s |> Map.add x true) board            
                let piece = spawnPiece (currentPiece.Index+1) newBoard
                1, piece, newBoard
        printBoard board currentPiece
        tick (n+nInc) stopAt currentPiece board moves


let run filename stopAt =
    let moves = 
        loadAll folder filename
        |> Seq.map (function |'<' -> Vec2D.left; |'>' -> Vec2D.right |_ -> failwithf "")
        |> Seq.toArray
    //printfn "Moves length %i" moves.Length
    tick 0 stopAt (spawnPiece 0 Map.empty) Map.empty moves
    |> abs |> ((+)1)
let test1 = run "test1.txt" 2022 
//3068
//let test2 = run "test1.txt" 1000000000000

let part1 = run "input.txt" 2022 

let part2 = 0


//
printfn "%i" part1
printfn "%i" part2

