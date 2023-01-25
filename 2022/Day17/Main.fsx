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


let shapes =  [|
                [|0,0; 1,0; 2,0; 3,0|] 
                [|1,0; 0,1; 1,1; 2,1; 1,2|]
                [|2,0; 2,1; 2,2; 1,2; 0,2|]
                [|0,0; 0,1; 0,2; 0,3|]
                [|0,0; 0,1; 1,0; 1,1|]
              |]
              |> Array.map (fun (p) -> p |> Array.map (fun (x,y) -> vec2D x y))
type Piece = { Shape : Vec2D array; Loc : Vec2D; Index : int64} with member x.ShapeIndex = int (x.Index%int64 shapes.Length)

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
    let shapeNN = int (shapeN % int64 shapes.Length)
    let shape = shapes[shapeNN]
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

let height (board:Map<Vec2D, bool>) =
    if board = Map.empty then 0 else
    board |> Map.toArray |> Array.map (fun (x,_) -> x.y) |> Array.min |> abs |> ((+)1)

let clean limit (board:Map<Vec2D, bool>) =
    let h = height board
    board |> Map.fold (fun s key value -> if key.y > 0-h+limit then s else s |> Map.add key value) Map.empty
    

let outline limit (board:Map<Vec2D, bool>) = 
    let board = clean limit board
    let h = height board
    board |> Map.fold (fun s key value -> s |> Map.add (key |> Vec2D.add (vec2D 0 (h-1))) value) Map.empty

let rec tick heights (dHeight:int64) (dBlocks:int64) (n:int64) (stopAt:int64) (currentPiece:Piece) (board:Map<Vec2D, bool>) (moves:Vec2D array) = 
    let board = 
        if currentPiece.Index % 1000L = 0 then
            if currentPiece.Index % 1000L = 0 then printfn "Index: %i, cleaning board" currentPiece.Index
            clean 100 board
        else board
    if currentPiece.Index + dBlocks >= stopAt then
        int64 (height board) + dHeight
    else
        let nMove = int (n % int64 (moves.Length))
        let move = moves[nMove]
        printBoard board currentPiece
        //Movie piece left/right if possible
        let currentPiece = 
            if Piece.canMove move board currentPiece then
                { currentPiece with Loc = currentPiece.Loc |> Vec2D.add move }
            else 
                currentPiece
        printBoard board currentPiece
        let heights, currentPiece, board, dHeight, dBlocks = 
            if Piece.canMove Vec2D.down board currentPiece then
                heights, { currentPiece with Loc = Vec2D.add currentPiece.Loc Vec2D.down }, board,dHeight, dBlocks
            else
                let newBoard = 
                    currentPiece.Shape 
                    |> Array.map (fun p -> Vec2D.add p currentPiece.Loc)
                    |> Array.fold (fun s x -> s |> Map.add x true) board      

                let heights, dHeight, dBlocks = 
                    //moves.Length*shapes.Length
                    if currentPiece.Index > int64(moves.Length*shapes.Length) then
                        let height = height newBoard
                        let key = currentPiece.ShapeIndex,nMove//,outline nMove newBoard
                        let info = {| pieceCount = currentPiece.Index;height = height |}
                        
                        match heights |> Map.tryFind key with
                        |Some (h:{|pieceCount:int64;height:int|}) -> 
                            if dHeight = 0 then
                                let cyclePieces = info.pieceCount - h.pieceCount
                                let cycleHeight = info.height - h.height
                                printfn "Found cycle of %i blocks giving height %i, pieces: %i " cyclePieces cycleHeight currentPiece.Index

                                let cycles = ((stopAt - currentPiece.Index) / cyclePieces)
                                let addedHeight = int64 cycleHeight * cycles
                                let addedBlocks = cycles * int64 cyclePieces
                                printfn "Cycles: %i, addedHeight: %i, addedBlocks: %i" cycles addedHeight addedBlocks
                                
                                heights,addedHeight,addedBlocks
                            else
                                heights,dHeight,dBlocks
                        |None -> 
                            heights |> Map.add key info,dHeight, dBlocks
                    else heights,dHeight, dBlocks
                
                let piece = spawnPiece (currentPiece.Index+1L) newBoard
                heights, piece, newBoard,dHeight, dBlocks
        printBoard board currentPiece
        tick heights dHeight dBlocks (n+1L) stopAt currentPiece board moves


let run filename stopAt =
    let moves = 
        loadAll folder filename
        |> Seq.map (function |'<' -> Vec2D.left; |'>' -> Vec2D.right |_ -> failwithf "")
        |> Seq.toArray
    tick Map.empty 0L 0L 0 stopAt (spawnPiece 0 Map.empty) Map.empty moves
    


//let test1 = run "test1.txt" 2022 
let test2 = run "test1.txt" 1000000000000L

//let part1 = run "input.txt" 2022 
//let part2 = run "input.txt" 1000000000000L
