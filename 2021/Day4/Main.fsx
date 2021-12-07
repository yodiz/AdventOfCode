#if INTERACTIVE
#load "../Common.fsx"
#else 
module Day4
#endif

open System
open AoC

let folder = __SOURCE_DIRECTORY__ + "\\"


type Board = {
    Number : int array array
    State : bool array array
}

module Board = 
    let checkBingo (board:Board) = 
        //For each row, check if bingo
        let hasRowBingo = 
            board.State
            |> Array.exists(fun x -> x |> Array.forall ((=)true))
        //For each column check if bingo
        let hasColBingo =            
            [|0..board.State.[0].Length - 1|]
            |> Array.exists (fun col -> board.State |> Array.map (fun x -> x.[col]) |> Array.forall ((=)true))

        hasRowBingo || hasColBingo

    let markNumber (board:Board) (num:int) = 
        let newState = 
            board.State
            |> Array.mapi (fun row x -> x |> Array.mapi (fun col isChecked -> isChecked || (board.Number.[row].[col] = num)))
        { board with State = newState}
        
        
    let calcScore (board:Board) (num:int) = 
        let q = 
            (board.Number, board.State)
            ||> Array.fold2
                (fun rs bn bs -> 
                    (bn,bs) ||> Array.fold2 (fun s (num) state -> if state then s  else s + num) rs
                )
                0
        q * num

let parseBoard (str:string) = 
    let nmrs = 
        str 
        |> Text.split "\r\n"
        |> Array.map (Text.split_noempty " " >> Array.map Int32.Parse) 
    {
        Number = nmrs
        State = nmrs |> Array.map (fun x -> x |> Array.map (fun _ -> false))
    }
    

let (nums,boards)  = 
    let inp = loadAll folder "input.txt" |> Text.split "\r\n\r\n"
    let nums = inp.[0] |> Text.split "," |> Array.map Int32.Parse
    let boards = inp.[1..] |> Array.map (parseBoard)
    nums,boards

let part1 = 
    let rec findWinningBoard nums (boards:Board array) = 
        match nums with
        |a::rest -> 
            let newBords = boards |> Array.map (fun x -> Board.markNumber x a)

            let winningBoard = newBords |> Array.tryFind (fun x -> Board.checkBingo x)
            match winningBoard with
            |Some winner -> 
                Board.calcScore winner a
            |None -> 
                findWinningBoard rest newBords
        |_ -> failwithf ""
    
    findWinningBoard (nums |> Array.toList) boards
        


let part2 = 
    let rec findWinningBoard lastWinning nums (boards:Board array) = 
        match nums with
        |a::rest -> 
            let newBords = 
                boards 
                |> Array.map 
                    (fun x -> 
                        if Board.checkBingo x then
                            x
                        else
                            Board.markNumber x a 
                    )
            
            let winningBoard = 
                boards 
                |> Array.tryPick 
                    (fun x -> 
                        if Board.checkBingo x then
                            None
                        else
                            let newB = Board.markNumber x a 
                            if Board.checkBingo newB then
                                Some newB
                            else None
                    )

            match winningBoard with
            |Some winner -> 
                findWinningBoard (Some (winner,a)) rest newBords
            |None -> 
                findWinningBoard lastWinning rest newBords
        |[] -> 
            lastWinning |> Option.get
    
    findWinningBoard None (nums |> Array.toList) boards
    |> (fun (b,n) -> Board.calcScore b n)


    


