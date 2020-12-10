#if INTERACTIVE
#load "../Common.fsx"
#endif
open AoC


type I = |Nop of int |Acc of int |Jmp of int
let parse (s:string) = 
    s 
    |> Text.split2 " "
    |> function |"acc", Int32 a -> I.Acc a
                |"jmp", Int32 a -> I.Jmp a
                |"nop", Int32 a -> I.Nop a
                |a,b -> failwithf "UNKN: %s" a 

let rec run state (src:_ array) acc loc preExec = 
    preExec state acc loc
    |> function 
        |state, None -> 
            src.[loc]
            |> function 
                |I.Nop _ -> run state src acc (loc+1) preExec
                |I.Acc a -> run state src (acc+a) (loc+1) preExec
                |I.Jmp d -> run state src acc (loc+d) preExec
        |state, Some a -> a 
    

let part1 = 
    let src = 
        load "Day8/input.txt"
        |> Array.map parse
    run Set.empty src 0 0 
        (fun s acc l -> 
            printfn "Running LoC %i" l
            if s |> Set.contains l then s, Some acc else s |> Set.add l, None
        )


let part2 = 
    let src = 
        load "Day8/input.txt"
        |> Array.map parse

    src 
    |> Array.fold
        (fun (s,i) _x -> 
            let newSrc = 
                src 
                |> Array.mapi (fun ii xx -> if ii = i then
                                                match xx with
                                                |I.Jmp a -> I.Nop a
                                                |I.Nop a -> I.Jmp a
                                                |a -> a
                                            else xx)
            run Set.empty newSrc 0 0 
                (fun s acc l -> 
                    if l >= src.Length then s, Some (Some acc) else
                    if s |> Set.contains l then s, Some None else s |> Set.add l, None
                )
            |> function
                 |Some x -> (Some x,i+1)
                 |None -> (s,i+1)
               

        )
        (None, 0)
    |> fst
