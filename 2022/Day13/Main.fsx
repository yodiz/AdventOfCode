#if INTERACTIVE
#load "../Common.fsx"
#else
module Day13
#endif

open System
open AoC

let folder = __SOURCE_DIRECTORY__ + "\\"

type Signal = |Value of int |Values of Signal list

let signalParser = 
    let signal, signalRef = Parser.forwardedToRef<Signal> ()
    let things = 
        let list = (Parser.sepby (Parser.char ',') signal |> Parser.map (fun x -> Signal.Values (x |> Array.toList)))
        (Parser.choice2
            (Parser.int32 |> Parser.map (fun n -> Signal.Value n))
            (Parser.pipe3 (Parser.char '[') list (Parser.char ']') |> Parser.map (fun (a,b,c) -> b))
        )
    signalRef.Value <- things
    signal

let load filename = 
    loadAll folder filename |> Text.split "\r\n\r\n"
    |> Array.map (fun pair -> pair |> Text.split2 "\r\n")
    |> Array.map (fun (l,r) -> Parser.runOrFail l signalParser, Parser.runOrFail r signalParser)

let rec cmpOne (l:Signal) (r:Signal) = 
    match l,r with 
    |Signal.Value l, Signal.Value r -> 
        if l < r then Some true
        elif l > r then Some false
        else None
    |Signal.Values l, Signal.Values r ->         
        cmpMulti l r
    |Signal.Value v, rv -> cmpOne (Signal.Values [Signal.Value v])  rv
    |lv, Signal.Value r -> cmpOne lv (Signal.Values [Signal.Value r])
and cmpMulti (left:Signal list) (right:Signal list) = 
    match left, right with 
    |[], [] -> None
    |[], _ -> Some true
    |_, [] -> Some false
    |l::lr,r::rr -> 
        let res = cmpOne l r
        match res with 
        |None -> cmpMulti lr rr
        |s -> s

let run filename = 
    load filename
    |> Array.fold 
        (fun (i,s) (l,r) ->
            i+1,
            match cmpOne l r with 
            |None -> failwithf "Wat??"
            |Some true -> s + i
            |Some false -> s
        )
        (1,0)
    |> snd


let run2 filename =     
    let keya = Signal.Values [Signal.Value 2]
    let keyb = Signal.Values [Signal.Value 6]
    let sorted = 
        load filename
        |> Array.collect (fun (a,b) -> [|a;b|])
        |> Array.append [| keya;keyb |]
        |> Array.sortWith (fun a b -> match cmpOne a b with |None -> failwithf "" |Some true -> 1 |Some false -> -1)
        |> Array.rev
        //|> Array.sort
    
    let a = sorted |> Array.findIndex ((=)keya) 
    let b = sorted |> Array.findIndex ((=)keyb) 
    (a+1) * (b+1)

let test1 = run "test1.txt"
let test2 = run2 "test1.txt"
let part1 = run "input.txt"
let part2 = run2 "input.txt"

printfn "%i" part1
printfn "%i" part2