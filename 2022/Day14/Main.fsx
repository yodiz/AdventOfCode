#if INTERACTIVE
#load "../Common.fsx"
#else
module Day14
#endif

open System
open AoC

let folder = __SOURCE_DIRECTORY__ + "\\"

//Rita upp rutnät enligt input
//Släpp sand från 500,0

//ticka sanden tills den fastnat.
// Om sanden faller under lägsta delen i planen är det slut

let toOne x = x / abs x

//503,4 -> 502,4 -> 502,9 -> 494,9
let parseLine (str:string) = 
    str 
    |> Text.split " -> "
    |> Array.map (fun x -> let (a,b) = x |> Text.split2 ","
                           Parse.int32 a, Parse.int32 b)

    |> Array.windowed 2
    |> Array.map (fun x -> x[0], x[1])
    |> Array.collect 
        (fun ((x1,y1),(x2,y2)) -> 
            //Draw line form p1 -> p2
            let kuken = 
                if x1 = x2 then [|for y = min y1 y2 to max y1 y2 do yield x1,y|]
                else            [|for x = min x1 x2 to max x1 x2 do yield x,y1|]
            kuken
        )
    |> Array.fold (fun s (x,y) -> s |> Map.add (x,y) (true)) Map.empty
    

let rec tick isTwo maxY (field:Map<int*int,bool>) = 
    //Drop sand, add to field and repeat
    let rec dropSand (sx,sy) =
        if isTwo = false && sy >= maxY then
            None
        else
            //Move down, downleft, downright 
            //if cant, place sand and proceed
            //if below lowest piece, stop all
            let d = 
                [sx,sy+1; sx-1,sy+1; sx+1,sy+1]
                |> List.tryPick 
                    (fun (x,y) -> 
                        if isTwo && y = maxY + 2 then 
                            None
                        elif field |> Map.tryFind (x,y) |> Option.isNone then 
                            Some (x,y) 
                        else None
                    )
        
            match d with 
            |Some (x,y) -> 
                dropSand (x,y)
            |None -> 
                Some (sx,sy)

    match dropSand (500,0) with 
    |Some (x,y) when (x,y) = (500,0) -> (field |> Map.add (x,y) (false))
    |Some (x,y) -> 
        tick isTwo maxY (field |> Map.add (x,y) (false))
    |None -> 
        field



let run isTwo filename = 
    let orgField =
        load folder filename 
        |> Array.map parseLine
        |> Array.fold 
            (fun s x -> 
                let a = s |> Map.toArray 
                let b = x |> Map.toArray
                Array.append a b |> Map.ofArray
            ) 
            Map.empty

    let maxY = 
        orgField |> Map.fold (fun s (x,y) value -> max y s) 0

    let field = tick isTwo maxY orgField
    field |> Map.fold (fun i (x,y) v -> if v then i else i + 1) 0


let test1 = run false "test1.txt"
let test2 = run true "test1.txt"

let part1 = run false "input.txt"
let part2 = run true "input.txt"



printfn "%i" part1
printfn "%i" part2

