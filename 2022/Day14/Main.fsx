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
    |> Array.fold (fun s (x,y) -> s |> Map.add (x,y) (Some true)) Map.empty
    

let rec tick (field:Map<int*int,bool option>) = 
    //Drop sand, add to field and repeat
    let rec dropSand (sx,sy) =
        //Move down, downleft, downright 
        //if cant, place sand and proceed
        //if below lowest piece, stop all
        let d = 
            [sx,sy+1; sx-1,sy+1; sx+1,sy+1]
            |> List.tryPick (fun (x,y) -> field |> Map.tryFind (x,y))
            
        match d with 
        |None -> 

        
    dropSand (500,0)



let run1 filename = 
    load folder filename 
    |> Array.map parseLine

let test1 = run1 "test1.txt"

let part1 = 0

let part2 = 0


//
printfn "%i" part1
printfn "%i" part2

