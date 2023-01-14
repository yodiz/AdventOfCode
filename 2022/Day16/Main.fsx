#if INTERACTIVE
#load "../Common.fsx"
#else
module Day16
#endif

open System
open AoC

let folder = __SOURCE_DIRECTORY__ + "\\"

type Way = {
    Id : string
    Pressure : int
    Targets : string array
    IsOpen : bool
}

//Valve JJ has flow rate=21; tunnel leads to valve II
module Parse = 
    open Parser

    let chars n = many' (fun x -> if x = n then (fun s -> parseError s "" |> Failure) else any) |> map (fun x -> System.String x)
    let pOptional<'a> p : Parser<'a option> = (fun s -> p s |> function |Success (s,t) -> Success (s, Some t) |Failure err -> Success (s,None)) 

    let parseLine (str:string) = 
        let ident = chars 2 
        let p = 
            string "Valve " >>. ident .>> string " has flow rate=" .>>. int32 
            .>> string "; tunnel" .>> pOptional (char 's') .>> string " lead" .>> pOptional (char 's')
            .>> string " to valve" .>> pOptional (char 's') .>> string " " 
            .>>. sepby (char ',' .>> (char ' ')) ident
            |> map (fun ((source,p), (ts))  -> source, { Id = source; Pressure = p; Targets = ts; IsOpen = false })
            
        Parser.runOrFail str p
type Action = Walk|Open


let run filename  =  
    let mutable calculated = Map.empty
    //Optimering, memoizeringen returnerar värden för x steg fram. Då 
    let mutable solutions = 0
    let rec follow (ways:Map<string,Way>) (action:Action) timeLeft (location:Way) (sinceLastOpen) =
        match calculated |> Map.tryFind (ways, action, timeLeft, location) with
        |Some x -> x
        |None ->
            let v = 
                let isAllOpen = ways |> Map.fold (fun s x v -> s && (v.IsOpen || v.Pressure = 0)) true
                let rpress = ways |> Map.fold (fun s x v -> s + if v.IsOpen then v.Pressure else 0) 0 
                if isAllOpen then
                    rpress * timeLeft
                else
                    //printfn "Walking to '%s' @  time left %i, released %i" location.Id timeLeft rpress
                    if timeLeft = 1 then 
                        if solutions % 1000 = 0 then
                            printfn "Found %i solutions" solutions
                        solutions <- solutions + 1
                        rpress
                    else
                        match action with 
                        |Walk -> 
                            location.Targets
                            |> Seq.map (fun t -> ways |> Map.find t)
                            |> Seq.collect 
                                (fun t -> 
                                    [
                                        if not t.IsOpen && t.Id <> "AA" && t.Pressure > 0 then
                                            yield follow ways Action.Open (timeLeft-1) (t) Set.empty

                                        if sinceLastOpen |> Set.contains t |> not then
                                            yield follow ways Action.Walk (timeLeft-1) (t) (sinceLastOpen |> Set.add t)
                                    ]
                                )
                            |> Seq.fold (fun s x -> max s x) 0
                            |> (fun x -> x + rpress)
                        |Open -> 
                            let newWays = ways |> Map.add location.Id { location with IsOpen = true }
                            (follow newWays Action.Walk (timeLeft-1) (location) Set.empty) + rpress
            calculated <- calculated |> Map.add (ways, action, timeLeft, location) v
            v
    let sw = System.Diagnostics.Stopwatch.StartNew()   
    let r = 
        let ways = load folder filename |> Array.map Parse.parseLine |> Map.ofArray
        let start = ways |> Map.find "AA"
        follow ways Action.Walk 30 start Set.empty
    printfn "Found result in %i solutions efter %ims" solutions sw.ElapsedMilliseconds
    r



//Följa alla vägar och returnera totalt tryck på slutet(?)!

//test ~300ms optimerad
//Found result in 134 solutions efter 321ms
//let test1 = run "test1.txt"
//let part1 = run "input.txt"
//Found result in 118240 solutions efter 356353ms

let part2 = 0


//
//printfn "%i" part1
//printfn "%i" part2

