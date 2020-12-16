#if INTERACTIVE
#load "../Common.fsx"
#endif
open AoC


type Mode = |Classes |Your |Nearby

let range (str:string) = 
    let (a,b) = str |> Text.split2 "-"
    a.Trim() |> Parse.int32, b.Trim() |> Parse.int32

type Class = { Key : string; Ranges : (int*int) list } 
type S = {
    Class : Class list
    Your : int list
    Nearby : int list list
}
  with static member empty = { Class=[]; Your = []; Nearby = []  }

let rec parse (s:S) (mode:Mode) lines = 
    match lines with 
    |[] -> { s with Nearby = List.rev s.Nearby }
    |a :: rest -> 
        //printfn "%s" a
        match a with 
        |"your ticket:" -> parse s Mode.Your rest 
        |"nearby tickets:" -> parse s Mode.Nearby rest 
        |"" -> parse s mode rest 
        |a -> 
            match mode with 
            |Mode.Classes -> 
                let (a,b) = a |> Text.split2 ":"
                let (op1,op2) = b |> Text.split2 " or " 
                let a = { Key = a; Ranges = [range op1; range op2] } 
                parse { s with Class = a::s.Class } mode rest 
            |Mode.Nearby -> 
                let n = a |> Text.split "," |> Array.map Parse.int32 |> Array.toList
                parse { s with Nearby = n :: s.Nearby } Mode.Nearby rest 
            |Mode.Your -> 
                let n = a |> Text.split "," |> Array.map Parse.int32 |> Array.toList
                parse { s with Your = List.append n s.Your } Mode.Nearby rest 


let part1 = 
    let s = parse S.empty Mode.Classes (load "Day16/input.txt" |> Array.toList)
    s.Nearby
    |> List.collect id
    |> List.filter (fun x -> s.Class |> List.collect (fun r -> r.Ranges) |> List.exists (fun (l,u) -> x >= l && x <= u) |> not)
    |> List.sum

let rec filter (classes:(int*Class list) list) = 
    let a = classes |> List.filter (fun (i,x) -> x |> List.length = 1)

    if a.Length = classes.Length then 
        classes |> List.map (fun (i,x) -> i, List.head x)
    else
        let a = a |> List.map (fun (i,x) -> (List.head x).Key)

        if a |> List.isEmpty then failwithf "Can not remove anything? %A" classes
        printfn "Can remove %A" a
        let removed = classes |> List.map (fun (i,x) -> i, if x |> List.length = 1 then x else x |> List.filter (fun c -> a |> List.exists (fun (d) -> d = c.Key) |> not))

        filter removed
  

let part2 = 
    let s = parse S.empty Mode.Classes (load "Day16/input.txt" |> Array.toList)
    let classIndex =
        let l = s.Nearby.Head.Length
        let a = 
            [0..l-1] 
            |> List.map 
                (fun i -> 
                    let nrs = s.Nearby |> List.map (fun x -> x |> List.item i)
                    //printfn "%A" nrs
                    let notExistInAnyRule x = 
                        s.Class |> List.exists (fun c -> c.Ranges |> List.exists (fun (l,u) -> x >= l && x <= u))
                        |> not
                    let c = 
                        s.Class 
                        |> List.filter 
                            (fun (k) -> 
                                nrs |> List.forall (fun x -> k.Ranges |> List.exists (fun (l,u) -> x >= l && x <= u) || x |> notExistInAnyRule)
                                )
                    printfn "%A is %A" nrs c
                    i, c
                )
        filter a

    classIndex 
    //|> List.map (fun (i,x) -> i,x.Key)K
    |> List.filter (fun (i,x) -> x.Key.StartsWith("departure"))
    |> List.map (fun (i,c) -> s.Your |> List.item i |> int64)
    |> List.reduce ((*))

    

