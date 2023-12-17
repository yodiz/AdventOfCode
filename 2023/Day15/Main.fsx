#if INTERACTIVE
#load "../Common.fsx"
#else 
module Day
#endif

let folder = __SOURCE_DIRECTORY__ + "\\"
let input  = loadAll folder "input.txt"
let inptest1  = loadAll folder "test1.txt"
let inptest2  = loadAll folder "test2.txt"

let hash (str:string) = 
    str.ToCharArray() 
    |> Array.fold
         (fun currentValue x -> 
            let c = currentValue + int(char x)
            let c = c * 17
            c % 256
         ) 
         0

// hash "rn"
// hash "cm"

let parse (input:string) =
    let words = Text.split "," input
    words


let solve1 input = 
    parse input
    |> Array.map hash
    |> Array.sum

type Op = |Set of string*int |Remove of string
    
let solve2 input =  
    parse input
    |> Array.map 
        (fun x -> 
            if x.EndsWith("-") then
                Remove (x.Substring(0,x.Length-1))
            else
                let (a,b) = Text.split2 "=" x
                Set (a, Parse.int32 b)
        )
    |> Array.fold 
        (fun (s:array<List<_*_>>) x -> 
            match x with
            |Op.Set (h, v) -> 
                let index = hash h
                if s[index] |> List.exists (fun (lh,_v) -> lh = h) then
                    s[index] <- s[index] |> List.map (fun (lh,lv) -> if lh = h then (lh,v) else lh,lv)
                else
                    s[index] <- List.append s[index] [h,v]                
                s
            |Op.Remove (h) -> 
                let index = hash h
                s[index] <- s[index] |> List.filter (fun (lh,v:int) -> if lh = h then false else true)
                s
        )
        ([|0..255|] |> Array.map (fun _ -> []))
    |> Array.mapi 
        (fun boxNr lenses -> 
            let boxNr = boxNr + 1  
            lenses 
            |> List.mapi 
                (fun lensNr (h,v) -> 
                    let lensNr = lensNr + 1
                    boxNr * lensNr * v
                )
        )
    |> Seq.collect id
    |> Seq.sum
    

let test1 = solve1 inptest1 
let test2 = solve2 inptest1

let part1 = solve1 input

//466493 - To high
let part2 = solve2 input 
