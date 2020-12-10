#if INTERACTIVE
#load "../Common.fsx"
#endif
open AoC

let input = load "Day2/input.txt"
let parseLine (inp:string) = 
    let [Int32 min; Int32 max; letter; str] = Regexp.matchOrFail "(\d+)-(\d+) (\w): (\w+)" inp    
    {| Min = min;Max = max; Letter = letter.ToCharArray().[0]; Str = str |}

let occour letter (str:string) =
    str.ToCharArray()
    |> Array.filter ((=)letter)
    |> Array.length

let part1 = 
    input 
    |> Array.map parseLine
    |> Array.filter 
        (fun x -> 
            match occour x.Letter x.Str with
            |a when a >= x.Min && a <= x.Max -> true
            |_ -> false
        ) 
    |> Array.length

let part2 = 
    input 
    |> Array.map parseLine
    |> Array.filter 
        (fun x -> 
            let a = x.Str.Chars (x.Min - 1) = x.Letter 
            let b = x.Str.Chars (x.Max - 1) = x.Letter
            a <> b && (a = true || b = true)
        ) 
    |> Array.length


[<EntryPoint>]
let main argv =
    printfn "Part 1 %i" part1
    printfn "Part 2 %i" part2
    0