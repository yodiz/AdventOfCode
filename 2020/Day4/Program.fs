#if INTERACTIVE
#load "../Common.fsx"
#endif
open AoC


let getPassports (inp:string) = 
    Text.split "\r\n\r\n" inp
    |> Array.map 
        (fun x -> 
            let entries = Text.splitm_noempty [|" "; "\r\n"|] x
            entries 
            |> Array.map (fun pair -> Text.split2 ":" pair)
            |> Map.ofArray
        )

let part1 =     
    let req = ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"]
    loadAll "Day4/input.txt" 
    |> getPassports
    |> Array.filter (fun x -> req |> List.forall (fun r -> x |> Map.containsKey r))
    |> Array.length    

let part2 = 
    let req = [
        "byr", (function |Int32 a when a >= 1920 && a <= 2002 -> true |_ -> false)
        "iyr", (function |Int32 a when a >= 2010 && a <= 2020 -> true |_ -> false)
        "eyr", (function |Int32 a when a >= 2020 && a <= 2030 -> true |_ -> false)
        "hgt", (fun x -> 
                    if x.EndsWith("cm") then 
                        match x.Substring(0,x.Length-2) with
                        |Int32 a when a >= 150 && a <= 193 -> true |_-> false 
                    elif x.EndsWith("in") then 
                        match x.Substring(0,x.Length-2) with
                        |Int32 a when a >= 59 && a <= 76 -> true |_ -> false
                    else false
                )
        "hcl", (fun x -> x.StartsWith("#") && x.Length = 7)
        "ecl", (fun x -> "amb blu brn gry grn hzl oth" |> Text.split " " |> Array.contains x)
        "pid", (fun x -> x.Length = 9 && x.ToCharArray() |> Array.forall System.Char.IsDigit)
    ]
    loadAll "Day4/input.txt" 
    |> getPassports
    |> Array.filter (fun x -> req |> List.forall (fun (key,validate) -> match x |> Map.tryFind key with |None -> false |Some v -> validate v))
    |> Array.length    

printfn "part1 %i" part1
printfn "part2 %i" part2