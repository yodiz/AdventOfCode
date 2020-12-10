module AoC


let folder = __SOURCE_DIRECTORY__ + "\\"
let load file = System.IO.File.ReadAllLines(System.IO.Path.Combine(folder,file))
let loadAll file = System.IO.File.ReadAllText(System.IO.Path.Combine(folder,file))

module Parse = 
    let parse fn (input:string) = match fn input with |true,value -> Some value |_ ->  None
    let parseOrFail what fn input = parse fn input |> function |Some s -> s |None -> failwithf "%s not a %s" input what
    let tryInt32 = parse System.Int32.TryParse
    let int32 = parseOrFail "int32" System.Int32.TryParse
    let tryInt64 = parse System.Int64.TryParse
    let int64 = parseOrFail "int64" System.Int64.TryParse

let (|Int32|_|) = Parse.tryInt32
let (|Int64|_|) = Parse.tryInt64
   
module Array = 
    let foldi<'state,'a> folder (s:'state) (arr:'a array) =
        arr|> Array.fold (fun (i,s) x -> (i+1),folder i s x) (0,s) |> snd

module Map = 
    ///Change existing value or use default value 
    let changeOrDefault key change def map = 
        map |> Map.add key (map |> Map.tryFind key |> function |Some s -> change s |None -> def)

module Test = 
    let equal<'a when 'a : equality> msg (expected:'a) (actual:'a) =
        if expected = actual then
            printfn "Success - %s" msg
        else failwithf "Fail - %s expected %A, got %A" msg expected actual
        

module Trace = 
    let mutable private traceEnable = false
    let trace_on() = traceEnable <- true  
    let trace_off() = traceEnable <- false  
    let output str = if traceEnable then printfn "%s" str else ()
    let withTrace fn = 
        trace_on() 
        fn() 
        trace_off()


module Text = 
    let splitm_noempty (sepBy:string array) (str:string)  =
       str.Split(sepBy, System.StringSplitOptions.RemoveEmptyEntries)
    let splitm (sepBy:string array) (str:string)  =
       str.Split(sepBy, System.StringSplitOptions.None)

    let split (sepBy:string) (str:string)  = splitm [|sepBy|] str 

    let split2 sepBy string = 
        match split sepBy string with 
        |[|a;b|] -> a,b
        |a -> failwithf "Expected two elements in '%s' Sep By '%s', but got %A"  string sepBy a

    let trim (s:string) = s.Trim()

module Regexp = 
    open System.Text.RegularExpressions

    ///Pattern - Groups
    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let matchOrFail pattern input = 
        match input with
        |Regex pattern groups -> groups
        |_ -> failwithf "Match %s failed on %s " pattern input
