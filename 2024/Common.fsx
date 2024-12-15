[<AutoOpen>]
module AoC

let folder = __SOURCE_DIRECTORY__
let load folder file = System.IO.File.ReadAllLines(System.IO.Path.Combine(folder,file))
let loadAll folder file = System.IO.File.ReadAllText(System.IO.Path.Combine(folder,file))

module Parse = 
    let parse fn (input:string) = match fn input with |true,value -> Some value |_ ->  None
    let parseOrFail what fn input = parse fn input |> function |Some s -> s |None -> failwithf "'%s' not a %s" input what
    let tryInt32 = parse System.Int32.TryParse
    let int32 = parseOrFail "int32" System.Int32.TryParse
    let tryInt64 = parse System.Int64.TryParse
    let int64 = parseOrFail "int64" System.Int64.TryParse

let (|Int32|_|) = Parse.tryInt32
let (|Int64|_|) = Parse.tryInt64
   
module Array = 
    let foldi<'state,'a> folder (s:'state) (arr:'a array) =
        arr|> Array.fold (fun (i,s) x -> (i+1),folder i s x) (0,s) |> snd
module List = 
    let foldi<'state,'a> folder (s:'state) (arr:'a list) =
        arr|> List.fold (fun (i,s) x -> (i+1),folder i s x) (0,s) |> snd

module Time = 
    let func fn a = 
        let sw = System.Diagnostics.Stopwatch.StartNew()
        let r = fn a
        sw.Stop()
        printfn "Timing Function, took %ims" sw.ElapsedMilliseconds
        r

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

    let split_noempty (sepBy:string) (str:string)  = splitm_noempty [|sepBy|] str 
    let split (sepBy:string) (str:string)  = splitm [|sepBy|] str 

    let split2 sepBy string = 
        match split sepBy string with 
        |[|a;b|] -> a,b
        |a -> failwithf "Expected two elements in '%s' Sep By '%s', but got %A"  string sepBy a

    let split2_noempty sepBy string = 
        match split_noempty sepBy string with 
        |[|a;b|] -> a,b
        |a -> failwithf "Expected two elements in '%s' Sep By '%s', but got %A"  string sepBy a


    let trim (s:string) = s.Trim()
    let trimc (c:char array) (s:string) = s.Trim(c)

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

type Pos = {x: int64;y:int64}
module Pos = 
    let create x y = { x = x; y = y }
    let sub (a:Pos) (b:Pos) = { x = a.x - b.x; y = a.y - b.y }
    let add (a:Pos) (b:Pos) = { x = a.x + b.x; y = a.y + b.y }

    module Dir = 
        let North = create 0 -1
        let South = create 0 1
        let West = create -1 0
        let East = create 1 0

        let turnRight = 
            function 
            |n when n = North -> East
            |n when n = South -> West
            |n when n = West -> North
            |n when n = East -> South
            |a -> failwithf "Cna not turn right for %A" a

    type Map = { Width : int; Height : int; Map : Map<Pos, char> }
    module Map = 
        let parse empty (input:string array) = 
            let map = 
                input 
                |> Array.mapi (fun y l -> l.ToCharArray() |> Array.mapi (fun x c -> match c with |q when q = empty -> None |c -> Some ((create x y), c)))
                |> Array.collect id
                |> Array.choose id
                |> Map.ofArray
            { Width = input[0].Length; Height = input.Length; Map = map }
        let print (map:Map) = 
            for y = 0 to map.Height - 1 do
                for x = 0 to map.Width - 1 do
                    let q = map.Map |> Map.tryFind (create x y) |> Option.defaultValue '.'
                    printf "%c" q
                printfn ""
        
        let rotate (map:Map) = 
            let m = map.Map |> Map.toList |> List.map (fun (k, p) -> create k.y k.x, p) |> Map.ofList
            { map with Map = m; Width = map.Height; Height = map.Width }

module Expect = 
    type ExpectResult<'a> = |Success of int*'a |Failure of string
    type Expect<'a> = string -> int -> ExpectResult<'a>

    let private mapResult<'a,'b> (mapper:'a -> 'b) = 
        function |Success (index,v) -> Success (index, mapper v) |Failure s -> Failure s

    let map<'a,'b> (mapper:'a -> 'b) (e:Expect<'a>) : Expect<'b> = 
        (fun src index -> e src index |> mapResult mapper)

    let zip (a:Expect<'a>) (b:Expect<'b>) : Expect<'a*'b> = 
        (fun src index -> 
            match a src index with 
            |Success (newIndex,value) -> b src newIndex |> mapResult (fun b -> value, b)
            |Failure str -> Failure str
        )        

    let private checkOob (src:string) index =
        if index >= src.Length then Failure "<EOL>" 
        else
            Success (index, ())

    let eChar c : Expect<char> = 
        zip 
            checkOob
            (fun src index -> 
                if src[index] = c then Success (index+1, c)
                else Failure ($"Expected '%c{c}', got '%c{src[index]}'")
            )
        |> map snd

    let eSatisfy fn : Expect<char> = 
        zip 
            checkOob
            (fun src index -> 
                if fn src[index] then Success (index+1, src[index])
                else Failure ($"Expected 'satisy cond', got '%c{src[index]}'")
            )
        |> map snd

    let eChain (e:Expect<'a> array) : Expect<'a array> = 
        (fun src index -> 
            let mutable ok = true
            let mutable i = 0
            let mutable index = index
            let mutable r = Failure ""
            let arr = ResizeArray()
            while ok && i < e.Length do
                let e = e[i]
                match e src index with 
                |Success (newIndex, v) -> 
                    index <- newIndex
                    i <- i + 1
                    arr.Add v
                    r <- Success (newIndex, v)
                |Failure f -> 
                    r <- Failure f
                    ok <- false
            r |> mapResult (fun x -> arr.ToArray())
        )

    let eString (str:string) : Expect<string> = 
        str.ToCharArray() |> Array.map (fun x -> eChar x) |> eChain |> map (System.String)

    let eWhile fn : Expect<char array> = 
        (fun src index -> 
            let mutable ok = true
            let mutable index = index
            let arr = ResizeArray()
            while ok do
                match eSatisfy fn src index with
                |Success (newIndex,v) -> 
                    index <- newIndex
                    arr.Add(v)
                |Failure err -> 
                    ok <- false
            Success (index, arr.ToArray())
        )

    //TODO  - kastar exception vid tom sträng.
    let eInt : Expect<int> = 
        eWhile (System.Char.IsNumber) 
        |> map (System.String >> Parse.int32)

    let eEither<'a> (a:Expect<'a>) (b:Expect<'a>) : Expect<'a> = 
        (fun src index -> 
            match a src index with 
            |Success (index,v) -> Success (index,v)
            |Failure str -> 
                match b src index with 
                |Success (index,v) -> Success (index,v)
                |Failure str2 -> Failure (str + " or " + str2)
        )

    let (.>) (a:Expect<'a>) (b:Expect<'b>) = zip a b |> map fst
    let (>.) (a:Expect<'a>) (b:Expect<'b>) = zip a b |> map snd
    let (.>.) (a:Expect<'a>) (b:Expect<'b>) = zip a b 

    let run str index e = 
        e str index
    let runOpt str index e = 
        run str index e |> function Success (_,v) -> Some v |Failure s -> None
