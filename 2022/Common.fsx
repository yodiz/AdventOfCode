module AoC



let load folder file = System.IO.File.ReadAllLines(System.IO.Path.Combine(folder,file))
let loadAll folder file = System.IO.File.ReadAllText(System.IO.Path.Combine(folder,file))

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
module List = 
    let foldi<'state,'a> folder (s:'state) (arr:'a list) =
        arr|> List.fold (fun (i,s) x -> (i+1),folder i s x) (0,s) |> snd


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

let memoize fn = 
    let m = System.Collections.Generic.Dictionary()
    (fun k -> 
        match m.TryGetValue(k) with
        |true,v -> v
        |false,_ -> 
            let v = fn k
            m[k] <- v
            v
    )

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

    let trim (s:string) = s.Trim()
    let trimc (c:char array) (s:string) = s.Trim(c)

module Perf = 
    let time name arg fn = 
        let sw = System.Diagnostics.Stopwatch.StartNew()
        let () = fn arg
        sw.Stop()
        printfn "[Perf]: '%s' in %ims" name sw.ElapsedMilliseconds
        
    let timen n name arg fn = 
        let sw = System.Diagnostics.Stopwatch.StartNew()
        [|0..n-1|] 
        |> Array.iter (fun _ -> fn arg)
        sw.Stop()
        printfn "[Perf]: '%s' %i times in %ims" name n sw.ElapsedMilliseconds
        ()



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



module Parser = 
    type ParseState = { String : string; Index : int }
    type ParseError = {
        State: ParseState
        ExpectedError : string
    }
    let parseError s exp = { State = s; ExpectedError = exp }
    type ParseResult<'t> = |Success of ParseState*'t |Failure of ParseError
    type Parser<'t> = ParseState -> ParseResult<'t>
    let run<'t> src p : ParseResult<'t> = p { String = src; Index = 0 }
    let current_str s = 
        //Expected: <Int32> at line xx column yy
        //...part of parsed source...
        //           ^
        let partString = s.String[s.Index..(s.Index+40)]
        let partString = if partString.Length < 40 then (partString) else partString + "..."
        if s.Index = 0 then partString else "..."+partString
    let runOrFail<'t> src p = 
        run<'t> src p 
        |> function |Success (_,v) -> v 
                    |Failure (err) ->
                        let got = current_str err.State
                        failwithf "Expected %s got %s" err.ExpectedError got
    let current s = s.String[s.Index]
    let err_expect expected s = parseError s expected |> Failure
        //Failure (sprintf "Expected %s, got %s" str got)
    let ok_adv s v = Success ({ s with Index = s.Index + 1 },v)
    let is_eos = (fun s -> s.Index >= s.String.Length)
    let eos = (fun s -> if is_eos s then (Success (s, ())) else err_expect "<eos>" s)
    let any = (fun s -> if is_eos s then err_expect "<any>" s else ok_adv s (current s))
    let many' (p:int -> Parser<'t>) (i:ParseState) =
        let mutable current_state = i
        let mutable ok = true
        let mutable n = 0
        [|
            while ok do
                match p n current_state with 
                |Failure _ -> ok <- false
                |Success (s,v) -> n <- n + 1
                                  current_state <- s
                                  yield v
        |]
        |> (fun v -> Success (current_state, v))
    let many (p:Parser<'t>) = many' (fun _ -> p)
    let many1' (p:int -> Parser<'t>) = 
        (fun s -> 
            let news = many' (fun x -> p x) s
            news
            |> function 
                |Success (s,v) -> 
                    if v.Length = 0 then err_expect "Expeceted at least one, got zero" s
                    else Success (s,v) 
                |Failure str -> Failure str
        )
    let many1 (p:Parser<'t>) = many1' (fun _ -> p)
    let satisfy fn : Parser<char> = 
        (fun s -> 
            if is_eos s then 
                err_expect "<satisfy c>" s
            else
                let v = s.String[s.Index]
                if fn v then Success ({s with Index = s.Index + 1 }, v)
                else err_expect "<satisfy c>" s
        )
    let map<'a,'b> (fn:'a -> 'b) (p:Parser<'a>) : Parser<'b> = 
        (fun s -> p s |> function |Success (s,v) -> Success (s, (fn v)) |Failure f -> Failure f)
    let bind (fn:'a -> Parser<'b>) p : Parser<'b> = 
            (fun s -> p s |> function |Success (s,v) -> fn v s |Failure f -> Failure f)
    let nString = 
        many1' ((fun i -> satisfy (fun x -> System.Char.IsNumber x || (i = 0 && (x = '-' || x = '+')))))
        |> map (fun (x:char array) -> System.String(x))
    let string (str:string) : Parser<string> = 
        (fun s -> 
            if s.String.Length >= s.Index + str.Length then
                let got = s.String.Substring(s.Index, str.Length)
                if str.Equals(got) then
                    ParseResult.Success ({ s with Index = s.Index + str.Length }, str)
                else
                    err_expect ("\"" + str + "\"") s
            else
                err_expect ("\"" + str + "\"") s
        )
    let explain<'a> experror (p:Parser<'a>) = 
        (fun s -> p s |> function Success (s,v) -> Success (s,v) |Failure f -> Failure { f with ExpectedError = experror })
    let int32 = nString |> map (fun x -> System.Int32.Parse x) |> explain "<int32>"
    let char c = satisfy ((=)c) |> explain (sprintf "'%c'" c)
    let pipe2 a b = a |> bind (fun a ->  b |> map (fun b -> a,b))
    let pipe3 a b c = pipe2 a b |> bind (fun (a,b) -> c |> map (fun (c) -> a,b,c))
    let sepby<'a,'b> (separator:Parser<'a>) (p:Parser<'b>) = 
        many' (function 0 -> p |_ -> pipe2 separator p |> map (fun (a:'a,b:'b) -> b))
    let choice (p:Parser<'a> seq) : Parser<'a> = 
        (fun s -> 
            p 
            |> Seq.tryPick (fun x -> x s |> function |Failure _ -> None |Success (s,v) -> Some (Success (s,v)))
            |> function Some s -> s |None -> err_expect "<choice of xxx>" s)
            
    let choice2 (p:Parser<'a>) (p2:Parser<'a>) : Parser<'a> = choice [p;p2]
    let choice3 p p2 p3 : Parser<'a> = choice [p;p2;p3]

    let forwardedToRef<'a> () = 
        let p : Parser<'a> ref = ref (fun s -> failwithf "Reference not set")
        (fun s -> p.Value s), p

    let (>>.) a b = pipe2 a b |> map (fun (a,b) -> b)
    let (.>>) a b = pipe2 a b |> map (fun (a,b) -> a)
    let (.>>.) a b = pipe2 a b |> map (fun (a,b) -> a,b)
