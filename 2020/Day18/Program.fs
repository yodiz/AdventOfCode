#if INTERACTIVE
#load "../Common.fsx"
#endif
open AoC


type Op = |Add|Mult
type AST = 
    |Val of int64
    |Op of (AST*Op option) list
    
let rec getNext op i (exp:string) (res:string) =
    if i = exp.Length then -1,(res.Trim()), None
    else
        let c = exp.Substring(i,1)
        match op, c  with
        |0,"+" -> (i+1),(res.Trim()), Some Op.Add
        |0,"*" -> (i+1),(res.Trim()), Some Op.Mult
        |0, "(" -> getNext (op+1) (i+1) (exp:string) (res)
        |op, "(" -> getNext (op+1) (i+1) (exp:string) (res+c)
        |1, ")" -> getNext 0 (i+1) (exp:string) (res)
        |op, ")" -> getNext (op-1) (i+1) (exp:string) (res+c)
        |_, a -> getNext op (i+1) (exp:string) (res+c)

let isNumeric (str:string) = str.ToCharArray() |> Array.forall (System.Char.IsDigit)

let rec eval (exp:string) = 
    if isNumeric exp then
        AST.Val (Parse.int64 exp)
    else
        let mutable loc = 0
        let a = 
            [
                while loc <> -1 do
                    let newLoc, expr, op = getNext 0 loc exp ""
                    let a = eval expr  
                    loc <- newLoc
                    a,op
            ]
            
        AST.Op a

let defOp = Option.defaultValue Op.Add

let rec reduceOp isop todo don =
    match todo with 
    |[] -> don |> List.rev
    |(a, aop) :: (b, bop) :: rest when isop (defOp aop) -> 
        let q = AST.Op [reduce isop a, aop; reduce isop b, None]
        match rest with 
        |[] -> reduceOp isop rest ((q,bop)::don)
        |rest -> reduceOp isop ((q,bop)::rest) don
    |(a, aop) :: rest ->
        reduceOp isop rest ((reduce isop a, aop)::don)

and reduce isop (e:AST) = 
    match e with 
    |AST.Val a -> e |AST.Op op -> (AST.Op (reduceOp isop op []))

let rec solve (e:AST) = 
    match e with 
    |AST.Val a -> a
    |AST.Op ((a,op) :: (b, _) :: []) ->
        let a = solve a
        let b = solve b
        match op with 
        |Some Op.Add -> a + b
        |Some Op.Mult ->a * b
        |_ -> failwithf "test #2"
    |AST.Op ((a,op) :: []) -> solve a
    |AST.Op (a) -> 
        failwithf "Test#3 %i %A %A" a.Length a e

let part1 = 
    load "Day18/input.txt"
    |> Array.fold (fun s x -> eval x |> reduce (fun _ -> true) |> solve |> (fun x -> x+s)) 0L

let part2 = 
    load "Day18/input.txt"
    |> Array.fold (fun s x -> 
                        eval x 
                        |> reduce (function |Op.Add -> true |_ -> false) 
                        |> reduce (function |Op.Mult -> true |_ -> false) 
                        |> solve |> (fun x -> x+s)) 0L


let a = eval "4 + (9 * (8 + 9 + 7 + 5 + 2) * (4 + 3 + 2 + 9 + 5 * 7)) * 2"
let b = reduce (function |Op.Add -> true |_ -> false)  a
let c = reduce (function |Op.Mult -> true |_ -> false) b
let d = solve c


//let a = eval "6 * 8"
//let b = reduce2 (fun _ -> true) a