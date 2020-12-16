#if INTERACTIVE
#load "../Common.fsx"
#endif
open AoC

type Op = |Mask of string |Mem of (int32*int64)

let bitStatus num n = (num >>> n) &&& 1L = 1L;
let setBit x y n = 
    if y then x ||| (1L <<< n)
    else x &&& ~~~(1L <<< n);


//setBit 0L true 2

let applyMask (num:int64) (str:string) : int64  = 
    str.ToCharArray()
    |> Array.rev
    |> Array.foldi 
        (fun i s x ->
            let n = x |> function |'0' -> false |'1' -> true |'X' -> (bitStatus num i)
            let ns = setBit s n i
            ns
        ) 0L

let parseLine (str:string) : Op = 
    match str with 
    |str when str.StartsWith("mask") -> 
        Op.Mask (str.Substring(7))
    |Regexp.Regex ".*\[(\d+)\].*\s(\d+)$" groups -> 
        Op.Mem (Parse.int32 groups.[0], Parse.int64 groups.[1]) 
    |a -> failwithf "What %s" a


let part1 = 
    load "Day14/input.txt"
    |> Array.map (parseLine)
    |> Array.fold 
        (fun (mask,s) op -> 
            match op with 
            |Op.Mem (loc,value) -> 
                let newVal = applyMask value mask                
                mask, s |> Map.add loc newVal
            |Op.Mask mask -> 
                mask, s
        ) 
        ("", Map.empty)
    |> snd |> Map.fold (fun s k v -> s + v) 0L
        



let rec all pre left  = 
    match left with
    |[] -> [pre |> List.rev]
    |a :: rest -> 
        
        match a with
        |Some x -> all (x::pre) rest
        |None -> 
            let a = all (true::pre) rest
            let b = all (false::pre) rest
            List.append a b
        
let applyMask2 (num:int64) (str:string) = 
    let m = 
        str.ToCharArray()
        |> Array.rev
        |> Array.foldi 
            (fun i s x ->
                let n = x |> function |'0' -> Some (bitStatus num i) |'1' -> Some true |'X' -> None
                n :: s
            ) 
            []
    all [] m
    
    




let part2 = 
    load "Day14/input.txt"
    |> Array.map (parseLine)
    |> Array.fold 
        (fun (mask,s) op -> 
            match op with 
            |Op.Mem (loc,value) -> 
                let x = 
                    applyMask2 (int64 loc) mask
                    |> List.map (fun x -> x |> List.rev |> List.foldi (fun i s x -> setBit s x i) 0L)
                    |> List.fold (fun map addr -> map |> Map.add addr value) s
                mask,x
            |Op.Mask mask -> 
                mask, s
        ) 
        ("", Map.empty)
    |> snd |> Map.fold (fun s k v -> s + v) 0L