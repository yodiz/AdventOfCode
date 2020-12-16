#if INTERACTIVE
#else
module Day4
#endif


let isNdigits (n:int) (x:int)=  x.ToString().Length = n
let isSameAdjecant (x:int) = x.ToString().ToCharArray() |> Array.fold (fun (s,p) n -> if n = p then true,n else s,n ) (false,' ') |> fst
let isIncreasing (x:int) = x.ToString().ToCharArray() |> Array.fold (fun (s, l) n -> if n < l then (false,n) else (s,n) ) (true, '0') |> fst

let isDoubleAdjecant (x:int) = 
    x.ToString().ToCharArray() 
    |> Array.fold 
        (fun (s,p,c) n -> 
            if p = ' ' then s,n,1
            elif n = p then s,n,c+1
            else 
                if c = 2 then true, n, 1
                else s,n,1
        ) 
        (false,' ',0) 
    |> (fun (x,_,c) -> x || c = 2)

let round1 = 
    [234208..765869]
    |> List.filter (fun x -> isNdigits 6 x && isSameAdjecant x && isIncreasing x)
    |> List.length


let round2Ok x = isNdigits 6 x && isDoubleAdjecant x && isIncreasing x
round2Ok 112233 // ok
round2Ok 123444 //not ok
round2Ok 111122 //ok

let round2 = 
    [234208..765869]
    |> List.filter (fun x -> isNdigits 6 x && isDoubleAdjecant x && isIncreasing x)
    |> List.length



