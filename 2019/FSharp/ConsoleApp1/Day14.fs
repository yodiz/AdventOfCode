#if COMPILED
module Day14
#endif

let lines = System.IO.File.ReadAllLines("""C:\Drive\Projects\Adventcode\2019\FSharp\ConsoleApp1\day14.txt""")


let pair (sep:string) (src:string) = 
    match src.Trim().Split([|sep|], System.StringSplitOptions.None) with
    |[|a;b|] -> a.Trim(),b.Trim()
    |a -> failwithf "Expected pair with sep '%s' in %s" sep src
let parseOne (s:string) = pair " " s |> (fun (a,b) -> System.Int64.Parse a,b)
let parseLine (str:string) =     
    let (srcs,target) = pair "=>" str
    srcs.Split(',') |> Array.map (fun x -> parseOne x),
    parseOne target
    
let lookup = 
    lines 
    |> Array.map (parseLine)
    |> Array.map (fun (srcs,(tAmount, tRes)) -> tRes, (tAmount, srcs |> List.ofArray))
    |> Map.ofArray


let addHave waste res amount = 
    if amount = 0L then waste else
        //printfn "Rest product %s %i" res amount
        match waste |> Map.tryFind res with
        |None -> waste |> Map.add res amount
        |Some x ->  waste |> Map.add res (amount+x)

let tryRemoveHave waste res amount = 
    match waste |> Map.tryFind res with
    |None -> 0L,waste
    |Some x -> 
        if x > amount then 
            //printfn "Use rest product Some %s %i" res amount
            amount, waste |> Map.add res (x - amount)
        else
            //printfn "Use rest product All %s %i" res x
            x, waste |> Map.remove res 
        
let dump m = 
    m |> Map.iter (fun k v -> printfn "%s - %i" k v)

let rec reduce  require have = 
    match require |> Map.tryPick (fun k v -> if k <> "ORE" then Some (k,v) else None) with
    |None -> require |> Map.find "ORE", have
    |Some (reqRes, amount) -> 
        let produces,moreReq  = lookup |> Map.find reqRes
        let rep = System.Math.Ceiling((float amount) / (float produces)) |> int64
        let totalProd = int64 rep * produces
        let extra = totalProd - amount
        
        let newRequire,newHave = 
            moreReq
            |> List.fold 
                (fun (s,sh) (a,res) -> 
                    let xx = a*rep

                    //Try use from have
                    let haveUsed,sh = tryRemoveHave sh res xx
                    let xx = xx - haveUsed

                    s |> Map.add res 
                        (match s |> Map.tryFind res with 
                         |Some x -> x+xx
                         |None -> xx)
                    ,sh
                )
                ((require |> Map.remove reqRes),(addHave have reqRes extra))

        reduce newRequire newHave
        
        

let findRange value fn (lower,upper) = 
    let mid = upper - ((upper - lower) / 2L)
    if fn mid < value then
        mid,upper
    else 
        lower,mid

let rec search value fn (lower,upper) = 
    let (l,u) = findRange value fn (lower,upper)
    if l = u then l
    elif l+1L=u then l
    else
        search value fn (l,u)

    
let round1 =         
    reduce (Map.empty |> Map.add "FUEL" 1L) Map.empty

let round2 =        
    let fn v = reduce (Map.empty |> Map.add "FUEL" v) Map. empty |> fst
    let max = search 1000000000000L fn (1L, (System.Int32.MaxValue |> int64))
    max




