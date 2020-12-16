#if COMPILED
module Day8
#endif


let input = 
    System.IO.File.ReadAllText("""c:\Drive\Projects\Adventcode\2019\FSharp\ConsoleApp1\Day8.input""")
    |> (fun x -> x.ToCharArray())
    |> Array.map (fun x -> System.Byte.Parse (x.ToString()))


let layers = 
    input
    |> Array.mapi 
        (fun i x -> 
            i / (25*6), x
        )
    |> Array.groupBy fst
    |> Array.map (fun (k,v) -> k,v |> Array.map snd)

let zeroLayer = 
    layers 
    |> Array.sortBy (fun (l,v) -> v |> Array.filter (fun x -> x = 0uy))
    |> Array.head
    |> snd

let a = zeroLayer |> Array.filter (fun x -> x = 1uy) |> Array.length
let b = zeroLayer |> Array.filter (fun x -> x = 2uy) |> Array.length

let round1 = a * b

//0 - black
//1 - white
//2 - transparent
let round2 = 
    let rendered = 
        layers
        |> Array.sortBy fst
        |> Array.map snd
        |> Array.reduce
            (fun s x -> 
                Array.map2
                    (fun (c1:byte) c2 -> 
                        match c1 with
                        |0uy|1uy -> c1
                        |2uy -> c2
                    )
                    s x
            )
    for y = 0 to 6 - 1 do
        for x = 0 to 25 - 1  do 
            let a = rendered.[x + y*25]
            match a with 
            |0uy -> printf " "
            |1uy -> printf "x"
            |2uy -> printf "#"
        printfn ""
