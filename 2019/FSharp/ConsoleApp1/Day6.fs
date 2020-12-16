#if COMPILED 
module Day6
#endif

let lines = System.IO.File.ReadAllLines("""c:\Drive\Projects\Adventcode\2019\FSharp\ConsoleApp1\Day6.input""")
let orbits = 
    lines 
    |> Array.map (fun x -> let [|a;b|] = x.Split(')') in a,b)
    |> Array.fold (fun s (source,orbiter) -> s |> Map.add orbiter source) Map.empty

let rec getOrbits i items x = 
    if x = "COM" then i, items
    else
        match orbits |> Map.tryFind x with
        |None -> failwithf "Galax collapse"
        |Some s -> 
            getOrbits (i+1) ((s, i) :: items) s

let round1 = 
    orbits 
    |> Map.fold 
        (fun i orbiter source -> 
            let a = getOrbits 0 [] orbiter |> fst
            a + i
        )
        0

let orbitsA = getOrbits 0 [] "YOU" |> snd
let orbitsB = getOrbits 0 [] "SAN" |> snd
let round2 = 
    Seq.fold2   
        (fun l (aa,ab) (bb,ba) -> 
            if aa <> bb then l
            else
                ab+ba
        )
        0 orbitsA orbitsB

//‏‎START:4:32:37 
//SLUT:  4:51
