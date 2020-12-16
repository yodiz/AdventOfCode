


let fuel m = max 0 (m / 3 - 2)
let rec ffuel m = 
    let r = fuel m
    if r > 0 then
        r + ffuel r
    else r

ffuel 1969
    
System.IO.File.ReadAllLines("c:\\temp\\aoc\\1.txt")
|> Array.map System.Int32.Parse
|> Array.fold (fun s x -> s + fuel x) 0

System.IO.File.ReadAllLines("c:\\temp\\aoc\\1.txt")
|> Array.map System.Int32.Parse
|> Array.fold (fun s x -> s + ffuel x) 0


let exec (src:int array) (pos:int) = 
    match src.[pos] with
    |1 -> 
        let s = src.[src.[pos+1]] + src.[src.[pos+2]]
        src.[src.[pos+3]] <- s
        Some (pos + 4)
    |2 -> 
        let s = src.[src.[pos+1]] * src.[src.[pos+2]]
        src.[src.[pos+3]] <- s
        Some (pos + 4)
    |99 -> None
    |n -> failwithf "Unknown opcode %i" n

let run (src:int array) = 
    let mutable isDone = false
    let mutable loc = 0
    while not isDone do
        match exec src loc with
        |Some s -> 
            loc <- s
        |None -> 
            isDone <- true
            //printfn "Done"

let src = "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,9,1,19,1,19,5,23,1,23,6,27,2,9,27,31,1,5,31,35,1,35,10,39,1,39,10,43,2,43,9,47,1,6,47,51,2,51,6,55,1,5,55,59,2,59,10,63,1,9,63,67,1,9,67,71,2,71,6,75,1,5,75,79,1,5,79,83,1,9,83,87,2,87,10,91,2,10,91,95,1,95,9,99,2,99,9,103,2,10,103,107,2,9,107,111,1,111,5,115,1,115,2,119,1,119,6,0,99,2,0,14,0"
let srcD = src.Split(',') |> Array.map System.Int32.Parse

let round1 () = 
    let srcD = src.Split(',') |> Array.map System.Int32.Parse
    srcD.[1] <- 12
    srcD.[2] <- 2
    run srcD
    srcD.[0]

let round2 () = 
    for noun = 0 to 100 do
        for verb = 0 to 100 do
            let srcD = src.Split(',') |> Array.map System.Int32.Parse
            srcD.[1] <- noun
            srcD.[2] <- verb
            run srcD
            if srcD.[0] = 19690720 then
                printfn "%i %i is correct" noun verb
                printfn "Res is : %i" (100 * noun + verb)
            else 
                ()
round2()