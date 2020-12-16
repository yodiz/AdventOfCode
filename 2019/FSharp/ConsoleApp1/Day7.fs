#if COMPILED
module Day7
#endif

///Right to left / zero index
let nthOfInt (n:int) (i:int) = i / (int (System.Math.Pow(float 10, float n))) % 10
let exec readInput writeOutput (src:int array) (pos:int) = 
    let r_par i = let x = src.[pos+i] in nthOfInt (i+1) src.[pos] |> function |0 -> src.[x] |1 -> x |x->failwithf "e%i" x
    let w_par i = src.[pos+i]

    match src.[pos] % 100 (*opcode*) with
    |1 -> src.[w_par 3] <- r_par 1 + r_par 2; pos+4
    |2 -> src.[w_par 3] <- r_par 1 * r_par 2; pos+4
    |3 -> let i = readInput() in printfn "INP:%i" i; src.[w_par 1] <- i; pos+2
    |4 -> writeOutput (r_par 1); pos+2
    |5 -> if r_par 1 <> 0 then r_par 2 else pos+3
    |6 -> if r_par 1 = 0 then r_par 2 else pos+3    
    |7 -> src.[w_par 3] <- if r_par 1 < r_par 2 then 1 else 0
          pos + 4
    |8 -> src.[w_par 3] <- if r_par 1 = r_par 2 then 1 else 0
          pos + 4
    |99 -> -1
    |n -> failwithf "Unknown opcode %i at pos %i" n pos

let rec run readInput writeOutput (src:int array) loc = 
    if loc = -1 then printfn "HALT"; ()
    else exec readInput writeOutput src loc |> run readInput writeOutput src

let runForInp (inp:int array) src = 
    let mutable i = 0
    let mutable o = -1
    run (fun () ->  i <- i + 1; inp.[i - 1]) 
        (fun x -> printfn "OUT:%i" x; o <- x) src 0
    o

let input = "3,8,1001,8,10,8,105,1,0,0,21,34,47,72,81,102,183,264,345,426,99999,3,9,102,5,9,9,1001,9,3,9,4,9,99,3,9,101,4,9,9,1002,9,3,9,4,9,99,3,9,102,3,9,9,101,2,9,9,102,5,9,9,1001,9,3,9,1002,9,4,9,4,9,99,3,9,101,5,9,9,4,9,99,3,9,101,3,9,9,1002,9,5,9,101,4,9,9,102,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,99"
let load () = input.Split(',') |> Array.map System.Int32.Parse
let runAmp signal phase = runForInp [|phase;signal|] (load())

let round1() = 
    let a = [
        for i1 = 0 to 4 do
            for i2 = 0 to 4 do
                for i3 = 0 to 4 do
                    for i4 = 0 to 4 do
                        for i5 = 0 to 4 do
                            if [|i1;i2;i3;i4;i5|] |> Set.ofArray |> Set.count = 5 then
                                yield [|i1;i2;i3;i4;i5|]
    ]    

    a |> List.maxBy (fun inp -> inp |> Array.fold runAmp 0)
    |> (fun inp -> inp |> Array.fold runAmp 0)



//Runs until halt or next output, with given input as next input, returns output,currentPos (-1 is halt)
let rec runNext input (src:int array) loc = 
    let mutable out = None
    let newPos = exec (fun () -> input) (fun x -> out <- Some x) src loc
    if newPos = -1 then 
        printfn "HALT"
        out, -1
    else
        printfn "OUT %A at %i" out newPos
        match out with 
        |Some s -> Some s, newPos
        |None -> runNext input src newPos

let rec feedback signal ampIndex (amps:(int[]*int) array) = 
    //Inp
    let src,loc = amps.[ampIndex]
    let out, curPos = runNext signal src loc
    match out with 
    |Some s -> 
        //Run next amp with out as inp
        feedback s ((ampIndex+1)%amps.Length) 
            (amps |> Array.mapi (fun i x -> if i = ampIndex then src,curPos else x))
    |None -> 
        //curPos should begin -1
        signal
        //failwithf "No outputr recevied at Amp:%i Pos:%i" ampIndex  curPos
        //()



let setupAmp phaseSetting = 
    let mem = load()
    let newPos = 
        exec (fun () -> phaseSetting) (fun x -> failwithf "No output expected") mem 0
    mem, newPos

let phaseSettings = [|9;8;7;6;5|]
let a() = feedback 0 0 (Array.init 5 (fun i -> setupAmp phaseSettings.[i])) 

let round2 () = 
    let a = [
        for i1 = 0 to 4 do
            for i2 = 0 to 4 do
                for i3 = 0 to 4 do
                    for i4 = 0 to 4 do
                        for i5 = 0 to 4 do
                            if [|i1;i2;i3;i4;i5|] |> Set.ofArray |> Set.count = 5 then
                                yield [|i1+5;i2+5;i3+5;i4+5;i5+5|]
    ]    
   

    a
    |> List.map 
        (fun x -> 
            feedback 0 0 (Array.init 5 (fun i -> setupAmp x.[i])) 
        )
    |> List.max



