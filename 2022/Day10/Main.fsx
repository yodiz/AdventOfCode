#if INTERACTIVE
#load "../Common.fsx"
#else
module Day10
#endif

open System
open AoC

let folder = __SOURCE_DIRECTORY__ + "\\"

type Cmd = |Noop |Addx of int
let time = function Noop -> 1 |Addx _ -> 2

let parseLine (str:string) = 
    match Text.split " " str with 
    |[|"noop"|] -> Noop
    |[|"addx"; Int32 x|] -> Addx x
    |a -> failwithf "unsupp %s" str

let input  = load folder "test1.txt" |> Array.map parseLine

let cycle (inp:_ array) i = inp[i%inp.Length]

let rec step<'state> (fn:'state -> _ -> int -> 'state) stopAt inp curRemCyc (state:'state) currCycle cmdIndex (reg:int) = 
    if stopAt = currCycle then
        state
    else
        if curRemCyc = 0 then
            let newreg = 
                if currCycle >= 0 then
                    let cmd = cycle inp cmdIndex
                    match cmd with 
                    |Noop -> reg
                    |Addx x -> reg + x
                else 
                    reg

            let newState = fn state currCycle newreg
            let cycles = time (cycle inp (cmdIndex+1))
            step fn stopAt inp cycles newState currCycle (cmdIndex+1) newreg
        else 
            step fn stopAt inp (curRemCyc - 1) state (currCycle+1) cmdIndex reg
        

let part1 : int = 
    let fn (state:int) currCycle reg = 
        if currCycle = 20 || currCycle % 40 = 0 then
            let signal = reg * currCycle
            printfn "Cycle: %i - strength %i: Reg: %i" currCycle signal reg
            state + signal 
        else state

    step<int> fn 220 input 0 0 -1 -1 1

let part2 = 0


//
printfn "%i" part1
printfn "%i" part2

