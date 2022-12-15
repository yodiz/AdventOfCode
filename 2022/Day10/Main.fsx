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

let input  = load folder "input.txt" |> Array.map parseLine
let cycle (inp:_ array) i = inp[i%inp.Length]
type Register = { x : int }


let rec step<'a> stopAt input fn toProcessLeft cmdIndex currentCycle (state:'a) (register:Register) = 
    if stopAt = currentCycle then
        state
    else
        let toProcessLeft, cmdIndex, register = 
            if toProcessLeft = 0 then
                //Run command
                let newreg = 
                    //First cycle is startup - do not run anything
                    if currentCycle = 0 then register
                    else
                        let cmd = cycle input cmdIndex
                        match cmd with 
                        |Noop -> register
                        |Addx x -> { register with x =  register.x + x }

                let nxtCmd = (cycle input (cmdIndex+1))
                let nxtCmdSteps = time nxtCmd
                (nxtCmdSteps - 1), (cmdIndex+1), newreg
            else
                (toProcessLeft - 1), cmdIndex, register

        let newState = fn state (currentCycle+1 (*Correction 0 index*)) register
        step stopAt input fn toProcessLeft cmdIndex (currentCycle+1) newState register    

let run<'a> stopAt input fn (state:'a) =
    step<'a> stopAt input fn 0 -1 0 state { x = 1 }    

let part1 : int = 
    let fn (state:int) currCycle reg = 
        if (currCycle - 20) % 40 = 0 then
            let signal = reg.x * currCycle
            printfn "Cycle: %i - strength %i: Reg: %i" currCycle signal reg.x
            state + signal 
        else state

    run<int> 220 input fn 0


let part2 = 
    let fn (state:Set<_>) currCycle (reg:Register) = 
        let crt_x = (currCycle - 1) % 40
        let crt_y = (currCycle - 1) / 40
        let visible = crt_x = reg.x || crt_x = reg.x - 1 || crt_x = reg.x + 1
        
        if visible then 
            state |> Set.add (crt_x,crt_y)
        else state

    let s = run<Set<_>> (6*40) input fn Set.empty

    //Draw
    [0..6-1]
    |> List.map (fun y -> [0..40-1] |> List.map (fun x -> if s |> Set.contains (x,y) then "#" else " ") |> String.concat "")
    |> String.concat "\r\n"



//
printfn "%i" part1
printfn "%s" part2

