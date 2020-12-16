#if COMPILED
module Day11
#endif

type Comp = { Mem : Map<int64, int64>; RelBase : int64; Pos : int64 } 
 with static member load (src:int64 array) = { Mem = Array.mapi (fun i x -> int64 i,x) src |> Map.ofArray; RelBase = 0L; Pos = 0L }
      static member load (input:string) = input.Split(',') |> Array.map System.Int64.Parse |> Comp.load
         
let nthOfInt (n:int) (i:int64) = i / (int64 (System.Math.Pow(float 10, float n))) % 10L //Right to left
let read n x = x.Mem |> Map.tryFind n |> function |Some s -> s |None -> 0L
let par pi x fn = fn ((nthOfInt (int pi+1) (read x.Pos x)),(read (x.Pos+pi) x)) 
let par_w pi x = par pi x (function |0L,c -> c                   |2L,c -> c + x.RelBase          |x,c -> failwithf "unsupp w%i" x)
let par_r pi x = par pi x (function |0L,c -> read c x |1L,c -> c |2L,c -> read (c + x.RelBase) x |x,c -> failwithf "unsupp r%i" x)
let setPos newPos x = { x with Pos = newPos }
let setVal memIndex value setPos x = { x with Mem = x.Mem |> Map.add memIndex value; Pos = setPos }
let fn2 fn (comp:Comp) = comp |> setVal (par_w 3L comp) (fn (par_r 1L comp) (par_r 2L comp)) (comp.Pos + 4L)
let dump comp = comp.Mem |> Map.iter (fun x p -> printf "%i=%i," x p); printfn "|" ; comp
let isHalted comp = comp.Pos = -1L 

let runOne (readInput,writeOutput) (comp:Comp) = 
    match (read comp.Pos comp) % 100L (*opcode*) with
    |1L -> fn2 (+) comp 
    |2L -> fn2 (*) comp
    |3L -> comp |> setVal (par_w 1L comp) (readInput()) (comp.Pos + 2L)
    |4L -> par_r 1L comp |> writeOutput; setPos (comp.Pos + 2L) comp
    |5L -> setPos (if par_r 1L comp <> 0L then par_r 2L comp else comp.Pos + 3L) comp 
    |6L -> setPos (if par_r 1L comp = 0L then par_r 2L comp else comp.Pos + 3L) comp 
    |7L -> setVal (par_w 3L comp) (if par_r 1L comp < par_r 2L comp then 1L else 0L) (comp.Pos + 4L) comp
    |8L -> setVal (par_w 3L comp) (if par_r 1L comp = par_r 2L comp then 1L else 0L) (comp.Pos + 4L) comp
    |9L -> { comp with RelBase = comp.RelBase + (par_r 1L comp); Pos = comp.Pos + 2L }    
    |99L -> setPos -1L comp
    |n -> failwithf "Unknown opcode %i at pos %i" n comp.Pos


let rec runWhileConditional io condition (comp:Comp)  = 
    if condition comp then runOne io comp |> runWhileConditional io condition else comp 

let rec runUntilHalt io (comp:Comp)  = runWhileConditional io (isHalted >> not) comp 


let runForInp (inp:int64 array) comp = 
    let mutable i = 0
    let mutable o = -1L
    runUntilHalt ((fun () ->  i <- i + 1; inp.[i - 1]),(fun x -> printfn "OUT:%i" x; o <- x)) comp |> ignore<Comp>
    o

let src = "3,8,1005,8,330,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,1,10,4,10,101,0,8,29,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,0,10,4,10,1001,8,0,51,1006,0,78,2,107,9,10,1006,0,87,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,1,8,10,4,10,1001,8,0,82,2,1103,5,10,1,101,8,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,0,8,10,4,10,101,0,8,112,1006,0,23,1006,0,20,1,2,11,10,1,1007,12,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,1,8,10,4,10,101,0,8,148,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,1002,8,1,170,2,101,12,10,2,5,7,10,1,102,10,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,1,10,4,10,1001,8,0,205,1,1004,10,10,2,6,13,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,0,10,4,10,1001,8,0,235,2,102,4,10,1006,0,16,1006,0,84,1006,0,96,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,0,8,10,4,10,1001,8,0,269,1006,0,49,2,1003,6,10,2,1104,14,10,1006,0,66,3,8,102,-1,8,10,101,1,10,10,4,10,108,0,8,10,4,10,1002,8,1,305,2,1,11,10,101,1,9,9,1007,9,1020,10,1005,10,15,99,109,652,104,0,104,1,21102,838479487744,1,1,21102,1,347,0,1106,0,451,21101,666567967640,0,1,21101,358,0,0,1106,0,451,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21101,28994219048,0,1,21102,405,1,0,1105,1,451,21102,3375459559,1,1,21101,0,416,0,1106,0,451,3,10,104,0,104,0,3,10,104,0,104,0,21102,838433665892,1,1,21102,1,439,0,1106,0,451,21102,988669698816,1,1,21102,450,1,0,1105,1,451,99,109,2,21201,-1,0,1,21102,1,40,2,21101,482,0,3,21102,472,1,0,1105,1,515,109,-2,2105,1,0,0,1,0,0,1,109,2,3,10,204,-1,1001,477,478,493,4,0,1001,477,1,477,108,4,477,10,1006,10,509,1101,0,0,477,109,-2,2105,1,0,0,109,4,1201,-1,0,514,1207,-3,0,10,1006,10,532,21101,0,0,-3,22102,1,-3,1,21201,-2,0,2,21102,1,1,3,21101,551,0,0,1106,0,556,109,-4,2105,1,0,109,5,1207,-3,1,10,1006,10,579,2207,-4,-2,10,1006,10,579,21201,-4,0,-4,1105,1,647,21201,-4,0,1,21201,-3,-1,2,21202,-2,2,3,21101,0,598,0,1106,0,556,21202,1,1,-4,21102,1,1,-1,2207,-4,-2,10,1006,10,617,21102,0,1,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,639,22102,1,-1,1,21101,0,639,0,106,0,514,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2105,1,0"

let comp = Comp.load src

type Robot = { mutable Dir : int; mutable Pos : int*int }
let getColor field (x,y) = 
    field |> Map.tryFind  (x,y) |> function |Some (x,c) -> c |None -> 0L

let paintField (field:Map<int*int,(int*int64)>) = 
    let (w,h) = field |> Map.fold (fun (w,h) (x,y) _ -> (max w x, max h y)) (0,0)
    for y = 0 to h do
        for x = 0 to w do
            getColor field (x,y)
            |> function |0L -> "." |1L -> "#" |a -> failwithf ""
            |> (fun x -> printf "%s" x)
        printfn ""

let test startField comp = 
    let mutable field = startField
    let robot = { Dir = 0; Pos = 0,0 }

    let mutable i = 0
    let inp = (fun () -> getColor field robot.Pos)
    let out = (fun out -> 
                if i % 2 = 0 then
                    field <- 
                        Map.add robot.Pos 
                            (
                                match field |> Map.tryFind (robot.Pos) with
                                |Some (x,_s) -> (x+1,out)
                                |None -> (if i = 0 then 0 else 1), out                             
                            )
                            field
                else
                    robot.Dir <- (robot.Dir + (if out = 0L then -1 elif out = 1L then 1 else failwithf "Not supported")) 
                                 |> (fun x -> if x = 4 then 0 elif x = -1 then 3 else x)
                    robot.Pos <- 
                        match robot.Dir, robot.Pos with
                        |0, (x,y) -> (x,y-1) // UP
                        |1, (x,y) -> (x+1,y) // RIGHT
                        |2, (x,y) -> (x,y+1) // DOWN
                        |3, (x,y) -> (x-1,y) // LEFT
                        |a -> failwithf "Invalid dir %A" a
                        
                i <- i + 1
                ()
              )
    let finalState = runWhileConditional (inp,out) (isHalted >> not) comp 
    field, robot, finalState

let round1 = 
    let f, r, s = test Map.empty comp
    f |> Map.count

let round2 = 
    let f, r, s = test (Map.empty |> Map.add (0,0) (0,1L)) comp
    f |> paintField



//Panels stars as black
//INP -> Color 
//OUT -> Paint Color 0 = Black, 1 = White
//OUT -> 0 = LEFT, 1 = RIGHT (Move)
