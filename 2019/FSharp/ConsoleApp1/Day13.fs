#if COMPILED
module Day13
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
let setMem memIndex value x = { x with Mem = x.Mem |> Map.add memIndex value; }
let setVal memIndex value setPos x = { x with Mem = x.Mem |> Map.add memIndex value; Pos = setPos }
let fn2 fn (comp:Comp) = comp |> setVal (par_w 3L comp) (fn (par_r 1L comp) (par_r 2L comp)) (comp.Pos + 4L)
let dump comp = comp.Mem |> Map.iter (fun x p -> printf "%i=%i," x p); printfn "|" ; comp
let isHalted comp = comp.Pos = -1L 
let cmp1 fn comp = setPos (if fn (par_r 1L comp) then par_r 2L comp else comp.Pos + 3L) comp 
let cmp2SetBool fn comp = setVal (par_w 3L comp) (if fn (par_r 1L comp) (par_r 2L comp) then 1L else 0L) (comp.Pos + 4L) comp
let io_state inp out initState = 
    let mutable state = initState in let mutable i = 0 in let mutable o = 0 in
    ((fun () -> let (ns,x) = inp i state in state <- ns; i <- i + 1; x),
     (fun x -> state <- out o state x; o <- o + 1)),(fun () -> state)

let runOne (readInput,writeOutput) (comp:Comp) = 
    match (read comp.Pos comp) % 100L (*opcode*) with
    |1L -> fn2 (+) comp 
    |2L -> fn2 (*) comp
    |3L -> comp |> setVal (par_w 1L comp) (readInput()) (comp.Pos + 2L)
    |4L -> writeOutput (par_r 1L comp); setPos (comp.Pos + 2L) comp
    |5L -> cmp1 ((<>)0L) comp 
    |6L -> cmp1 ((=)0L) comp 
    |7L -> cmp2SetBool (<) comp
    |8L -> cmp2SetBool (=) comp
    |9L -> { comp with RelBase = comp.RelBase + (par_r 1L comp); Pos = comp.Pos + 2L }    
    |99L -> setPos -1L comp
    |n -> failwithf "Unknown opcode %i at pos %i" n comp.Pos

let rec runWhileConditional io condition (comp:Comp)  = 
    if condition comp then runOne io comp |> runWhileConditional io condition else comp 

let src = System.IO.File.ReadAllText("""C:\Drive\Projects\Adventcode\2019\FSharp\ConsoleApp1\day13.input""")
let comp = Comp.load src

type Pos = { x: int64; y:int64 } with static member empty = { x=0L; y=0L }
type Game = {
    Score : int64
    _c : Pos
    Field : Map<Pos,int64>
    Ball : Pos
    Paddle : Pos
}

let play gameStart comp = 
    let io,get = 
        io_state    
            (fun i (s:Game) -> s,s.Ball.x - s.Paddle.x |> min 1L |> max -1L)
            (fun i s out -> 
                match i%3 with
                |0 -> { s with _c = { s._c with x = out } }
                |1 -> { s with _c = { s._c with y = out }  }
                |2 -> 
                    if s._c.x = -1L && s._c.y = 0L then  { s with Score = out }
                    else
                        match out with 
                        |0L|1L|2L -> 
                            { s with Field = Map.add s._c out s.Field  }
                        |3L -> { s with Paddle = s._c }
                        |4L -> { s with Ball = s._c }
                        |a -> failwithf "ASLdkij %A" a                    
                |a -> failwith "Not poss"
            )
            gameStart
    
    let finalState = runWhileConditional io (isHalted >> not) comp 
    get()

let startGame = { Score = 0L; _c = Pos.empty; Field = Map.empty; Ball = Pos.empty; Paddle = Pos.empty }
let round1 = 
    play startGame comp
    |> (fun x -> x.Field |> Map.filter (fun k v -> v = 2L) |> Map.count)
    
let round2 = 
    play startGame (comp |> setMem 0L 2L)
    |> (fun x -> x.Score)
    
    