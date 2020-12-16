#if COMPILED
module Day15
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

type IoComp = {
    Comp : Comp
    Inp : int64 list
    Out : int64 list
}
let ioComp comp = { Comp = comp; Inp = []; Out = [] }
let addInp inp comp = { comp with Inp = inp :: comp.Inp }
let runIoComp (ioComp:IoComp) = 
    let mutable out = None
    let mutable inp = None
    let a = (fun () -> match ioComp.Inp with |x :: rest -> inp <- Some rest; (*printfn "INP:%i" x;*) x |[] -> failwithf "No input available")
    let b = (fun (o:int64) -> (*printfn "OUT:%i" o;*) out <- Some o)
    let comp = runOne (a,b) ioComp.Comp
    
    { ioComp with   Comp=comp; 
                    Out = match out with |None -> ioComp.Out |Some s -> s :: ioComp.Out 
                    Inp = match inp with |None -> ioComp.Inp |Some s -> s }

let rec runWhileConditional condition (comp:IoComp)  = 
    if condition comp then runIoComp comp |> runWhileConditional condition else comp 

let src = System.IO.File.ReadAllText("""C:\Drive\Projects\Adventcode\2019\FSharp\ConsoleApp1\day15.input""")
let comp = Comp.load src


type Pos = { x : int64; y:int64 }
let posToStr (x:Pos) = sprintf "x=%i, y=%i" x.x x.y
let posZeo = { x = 0L;y = 0L }
type Game = {
    Pos : Pos
    Visited : Set<Pos>
    LastMove : int64
    Moves : int
    Comp : IoComp
    Wait : bool
}

type GlobalState = {
    mutable Oxygen : Pos
    mutable Walls : Set<Pos>
}

let rec play (state:GlobalState) (game:Game) : Game option = 
    match game.Comp.Out, game.Comp.Inp with
    |[], [] when game.Wait = false -> //No input, provide it
        [1L..4L]
        |> List.choose (fun x -> play state { game with Comp = addInp x game.Comp; LastMove = x; Wait = true; } |> Some) 
        |> List.fold 
            (fun s x -> 
                match s,x with 
                |None,None -> None
                |None,Some a -> Some a 
                |Some a, None -> Some a
                |Some a, Some b -> if a.Moves < b.Moves then Some a else Some b
            ) None
    |out :: rest, _ -> 
        let pos = 
            match game.LastMove with
            |1L (*N*)-> { game.Pos with y = game.Pos.y-1L }
            |2L (*S*)-> { game.Pos with y = game.Pos.y+1L }
            |3L (*W*)-> { game.Pos with x = game.Pos.x-1L }
            |4L (*E*)-> { game.Pos with x = game.Pos.x+1L }
            |a -> failwithf "a%A" a                 

        match out with
        |0L (*wall*)-> 
            state.Walls <- Set.add pos state.Walls
            None
        |1L -> 
            if game.Visited |> Set.contains pos then
                None
            else
                play state { game with Comp = { game.Comp with Out = rest };
                                       Visited = Set.add pos game.Visited
                                       Moves = game.Moves + 1 
                                       Pos = pos
                                       Wait = false;
                                       }
        |2L (*goal*) -> 
            state.Oxygen <- pos
            Some { game with Pos = pos; Moves = game.Moves + 1 }
        |a -> failwithf "b%A" a
    |[], a ->  
        play state { game with Comp = runIoComp game.Comp }

let dumpMap (walls:Set<Pos>) (oxygen:Set<Pos>) = 
    let l,u = 
        walls 
        |> Set.fold 
            (fun (l:Pos,u:Pos) x -> 
                {l with x = min l.x x.x; y= min l.y x.y },
                {u with x = max l.x x.x; y= max l.y x.y }
            )
            ({ x = 0L; y=0L },{ x = 0L; y=0L })

    for y = int l.y to int u.y do
        for x = int l.x to int u.x do
            let isWall = walls |> Set.contains { x=int64 x; y=int64 y; }
            let isOxygen = oxygen |> Set.contains { x=int64 x; y=int64 y; } 
            //printfn "%i,%i=%b" x y isWall
            if isOxygen then 
                printf "o"
            elif isWall then 
                printf "#"
            else
                printf "."
        printfn ""

    
let round1 = 
    let state = { Walls = Set.empty; Oxygen = posZeo }
    let game = play state { Pos = posZeo;  Visited = Set.empty |> Set.add posZeo; Moves = 0; Comp = ioComp comp; LastMove = 0L; Wait = false } 
    game |> Option.map (fun x -> x.Moves)


let getNeighbour pos (walls:Set<Pos>) (oxygen:Set<Pos>) = 
    if Set.contains pos oxygen then "o"
    elif Set.contains pos walls then "#"
    else "."

let rec tickOxygen nth (walls:Set<Pos>) (oxygen:Set<Pos>) = 
    //Foreach oxygen
    // make neighbour oxygen unless its is a wall
    //If alla oxygen has oxygen as neighbor return i
    let oxygened, set = 
        oxygen
        |> Set.fold 
            (fun (i,o) pos -> 
                let a = 
                    [(0,-1);(0,1);(-1,0);(1,0)]
                    |> List.fold 
                        (fun (i,o) (x,y) -> 
                            let newPos = { pos with x = pos.x + int64 x; y = pos.y + int64 y }
                            match getNeighbour newPos walls oxygen with
                            |"o"|"#" -> i, o
                            |"." -> i+1, o |> Set.add newPos
                        )
                        (i,o)
                a
            )
            (0,oxygen)

    //dumpMap walls set
    //System.Threading.Thread.Sleep(50)

    if oxygened = 0 then nth
    else tickOxygen (nth+1) walls set


let round2 = 
    let state = { Walls = Set.empty; Oxygen = posZeo }
    let game = play state { Pos = posZeo; Visited = Set.empty |> Set.add posZeo; Moves = 0; Comp = ioComp comp; LastMove = 0L; Wait = false } 
    
    let n = tickOxygen 0 state.Walls (Set.empty |> Set.add state.Oxygen)
    n
    
    


    