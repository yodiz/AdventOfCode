#if COMPILED
module Day12

open System.Numerics
open Microsoft.VisualBasic.CompilerServices

#endif


//Moon Pos and Velocity
//Vel starts at 0


//Tick -
//  Update all velocity (Apply gravity)
//  Update all positions

//Gravity apply



let inpM = """<x=6, y=-2, z=-7>
<x=-6, y=-7, z=-4>
<x=-9, y=11, z=0>
<x=-3, y=-4, z=6>"""

type Vec = { mutable x : int ; mutable y : int; mutable z : int } 
let vec x y z = { x = x; y = y; z = z }
let zeroVec () = vec 0 0 0
let vecAdd (a:Vec) (b:Vec) = { x = a.x+b.x; y = a.y + b.y; z = a.z + b.z }
type Moon = { Pos : Vec; Vel : Vec }
let moon p v = { Pos = p; Vel = v }


let moons (inp:string) = 
    "><= \rxyz".ToCharArray() |> Array.fold (fun (inp:string) x -> inp.Replace(x.ToString(), "")) inp
    |> (fun x -> x.Split('\n'))
    |> Array.map (fun x -> let [|x;y;z|] = x.Split(',') |> Array.map System.Int32.Parse
                           moon (vec x y z) (zeroVec ()))
    
    
let d_part_vec src affecter = 
    if affecter > src then 1
    elif src > affecter then -1
    else 0

let vecToStr (v:Vec) = sprintf "<x=%i, y= %i, z= %i>" v.x v.y v.z
let dumpMoons (moons:Moon array)  = 
    moons |> Array.iter (fun x -> printfn "pos=%s, vel=%s" (vecToStr x.Pos) (vecToStr x.Vel))
    

let tick (moons:Moon array) = 
    for i = 0 to moons.Length - 1 do
        //let x = moons.[i]
        let (dx,dy,dz) = 
            moons 
            |> Array.fold
                (fun (i2,(ax,ay,az)) b -> 
                    if i = i2 then 
                        (i2+1,(ax,ay,az))
                    else
                        let dx = d_part_vec moons.[i].Pos.x b.Pos.x
                        let dy = d_part_vec moons.[i].Pos.y b.Pos.y
                        let dz = d_part_vec moons.[i].Pos.z b.Pos.z
                        (i2+1,(ax+dx,ay+dy,az+dz))
                )
                (0,(0,0,0))
            |> snd   
        //printfn "dx %i, %s" dx (vecToStr moons.[i].Vel)

        moons.[i].Vel.x <- moons.[i].Vel.x + dx
        moons.[i].Vel.y <- moons.[i].Vel.y + dy
        moons.[i].Vel.z <- moons.[i].Vel.z + dz
        //printfn "After: %i %s" i (vecToStr moons.[i].Vel)        

        
    for i = 0 to moons.Length - 1 do
        moons.[i].Pos.x <- moons.[i].Pos.x + moons.[i].Vel.x
        moons.[i].Pos.y <- moons.[i].Pos.y + moons.[i].Vel.y
        moons.[i].Pos.z <- moons.[i].Pos.z + moons.[i].Vel.z

    

    
let tick_while cond (moons:Moon array) = 
    let rec t i = 
        if cond i moons then 
            tick moons
            t (i+1L) 
        else i
    t 0L

let tickx x moons = 
    tick_while (fun i _ -> x > i) moons
    


let vecEnergy (v:Vec) = abs v.x+ abs v.y + abs v.z
let getEnergy (moon:Moon) = vecEnergy moon.Pos * vecEnergy moon.Vel
let getEnergies moons = moons |> Array.fold (fun s x -> getEnergy x + s) 0

let round1 = 
    let moons = moons inpM
    let n = moons |> tickx 1000L 
    dumpMoons moons
    getEnergies moons
    
let rec gcd x y = if y = 0L then abs x else gcd y (x % y)
let lcm x y = x * y / (gcd x y)

let round2 = 
    let moonStart =  moons inpM
    let moons = moons inpM
    let mutable xOk = None;
    let mutable YOk = None;
    let mutable ZOk = None;
    tick_while 
        (fun x mc ->
            let equalPosX = (moonStart,mc) ||> Array.forall2 (fun a b -> a.Pos.x = b.Pos.x && a.Vel.x = b.Vel.x)
            let equalPosy = (moonStart,mc) ||> Array.forall2 (fun a b -> a.Pos.y = b.Pos.y && a.Vel.y = b.Vel.y)
            let equalPosz = (moonStart,mc) ||> Array.forall2 (fun a b -> a.Pos.z = b.Pos.z && a.Vel.z = b.Vel.z)

            if x > 0L then
                if equalPosX && xOk |> Option.isNone then xOk <- Some x; printfn "Found x at %i" x
                if equalPosy && YOk |> Option.isNone then YOk <- Some x; printfn "Found y at %i" x
                if equalPosz && ZOk |> Option.isNone then ZOk <- Some x; printfn "Found z at %i" x
                        
            xOk |> Option.isNone ||  YOk |> Option.isNone || ZOk |> Option.isNone
        ) 
        moons
    |> ignore
    let xv = xOk.Value
    let yv = YOk.Value
    let zv = ZOk.Value

    lcm xv yv |> lcm zv






    
    
    



//Check how often x repeats - 

//i x x takes 10
// and y takes 15

//Pattern should be repeated efery 30th
