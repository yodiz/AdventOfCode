#if INTERACTIVE
#load "../Common.fsx"
#endif
open AoC

let rec find dep ns i = 
    match ns |> Array.tryFind (fun n -> i % n = 0 && i > dep) with
    |Some x -> x,i
    |None -> find dep ns (i+1)
    

let solve inp = 
    let dep = inp |> Array.item 0 |> Parse.int32
    let ns = inp |> Array.item 1 |> (fun (x:string) -> x.Split(',') |> Array.choose Parse.tryInt32)

    let id, leave = find dep ns 0 
    let wait = leave - dep
    id * wait
    

let part1 = solve (load "Day13/input.txt")


let rec gcd x y = if y = 0L then abs x else gcd y (x % y)
let lcm x y = x * y / (gcd x y)


let rec find2 i (ns:(int64*int64) array) step t = 
    let succ = 
        ns 
        |> Array.map (fun (index,x) -> x, (t+index) % x = 0L)
    
    succ |> Array.forall snd
    |> function 
        |true -> t 
        |false -> 
            //Om någon stämmer, så vet vi att svaret är en multipel av lcm(step, bussen)
            // Eftersom bussen är ett primtal
            // Fler och fler kommer stämma, så vi tar max
            let step : int64 = 
                succ
                |> Array.fold (fun step (v,ok) -> if ok then max step (lcm step v) else step ) step

            find2 (i+1) ns step (t + step)


let find3 (ns:int64 option array) = 
    let a = ns |> Array.indexed |> Array.choose (fun (a,b) -> b |> Option.map (fun b -> int64 a,b))
    find2 0 a (1L) (0L)


let parse = (fun (x:string) -> x.Split(',') |> Array.map Parse.tryInt64)

let test5 = find3 (parse "17,x,13,19") //3417L
let test2 = find3 (parse "67,7,x,59,61") // 1261476
let test3 = find3 (parse "1789,37,47,1889")  

let part2 = 
    let [|_;i|] = (load "Day13/input.txt")
    find3 (parse i) 
