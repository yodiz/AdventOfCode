

type Tile = |Wall |Empty
type UnitType = |Elf|Goblin
type Unit = {
    UnitType : UnitType
    mutable x : int
    mutable y : int
    mutable hp : int
}
    with member x.dead = x.hp <= 0


let parseLine (y:int) (str:string) = 
    let t = str.Split(' ')
    let str = t.[0]
    str |> Seq.mapi 
        (fun x t -> match t with 
                    |'#' -> Tile.Wall, None |'.' -> Tile.Empty, None
                    |'E' -> Tile.Empty, Some { UnitType = UnitType.Elf; hp = 200; x=x; y=y }
                    |'G' -> Tile.Empty, Some { UnitType = UnitType.Goblin; hp = 200; x=x; y=y }
                    |a -> failwithf "%A not supported in '%s'" a str)
    |> Seq.toArray
let parseBoard (lines:string array) = 
    let (_,t, u) = 
        lines |> Array.fold 
            (fun (i, tiles, units) x -> 
                let a = parseLine i x
                let t  = a |> Array.map fst
                let u = a |> Array.fold (fun s (_,x) -> match x with |None -> s |Some x -> x :: s) units
                (i+1, t::tiles, u)
            ) (0, [], [])
    t |> List.rev |> Array.ofList, u |> Array.ofList
   

let rec yieldNextUntil from (x,y) rendered target path = 
    if (x,y) = target then
        path
    else
        let (nx,ny,ni) = 
            [
                rendered |> Map.tryFind (x-1,y)
                rendered |> Map.tryFind (x+1,y)
                rendered |> Map.tryFind (x,y-1)
                rendered |> Map.tryFind (x,y+1)
            ]
            |> List.choose id
            |> List.filter (fun (x2,y2,i) -> (x2,y2) <> from)
            |> List.groupBy (fun (_,_,i) -> i)
            |> List.minBy fst
            //if equal - we choulse prioriotize UP/LEFT
            |> (fun (k,v) -> v |> List.sortBy (fun (x,y,i) -> y,x) |> List.pick Some)

            

            //|> List.minBy (fun (x,y,i) -> i)
        yieldNextUntil (x,y) (nx,ny) rendered target ((nx,ny) :: path)

//https://en.wikipedia.org/wiki/Pathfinding
let rec samplePathFindNext (paths) obstacles current target = 
    //let (x,y,i:int) = paths |> List.item current
    match (paths |> List.tryItem current) with
    |None -> None //No path
    |Some (x,y,i:int) -> 
        if (x,y) = target then 
            let rendered = 
                paths 
                |> List.groupBy (fun (x,y,i) -> x,y)
                |> List.map (fun (k, v) -> k, v |> List.map (fun (x,y,a) -> x,y,a) |> List.min)
                |> List.map (fun ((x,y),i) -> (x,y),i)
                |> Map.ofList

            let (tx,ty,ti) = paths |> List.item 0
            yieldNextUntil (-1,-1) (x,y) rendered (tx,ty) [(x,y)] 
            |> Some
        elif current > 255 then
            None
        else
            let nextFour = [(x-1,y,i+1);(x+1,y,i+1);(x,y-1,i+1);(x,y+1,i+1)]
            let ok = 
                nextFour
                |> List.choose 
                    (fun (x,y,i) -> 
                        let betterExits = paths |> List.exists (fun (x2,y2,i2) -> x2=x && y2=y && i2 <= i)
                        if obstacles |> Set.contains(x,y) || betterExits then None
                        else Some (x,y,i)
                    )
            //printfn "%i" current
            samplePathFindNext (List.append paths ok)  obstacles (current+1) target

type PathFound = {
    Segments : (int*int) list
}

type Path = |NotFound |Found of PathFound


let findPath (sourceX, sourceY) target obstacles = 
    samplePathFindNext [sourceX, sourceY, 0] obstacles 0 target
    |> function |Some s -> Path.Found { Segments = s } |None -> Path.NotFound

let apa = [(3,3)] |> Set.ofList
findPath (3,4) (3,2) apa 


let getAttackable (x:Unit) units =
    units 
    |> Array.filter 
        (fun u -> u.UnitType <> x.UnitType && 
                  not u.dead &&
                   (
                       u.x = x.x - 1 && u.y = x.y
                    || u.x = x.x + 1 && u.y = x.y
                    || u.x = x.x && u.y = x.y - 1
                    || u.x = x.x && u.y = x.y + 1
                   )
        )
    |> Array.sortBy (fun x -> x.hp, x.y, x.x)
    |> Array.tryPick Some

let handleUnit boardBlockedSpots (x:Unit) units = 
    if x.dead then ()
    else
        let toAttack = getAttackable x units

        //Move
        match toAttack with
        |Some inRange -> ///if target is in range, noop
            ()
            
        |None -> 
            //Find spots to attack from 
            let attackingSpots = 
                units 
                |> Array.filter (fun e -> e.UnitType <> x.UnitType && not e.dead)
                |> Array.collect (fun e -> [|(e.x-1,e.y); (e.x+1,e.y); (e.x,e.y-1); (e.x,e.y+1)|])
                |> Set.ofArray
            let getBlockedSpots = units |> Array.filter (fun u -> not u.dead) |> Array.map (fun u -> u.x,u.y) |> Set.ofArray
                    
            let blocking = (Set.union boardBlockedSpots getBlockedSpots)
            let freeSpots = 
                Set.difference attackingSpots blocking

            let paths = 
                freeSpots 
                |> Set.toList
                |> List.choose (fun (loc) -> match findPath (x.x, x.y) loc blocking with 
                                             |Path.NotFound -> None
                                             |Path.Found x -> Some x)
                        
            if List.isEmpty paths then //No movement possible - noop
                //printfn "%i %i %A can not move" x.x x.y x.UnitType
                ()
            else
                let (minLen, paths) = 
                    paths 
                    |> List.groupBy (fun x -> x.Segments.Length)
                    |> List.minBy fst
                        

                let choosenPath = 
                    paths 
                    |> List.sortBy (fun (a) -> 
                                        let (x,y) = a.Segments |> List.item (a.Segments.Length - 1)
                                        y,x
                                        )
                    |> List.pick Some

                let moveToX, moveToY = 
                    match choosenPath.Segments with 
                    |current :: wanted :: rest -> 
                        //printfn "Current: %A, Wanted %A, Rest: %A" current wanted rest
                        wanted
                    |a -> failwithf "Not expected %A (MinLength %i)" a minLen
                       

                //printfn "%i %i %A move to %i %i" x.x x.y x.UnitType moveToX moveToY
                //printfn "Path: %A, AttackSpots: %A" choosenPath.Segments attackingSpots
                //units 
                //|> Array.iter (fun u -> printfn "%i %i %A" u.x u.y u.UnitType)
                //printfn "Units: %s" (units |> Array.map (fun u -> sprintf "('%A' %i %i %i %A)" u.UnitType u.x x.y u.hp u.dead) |> String.concat ", ")
                x.x <- moveToX
                x.y <- moveToY
                ()                        
            () 

        let toAttack = getAttackable x units
                    
        match toAttack with 
        |Some target -> 
            target.hp <- target.hp - 3
        |None -> ()


let printBoard (board:Tile array array) (units:Unit array) = 
    let units =
        units |> Array.filter (fun x -> x.dead |> not) 
              |> Array.map (fun x -> (x.x,x.y), x.UnitType)
              |> Map.ofArray
    board 
    |> Array.mapi 
        (fun y t -> 
            t |> Array.mapi (fun x t -> 
                                match units |> Map.tryFind (x,y) with 
                                |Some UnitType.Elf -> "E"
                                |Some UnitType.Goblin -> "G"
                                |None ->     
                                    match t with |Tile.Wall -> "#" 
                                                 |Tile.Empty -> "."  )
            |> String.concat ""
        )
    |> Array.iter (fun s -> printfn "%s" s)


let rec tick tickCount (board:Tile array array) units = 
    let boardBlockedSpots = 
        board |> Array.mapi (fun y c -> c |> Array.mapi (fun x t -> if t = Tile.Wall then Some (x,y) else None))
        |> Array.collect id |> Array.choose id
        |> Set.ofArray

    units |> Array.sortInPlaceBy (fun x -> x.y, x.x)

    units
    |> Array.iter (fun x -> handleUnit boardBlockedSpots x units)

    //Check win condition
    let (elfs, goblins) = 
        units 
        |> Array.filter (fun u -> not u.dead)
        |> Array.partition (fun u -> u.UnitType = UnitType.Elf)
    
    let elfsHp = elfs |> Array.fold (fun s u -> s + u.hp) 0
    let goblinsHp = goblins |> Array.fold (fun s u -> s + u.hp) 0

    printfn "%i - elfs: %i, Goblins: %i" tickCount elfsHp goblinsHp
    printBoard board units


    if elfsHp = 0 || goblinsHp = 0 then
        tickCount, elfsHp, goblinsHp, (max elfsHp goblinsHp) * tickCount
    else
        //System.Threading.Thread.Sleep(100)
        tick (tickCount + 1) board units


let input = """################################
##############..###G.G#####..###
#######...#####........#.##.####
#######..G######.#...........###
#######..G..###.............####
########.GG.##.G.##.......E#####
##########........#........##..#
##############GG...#...........#
##############.....#..........##
#G.G...####....#G......G.#...###
#G..#..##........G.........E.###
#..###...G#............E.......#
#...G...G.....#####............#
#....#....#G.#######...........#
#.##....#.#.#########.#..#...E.#
####...##G..#########.....E...E#
#####...#...#########.#.#....E##
#####.......#########.###......#
######......#########...######.#
########.....#######..#..#######
########......#####...##.#######
########............E.##.#######
####.........##......##..#######
####....#..E...E...####.########
####.....#...........##.########
#####....##.#........###########
#####.....#####....#############
#####.#..######....#############
####..######....################
####..###.#.....################
####...##...####################
################################"""

let board,units = input.Split([|"\n"|], System.StringSplitOptions.RemoveEmptyEntries) |> parseBoard   
printBoard board units
//let (ticks, elfHp, goblinHp, score) = tick 0 board units

//222831 <-- right
//220320 to low
//223074 to high


//2754 * 79
//2754 * 80



//parseLine 0 "#.G.E.#"

//units 
//|> Array.iter (fun u -> printfn "%i %i %A %i" u.x u.y u.UnitType u.hp)