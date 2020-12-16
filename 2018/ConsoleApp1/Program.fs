// Learn more about F# at http://fsharp.org

open System

//let parseInt (x:string) = System.Int32.Parse(x)

//type Point = { x: int; y : int } with override x.ToString() = sprintf "(%i, %i)" x.x x.y
//module Point = 
//    let create x y = { x = x; y = y }

//let advent6 () = 
//    let distance (a:Point) (b:Point) = abs(b.x - a.x) + abs(b.y - a.y)

//    let lines = System.IO.File.ReadAllLines("C:\Dropbox\Projects\Adventcode\ConsoleApp1\ConsoleApp1\input6.txt")
//    let cords = 
//        lines |> Array.map (fun x -> match x.Split([|","|], System.StringSplitOptions.None) with [|x;y|] -> Point.create (parseInt x) (parseInt y) |_ -> failwithf "")
        
//    let maxX = cords |> Seq.maxBy (fun p -> p.x) |> (fun p -> p.x) |> (fun x -> x+1)
//    let maxY = cords |> Seq.maxBy (fun p -> p.y) |> (fun p -> p.y) |> (fun x -> x+1)
//    let cordsAssignment = 
//        cords |> Array.toSeq |> Seq.mapi (fun i x -> x, sprintf "%s" ((char (65+i)).ToString())) |> Map.ofSeq
//    let getCordAssignment t =  cordsAssignment |> Map.find t

//    let board : Option<Point> array = Array.init (maxX * maxY) (fun x -> None)
//    for i = 0 to (maxX * maxY) - 1 do
//        let c = Point.create (i % maxX) (i/maxX)

//        let closest = 
//            cords 
//            |> Seq.map (fun t -> t, distance t c)
//            |> Seq.sortBy (fun (t, d) -> abs d)
//            |> Seq.toList
//            |> function |(_,da) :: (_,db) :: _ when da = db (*Same distance as another*) -> None
//                        |(t,_) :: _ (*Closest Target *) -> Some t
//                        |_ -> failwithf "#1"
//        board.[i] <- closest

//    let emptyAreaMap = cords |> Seq.map (fun p -> p, 0) |> Map.ofSeq
//    //For each target get if it is infinite area, if so remove it. Area is infinite if it has a spot in "the border"
//    let areaMap = 
//        board 
//        |> Array.fold 
//            (fun (i, areas) x -> 
//                let c = Point.create (i % maxX) (i/maxX)
                
//                match x with 
//                |Some closest -> 
//                    let a = areas |> Map.find closest
//                    let isInf = c.x = 0 || c.y = 0 || c.x = maxX - 1 || c.y = maxY - 1 || a = -1
//                    if a <> -1 && isInf then printfn "%s is Inf (%s)" (getCordAssignment closest) (c.ToString())
//                    let areas = areas |> Map.add closest (if isInf then -1 else a + 1)
//                    (i+1, areas)
//                |None -> (i+1, areas))
//            (0, emptyAreaMap)
//        |> snd

//    //4029 to large
//    let largestArea = 
//        areaMap |> Map.toSeq |> Seq.filter (fun (p,a) -> a > -1) |> Seq.sortByDescending (fun (p,a) -> a) |> Seq.pick Some

//    //for y = 0 to maxY - 1 do
//    //    for x = 0 to maxX - 1 do
//    //        let p = board.[x + y*maxX]
//    //        match p with 
//    //        |Some s -> 
//    //            if s = Point.create x y then
//    //                printf "%s" (getCordAssignment s) 
//    //            else
//    //                printf "%s" ((getCordAssignment s).ToLower())
//    //        |None -> printf "."
//    //    printfn ""


//    //Part 2 -
    
//    //Point is part of region if it is next to a point in the region 
//    let pointIsPartOfRegion (p:Point) (region:Point list) = 
//        region |> List.exists (fun rp -> abs (rp.x - p.x) <= 1 && abs (rp.y - p.y) <= 1)

//    let (_,regions,total) = 
//        board 
//        |> Array.fold 
//            (fun (i, regions, s) x -> 
//               let c = Point.create (i % maxX) (i/maxX)
               
//               let totalDistance = 
//                   cords
//                   |> Array.map (distance c)
//                   |> Array.sum
//               if totalDistance < 10000 then
//                   //Check if point is touching region, if not create a new region with point
//                   match regions |> List.tryFind (fun region -> pointIsPartOfRegion c region) with
//                   |Some existingRegion -> 
//                    let regions = regions |> List.map (fun r -> if pointIsPartOfRegion c r then c::r else r)
//                    (i+1, regions, s+1)
//                   |None -> 
//                    (i+1, [c] :: regions, s+1) 

//               else
//                (i+1, regions, s)

               
//            )
//            (0, [], 0)
        

//    regions |> List.sortByDescending (fun r -> r.Length) |> List.pick Some |> (fun x -> x.Length)

//    //41169 <-- to low

//    largestArea |> snd




//position=<-10378, -52421> velocity=< 1,  5>

type Vector = {
    mutable x : int
    mutable y : int
    dx : int
    dy : int
}

let splitPairBy (line:string) (by:char) = 
    match line.Split(by) with
    |[|a; b|] -> a.Trim(),b.Trim()
    |a -> failwithf "Expected pair but got %A '%s'" a line

let parseVector (str:string) = 
    let n = str.IndexOf("<")
    let ns = str.IndexOf(">", n)
    let pos = str.Substring(n+1, ns - n - 1)

    let n2 = str.IndexOf("<", ns)
    let n2s = str.IndexOf(">", n2)
    let vec = str.Substring(n2+1, n2s - n2 - 1)
    let (x,y) = splitPairBy pos ','
    let (dx,dy) = splitPairBy vec ','

    {
        x = System.Int32.Parse x
        y = System.Int32.Parse y
        dx = System.Int32.Parse dx
        dy = System.Int32.Parse dy
    }


//parseVector "position=<-10378, -52421> velocity=< 1,  5>"


let advance (vectors:Vector array) = 
    vectors 
    |> Array.iter (fun v -> v.x <- v.x + v.dx; v.y <- v.y + v.dy;)

let back (vectors:Vector array) = 
    vectors 
    |> Array.iter (fun v -> v.x <- v.x - v.dx; v.y <- v.y - v.dy;)


let printVectors time offsetX offsetY vectorLImit (str:string) (vectors:Vector array) = 
    

    let r =
        vectors 
        |> Array.fold 
            (fun s v -> 
                if v.x + offsetX >= 0 && v.y + offsetY >= 0 && v.x + offsetX < System.Console.WindowWidth && v.y + offsetY < System.Console.WindowHeight then
                    //System.Console.SetCursorPosition(v.x, v.y)
                    //System.Console.Write("x")
                    (s+1)
                else s 
            )
            0

    if r > vectorLImit then
        System.Console.Clear()
        let a = 
            vectors 
            |> Array.fold 
                (fun s v -> 
                    if v.x + offsetX >= 0 && v.y + offsetY >= 0 && v.x + offsetX < System.Console.WindowWidth && v.y + offsetY < System.Console.WindowHeight then
                        System.Console.SetCursorPosition(v.x + offsetX, v.y + offsetY)
                        System.Console.Write("x")
                        (s+1)
                    else s 
                )
                0
        ()
        System.Console.SetCursorPosition(0, 0)
        System.Console.Write(str)
    else ()

    if time % 1000 = 0 then
        System.Console.SetCursorPosition(0, 0)
        System.Console.Write("Time {0}, {1}, {2}, {3}, {4}", time, vectors.[0].x, vectors.[0].y, vectors.[0].dx, vectors.[0].dy)

    r

let advetn10 () = 
    let lines = System.IO.File.ReadAllLines("C:\Dropbox\Projects\Adventcode\ConsoleApp1\ConsoleApp1\input10.txt")
    let vectors = lines |> Array.map parseVector

    //vectors.Length

    let mutable offsetX = 0
    let mutable offsetY = 0
    let mutable auto = true
    let mutable time = 0
    
    for i = 0 to Int32.MaxValue do
        if auto then 
            advance vectors
            time <- time + 1

        if i > 10000 then
            if (printVectors i offsetX offsetY 1 (sprintf "%i" i) vectors > 1) then
                System.Console.SetCursorPosition(0, 1)
                System.Console.Write("Enter Command: {0}, {1}, time: {2}", offsetX, offsetY, time)


                let c = System.Console.ReadKey()
                match c.KeyChar with 
                |' ' -> advance vectors; time <- time + 1
                |'b' -> back vectors; time <- time + 1
                |'w' -> offsetY <- offsetY + 1
                |'s' -> offsetY <- offsetY - 1
                |'d' -> offsetX <- offsetX - 1
                |'a' -> offsetX <- offsetX + 1
                |_ -> ()

                auto <- false
            else 
                //auto <- true
                
        
    ()





[<EntryPoint>]
let main argv =
    advetn10 ()
    printfn "Hello World from F#!"
    0 // return an integer exit code
