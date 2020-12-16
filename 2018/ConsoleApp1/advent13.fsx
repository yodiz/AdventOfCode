type Track = 
    |Empty
    |Horizontal
    |Vertical
    |Intersection
    |TurnUpLeft_DownRight // - /
    |TurnUpRight_DownLeft // - \

type Direction = Left|Right|Up|Down
type Cart = { Direction : Direction; x : int; y : int; intersections : int  }


let input = System.IO.File.ReadAllLines("""C:\Users\se-mikkjel-01\Dropbox\Projects\Adventcode\ConsoleApp1\ConsoleApp1\advent13.txt""")
let parseLine (y:int) (str:string) = 
    str.ToCharArray() 
    |> Array.fold (fun (i, carts, tracks) x -> 
        let track, cart = 
            match x with 
                      |' ' -> Track.Empty, None
                      |'|' -> Track.Vertical, None
                      |'-' -> Track.Horizontal, None
                      |'+' -> Track.Intersection, None
                      |'/' -> Track.TurnUpLeft_DownRight, None
                      |'\\' -> Track.TurnUpRight_DownLeft, None
                      |'>' -> Track.Horizontal, (Some { Direction = Right; x = i; y = y; intersections = 0 } )
                      |'<' -> Track.Horizontal, (Some { Direction = Left; x = i; y = y; intersections = 0 } )
                      |'v' -> Track.Vertical, (Some { Direction = Down; x = i; y = y; intersections = 0 } )
                      |'^' -> Track.Vertical, (Some { Direction = Up; x = i; y = y; intersections = 0 } )
                      |a -> failwithf "Got %A, not defined" a
        
        (i+1, (match cart with |Some c -> c :: carts |None -> carts), (Array.append tracks [|track|]))
        )
        (0, [], [||])
        

let (width, height, carts', track') = 
    input |> Array.fold 
        (fun (maxWidth,i, carts, track) x ->  
            let width, newCarts, newTrack = parseLine i x
            ((max maxWidth width),i+1, List.append carts newCarts, newTrack::track)
        ) 
        (-1,0,[], [])

let track = track' |> List.rev |> Array.ofList
let carts = carts' |> List.indexed 

///Move cart in direction
let moveCarts (carts:(int*Cart) list) = 
    let sortedCarts = 
        carts 
        |> List.sortBy(fun (i,x) -> x.y, x.x)

    let movedCarts = 
        sortedCarts
        |> List.fold 
            (fun (collisions,iterMovedCarts) (cartIndex,c) -> 
                if collisions |> List.exists (fun (i,_c) -> i = cartIndex) then
                    //Has been colided - sopigt, vi borde ha en rekursiv funktion istl som tar en lsita som input 
                    // där den processerar nästa head eller något 
                    (collisions,iterMovedCarts) 
                else
                //Move cart in direction, if it hits a intersection, turn according to rule
                let (x,y) = match c.Direction with 
                            |Right -> c.x+1,c.y 
                            |Left -> c.x-1,c.y 
                            |Down -> c.x,c.y+1 
                            |Up -> c.x,c.y-1
            
                let nextTrack = track.[y].[x]
                let turnedCart = 
                    match nextTrack with
                    |Track.Empty -> failwithf "Derailed..."
                    |Track.Vertical |Track.Horizontal -> c
                    |Track.TurnUpLeft_DownRight -> // - /
                        match c.Direction with 
                        |Up -> { c with Direction = Right }
                        |Left -> { c with Direction = Down }
                        |Down -> { c with Direction = Left }
                        |Right -> { c with Direction = Up }
                    |Track.TurnUpRight_DownLeft -> // - \
                        match c.Direction with 
                        |Up -> { c with Direction = Left }
                        |Left -> { c with Direction = Up }
                        |Down -> { c with Direction = Right }
                        |Right -> { c with Direction = Down }
                    |Track.Intersection -> 
                        //it turns left the first time, goes straight the second time, turns right
                        match c.intersections % 3 with 
                        |0 -> 
                            match c.Direction with 
                            |Up -> { c with Direction = Left; intersections = c.intersections + 1 }
                            |Left -> { c with Direction = Down; intersections = c.intersections + 1 }
                            |Down -> { c with Direction = Right; intersections = c.intersections + 1 }
                            |Right -> { c with Direction = Up; intersections = c.intersections + 1 }
                        |1 -> { c with intersections = c.intersections + 1 }
                        |2 -> 
                            match c.Direction with 
                            |Up -> { c with Direction = Right; intersections = c.intersections + 1 }
                            |Left -> { c with Direction = Up; intersections = c.intersections + 1 }
                            |Down -> { c with Direction = Left; intersections = c.intersections + 1 }
                            |Right -> { c with Direction = Down; intersections = c.intersections + 1 }
                        |_ -> failwithf "Not possible"
                let movedAndTurned = { turnedCart with x = x; y = y } 

            
                //Check for collision
                let thisCollided, collided, livingCarts = 
                    iterMovedCarts
                    |> List.fold 
                        (fun (thisCollided,colided,carts) (i2, c2) -> 
                            if i2 = cartIndex then (thisCollided, colided, carts)
                            else
                                let collides = i2 <> cartIndex && x = c2.x && y = c2.y
                                if collides then  (true, ((i2, c2)::colided), carts)
                                else              (thisCollided, colided,( i2, c2)::carts)
                        )
                        (false, collisions, [])

                if thisCollided then
                    (cartIndex, movedAndTurned) :: collided, livingCarts
                else
                    collided, (cartIndex, movedAndTurned) :: livingCarts
            )
            ([], sortedCarts)


    movedCarts
    
let part1 () = 
    Seq.initInfinite id
    |> Seq.fold  
        (fun carts x -> 
            let (coll, carts) = moveCarts carts 
            if (coll.Length > 0) then failwithf "Collision at %i,%i" (snd coll.Head).x (snd coll.Head).y
            (carts)
        )
        carts

let part2 () = 
    Seq.initInfinite id
    |> Seq.fold  
        (fun carts x -> 
            let (coll, carts) = moveCarts carts
            match coll with |[] -> () |a -> printfn "Colided %i, remaining: %i" a.Length carts.Length 
            if carts.Length = 1 then 
                failwithf "One left at %A" carts.Head
            elif carts.Length = 2 then 
                failwithf "Two left?? at %A" carts.Head
            else ()
            (carts)
        )
        carts



