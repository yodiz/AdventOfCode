
let increasedPower serialNr x y = 
    let rackId = x + 10
    let a = rackId * y
    let b = a + serialNr 
    let c = b * rackId
    let d = c / 100 % 10
    let e = d - 5 
    e

increasedPower 8 3 5
increasedPower 57 122 79

let addEdges (grid:(_*int) array) squareSize x y gridSize sumToAdd = 
    if squareSize = 1 then 
        snd (grid.[x + (y * gridSize)])
    else
        [1..squareSize]
        |> List.fold (fun s i -> 
                        if i = squareSize then 
                            s + snd (grid.[x + squareSize - 1  + ((y + squareSize - 1) * gridSize)])
                        else
                            s
                            +
                            snd (grid.[x + squareSize - 1 + ((y + i - 1) * gridSize)])
                            +
                            snd (grid.[x + i - 1 + ((y + squareSize - 1) * gridSize)])   
                    )
                    sumToAdd

let serialId = 6042
let grid = Array.init (300*300) 
            (fun i -> i, increasedPower serialId (i%300) (i/300))

let gridSquares minGridSize maxGridSize = 
    grid
    |> Seq.fold 
        (fun maxSquare (i, power) -> 
            let x = (i%300)
            let y = (i/300)
            let wantedmaxSize = maxGridSize |> min (299 - x) |> min (299 - y)

            Seq.init (wantedmaxSize) (fun x -> x + 1)
            |> Seq.fold
                (fun (maxSquare,lastSum) squareSize -> 
                    //Calc sum of new addition
                    let newSum = addEdges grid squareSize x y 300 lastSum

                    if squareSize >= minGridSize && newSum > snd maxSquare then
                        (((x,y,squareSize),newSum), newSum)
                    else
                        (maxSquare, newSum)    
                )
                maxSquare
        )
        (((-1,-1,-1),-1), 0)

           
        

let part1 = gridSquares 3 3 

let part2 () = 
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let a = gridSquares 1 300
    sw.Stop()
    printfn "%ims duration" sw.ElapsedMilliseconds
    a
    