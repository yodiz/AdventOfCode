let createNewRecepie (elfesCurrent:int array) (recepies:int array) generatedToIndex = 
    let newSum = elfesCurrent |> Array.fold (fun s x -> s + recepies.[x]) 0
    //printfn "Test"
    let newIndex = 
        if newSum >= 10 then 
            recepies.[generatedToIndex] <- newSum/10 
            recepies.[generatedToIndex+1] <- newSum%10 
            generatedToIndex+2
        else 
            recepies.[generatedToIndex] <- newSum 
            generatedToIndex+1

    let newCurrent = 
        elfesCurrent 
        |> Array.mapi 
            (fun e i -> 
                let a = i + (recepies.[i] + 1)
                let b = a % newIndex
                b
            )
    newCurrent, newIndex


let formatStr (elfesCurrent:int array) (start:int array) toIndex =
    start.[0..toIndex - 1]
    |> Seq.mapi 
        (fun i x -> 
            match elfesCurrent |> Array.tryFindIndex ((=)i) with
            |Some 0 -> sprintf "(%i)" x
            |Some i -> sprintf "[%i]" x
            |None ->   sprintf " %i " x
        ) 
    |> String.concat ""




let rec next recepies endReq (c,index) = 
    let (c2,newIndex) = createNewRecepie c recepies index; 
    //formatStr c2 recepies newIndex |> printfn "%s"
    if newIndex > endReq + 10 then
        let score = recepies.[endReq..endReq+9] |> Array.map (sprintf "%i") |> String.concat ""
        score,recepies
    else
        next recepies endReq (c2,newIndex)

let getScore elves recpStart endAt = 
    let recepies = Array.init (endAt + 15) (fun _ -> 0)
    recpStart |> Array.iteri (fun i x -> recepies.[i] <- x)

    let score = next recepies endAt (elves,recpStart.Length)
    score
    
//getScore [|0;1|] [|3;7|] 9
//getScore [|0;1|] [|3;7|] 5
//getScore [|0;1|] [|3;7|] 18
//getScore [|0;1|] [|3;7|] 2018
let input = 209231
let part1,arr = getScore [|0;1|] [|3;7|] input
let inputArr = input.ToString() |> Seq.map (fun x -> System.Int32.Parse(string x)) |> Seq.toArray


let _,arr2 = getScore [|0;1|] [|3;7|] 30000000

let part2 = 
    arr2
    |> Array.fold 
        (fun s x -> 
            if s > inputArr.Length then
                let (matchFound, _) = 
                    inputArr 
                        |> Array.fold 
                            (fun (state,i) x -> 
                                if state then 
                                    let correct = x = arr2.[s - inputArr.Length + i]
                                    correct, (i+1)
                                else
                                    state,(i+1)
                            ) 
                            (true,0)
                if matchFound then
                    failwithf "Foudn at %i" (s - inputArr.Length)
                (s+1)
            else
                (s+1)
        ) 
        0
