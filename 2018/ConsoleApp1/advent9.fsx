

type CircleNode(value) as self = 
    let mutable left = self
    let mutable right = self
    member x.Left = left
    member x.Right = right
    member x.Value = value
    member x.SetLeft l = left <- l
    member x.SetRight r = right <- r

    member x.RemoveMarble () = 
        let right = x.Right
        let left = x.Left
        x.Left.SetRight(right)
        x.Right.SetLeft(left)
        x.SetRight(x)
        x.SetLeft(x)
        left, x, right

        
    member x.AddToRight (newValue:int) = 
        let newMarble = CircleNode(newValue)
        newMarble.SetLeft x
        newMarble.SetRight x.Right

        x.Right.SetLeft newMarble
        x.SetRight newMarble

        if x.Left = x then
            x.SetLeft newMarble
        newMarble
        

    
let rec printCircle (node:CircleNode) (startNode:CircleNode) (currentNode:CircleNode) = 
    if node = currentNode then
        printf "(%i) " node.Value
    else
        printf "%i " node.Value
    if node.Right <> startNode then
        printCircle node.Right startNode currentNode
    else
        printfn " " 

let rec locateToLeft i condition (node:CircleNode) = 
    if condition i node then node
    else 
        locateToLeft (i + 1) condition node.Left


let rec placeNextMarble turn stopCondition (currentMarble:CircleNode) (value:int) (score:Map<int, int list>) nPlayers = 
    if stopCondition turn value score then
        value, score
    else
        if value % 23 = 0 then
            let next7 = locateToLeft 0 (fun i n -> i = 7) currentMarble
            let (left, removed, right) = next7.RemoveMarble()

            //printCircle right right right
            let player = turn % nPlayers
            let currentScore = score |> Map.tryFind player |> function |Some s -> s |None -> []
            let newScore = score |> Map.add player (value :: removed.Value :: currentScore)
            
            //printCircle right right right
            placeNextMarble (turn + 1) stopCondition right (value + 1) newScore nPlayers
        else
            let current = currentMarble.Right.AddToRight(value)
            //printCircle current current current

            placeNextMarble (turn + 1) stopCondition current (value + 1) score nPlayers
        

let getHighestScore (score:Map<int, int list>) = 
    score 
    |> Map.toSeq 
    |> Seq.map (fun (player, marbles) -> player, List.sum (marbles |> List.map int64))
    |> Seq.sortByDescending snd
    |> Seq.find (fun _ -> true)

let advent9() = 
    let (value, score) = 
        placeNextMarble 1 (fun turn value players -> value = 25) (CircleNode(0)) 1 Map.empty 9

    let (value, score) = 
        placeNextMarble 1 (fun turn value players -> value = 1618) (CircleNode(0)) 1 Map.empty 10

    let (part1) = 
        placeNextMarble 1 (fun turn value players -> value = 71944) (CircleNode(0)) 1 Map.empty 423 
        |> snd |> getHighestScore


    let (part2) = 
        placeNextMarble 1 (fun turn value players -> value = 7194400) (CircleNode(0)) 1 Map.empty 423 
        |> snd |> getHighestScore 

    let part2 = getHighestScore score

    


    ()