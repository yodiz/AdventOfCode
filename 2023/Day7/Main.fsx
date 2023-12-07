#if INTERACTIVE
#load "../Common.fsx"
#else 
module AoC
#endif

let folder = __SOURCE_DIRECTORY__ + "\\"
let input  = load folder "input.txt"
let inptest1  = load folder "test1.txt"

let parseCards (str:string) = 
    let hand, rank = Text.split2 " " str
    hand, Parse.int32 rank 

type Type = |HighCard = 0 |OnePair = 1 |TwoPair = 2 |ThreeOfAKind = 3 |FullHouse = 4 |FourOfAKind = 5 |FiveOfAKind = 6

let getType (hand:string) = 
    let xx = 
        hand.ToCharArray() |> Array.sort
        |> Array.groupBy id
        |> Array.map (fun (k,v) -> k, v.Length)
    
    let hasNCardsEx n exclude = xx |> Array.tryFind (fun (k,v) -> v = n && k <> exclude) |> Option.map fst
    let hasNCards n = xx |> Array.exists (fun (k,v) -> v = n) 
    
    if hasNCards 5 then Type.FiveOfAKind
    elif hasNCards 4 then Type.FourOfAKind
    elif hasNCards 3 && hasNCards 2 then Type.FullHouse
    elif hasNCards 3 then Type.ThreeOfAKind
    elif hasNCards 2 then  match hasNCardsEx 2 ' ' with 
                           |Some fst -> match hasNCardsEx 2 fst with 
                                        |Some c -> Type.TwoPair
                                        |None -> Type.OnePair
                           |None -> failwithf ""
    else Type.HighCard

let solve1 input = 
    let cards = ['A'; 'K'; 'Q'; 'J'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2'] |> List.rev |> List.mapi (fun i x -> x,i+2) |> Map.ofList
    input
    |> Array.map parseCards
    |> Array.map (fun (hand,rank) -> getType hand, hand, rank)
    |> Array.sortBy (fun (t,h,r) -> t, h.ToCharArray() |> Array.map (fun x -> cards |> Map.find x))
    |> Array.fold 
        (fun (i:int,s:int64) (t,h,r) -> (i+1),(int64(i*r)+s)) 
        (1,0L)
    |> snd
    

let replaceJ (hand:string) (n:char) = 
    let p = hand.IndexOf('J')
    hand.Substring(0, p) + n.ToString() + hand.Substring(p+1,hand.Length - 1 - p)

let rec translateJokers' cards (hand:string) = 
    if hand.Contains("J") then
        cards
        |> Map.toList 
        |> List.map fst
        |> List.filter (fun c -> c <> 'J')
        |> List.map (replaceJ hand)
        |> List.collect (translateJokers' cards)
    else 
        [hand]
let translateJokers cards hand =  
    translateJokers' cards hand
    |> List.map getType
    |> List.sortDescending
    |> List.head


let solve2 input = 
    let cards = ['A'; 'K'; 'Q'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2'; 'J'] |> List.rev |> List.mapi (fun i x -> x,i+2) |> Map.ofList
    input
    |> Array.map parseCards
    |> Array.map (fun (hand,rank) -> translateJokers cards hand, hand, rank)
    |> Array.sortBy (fun (t,h,r) -> t, h.ToCharArray() |> Array.map (fun x -> cards |> Map.find x))
    |> Array.fold 
        (fun (i:int,s:int64) (t,h,r) -> (i+1),(int64(i*r)+s)) 
        (1,0L)
    |> snd
let aTest = solve1 inptest1
let aTest2 = solve2 inptest1

let part1 = solve1 input 
let part2 = solve2 input
