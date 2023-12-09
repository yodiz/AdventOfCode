#if INTERACTIVE
#load "../Common.fsx"
#else 
module AoC
#endif

let folder = __SOURCE_DIRECTORY__ + "\\"
let input  = load folder "input.txt"
let inptest1  = load folder "test1.txt"

let parse (s:string) = s |> Text.split " " |> Array.map Parse.int32

let rec getNext res (prevLine:int array)  = 
    let nextLine = Array.init (prevLine.Length - 1) (fun _ -> 0)
    for i = 1 to prevLine.Length - 1 do 
        nextLine[i-1] <- prevLine[i] - prevLine[i-1]
    if nextLine |> Array.forall (fun x -> x = 0) then
        res |> List.rev
    else 
        getNext (nextLine :: res) nextLine
let getNextLine line = 
    line::(getNext [] line )
    
let extendRight (lines:List<array<int>>) = 
    lines 
    |> List.rev
    |> List.fold 
        (fun (newLines:array<int> list) nums -> 
            let nextNum = 
                match newLines with 
                |[] -> 
                    nums[0]
                |prevLine::rest -> 
                    nums[nums.Length-1] + prevLine[prevLine.Length-1]           
            (Array.append nums [|nextNum|]) :: newLines
        )
        []   
    
let extendLeft(lines:List<array<int>>) = 
    lines |> List.rev
    |> List.fold 
        (fun (newLines:array<int> list) nums -> 
            let nextNum = 
                match newLines with 
                |[] -> 
                    nums[0]
                |prevLine::rest -> 
                    nums[0] - prevLine[0]           
            (Array.append [|nextNum|] nums) :: newLines
        )
        []   


getNextLine [|10;13;16 ;21 ;30 ;45|] 
|> extendRight
|> extendLeft


let solve1 input = 
    input
    |> Array.map parse
    |> Array.map (getNextLine >> extendRight)
    |> Array.map (fun x -> x.Head[x.Head.Length - 1] )
    |> Array.sum
    
let solve2 input = 
    input
    |> Array.map parse
    |> Array.map (getNextLine >> extendLeft)
    |> Array.map (fun x -> x.Head[0] )
    |> Array.sum


Test.equal "Test1" 114 (solve1 inptest1)
let aTest = solve1 inptest1

let part1 = solve1 input 
let part2 = solve2 input
