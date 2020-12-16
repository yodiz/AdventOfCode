open System

let initialStateStr = "#.##.#.##..#.#...##...#......##..#..###..##..#.#.....##..###...#.#..#...######...#####..##....#..###" 
let patternsStr = """##.## => .
##... => #
..#.# => #
#.... => .
#..#. => #
.#### => .
.#..# => .
.##.# => .
#.##. => #
####. => .
..##. => .
##..# => .
.#.## => #
.#... => .
.##.. => #
..#.. => #
#..## => #
#.#.. => #
..### => #
...#. => #
###.. => .
##.#. => #
#.#.# => #
##### => #
....# => .
#.### => .
.#.#. => #
.###. => #
...## => .
..... => .
###.# => #
#...# => ."""

let parseInitState (str:string) = str.ToCharArray() |> Array.map (function '#' -> true |'.' -> false |_ -> failwithf "")
let parsePattern (str:string) = 
    str.Split([|" => "|], System.StringSplitOptions.None) 
    |> function |[|a; b|] -> (parseInitState a,b = "#") |a -> failwithf "Invalid %A" a

                                            
let initialState = parseInitState (initialStateStr)
let patterns = patternsStr.Split([|"\n"|], System.StringSplitOptions.None) |> Array.map parsePattern

let isMatch (source:bool array) (pattern:bool array) = Array.forall2 (fun a b -> a = b) source pattern
let stateToStr (s:bool array) = s |> Array.map (function |true -> "#" |false -> ".") |> String.concat ""   
let patternLength = 5

let matchPatterns (source:bool array) isLeft = 
    let source = 
        if source.Length < patternLength then 
            (* if source is shorter than pattern, assume no plants in missing source *)
            let s = 
                if isLeft then //left or right part missing
                    Array.append (Array.init (patternLength - source.Length) (fun _ -> false) ) source
                else 
                    Array.append source (Array.init (patternLength - source.Length) (fun _ -> false) )
            s
        else source
    patterns |> Array.tryPick (fun (pattern,r) -> if isMatch source pattern then Some r else None)
    |> function 
        |Some s -> s 
        |None -> 
            //Doesnt happen. But example leaves out negative parts so we set false if not found
            false
            
matchPatterns (parseInitState "#..#.") true
matchPatterns (parseInitState "#..") true
matchPatterns (parseInitState "..#") false

let rec advanceGeneration stopCondition generation (state:bool[]) indexOffset =   
    let leftIncrease = matchPatterns [|false; false; false; state.[0];state.[1]|] false
    let rightIncrease = matchPatterns [|state.[state.Length-2];state.[state.Length-1]; false;false;false|] false
    
    let middle = 
        state 
        |> Array.mapi 
            (fun i _x ->
                let sliceStart = max 0 (i-2)
                let sliceEnd = min (i+2) (state.Length-1)
                matchPatterns state.[sliceStart..sliceEnd] (i < 2)
            )

    let newState, indexOffset =         
        if leftIncrease then     
            Array.concat [[|leftIncrease|]; middle; [|rightIncrease|]], (indexOffset-1)
        else 
            //Trim Start
            let middle, indexOffset = 
                if middle.[0] then  middle, indexOffset
                else                middle.[1..], indexOffset + 1
            if rightIncrease then Array.concat [middle; [|rightIncrease|]], indexOffset
            else                  middle, indexOffset

    match stopCondition (generation+1L) state newState indexOffset with
    |Some s -> s
    |None -> 
        advanceGeneration stopCondition (generation+1L) newState (indexOffset)


//Hämta värdet för krukor, jekla lurigt formulerat!!!
let getPotValues (r) indexOffset = 
    r |> Array.fold 
            (fun (o,s) x -> if x then ((o+1), s+o) else ((o+1),s)) 
            (indexOffset,0)
    |> snd


let part1 = 
    advanceGeneration 
        (fun gen prevState state indexOffset -> 
            printfn "%i: %s" gen (stateToStr state); 
            if gen = 20L then Some (getPotValues state indexOffset) else None) 
        0L initialState 
        0

let part2 = 
    advanceGeneration 
        (fun x previousState state indexOffset -> 
            printfn "%i: %s" x (stateToStr state)

            if state = previousState then //Pattern stable
                let value = (getPotValues state indexOffset)
                let remaining = (50000000000L - x)
                let diffValue = value - (getPotValues previousState (indexOffset-1))
                let value = remaining * int64 diffValue + int64 value

                Some value           
            else
                None
        ) 
        0L initialState 
        0

