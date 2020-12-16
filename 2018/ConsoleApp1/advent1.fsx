let advent1() = 
    let lines = System.IO.File.ReadAllLines("C:\Dropbox\Projects\Adventcode\ConsoleApp1\ConsoleApp1\input1.txt")

    let freq = lines |> Array.map (System.Int32.Parse)

    let res1 = freq |> Array.sum

    Seq.initInfinite (fun x -> freq)
    |> Seq.collect id
    |> Seq.fold 
        (fun (currentFreq, usedFreq) hz ->        
          let newFreq = currentFreq+hz  
          let newFreqMap = 
            usedFreq 
                |> Map.add newFreq 
                        (usedFreq |> Map.tryFind newFreq |> function |Some s -> failwithf "%i found for %inth time" newFreq (s+1); s + 1 |None -> 1)

          newFreq, newFreqMap
        ) 
        (0, Map.empty)


let advent2() = 
    let findTwoAndThrees (line:string) = 
        let m = 
            line.ToCharArray() 
            |> Seq.groupBy id |> Seq.map (fun (l, c) -> l, c |> Seq.length)
            |> Seq.map (fun (c, d) -> d, c)
            |> Map.ofSeq
        m |> Map.containsKey 2, m |> Map.containsKey 3



    //findTwoAndThrees "aa"

    let lines = System.IO.File.ReadAllLines("C:\Dropbox\Projects\Adventcode\ConsoleApp1\ConsoleApp1\input2.txt")
    let twos, threes = 
        lines
        |> Array.fold 
            (fun (twos, threes) line -> 
                let (hasTwo, hasThree) = findTwoAndThrees line

                ((if hasTwo then twos + 1 else twos), (if hasThree then threes + 1 else threes))
            )
            (0,0)

    let part1 = twos * threes

    //For each line, check every other line for one that diffs "by one"
    // If they diff by just one, remove the one char and return the rest of the string
    let stringsOffBy (a:string) (b:string) = 
        Array.fold2
            (fun diff (a:char) (b:char) -> diff + (if a = b then 0 else 1) ) 0
            (a.ToCharArray()) (b.ToCharArray())

    let q = 
        lines
        |> Array.map 
            (fun lineA -> 
                let a = 
                    lines 
                    |> Array.choose
                        (fun lineB -> 
                            let offBy = stringsOffBy lineA lineB
                            //printfn "%s - %s - %i" lineA lineB offBy
                            if offBy = 1 then
                                printfn "FOUND"
                                Some (lineA, lineB) else None)
                if a.Length > 0 then
                    printfn "%A" a
                ()

                //xretqmmonskvzupalfiwhjcfdb
                //xretqmmonskvzupalfiwhcfdb

            )
    

    ()


type Rectangle = {
    x : int
    y : int
    w : int
    h : int
}
 with member x.right = x.x+x.w
      member x.left = x.x
      member x.top = x.y
      member x.bottom = x.y+x.h
    



let advent3() = 
    let overlapArea (a:Rectangle) (b:Rectangle) = 
        let x_overlap = max 0 ((min a.right b.right) - (max a.left b.left))
        let y_overlap = max 0 ((min a.bottom b.bottom) - (max a.top b.top))
        let overlapArea = x_overlap * y_overlap;
        overlapArea
        

    let parseInt = System.Int32.Parse 
    let splitPairBy (line:string) (by:char) = 
        match line.Split(by) with
        |[|a; b|] -> a.Trim(),b.Trim()
        |a -> failwithf "Expected pair but got %A '%s'" a line
    let parseRectangle (line:string) = 
        let (id, rest) = splitPairBy line '@'
        let (coord, size) = splitPairBy rest ':'
        let (x,y) = splitPairBy coord ','
        let (w,h) = splitPairBy size 'x'
        let x = parseInt x
        let y = parseInt y
        let w = parseInt w
        let h = parseInt h
        id,
        { x = x; y = y; w = w; h = h }

    let rects = System.IO.File.ReadAllLines("C:\Dropbox\Projects\Adventcode\ConsoleApp1\ConsoleApp1\input3.txt") |> Array.map parseRectangle

    let maxX = rects |> Array.map (fun (_,x) -> x.right) |> Array.max
    let maxY = rects |> Array.map (fun (_,x) -> x.bottom) |> Array.max

    //Foreach rect, check overlapping area with all others

    let fabric = Array.init (maxX * maxY) (fun _ -> 0)
    
    rects
    |> Array.iter 
        (fun (_,r) -> 
            //Mark each point in fabrix as used that rect use
            for x = r.left to r.right - 1  do
                for y = r.top to r.bottom - 1 do
                    let i = x + y * maxX
                    fabric.[i] <- fabric.[i] + 1
                    ()
        )
    
    let part1 = fabric |> Array.where (fun x -> x > 1) |> Array.length
    
    let part2 = 
        rects
        |> Array.where 
            (fun (id, r) -> 
                let isOk = rects |> Array.forall (fun (id2, r2) -> id = id2 || overlapArea r r2 = 0)
                isOk
            )

    ()

type Event = |ShiftStart of int |Sleep |WakeUp

type Entry = 
    {
        Time : System.DateTime
        Event : Event
    }

let advent4() = 
    let parseEntry (line:string) = 
        let dateStr = line.Substring(1, 16)
        let rest = line.Substring(18)
        let date = System.DateTime.Parse(dateStr)
        let event = 
            if rest.StartsWith(" falls") then Event.Sleep
            elif rest.StartsWith(" wakes") then Event.WakeUp
            elif rest.StartsWith(" Guard") then 
                let hash = rest.Substring(8)
                let space = hash.IndexOf(" ")
                let worker = hash.Substring(0, space) |> System.Int32.Parse
                Event.ShiftStart worker
            else failwithf "a"
        { Time = date; Event = event }
    parseEntry "[1518-11-01 00:04] Guard #449 begins shift"
    parseEntry "[1518-09-09 00:47] falls asleep"
    parseEntry "[1518-10-22 00:49] wakes up"
        
    let lines = System.IO.File.ReadAllLines("C:\Dropbox\Projects\Adventcode\ConsoleApp1\ConsoleApp1\input4.txt") |> Array.map parseEntry


    let (_,sleepLog,_) = 
        lines 
        |> Array.sortBy (fun x -> x.Time)
        |> Array.fold 
            (fun (currentWorker, sleepMap:Map<int, _>, sleepMinute) entry -> 
                match entry.Event with 
                |ShiftStart worker -> worker, sleepMap, -1
                |Sleep -> currentWorker, sleepMap, entry.Time.Minute
                |WakeUp -> 
                    //Note that guards count as asleep on the minute they fall asleep, and they count as awake on the minute they wake up.
                    let sleepingMinutesPerDay = entry.Time.Date, [sleepMinute..entry.Time.Minute-1]
                    let sleptFor = entry.Time.Minute - sleepMinute
                    //if currentWorker = 10 then
                    //    printfn "%i slept for %i minutes" currentWorker sleptFor
                    let newSleepMap = 
                        sleepMap
                        |> Map.add currentWorker 
                            (sleepMap |> Map.tryFind currentWorker 
                                      |> function |None -> sleptFor, [sleepingMinutesPerDay] 
                                                  |Some (m,s) -> m+sleptFor, (sleepingMinutesPerDay :: s))
                    currentWorker, newSleepMap, -1
            )
            (-1, Map.empty, -1)

    //Guard and minute the guards sleep on
    let guardSleepMinutes = 
        sleepLog
        |> Map.toSeq
        |> Seq.collect (fun (guard,(sleepMinutes, x)) -> x |> List.collect (fun (d, m) -> m |> List.map (fun im -> guard, im)))
        |> Seq.groupBy fst
        |> Seq.map (fun ((guard), n) -> (guard), n |> Seq.toList)
        |> Seq.toList
        |> Map.ofList
        

    let (mostSleepingGuard, _slept) = 
        sleepLog |> Map.toSeq 
        |> Seq.sortByDescending (fun (guard, (slept, _)) -> slept) |> Seq.pick Some
        |> (fun (guard, (slept, _)) -> guard, slept )
        
    let vurnableMinute = guardSleepMinutes |> Map.find mostSleepingGuard |> List.countBy snd |> List.sortByDescending snd |> List.head |> fst

    let part1 = mostSleepingGuard * vurnableMinute

    let guardAndMostSleepingMinute = 
        guardSleepMinutes
        |> Map.toList
        |> List.map (fun (guard, x) -> guard, x |> List.countBy snd |> List.sortByDescending snd |> List.head)

    let (guard, (minute, sleptFreq)) = 
        guardAndMostSleepingMinute
        |> List.sortByDescending (fun (guard, (minute, freq)) -> freq)
        |> List.head

    let part2 = guard*minute
        
    ()


let advent5 () = 
    let isReactable (a:char) (b:char) = 
        System.Char.ToLower(a) = System.Char.ToLower(b) && 
        a <> b
    let rec react (str:string) = 
        let reacted = 
            seq {
                let mutable i = 0
                while i < str.Length do
                    let current = str.[i]
                    if i = str.Length - 1 then //Last char
                        yield current
                    else
                        let next = str.[i+1]
                        if isReactable current next then
                            i <- i + 1
                        else
                            yield current
                    i <- i + 1
            }
        let newStr = System.String(reacted |> Seq.toArray)
        if newStr <> str then react newStr else newStr

    let line = System.IO.File.ReadAllText("C:\Dropbox\Projects\Adventcode\ConsoleApp1\ConsoleApp1\input5.txt")
    let part1 = (react line).Length

    let part2 = 
        [1..26] |> List.map (fun x -> char (x + 64) |> string)
        |> List.map 
            (fun pol -> 
                let r = react (line.Replace(pol, "").Replace(pol.ToLower(), ""))
                r.Length
            )
        |> List.min

    ()
    //Scan line and find first reacting things. 

type Rule = {
    ReqOp : char
    Op : char
}

let advent7() = 
    //Step C must be finished before step A can begin.
    let parseRequirement (str:string) = 
        let first = str.Substring(5,1).Chars 0
        let second= str.Substring(36,1).Chars 0
        { ReqOp = first; Op = second } 
    //parseRequirement "Step C must be finished before step A can begin."

    let rec getNextChar (rules:Rule list ) (opsToRun:(char*int) list) executedOps workSecs = 
        match opsToRun with 
        |[] -> executedOps |> List.rev, workSecs
        |a -> 
            //get first op that has no requirements - sorted by op value
            let newOpsToRun, completedOps, _ = 
                opsToRun
                |> List.sort
                |> List.fold //Find first op that satisfies its rules
                    (fun (opsToKeepRunning, completedOps, remaingSecs) (op, remainingworkInSec) -> 
                        let rulesForOp = rules |> List.filter (fun x -> x.Op = op)
                        let satisfiesRules = rulesForOp |> List.forall (fun rule -> executedOps |> List.contains rule.ReqOp)

                        if satisfiesRules && remaingSecs > 0 then
                            if remainingworkInSec - 1 = 0 then
                                //Work done
                                opsToKeepRunning, op :: completedOps, remaingSecs
                            else
                                (op, remainingworkInSec - 1) :: opsToKeepRunning, completedOps, (remaingSecs - 1)
                        else
                            //No changes
                            (op, remainingworkInSec) :: opsToKeepRunning, completedOps, remaingSecs
                    )
                    ([], [], 5)
            getNextChar (rules) newOpsToRun (executedOps |> List.append completedOps) (workSecs+1)

    let rules = System.IO.File.ReadAllLines("c:\Users\se-mikkjel-01\Dropbox\Projects\Adventcode\ConsoleApp1\ConsoleApp1\input7.txt") |> Array.map parseRequirement
    let opsToRun = 
        rules |> Array.collect (fun x -> [|x.Op;x.ReqOp|]) |> Array.distinct |> Array.toList
        |> List.map (fun op -> 
                        let durationInSec = 60 + (int op - 64); 
                        op, durationInSec)
        

    let a = getNextChar (rules |> Array.toList) opsToRun [] 0
    let part1 = 
        a |> fst
        |> List.map (string) |> String.concat ""

    let part2 = a |> snd
    
    
    

    ()
    //Sort value = charValue *  


type Node = {
    Children : Node list
    ChildrenN : int
    Metadata : int list
}

let advent8 () = 
    let line = System.IO.File.ReadAllText("C:\Dropbox\Projects\Adventcode\ConsoleApp1\ConsoleApp1\input8.txt")
    let src = line.Split(' ') |> Array.map (System.Int32.Parse) |> List.ofArray

    let rec readTree (numbers:int list) : (int list)*(Node) = 
        match numbers with 
        |children :: metadata :: rest -> 
            let (rest, childNodes) = 
                List.init children id
                |> List.fold 
                    (fun (rest, trees) _x ->  
                        let (rest, tree) = readTree rest
                        (rest, (tree :: trees))
                    )
                    (rest, [])
            
            let (rest, values) = 
                List.init metadata id
                |> List.fold 
                    (fun (rest, values) x -> 
                        match rest with 
                        |a :: rest -> rest, (a :: values)
                        |_ -> failwithf "Expect a value"
                    ) 
                    (rest, [])
            
            let node = 
                { 
                    Children = childNodes |> List.rev
                    ChildrenN = children
                    Metadata = values
                }
            (rest, node)
        |_ -> failwithf "expect at least two numbers"

    let (_, baseNode) = readTree src

    let rec walkNodes (n:Node) fn s = 
        let s1 = n.Children |> List.fold (fun s x -> walkNodes x fn s) s
        let s2 = fn s1 n
        s2
    let part1 = walkNodes baseNode (fun sum n -> sum+(n.Metadata |> List.sum)) 0
    let rec getNodeValue (n:Node) = 
        if n.ChildrenN = 0 then
            n.Metadata |> List.sum
        else
            let indexValues = 
                n.Metadata
                |> List.map 
                    (fun m ->
                        if m > n.ChildrenN then //Index outside of children - invalid
                            0
                        else 
                            getNodeValue (n.Children |> List.item (m-1))
                    )
                |> List.sum
            indexValues

    
    let part2 = getNodeValue baseNode
    ()

        


