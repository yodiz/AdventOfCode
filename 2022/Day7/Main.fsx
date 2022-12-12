#if INTERACTIVE
#load "../Common.fsx"
#else
module Day7
#endif

open System
open AoC

let folder = __SOURCE_DIRECTORY__ + "\\"

//Current Dir, Struct

let parseLine (str:string) = 
    ()

let input  = load folder "input.txt" 

//type File = { Name : string; Size : int }
type Dir = { Parts : string list }


let hd = 
    input 
    |> Array.fold 
        (fun (curDir,str:Map<Dir, Map<string,int>>) inp -> 
            match inp with 
            |a when a.StartsWith("$ cd") -> 
                match a.Substring(5) with
                |"/" -> ([],str)
                |".." -> (curDir |> List.take ((curDir |> List.length) - 1),str)
                |d -> (List.append curDir [d],str)
            |a when a.StartsWith ("$ ls") -> (curDir,str)
            |x -> 
                if x.StartsWith("dir ") then
                    let (_dir, name) = x |> AoC.Text.split2 " "
                    let k = { Parts = List.append curDir [name]}
                    let s = str |> Map.tryFind k |> Option.defaultValue Map.empty
                    let s22 = s 
                    let s2 = str |> Map.add k s22
                    (curDir,s2)

                    //(curDir,str)
                else
                    let (Int32 size, name) = x |> AoC.Text.split2 " "
                    //let f = { Size = size; Name = name }
                    let k = { Parts = curDir }
                    let s = str |> Map.tryFind k |> Option.defaultValue Map.empty
                    let s22 = s |> Map.add name size
                    let s2 = str |> Map.add k s22
                    (curDir,s2)
        ) 
        ([],Map.empty)

let rec startsWithSame (a: _ list) (b: _ list) = 
    match a, b with 
    |[], _ -> true
    |ha::ta, hb::tb when ha = hb -> startsWithSame ta tb
    |_ -> false


let part1 = 
    let a = hd |> snd
    let b = a |> Map.toList    
    let totals = 
        b
        |> List.map 
            (fun (d,_f) -> 
                let a = 
                    b
                    |> List.filter (fun (d2, f2) -> startsWithSame d.Parts d2.Parts)
                    |> List.map snd
                    |> List.collect (fun m -> m |> Map.toList)
                    |> List.map snd
                    |> List.sum
                d,a
            )
        |> List.filter (fun (d,s) -> s <= 100000)
        |> List.sumBy snd
    totals


let part2 = 
    let a = hd |> snd
    let b = a |> Map.toList    
    let totals = 
        b
        |> List.map 
            (fun (d,_f) -> 
                let a = 
                    b
                    |> List.filter (fun (d2, f2) -> startsWithSame d.Parts d2.Parts)
                    |> List.map snd
                    |> List.collect (fun m -> m |> Map.toList)
                    |> List.map snd
                    |> List.sum
                d,a
            )
    let totalSize = 
        totals
        |> List.find (fun (d,_) -> d.Parts = [])
        |> (fun (_,x) -> x)

    let hdSize = 70000000
    let needed = 30000000
    let unused = hdSize - totalSize
    let toDelete = needed - unused

    totals
    |> List.filter (fun (d,s) -> s > toDelete)
    |> List.map snd
    |> List.sort
    |> List.head

    


//
printfn "%i" part1
printfn "%i" part2

