﻿#if INTERACTIVE
#load "../Common.fsx"
#else
module Dayx
#endif

open System
open AoC

let folder = __SOURCE_DIRECTORY__ + "\\"

let parseLine (str:string) = 
    ()

let input  = load folder "input.txt" |> Array.map parseLine

let part1 = 0

let part2 = 0
