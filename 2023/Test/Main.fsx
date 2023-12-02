#if INTERACTIVE
#load "../Common.fsx"
// #r "nuget:Expecto"
// #r "nuget:Expecto.BenchmarkDotNet"
// #r "nuget:Expecto.FsCheck"
// #r "nuget:Expecto.Hopac"
#r "nuget:Expecto.VisualStudio.TestAdapter"
#else 
module DayTest
#endif

open Expecto

let tests =
  test "A simple test" {
    let subject = "Hello World"
    Expect.equal subject "Hello World" "The strings should equal"
  }

let testsas () = 
    // runTestsWithCLIArgs [] [||] tests
    ()