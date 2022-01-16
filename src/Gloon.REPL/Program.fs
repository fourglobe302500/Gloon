open Gloon
open System

[<EntryPoint>]
let main argv =
  printf "Enter line: "

  Console.ReadLine()
  |> Compiler.compile
  |> printfn "%s"

  0
