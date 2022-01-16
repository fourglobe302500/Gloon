open Gloon
open System

[<EntryPoint>]
let main argv =
  while true do
    printf "Enter line: "

    Console.ReadLine()
    |> Compiler.compile
    |> printfn "%A"

  0
