open System
open todo.validation

[<EntryPoint>]
let rec main args =  
    printf "Username: " 
    let name = Console.ReadLine()
    printfn "%s" $"Welcome {name}"
    printfn "----"
    
    mainAction name () |> ignore 

    0
