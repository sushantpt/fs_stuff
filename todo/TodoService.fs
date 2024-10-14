namespace todo

open System

module todoFunctionality = 

    type Progressbar = { percent : int } 
                        with
                        static member Create(percent : int) =
                         match percent with 
                            | p when p  > 100 -> failwith "Percent cannot be more than 100"
                            | p when p < 0 -> failwith "Percent cannot be -ve."
                            | p -> percent = p 

    type todo = {
        id : int
        belongsTo : string
        name : string
        description : string
        createdOn : DateTime
        hasCompleted : bool
        progress : Progressbar
        abandon : bool
    }

    type todos = list<todo>

    let mutable todo_db : list<todo> = []
    
    let createTodo (data : todo) : todo = 
        todo_db <- data :: todo_db
        data

    let getTodos username : list<todo> =
        todo_db
        |> List.filter (fun x -> x.belongsTo = username) 
        |> List.sortBy (fun x -> x.createdOn)
    
    let getLatestTodo username : todo = 
        todo_db
        |> List.filter (fun x -> x.belongsTo = username)
        |> List.sortByDescending (fun x -> x.createdOn)
        |> List.head

    let removeTodo username id = 
        todo_db <- todo_db
        |> List.filter (fun x -> x.belongsTo = username && x.id <> id)
