namespace todo
open System
open todoFunctionality

module validation = 
    let validateTodoData (data : todo) = 
        match data with
        | { name = "" } -> failwith "name cannot be empty"
        | { description = "" } -> failwith "description cannot be empty"
        | { hasCompleted = true; progress = { percent = p }} when p < 100 -> failwith "completed todos must have 100% progress"
        | { abandon = true; hasCompleted = true } -> failwith "abandoned todos cannot be marked as completed"
        | _ -> "g2g"

    /// function to create new todo
    let optionToCreateTodo (username : string) : todo = 
        printfn "Create todo :"
        printf "Name: "
        let name = Console.ReadLine()
        printfn ""
        printf "Description: "
        let description = Console.ReadLine()
        printfn ""
        let progressPercent = Random().Next(1, 100)
        let add1 = (+) 1
        let newTodo : todo =
            {
                id = todo_db.Length |> add1
                name = name
                belongsTo = username
                description = description
                createdOn = DateTime.Now
                hasCompleted = false
                progress = { percent = progressPercent }
                abandon = false
            }
        
        let validate = 
            try
                validateTodoData newTodo
            with
            | ex -> ex.Message

        if validate = "g2g" then
            newTodo
        else
            failwith validate
        
    let uiOptionToUpdate (data : todo) = 
        printf "Name: "
        let name = Console.ReadLine()
        printf "Desc: "
        let desc = Console.ReadLine()
        printf "Progress: "
        let progress = int (Console.ReadLine())
        let updatedtodo : todo = {
            id = data.id
            name = name
            description = desc
            belongsTo = data.belongsTo
            createdOn = data.createdOn
            abandon = data.abandon
            hasCompleted = data.hasCompleted
            progress = { percent = progress }
        }
        let removeFromDb = removeTodo data.belongsTo data.id
        let add = createTodo updatedtodo
        updatedtodo
        
    let optionToUpdate (username : string) = 
        printf "Enter id: "
        let id : int = int(Console.ReadLine())
        let todoToUpdate = todo_db |> List.tryFind (fun x -> x.id = id)
        let updatingF = function 
            | Some todo -> uiOptionToUpdate todo
            | None -> failwith "not found"
        updatingF todoToUpdate

    /// ui option to create and and after creating
    let uiOptionToCreate name = 
        let newTodo = optionToCreateTodo name

        let create todo = createTodo todo

        create newTodo |> ignore
        printfn "Created."
        printfn "------------"

    /// stats of todos
    let stats = 
        let todoCount : int = todo_db.Length
        let completedtask : int = 
            todo_db |> List.filter (fun x -> x.hasCompleted) |> List.length
        let onprogresscount : int = 
            todo_db |> List.filter (fun x -> x.hasCompleted = false) |> List.length
        let abandoned : int = 
            todo_db |> List.filter (fun x -> x.abandon) |> List.length
        printfn $"Todo count: {todoCount}, Completed: {completedtask}, On progress = {onprogresscount}, Abandoned: {abandoned}"
    

    /// main action which will be recursive 
    let rec mainAction name () = 
        printfn "press 'n' to create todo or 's' to view stats, 'a' to view all, 'l' for latest, 'u' to update, or 'q' to quit: "
        let input = Console.ReadKey(intercept = true)
        try
            match input.Key with
            | ConsoleKey.N -> 
                uiOptionToCreate name
                mainAction name ()
            | ConsoleKey.A -> 
                stats
                printfn "\nAll Todos: %O" (getTodos name)
                mainAction name ()
            | ConsoleKey.L -> 
                stats
                printfn "\nLatest Todo: %O" (getLatestTodo name)
                mainAction name ()
            | ConsoleKey.U -> 
                let update = optionToUpdate name 
                update |> ignore
                mainAction name ()
            | ConsoleKey.S ->
                stats
                mainAction name ()
            | ConsoleKey.Q -> 
                stats
                printfn "\nQuiting ..."
                0
            | _ -> 
                printfn "\nInvalid key. Please try again."
                mainAction name ()
        with
        | ex -> 
            printfn "Error: %s" ex.Message
            mainAction name ()
