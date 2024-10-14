//namespace problemo

    module basicStuff = 
        let add a b = a + b
        let sub a b = a - b
        let mul a b = a * b

        let dosomemath = add 1 2 |> sub 2 |> mul 2 
        // 1 + 2 = 3
        // 2 - 3 = -1
        // 2 * -1 = -2
        //-------------
        //          -2
        printfn "%d" dosomemath

        let (|Even|Odd|) n = if n % 2 = 0 then Even else Odd
        let numberType  n = 
            match n with
            | Even ->  $"{n} is even"
            | Odd ->  $"{n} is odd"

        printfn "%O" (numberType 8)


        /// gets factorial. acc = acumulator to store current result and n = n!
        let rec factorial' acc n =
            if n <= 1 then acc
            else factorial' (acc * n) (n - 1)

        /// another way to get factorial. sorta tree
        /// factorial 5
        ///   |
        ///   5 * factorial 4
        ///       |
        ///       4 * factorial 3
        ///           |
        ///           3 * factorial 2
        ///               |
        ///               2 * factorial 1
        ///                   |
        ///                   1
        let rec factorial n =
            if n <= 1 then 1
            else n * factorial (n - 1)

        printfn "%O" (factorial 5)

        /// finding factorial using fold
        let factorial'' n = 
            let nums = [1..n]
            let result = 
                nums 
                |> List.fold (fun acc x  -> acc * x) 1 // fold takes 2 args : function to apply on each and '1' intial value of accumulator. 
            result
        printfn "%i" (factorial'' 5)

        (* 
            function composition. 
            eg: do something like: (a + b) * 2 where -> a is 1 and b is 2)
                                  = (1 + 2) * 2)
                                  = 3 * 2
                                  = 6
        *) 
        let add' a b : int =  a + b
        let mul2 b  : int = b * 2
        let composed a = add' a >> mul2 // here, add' last arg is deferred to another function call. this is aka: partial application 
        let composedResult = composed 1 2 
        printfn "%O" (composedResult)
        
        // more on deferred arguments or partial application.
        let sub' a b = a - b
        let firstval x = sub' x
        let secondval y = firstval y
        let result' = secondval 2 3 // results -1
        
        printfn "%O" result'
        // ' in sub' or add' is not f# syntax. its just name of the value.

        (* 
            some built in func of list
        *)
        let sumOfList (ls : list<int>) = ls |> List.sum 
        printfn "%A" (sumOfList [1..4]) // sum of 1,2,3,4 = 10 

        let reverseAList ls = ls |> List.rev
        printfn "%A" (reverseAList [1..4]) // rev of 1,2,3,4 = 4,3,2,1 

        let getSq (ls : list<int>) = 
            ls 
            |> List.map (fun x -> x * x)
        printfn "%A" (getSq [1..4]) // sq of 1,2,3,4 = 1,4,9,16 

        let predicateList f (ls : list<int>) = 
            ls
            |> List.filter (f)
        printfn "%A" (predicateList (fun x -> x % 2 = 0 ) [1..10] ) // get only even nums in 1..10

        let createTuple (l : list<int>) (l2 : list<char>) = List.zip l l2
        printfn "%A" (createTuple [1..5] ['a'..'e']) // here, both argument's lenght must be equal else will get System.ArgumentException: The lists had different lengths.
        //printfn "%A" (createTuple [1..5] ['a'..'z']) // System.ArgumentException: The lists had different lengths. cuz l's length = 5 and l2's length = 26

        let predicateList' f len = 
            List.init len f 
        printfn "%A" (predicateList' (fun x -> x * 2 = 0 ) 10) // get a list with length of 10 applying the predicate

        let getFirst'n'Square n = 
            List.init n (fun x -> x * x)
        printfn "%A" (getFirst'n'Square 10) // get a list with length of 10 applying the predicate i.e get 10 squared numbers

        (* 
            find if a string is palindrome. eg: racecar reverse is racecar so its palindrome. fast reverse is tsaf so its not a palindrome string.
            idea: take value : string as arg, convert it to array, reverse it. if value equals reveresed arr, its palindrome 
        *)
        let isPalindrome (value : string) = 
            let revValue = 
                value 
                |> Seq.rev
                |> Seq.toArray // Seq is deferred by default. so to actually work with reversed value it must be converted into array. 
                |> System.String // not string but System.String cuz string is a alias of System.String and System.String actually has a constructor to convert char[] to string
            value = revValue
        
        printfn "%O" (isPalindrome "fast")  // false
        printfn "%O" (isPalindrome "racecar")  // true


