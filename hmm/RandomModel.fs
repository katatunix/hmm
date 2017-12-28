namespace hmm

module RandomModel =

    let private random = System.Random ()

    let private randomProbArray num =
        if num = 1 then
            [| 1.0 |]
        else
            let a = Array.init (num - 1) (fun _ -> random.Next 100) |> Array.sort
            Array.init num (fun i ->
                if i = 0 then
                    a.[0]
                elif i = num - 1 then
                    100 - a.[i - 1]
                else
                    a.[i] - a.[i - 1])
            |> Array.map (fun i -> (float i) / 100.0)

    let private randomProbArray2D rows cols =
        let result = Array2D.create rows cols 0.0
        for i = 0 to rows - 1 do
            let a = randomProbArray cols
            for j = 0 to cols - 1 do
                result.[i, j] <- a.[j]
        result

    let randomModel stateNum actionNum =
        {   StateNum = stateNum
            ActionNum = actionNum

            StartProb = randomProbArray stateNum
            StateChangeProb = randomProbArray2D stateNum stateNum
            ActionProb = randomProbArray2D stateNum actionNum }
