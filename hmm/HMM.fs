namespace hmm

module HMM =

    let private random = System.Random ()

    type Model = {
        StateNum : int
        ActionNum : int

        StartProb : float []
        StateChangeProb : float [,]
        ActionProb : float [,] }

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

    let private forward (model : Model) (actions : int []) =
        let {   StateNum = stateNum
                ActionNum = actionNum
                StartProb = startProb
                StateChangeProb = stateChangeProb
                ActionProb = actionProb } = model

        let result = Array2D.create stateNum actions.Length 0.0

        for state = 0 to stateNum - 1 do
            result.[state, 0] <- startProb.[state] * actionProb.[state, actions.[0]]

        for i = 1 to actions.Length - 1 do
            for state = 0 to stateNum - 1 do
                result.[state, i] <-
                    Seq.init stateNum (fun prevState ->
                        result.[prevState, i - 1]
                        * stateChangeProb.[prevState, state]
                        * actionProb.[state, actions.[i]])
                    |> Seq.sum
        result

    let probOfGenerating (model : Model) (actions : int []) =
        let a = forward model actions
        Seq.init model.StateNum (fun state -> a.[state, actions.Length - 1])
        |> Seq.sum

    let private backward (model : Model) (actions : int []) =
        let {   StateNum = stateNum
                ActionNum = actionNum
                StartProb = startProb
                StateChangeProb = stateChangeProb
                ActionProb = actionProb } = model

        let result = Array2D.create stateNum actions.Length 0.0

        for state = 0 to stateNum - 1 do
            result.[state, actions.Length - 1] <- 1.0

        for i = actions.Length - 2 downto 0 do
            for state = 0 to stateNum - 1 do
                result.[state, i] <-
                    Seq.init stateNum (fun nextState ->
                        stateChangeProb.[state, nextState]
                        * actionProb.[nextState, actions.[i + 1]]
                        * result.[nextState, i + 1])
                    |> Seq.sum
        result

    let private improveStateChangeProb (model : Model) (actions : int [])
                                        (a : float [,]) (b : float [,]) =
        let transitions = Array2D.init model.StateNum model.StateNum (fun iState jState ->
            Seq.init (actions.Length - 1) (fun action ->
                a.[iState, action]
                * model.StateChangeProb.[iState, jState]
                * model.ActionProb.[jState, actions.[action + 1]]
                * b.[jState, action + 1])
            |> Seq.sum)
        Array2D.init model.StateNum model.StateNum (fun iState jState ->
            transitions.[iState, jState]
            / (Seq.init model.StateNum (fun kState -> transitions.[iState, kState]) |> Seq.sum))

    let private improveActionProb (model : Model) (actions : int [])
                                    (a : float [,]) (b : float [,]) =
        Array2D.init model.StateNum model.ActionNum (fun state action ->
            let count filter =
                seq { 0 .. actions.Length - 1 }
                |> Seq.filter filter
                |> Seq.map (fun actionIdx -> a.[state, actionIdx] * b.[state, actionIdx])
                |> Seq.sum
            count (fun actionIdx -> actions.[actionIdx] = action)
            / (count (fun _ -> true)))

    let private improve (model : Model) (actions : int []) =
        let {   StateNum = stateNum
                ActionNum = actionNum
                StartProb = startProb
                StateChangeProb = stateChangeProb
                ActionProb = actionProb } = model
        let a = forward model actions
        let b = backward model actions
        let total = Seq.init stateNum (fun state -> a.[state, actions.Length - 1]) |> Seq.sum
        if total < 0.00001 then
            model
        else
            { model with
                StartProb = Array.init stateNum (fun state -> a.[state, 0] * b.[state, 0] / total)
                StateChangeProb = improveStateChangeProb model actions a b
                ActionProb = improveActionProb model actions a b }

    let buildModel initModel actions iterationNum targetProb progress =
        let rec loop model count =
            if count > iterationNum || probOfGenerating model actions >= targetProb then
                model
            else
                let model' = improve model actions
                progress count model'
                loop model' (count + 1)
        loop initModel 1
