namespace hmm

module ProbOfGen =

    let forward (model : Model) (actions : int []) =
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

    let probOfGen (model : Model) (actions : int []) =
        let a = forward model actions
        Seq.init model.StateNum (fun state -> a.[state, actions.Length - 1])
        |> Seq.sum

    let backward (model : Model) (actions : int []) =
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
