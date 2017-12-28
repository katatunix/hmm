namespace hmm

open ProbOfGen

module BuildModel =

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
            if count > iterationNum || probOfGen model actions >= targetProb then
                model
            else
                let model' = improve model actions
                progress count model'
                loop model' (count + 1)
        loop initModel 1
