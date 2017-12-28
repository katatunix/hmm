namespace hmm

[<AutoOpen>]
module Print =

    let printArray (a : float []) =
        for i = 0 to a.Length - 1 do
            printf "%.2f\t" a.[i]
        printfn ""

    let printArray2D (a : float [,]) rows cols =
        for i = 0 to rows - 1 do
            for j = 0 to cols - 1 do
                printf "%.2f\t" a.[i, j]
            printfn ""

    let printModel (model : Model) =
        printfn "StartProb:"
        printArray model.StartProb

        printfn "StateChangeProb:"
        printArray2D model.StateChangeProb model.StateNum model.StateNum

        printfn "ActionProb:"
        printArray2D model.ActionProb model.StateNum model.ActionNum
