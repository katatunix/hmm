namespace hmm

open RandomModel
open ProbOfGen
open BuildModel

module Main =

    let hr () = printfn "================================================================"

    [<EntryPoint>]
    let main argv = 
        let initModel =
            {   StateNum = 5
                ActionNum = 4
                StartProb = [|0.2; 0.2; 0.2; 0.2; 0.2|]
                StateChangeProb = array2D [ [0.2; 0.1; 0.3; 0.2; 0.2]
                                            [0.2; 0.2; 0.2; 0.2; 0.2]
                                            [0.2; 0.2; 0.2; 0.2; 0.2]
                                            [0.2; 0.2; 0.2; 0.2; 0.2]
                                            [0.2; 0.2; 0.2; 0.2; 0.2] ]
                ActionProb = array2D [  [0.1; 0.2; 0.3; 0.4]
                                        [0.1; 0.2; 0.3; 0.4]
                                        [0.1; 0.2; 0.3; 0.4]
                                        [0.1; 0.2; 0.3; 0.4]
                                        [0.1; 0.2; 0.3; 0.4] ] }
        let initModel = randomModel 5 4

        let actions = [| 0; 1; 2; 3 |]
        let iterationNum = 100
        let targetProb = 0.8

        printfn "INIT MODEL:"
        printModel initModel
        printfn "INIT PROB: %f" (probOfGen initModel actions)

        hr ()
        let model = buildModel initModel actions iterationNum targetProb (fun i model ->
            printfn "Iteration %d. Improved prob: %f" i (probOfGen model actions))

        hr ()
        printfn "RESULT MODEL:"
        printModel model

        0
