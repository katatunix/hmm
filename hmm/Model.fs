namespace hmm

type Model = {
    StateNum : int
    ActionNum : int

    StartProb : float []
    StateChangeProb : float [,]
    ActionProb : float [,] }
