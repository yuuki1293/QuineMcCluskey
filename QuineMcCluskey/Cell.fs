module Cell

type Cell =
    | False
    | True
    | Both
    override this.ToString() =
        match this with
        | False -> "0"
        | True -> "1"
        | Both -> "-"