module Utilities

module Seq =
    //implementation modified from Seq.exists
    let all f (source : seq<'T>) =
        use e = source.GetEnumerator()
        let mutable state = true
        while (state && e.MoveNext()) do
            state <- f e.Current
        state

module Array2D =
    let toSeq (array:'T[,]) = array |> Seq.cast<'T>
    let lengths array = Array2D.length1 array, Array2D.length2 array

    let initFrom (array:_[,]) =
        let length1, length2 = lengths array
        Array2D.init length1 length2

    let rotate90 array =
        let length1, length2 = lengths array
        Array2D.init length2 length1 (fun x y -> array.[y, length2 - 1 - x])

    let allRotations array = 
        let first = rotate90 array
        let second = rotate90 first
        let third = rotate90 second
        [array; first; second; third]