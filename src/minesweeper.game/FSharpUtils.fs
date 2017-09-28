module FSharpUtils

module Seq =
    //implementation modified from Seq.exists
    let all f (source : seq<'T>) =
        use e = source.GetEnumerator()
        let mutable state = true
        while (state && e.MoveNext()) do
            state <- f e.Current
        state

