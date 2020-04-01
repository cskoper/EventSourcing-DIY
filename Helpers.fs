module Helper

open Projections

let printUl list = 
    list 
    |> List.iteri (fun i item -> printfn "%i: %A" (i+1) item)

let printEvents events =
    events 
    |> List.length 
    |> printfn "History (Length: %i)"
    
    events 
    |> printUl

let printSoldFlavour flavour state =
    state
    |> soldOfFlavour flavour
    |> printfn "Sold %A: %i" flavour
