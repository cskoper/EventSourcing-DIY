module Helper

let printUl list = 
    list 
    |> List.iteri (fun i item -> printfn "%i: %A" (i+1) item)

let printEvents events =
    events 
    |> List.length 
    |> printfn "History (Length: %i)"
    
    events 
    |> printUl
