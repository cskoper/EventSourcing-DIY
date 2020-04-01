module Projections

open Domain

type Projection<'State,'Event> =
    { Init : 'State   //this pretty much will always be an empty state
      Update : 'State -> 'Event -> 'State }   //we get the current state, we get an event, we change the current state and return a new state

let project projection events =
    events |> List.fold projection.Update projection.Init

let soldOfFlavour flavour state =
    state
    |> Map.tryFind flavour
    |> Option.defaultValue 0

let updateSoldFlavours (state : Map<Flavour,int>) (event : Event) =
    match event with
    | FlavourSold flavour ->
        state
        |> soldOfFlavour flavour
        |> fun portions -> state |> Map.add flavour (portions + 1)   //`Map.add` updates the value if the key is already in the map
    | _ -> state

let soldFlavours : Projection<Map<Flavour,int>,Event> =
    { Init = Map.empty
      Update = updateSoldFlavours }

let restock flavour amount stock =
    stock
    |> Map.tryFind flavour
    |> Option.map (fun portions -> stock |> Map.add flavour (portions + amount))
    |> Option.defaultValue stock

let updateFlavoursInStock stock event =
    match event with 
    | FlavourSold flavour -> 
        stock |> restock flavour -1

    | FlavourRestocked (flavour,amount) ->
        stock |> restock flavour amount
 
    | _ -> stock


let flavoursInStock : Projection<Map<Flavour,int>,Event> =
    { Init = Map.empty
      Update = updateFlavoursInStock }

let stockOf flavour stock =
    stock
    |> Map.tryFind flavour
    |> Option.defaultValue  0


















