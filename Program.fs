open EventStore
open Domain
open Projections
open Helper
open Behavior

[<EntryPoint>]
let main _ =
    let eventStore : EventStore<Event> = EventStore.initialize()

    eventStore.Evolve (sellFlavour Vanilla) // Here we're partially applying the `sellFlavour` function.
    eventStore.Evolve (sellFlavour Strawberry)

    eventStore.Evolve (restock Vanilla 3)
    eventStore.Evolve (sellFlavour Vanilla)

    let events = eventStore.Get ()    

    events |> printEvents

    let sold : Map<Flavour,int> =
        events
        |> project soldFlavours

    printSoldFlavour Vanilla sold
    printSoldFlavour Strawberry sold

    let stock =
        events |> project flavoursInStock

    // printStockOfFlavour Vanilla stock    

    0