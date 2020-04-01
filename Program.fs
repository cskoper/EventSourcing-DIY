open Infrastructure
open Domain
open Helper

[<EntryPoint>]
let main _ =
    let eventStore : EventStore<Event> = EventStore.initialize()

    eventStore.Append [ FlavourRestrocked (Vanilla, 3) ]
    eventStore.Append [ FlavourSold Vanilla ]
    eventStore.Append [ FlavourSold Vanilla ]
    eventStore.Append [ FlavourSold Vanilla ; FlavourWentOutOfStock Vanilla ]
    eventStore.Append [ FlavourSold Strawberry ]

    eventStore.Get ()    
    |> printEvents

    0