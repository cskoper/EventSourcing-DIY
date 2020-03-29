module Infrastructure =
    type EventStore<'Event> =
        { Get : unit -> 'Event list
          Append : 'Event list -> unit }
        
    module EventStore = 
        type Msg<'Event> =  // What can the MB do? 
            | Append of 'Event list
            | Get of AsyncReplyChannel<'Event list> // What kind of reply do we expect
        let initialize () : EventStore<'Event> =
            let agent =
                MailboxProcessor.Start(fun inbox ->
                    let rec loop history =  // state can be any state the agent should store
                        async
                            { let! msg = inbox.Receive()
                              match msg with
                              | Append events ->
                                  return! loop (history @ events)
                              | Get reply -> 
                                  reply.Reply history  //reply on the given channel
                                  return! loop history }  // call the recursive function to let the agent live
                    loop [])
            let append events = agent.Post (Append events)
            let get () = agent.PostAndReply Get  // Because `CurrentState` itself is a function itself, we need not use a lambda expression.
            { Get = get; Append = append }
       
module Domain =
    type Flavour =
        | Strawberry
        | Vanilla
    type Event =
        | Flavour_sold of Flavour
        | Flavour_restocked of Flavour * int
        | Flavour_went_out_of_stock of Flavour
        | Flavour_was_not_in_stock of Flavour

module Helper =
    let printUl list = list |> List.iteri (fun i item -> printfn " %i: %A" (i + 1) item)
    let printEvents events =
        events |> List.length |> printfn "History (Length %i)"
        events |> printUl

open Infrastructure
open Domain
open Helper

[<EntryPoint>]
let main _ =
    let eventStore : EventStore<Event> = EventStore.initialize()
    
    eventStore.Append [ Flavour_restocked (Vanilla, 3) ]
    eventStore.Append [ Flavour_sold Vanilla ]
    eventStore.Append [ Flavour_sold Vanilla ]
    eventStore.Append [ Flavour_sold Vanilla; Flavour_went_out_of_stock Vanilla ]
    
    let events = eventStore.Get()
    events |> printEvents
    0 // return an integer exit code
