module Infrastructure =
    type EventStore<'Event> =
        { Get : unit -> 'Event list
          Append : 'Event list -> unit }
        
    module EventStore =
        // What can the MB do? 
        type Msg = 
            | Increment
            // What kind of reply do we expect
            | CurrentState of AsyncReplyChannel<int> 
            
        let agent =
            MailboxProcessor.Start(fun inbox ->
                // state can be any state the agent should store
                let rec loop state =     
                    async {
                        let! msg = inbox.Receive()
                        match msg with
                        // call the recursive function to let the agent live
                        | Increment -> return! loop (state + 1) 
                        | CurrentState reply ->
                            //reply on the given channel 
                            reply.Reply state 
                            // call the recursive function to let the agent live
                            return! loop state }
                loop 0 )
        
        agent.Post Increment
        // Because `CurrentState` itself is a function itself, we need not use a lambda expression.
        let value = agent.PostAndReply CurrentState
       
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
    let printUl list =
        list
        |> List.iteri (fun i item -> printfn " %i: %A" (i + 1) item)
        
    let printEvents events =
        events
        |> List.length
        |> printfn "History (Length %i)"
        
        events |> printUl

open Infrastructure
open Domain
open Helper

[<EntryPoint>]
let main argv =
    let eventStore : EventStore<Event> = EventStore.initialize()
    
    eventStore.Append [ Flavour_restocked (Vanilla, 3) ]
    eventStore.Append [ Flavour_sold (Vanilla) ]
    eventStore.Append [ Flavour_sold (Vanilla) ]
    eventStore.Append [ Flavour_sold (Vanilla) ]
    
    eventStore.Get() |> printEvents
    
    printfn "Hello World from F#!"
    0 // return an integer exit code
