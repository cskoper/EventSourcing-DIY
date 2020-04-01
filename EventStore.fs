module EventStore

type EventProducer<'Event> =
    'Event list -> 'Event list

type EventStore<'Event> =
    { Get : unit -> 'Event list
      Append : 'Event list -> unit
      Evolve : EventProducer<'Event> -> unit }

module EventStore =
    type Msg<'Event> = // What the MB can do
        | Append of 'Event list
        | Get of AsyncReplyChannel<'Event list> // what kind of reply do we expect?
        | Evolve of EventProducer<'Event>

    let initialize () : EventStore<'Event> =
        let agent =
            MailboxProcessor.Start(fun inbox ->
                let rec loop history = // state can be any state the agent should store
                    async {
                        match! inbox.Receive() with  // await the result and then match
                        | Append events -> 
                            return! loop (history @ events) // call the recursive function to let the agent live

                        | Get reply -> 
                            reply.Reply history // reply on the given channel
                            return! loop history // call the recursive function to let the agent live

                        | Evolve eventProducer ->
                            let newEvents = 
                                eventProducer history
                            return! loop (history @ newEvents) 
                    }
                loop []
            )        

        // `let get () = agent.PostAndReply(fun reply -> Get reply)` can be shortened to:
        let get () = agent.PostAndReply Get
        let append events = agent.Post (Append events)
        let evolve eventProducer = agent.Post (Evolve eventProducer)        

        { Get = get
          Append = append
          Evolve = evolve }        




