module Infrastructure

type EventStore<'Event> =
    {
        Get : unit -> 'Event list
        Append : 'Event list -> unit
    }

module EventStore =
    type Msg<'Event> = // What the MB can do
        | Append of 'Event list
        | Get of AsyncReplyChannel<'Event list> // what kind of reply do we expect?

    let initialize () : EventStore<'Event> =
        let agent =
            MailboxProcessor.Start(fun inbox ->
                let rec loop history = // state can be any state the agent should store
                    async {
                        match! inbox.Receive() with // await the result and then match
                        | Append events -> 
                            return! loop (history @ events) // call the recursive function to let the agent live
                        | Get reply -> 
                            reply.Reply history // reply on the given channel
                            return! loop history // call the recursive function to let the agent live
                    }
                loop []
            )        

        let append events =
            agent.Post (Append events)

        // `let get () = agent.PostAndReply(fun reply -> Get reply)` can be shortened to:
        let get () =
            agent.PostAndReply Get

        {
            Get = get
            Append = append
        }        




