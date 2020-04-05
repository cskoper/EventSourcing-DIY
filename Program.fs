module EventStore =
    type EventProducer<'Event> = 'Event list -> 'Event list

    type Aggregate = System.Guid

    type EventStore<'Event> =
        { Get : unit -> Map<Aggregate,'Event list>
          GetStream : Aggregate -> 'Event list
          Append : Aggregate -> 'Event list -> unit
          Evolve : Aggregate -> EventProducer<'Event> -> unit }
    
    type Msg<'Event> =
        | Get of AsyncReplyChannel<Map<Aggregate,'Event list>>
        | GetStream of Aggregate * AsyncReplyChannel<'Event list>
        | Append of Aggregate * 'Event list
        | Evolve of Aggregate * EventProducer<'Event>

    let eventsForAggregate aggregate history = history |> Map.tryFind aggregate |> Option.defaultValue []

    let initialize () : EventStore<'Event> =
        let agent = MailboxProcessor.Start(fun inbox ->
            let rec loop history =
                async {
                    match! inbox.Receive() with
                    | Get reply -> 
                        reply.Reply history
                        return! loop history
                    | GetStream (aggregate, reply) ->
                        reply.Reply (history |> eventsForAggregate aggregate)
                        return! loop history
                    | Append (aggregate, events) -> 
                        let streamEvents = history |> eventsForAggregate aggregate
                        let newHistory = history |> Map.add aggregate (streamEvents @ events) 
                        return! loop newHistory
                    | Evolve (aggregate,eventProducer) ->
                        let streamEvents = history |> eventsForAggregate aggregate
                        let newEvents = eventProducer streamEvents
                        let newHistory = history |> Map.add aggregate (streamEvents @ newEvents)
                        return! loop newHistory }
            loop Map.empty )        

        { Get = fun () -> agent.PostAndReply Get
          GetStream = fun aggregate -> agent.PostAndReply (fun reply -> GetStream (aggregate,reply))
          Append = fun aggregate events -> agent.Post (Append (aggregate, events))
          Evolve = fun aggregate eventProducer -> agent.Post (Evolve (aggregate, eventProducer)) }      

module Domain =
    type Flavour = Strawberry | Vanilla

    type Event =
        | FlavourSold of Flavour
        | FlavourRestocked of Flavour * int
        | FlavourWentOutOfStock of Flavour
        | FlavourWasNotInStock of Flavour

module Projections =
    open Domain

    type Projection<'State,'Event> = 
        { Init : 'State 
          Update : 'State -> 'Event -> 'State }

    let project projection events = events |> List.fold projection.Update projection.Init
    let soldOfFlavour flavour state = state |> Map.tryFind flavour |> Option.defaultValue 0

    let updateSoldFlavours (state : Map<Flavour,int>) (event : Event) =
        match event with
        | FlavourSold flavour -> state |> soldOfFlavour flavour |> fun portions -> state |> Map.add flavour (portions + 1)
        | _ -> state

    let soldFlavours : Projection<Map<Flavour,int>,Event> = { Init = Map.empty ; Update = updateSoldFlavours }

    let restock flavour amount stock =
        stock |> Map.tryFind flavour |> Option.defaultValue 0 |> fun portions -> stock |> Map.add flavour (portions + amount)

    let updateFlavoursInStock stock event =
        match event with 
        | FlavourSold flavour -> stock |> restock flavour -1
        | FlavourRestocked (flavour,amount) -> stock |> restock flavour amount
        | _ -> stock

    let flavoursInStock : Projection<Map<Flavour,int>,Event> = { Init = Map.empty ; Update = updateFlavoursInStock }
    let stockOf flavour stock = stock |> Map.tryFind flavour |> Option.defaultValue  0

module Behavior =
    open Domain
    open Projections

    let sellFlavour (flavour : Flavour) (events : Event list) = 
        match events |> project flavoursInStock |> stockOf flavour with
        | 0 -> [ FlavourWasNotInStock flavour ]
        | 1 -> [ FlavourSold flavour; FlavourWentOutOfStock flavour ]
        | _ -> [ FlavourSold flavour ]

    let restock flavour amount events = [ FlavourRestocked (flavour, amount) ]

module Tests =
    open Expecto
    open Expecto.Expect
    open Domain
    open Behavior

    let Given = id
    let When eventProducer events = eventProducer events
    let Then expectedEvents currentEvents = equal currentEvents expectedEvents "new events should equal expected events."

    let tests =
        testList "SellFlavour" [
            test "FlavourSold happy path" {
                Given [ FlavourRestocked (Vanilla, 3) ] 
                |> When (sellFlavour Vanilla) 
                |> Then [ FlavourSold Vanilla ] }
            test "FlavourSold, FlavourWentOutOfStock" {
                Given [ FlavourRestocked (Vanilla, 3); FlavourSold Vanilla; FlavourSold Vanilla ]
                |> When (sellFlavour Vanilla)
                |> Then [ FlavourSold Vanilla; FlavourWentOutOfStock Vanilla ] }
            test "FlavourWasNotInStock" {
                Given [ FlavourRestocked (Vanilla, 3); FlavourSold Vanilla; FlavourSold Vanilla; FlavourSold Vanilla ]
                |> When (sellFlavour Vanilla)
                |> Then [ FlavourWasNotInStock Vanilla ] } ]

module Helpers =
    open Projections
    open Expecto

    let printUl list = list  |> List.iteri (fun i item -> printfn "%i: %A" (i+1) item)

    let printEvents header events = 
        events |> List.length |> printfn "History for %s (Length: %i)" header
        events |> printUl

    let printSoldFlavour flavour state = state |> soldOfFlavour flavour |> printfn "Sold %A: %i" flavour
    let printStockOf flavour state = state |> stockOf flavour |> printfn "Stock of %A: %i" flavour

    let runTests () = runTests defaultConfig Tests.tests |> ignore 

open EventStore
open Domain
open Helpers
open Behavior
open Projections
open Tests

[<EntryPoint>]
let main _ =
    runTests ()

    let eventStore : EventStore<Event> = EventStore.initialize()

    let truck1 = System.Guid.NewGuid()
    let truck2 = System.Guid.NewGuid()

    eventStore.Evolve truck1 (Behavior.sellFlavour Vanilla)
    eventStore.Evolve truck1 (Behavior.sellFlavour Strawberry)
    eventStore.Evolve truck1 (Behavior.restock Vanilla 3)
    eventStore.Evolve truck1 (Behavior.sellFlavour Vanilla)
    eventStore.Evolve truck2 (Behavior.sellFlavour Vanilla)

    let eventsTruck1 = eventStore.GetStream truck1
    let eventsTruck2 = eventStore.GetStream truck2

    eventsTruck1 |> printEvents "Truck 1"
    eventsTruck2  |> printEvents "Truck 2"

    let sold : Map<Flavour,int> = 
        eventsTruck1 |> project soldFlavours

    printSoldFlavour Vanilla sold
    printSoldFlavour Strawberry sold

    let stock = eventsTruck1 |> project flavoursInStock 

    printStockOf Vanilla stock

    0