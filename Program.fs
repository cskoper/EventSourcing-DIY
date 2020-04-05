module EventStore =
    type EventProducer<'Event> = 'Event list -> 'Event list

    type EventStore<'Event> =
        { Get : unit -> 'Event list
          Append : 'Event list -> unit
          Evolve : EventProducer<'Event> -> unit }
    
    type Msg<'Event> =
        | Append of 'Event list
        | Get of AsyncReplyChannel<'Event list>
        | Evolve of EventProducer<'Event>

    let initialize () : EventStore<'Event> =
        let agent = MailboxProcessor.Start(fun inbox ->
            let rec loop history =
                async {
                    match! inbox.Receive() with
                    | Append events -> return! loop (history @ events)
                    | Get reply -> reply.Reply history ; return! loop history
                    | Evolve eventProducer -> return! loop (history @ eventProducer history) }
            loop [])        

        { Get = fun () -> agent.PostAndReply Get
          Append = fun events -> agent.Post (Append events)
          Evolve = fun eventProducer -> agent.Post (Evolve eventProducer) }      

module Domain =
    type Flavour = Strawberry | Vanilla
    type Event =
        | FlavourSold of Flavour
        | FlavourRestocked of Flavour * int
        | FlavourWentOutOfStock of Flavour
        | FlavourWasNotInStock of Flavour

module Projections =
    open Domain
    open EventStore

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

    let printUl list = list |> List.iteri (fun i item -> printfn "%i: %A" (i+1) item)
    let printEvents events = events |> List.length  |> printfn "History (Length: %i)" ; events |> printUl
    let printSoldFlavour flavour state = state |> soldOfFlavour flavour |> printfn "Sold %A: %i" flavour
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
    eventStore.Evolve (sellFlavour Vanilla)
    eventStore.Evolve (sellFlavour Strawberry)
    eventStore.Evolve (Behavior.restock Vanilla 3)
    eventStore.Evolve (sellFlavour Vanilla)

    let events = eventStore.Get ()    
    events |> printEvents

    let sold : Map<Flavour,int> = events |> project soldFlavours
    printSoldFlavour Vanilla sold
    printSoldFlavour Strawberry sold

    let stock = events |> project flavoursInStock 

    0