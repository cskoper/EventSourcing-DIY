module Tests

open Expecto
open Expecto.Expect
open Domain
open Behavior

let Given = id

let When eventProducer events =
    eventProducer events

let Then expectedEvents currentEvents =
    equal currentEvents expectedEvents "new events should equal expected events."

let tests =
    testList "SellFlavour" [
            test "FlavourSold happy path" {
                Given [ FlavourRestocked (Vanilla, 3) ]
                |> When (sellFlavour Vanilla)
                |> Then [ FlavourSold Vanilla ]
            }

            test "FlavourSold, FlavourWentOutOfStock" {
                Given 
                    [ FlavourRestocked (Vanilla, 3) 
                      FlavourSold Vanilla 
                      FlavourSold Vanilla ]
                |> When (sellFlavour Vanilla)
                |> Then 
                    [ FlavourSold Vanilla
                      FlavourWentOutOfStock Vanilla ]                  
            }

            test "FlavourWasNotInStock" {
                Given 
                    [ FlavourRestocked (Vanilla, 3) 
                      FlavourSold Vanilla 
                      FlavourSold Vanilla
                      FlavourSold Vanilla ]
                |> When (sellFlavour Vanilla)
                |> Then [ FlavourWasNotInStock Vanilla ]
            }
 
        ]