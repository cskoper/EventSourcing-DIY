module Behavior

open Domain
open Projections

let sellFlavour (flavour : Flavour) (events : Event list) = 
    let stock = // get stock for a specific flavour
        events 
        |> project flavoursInStock 
        |> stockOf flavour

    match stock with // check constaints for Flavour sold
    | 0 -> [ FlavourWasNotInStock flavour ]
    | 1 -> [ FlavourSold flavour; FlavourWentOutOfStock flavour ]
    | _ -> [ FlavourSold flavour ]

let restock flavour amount events =
    [ FlavourRestocked (flavour, amount) ]