module Domain

type Flavour =
    | Strawberry
    | Vanilla

type Event =
    | FlavourSold of Flavour
    | FlavourRestrocked of Flavour * int
    | FlavourWentOutOfStock of Flavour
    | FlavourWasNotInStock of Flavour