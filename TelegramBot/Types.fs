namespace TelegramBot

[<AutoOpen>]
module DomainTypes =
    type Currency =
        | USD
        | EURO

    type BankProvider =
        | Prior
        | Alfa
