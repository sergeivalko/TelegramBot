namespace TelegramBot.Currencies

[<RequireQualifiedAccess>]
module CurrenciesKeyboard =
    open Funogram.Keyboard.Inline
    open TelegramBot

    let private serializeCurrency currency =
        match currency with
        | USD -> "USD"
        | EURO -> "EURO"

    let private deserializeCurrency (currency: string) : Currency option =
        match currency with
        | "USD" -> Some USD
        | "EURO" -> Some EURO
        | _ -> None

    let serializeProvider provider =
        match provider with
        | Alfa -> "Alfa"
        | Prior -> "Prior"

    let deserializeProvider (provider: string) : BankProvider option =
        match provider with
        | "Alfa" -> Some Alfa
        | "Prior" -> Some Prior
        | _ -> None

    let showSelectionBankProvider message currency callback : KeyboardDefinition<BankProvider> =
        { Id = "Bank"
          DisableNotification = false
          HideAfterConfirm = true
          InitialState = Prior
          GetMessageText = fun _ -> message
          Serialize = serializeProvider
          TryDeserialize = deserializeProvider
          DoWhenConfirmed = callback currency
          GetKeysByState =
            fun keys _ ->
                let OK = keys.Confirm

                keys {
                    yield OK("Prior", Prior)
                    yield OK("Alfa", Alfa)
                } }

    let create text callback : KeyboardDefinition<Currency> =
        { Id = "USD"
          DisableNotification = false
          HideAfterConfirm = true
          InitialState = USD
          GetMessageText = fun _ -> text
          Serialize = serializeCurrency
          TryDeserialize = deserializeCurrency
          DoWhenConfirmed = callback
          GetKeysByState =
            fun keys _ ->
                let OK = keys.Confirm

                keys {
                    yield OK("USD", USD)
                    yield OK("EURO", EURO)
                } }
