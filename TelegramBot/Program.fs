open System.IO
open Funogram.Api
open Funogram.Types
open Funogram.Bot
open Funogram.Keyboard.Inline
open TelegramBot
open TelegramBot.BankProviders
open TelegramBot.Currencies

[<Literal>]
let TokenFileName = "token"

let processMessageBuild config =

    let processResultWithValue (result: Result<'a, ApiResponseError>) =
        match result with
        | Ok v -> Some v
        | Error e ->
            printfn $"Error: %s{e.Description}"
            None

    let processResult (result: Result<'a, ApiResponseError>) = processResultWithValue result |> ignore

    let botResult data =
        apiUntyped config data |> Async.RunSynchronously

    let bot data = botResult data |> processResult

    let updateArrived ctx =
        let userId =
            if ctx.Update.Message.IsSome then
                ctx.Update.Message.Value.From.Value.Id
            else
                ctx.Update.CallbackQuery.Value.From.Id

        let sendMessageFormatted text parseMode =
            (sendMessageBase (ChatId.Int(userId)) text (Some parseMode) None None None None)
            |> bot

        let say s =
            sendMessageFormatted s ParseMode.Markdown

        let showKeyboard def = InlineKeyboard.show bot userId def

        let showInfo (currency: Currency) =
            fun (_, bankProvider: BankProvider) ->
                let currencyStr = currency.ToString()

                let currencyInformation =
                    match bankProvider with
                    | Alfa -> AlfaBankProvider.getInformation currencyStr
                    | Prior -> PriorBankProvider.getInformation currencyStr

                currencyInformation |> say

        let onShowCommands =
            """Commands:
        /currencies - Show currencies"""

        let showCommands () = bot (sendMessage userId onShowCommands)
        
        let showCurrencies =
            (CurrenciesKeyboard.create "Please select currency" (fun (_, selectedCurrency) ->
                (CurrenciesKeyboard.showSelectionBankProvider "Select bank" selectedCurrency showInfo)
                |> showKeyboard))
        
        let commands =
            [ cmd "/currencies" (fun _ -> showCurrencies |> showKeyboard)
              cmd "/commands" (fun _ -> showCommands ()) ]

        let notHandled =
            processCommands ctx (commands @ InlineKeyboard.getRegisteredHandlers ())

        if notHandled then showCommands ()

    updateArrived

let start token =
    let config =
        { defaultConfig with Token = token }

    let updateArrived =
        processMessageBuild config

    startBot config updateArrived None

[<EntryPoint>]
let main _ =
    printfn "Bot started..."

    let startBot =
        if File.Exists(TokenFileName) then
            start (File.ReadAllText(TokenFileName))
        else
            printf "Please, enter bot token: "
            let token = System.Console.ReadLine()
            File.WriteAllText(TokenFileName, token)
            start token

    startBot |> Async.RunSynchronously
    0
