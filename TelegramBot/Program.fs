open System.IO
open Funogram.Api
open Funogram.Types
open Funogram.Bot
open Funogram.Keyboard.Inline
open TelegramBot
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

    let processResult (result: Result<'a, ApiResponseError>) =
        processResultWithValue result |> ignore

    let botResult data = apiUntyped config data |> Async.RunSynchronously
    let bot data = botResult data |> processResult
    let updateArrived ctx =
        let userId = if ctx.Update.Message.IsSome then ctx.Update.Message.Value.From.Value.Id
                     else ctx.Update.CallbackQuery.Value.From.Id       
        let sendMessageFormatted text parseMode = (sendMessageBase (ChatId.Int(userId)) text (Some parseMode) None None None None) |> bot
        let say s= sendMessageFormatted s ParseMode.Markdown     
        let showKeyboard def=
                InlineKeyboard.show bot userId def
        
        let showInfo (currency: Currency) =
            fun (_, bankProvider: BankProvider) ->
                let currencyStr = currency.ToString()
                let currencyInformation =
                    match bankProvider with
                    | Alfa -> AlfaProvider.getInformation currencyStr
                    | Prior -> PriorProvider.getInformation currencyStr
                currencyInformation |> say
                
        let onShowCommands ="""Commands:
        /currencies - Show currencies"""
        
        let onShowCurrencies=
            (Currencies.create 
                        "Please select currency"
                        (fun (_, selectedCurrency)->
                                (Currencies.showSelectionBankProvider
                                    "Select bank"
                                    selectedCurrency
                                     showInfo
                                     ) |> showKeyboard
                                )
                        )
                                     
        let commands=[
                cmd "/currencies"  (fun _ -> onShowCurrencies|>showKeyboard)
                cmd "/commands"  (fun _ -> bot (sendMessage userId onShowCommands))
            ]
        let notHandled =
            processCommands ctx (commands @ InlineKeyboard.getRegisteredHandlers())
        if notHandled then             
            ()         
    updateArrived

let start token =
    let config = { defaultConfig with Token = token }
    let updateArrived = processMessageBuild config
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
