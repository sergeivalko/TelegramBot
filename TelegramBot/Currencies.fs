namespace TelegramBot.Currencies

open FSharp.Data

[<RequireQualifiedAccess>]
module Currencies =
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
   
    let deserializeProvider (provider:string) : BankProvider option =
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
                  }
        }

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
                  }
        }

[<RequireQualifiedAccess>]
module AlfaProvider =

    [<Literal>]
    let PageUrl = "https://www.alfabank.by/exchange/digital/"
    
    type AlfaCurrencyProvider = JsonProvider<"alfa.json">
    
    let private downloadPage (page:string) =
        let downloadedPage = HtmlDocument.Load page
        downloadedPage

    let private getMappedCurrency currency =
        match currency with
        | "USD" -> "USD"
        | "EURO" -> "EUR"
        |_ -> "USD"
    
    let private parsePage (page:HtmlDocument) =
        let parsedPage =
            page.Descendants["div"]
            |> Seq.choose (fun node ->
                node.TryGetAttribute "data-initial"
                |> Option.map (fun attribute -> attribute |> HtmlAttribute.value) )
            |> Seq.find (fun value -> value.Contains "filterData"  )
            
        let currencyInfo = AlfaCurrencyProvider.Parse parsedPage
        currencyInfo
    
    let private getInSyncCurrency currency (currencyInfo:AlfaCurrencyProvider.Root) =
        let inSyncBlock =
            currencyInfo.InitialItems[0].CurrenciesData[0].Value.ExchangeRate
            |> Array.filter (fun item -> item.Icon = currency)
        
        let inSyncBlockInfo = 
            match inSyncBlock |> Array.isEmpty with
            |true -> "Not found"
            |false -> $"Purchase: {inSyncBlock[0].Purchase.Value} Sell: {inSyncBlock[0].Sell.Value}"        
        
        inSyncBlockInfo
    
    let private getACurrency currency (currencyInfo:AlfaCurrencyProvider.Root) =
        let aCurrencyBlock =
            currencyInfo.InitialItems[1].CurrenciesData[0].Value.ExchangeRate
            |> Array.filter (fun item -> item.Icon = currency)
            
        let aCurrencyBlockInfo = 
            match aCurrencyBlock |> Array.isEmpty with
            |true -> "Not found"
            |false -> $"Purchase: {aCurrencyBlock[0].Purchase.Value} Sell: {aCurrencyBlock[0].Sell.Value}"
            
        aCurrencyBlockInfo     

    let private getTransferCurrency currency (currencyInfo:AlfaCurrencyProvider.Root) =
        let transferBlock =
            currencyInfo.InitialItems[2].CurrenciesData[0].Value.ExchangeRate
            |> Array.filter (fun item -> item.Icon = currency)        
        
        let transferBlockInfo = 
            match transferBlock |> Array.isEmpty with
            |true -> "Not found"
            |false -> $"Purchase: {transferBlock[0].Purchase.Value} Sell: {transferBlock[0].Sell.Value}"
            
        transferBlockInfo
    
    let private getFullInformation inSyncInfo aCurrencyInfo transferInfo =
        let fullInto = $"InSync :{inSyncInfo}\nA-Currency: {aCurrencyInfo}\nTransfer: {transferInfo}"
        fullInto
    
    let getInformation (currency: string) =
        let downloadedPage = downloadPage PageUrl
        let parsedJson = parsePage downloadedPage
        let matchedCurrency = getMappedCurrency currency
        let inSyncInfo = getInSyncCurrency matchedCurrency parsedJson
        let aCurrencyInfo = getACurrency matchedCurrency parsedJson
        let transferInfo = getTransferCurrency matchedCurrency parsedJson
        let fullInfo = getFullInformation inSyncInfo aCurrencyInfo transferInfo
        fullInfo

[<RequireQualifiedAccess>]
module PriorProvider =

    [<Literal>]
    let PageUrl = "https://myfin.by/bank/priorbank/currency"
    type PriorCurrencyProvider = HtmlProvider<PageUrl>
    
    let getFullInformation currency =
        let downloadedTable = PriorCurrencyProvider().Tables.``Актуальные курсы валют Приорбанк``
        
        let currencyStr =
            match currency with
            | "EURO" -> "Евро"
            | _ -> "Доллар США"
        
        let rowInfo =
            downloadedTable.Rows
            |> Array.find (fun item -> item.Валюта = currencyStr)
            
        $"Purchase: {rowInfo.Покупка}  Sell: {rowInfo.Продажа}"
    
    let getInformation currency =
        let fullInfo = getFullInformation currency 
        fullInfo