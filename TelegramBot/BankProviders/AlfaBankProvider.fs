namespace TelegramBot.BankProviders

[<RequireQualifiedAccess>]
module AlfaBankProvider =
    open FSharp.Data

    [<Literal>]
    let PageUrl =
        "https://www.alfabank.by/exchange/digital/"

    type AlfaCurrencyProvider = JsonProvider<"BankProviders\\Example\\alfa.json">

    let private downloadPage (page: string) =
        let downloadedPage = HtmlDocument.Load page
        downloadedPage

    let private getMappedCurrency currency =
        match currency with
        | "USD" -> "USD"
        | "EURO" -> "EUR"
        | _ -> "USD"

    let private parsePage (page: HtmlDocument) =
        let parsedPage =
            page.Descendants["div"]
            |> Seq.choose (fun node ->
                node.TryGetAttribute "data-initial"
                |> Option.map (fun attribute -> attribute |> HtmlAttribute.value))
            |> Seq.find (fun value -> value.Contains "filterData")

        let currencyInfo =
            AlfaCurrencyProvider.Parse parsedPage

        currencyInfo

    let private getInSyncCurrency currency (currencyInfo: AlfaCurrencyProvider.Root) =
        let inSyncBlock =
            currencyInfo.InitialItems[0].CurrenciesData[0]
                .Value
                .ExchangeRate
            |> Array.filter (fun item -> item.Icon = currency)

        let inSyncBlockInfo =
            match inSyncBlock |> Array.isEmpty with
            | true -> "Not found"
            | false -> $"Purchase: {inSyncBlock[0].Purchase.Value} Sell: {inSyncBlock[0].Sell.Value}"

        inSyncBlockInfo

    let private getACurrency currency (currencyInfo: AlfaCurrencyProvider.Root) =
        let aCurrencyBlock =
            currencyInfo.InitialItems[1].CurrenciesData[0]
                .Value
                .ExchangeRate
            |> Array.filter (fun item -> item.Icon = currency)

        let aCurrencyBlockInfo =
            match aCurrencyBlock |> Array.isEmpty with
            | true -> "Not found"
            | false -> $"Purchase: {aCurrencyBlock[0].Purchase.Value} Sell: {aCurrencyBlock[0].Sell.Value}"

        aCurrencyBlockInfo

    let private getTransferCurrency currency (currencyInfo: AlfaCurrencyProvider.Root) =
        let transferBlock =
            currencyInfo.InitialItems[2].CurrenciesData[0]
                .Value
                .ExchangeRate
            |> Array.filter (fun item -> item.Icon = currency)

        let transferBlockInfo =
            match transferBlock |> Array.isEmpty with
            | true -> "Not found"
            | false -> $"Purchase: {transferBlock[0].Purchase.Value} Sell: {transferBlock[0].Sell.Value}"

        transferBlockInfo

    let private getFullInformation inSyncInfo aCurrencyInfo transferInfo =
        let fullInto =
            $"InSync :{inSyncInfo}\nA-Currency: {aCurrencyInfo}\nTransfer: {transferInfo}"

        fullInto

    let getInformation (currency: string) =
        let downloadedPage = downloadPage PageUrl
        let parsedJson = parsePage downloadedPage

        let matchedCurrency =
            getMappedCurrency currency

        let inSyncInfo =
            getInSyncCurrency matchedCurrency parsedJson

        let aCurrencyInfo =
            getACurrency matchedCurrency parsedJson

        let transferInfo =
            getTransferCurrency matchedCurrency parsedJson

        let fullInfo =
            getFullInformation inSyncInfo aCurrencyInfo transferInfo

        fullInfo
