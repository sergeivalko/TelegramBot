namespace TelegramBot.BankProviders

[<RequireQualifiedAccess>]
module PriorBankProvider =
    open FSharp.Data

    [<Literal>]
    let PageUrl =
        "https://myfin.by/bank/priorbank/currency"

    type PriorCurrencyProvider = HtmlProvider<PageUrl>

    let getFullInformation currency =
        let downloadedTable =
            PriorCurrencyProvider()
                .Tables
                .``Актуальные курсы валют Приорбанк``

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
