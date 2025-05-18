module FGR.Naive.UpdateQualityTests

open FGR.Naive
open Xunit
open Swensen.Unquote

let regularItem = { Name = "foo"; SellIn = 30; Quality = 30 }

let agedBrie =
    { regularItem with Name = "Aged Brie" }

let tickets =
    { regularItem with Name = "Backstage passes to a TAFKAL80ETC concert" }

let sulfuras =
    { regularItem
      with Name = "Sulfuras, Hand of Ragnaros"; Quality = 80 }

let verifyQuality expectedQuality (item: array<Item>) =
    test <@ item[0].Quality = expectedQuality @>

let verifySellIn expectedSellIn (item: array<Item>) =
    test <@ item[0].SellIn = expectedSellIn @>

let verifyDecrement (item: Item) =
    [| { item with SellIn = 10 } |] |> updateQuality |> verifySellIn 9
    
[<Fact>]
let ``Non legendary items SellIn decreases by 1`` () =
    [regularItem; agedBrie; tickets]
        |> Seq.iter verifyDecrement

[<Fact>]
let ``Regular item Quality decreases by 1`` () =
    [| { regularItem with Quality = 10 } |]
    |> updateQuality
    |> verifyQuality 9

[<Fact>]
let ``Item Quality does not go below 0`` () =
    [| { regularItem with Quality = 0 } |]
    |> updateQuality
    |> verifyQuality 0

[<Fact>]
let ``Item Quality decreases twice as fast when SellIn is due`` () =
    [| { regularItem with SellIn = 0; Quality = 10 } |]
    |> updateQuality
    |> verifyQuality 8

[<Fact>]
let ``Aged Brie Quality increases`` () =
    [| { agedBrie with Quality = 10 } |] |> updateQuality |> verifyQuality 11

[<Fact>]
let ``Aged Brie Quality increases twice as fast when SellIn is due`` () =
    let item =
        [| { agedBrie with
               Quality = 10
               SellIn = 0 } |]
        |> updateQuality

    test <@ item[0].Quality = 12 @>

[<Fact>]
let ``Item Quality cannot be over 50`` () =
    [| { agedBrie with Quality = 50 } |]
        |> updateQuality
        |> verifyQuality 50

[<Fact>]
let ``Tickets to concert Quality increases`` () =
    [| { tickets with Quality = 10 } |]
    |> updateQuality
    |> verifyQuality 11

[<Fact>]
let ``Tickets to concert Quality increases twice as fast when SellIn is less than 10 days away`` () =
        [| { tickets with SellIn = 10; Quality = 10 } |]
        |> updateQuality
        |> verifyQuality 12

[<Fact>]
let ``Tickets to concert Quality increases three times as fast when concert is less than 5 days away`` () =
    [| { tickets with SellIn = 5; Quality = 10 } |]
    |> updateQuality
    |> verifyQuality 13

[<Fact>]
let ``Tickets to concert Quality drops to 0 after the concert`` () =
    [| { tickets with SellIn = 0; Quality = 10 } |]
    |> updateQuality
    |> verifyQuality 0

[<Fact>]
let ``Sulfuras quality does not change`` () =
    [| { sulfuras with Quality = 80 } |]
    |> updateQuality
    |> verifyQuality 80

[<Fact>]
let ``Sulfuras sellIn does not change`` () =
    [| { sulfuras with SellIn = 15 } |]
    |> updateQuality
    |> verifySellIn 15
