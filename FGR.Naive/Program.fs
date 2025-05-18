module FGR.Naive
type Item =
    { Name: string
      SellIn: int
      Quality: int }
    
let maxQuality = 50
let minQuality = 0
let isExpired item = item.SellIn <= 0
let expiresIn days item =
    item.SellIn <= days  

let increaseQuality amount item =
    let newQuality = item.Quality + amount    
    { item with Quality = min maxQuality newQuality } 

let decreaseQuality amount item =
    let newQuality = item.Quality - amount
    { item with Quality = max minQuality newQuality }
        
let decreaseSellin item =
    { item with SellIn = item.SellIn - 1 }
        
let updateQuality(items : array<Item>) =
        items
        |> Array.map (fun item ->
            match item.Name with
            | "Aged Brie" ->
                let qualityIncrease =
                    if isExpired item then 2 else 1
                item
                |> increaseQuality qualityIncrease
                |> decreaseSellin         
            | "Sulfuras, Hand of Ragnaros" -> item
            | "Backstage passes to a TAFKAL80ETC concert" ->
                if isExpired item then
                    { item with Quality = 0; }
                elif item.SellIn <= 5 then
                    item |> increaseQuality 3 
                elif item.SellIn <= 10 then
                    item |> increaseQuality 2
                else 
                    item |> increaseQuality 1
                |> decreaseSellin
            | _ ->
                if isExpired item then
                   item |> decreaseQuality 2
                else
                    item |> decreaseQuality 1
                |> decreaseSellin)

module Program =
    [<EntryPoint>]
    let main _ =
        printfn "OMGHAI!"
        let mutable Items =
            [|{Name = "+5 Dexterity Vest"; SellIn = 10; Quality = 20};
            {Name = "Aged Brie"; SellIn = 2; Quality = 0};
            {Name = "Elixir of the Mongoose"; SellIn = 5; Quality = 7};
            {Name = "Sulfuras, Hand of Ragnaros"; SellIn = 0; Quality = 80};
            {Name = "Sulfuras, Hand of Ragnaros"; SellIn = -1; Quality = 80};
            {Name = "Backstage passes to a TAFKAL80ETC concert"; SellIn = 15; Quality = 20};
            {Name = "Backstage passes to a TAFKAL80ETC concert"; SellIn = 10; Quality = 49};
            {Name = "Backstage passes to a TAFKAL80ETC concert"; SellIn = 5; Quality = 49};
            {Name = "Conjured Mana Cake"; SellIn = 3; Quality = 6}|]

        for i = 0 to 30 do
            printfn "-------- day %d --------" i
            printfn "name, sellIn, quality"
            for j = 0 to Items.Length - 1 do
                 printfn "%s, %d, %d" Items[j].Name Items[j].SellIn Items[j].Quality
            printfn ""
            Items <- Items |> updateQuality
        0