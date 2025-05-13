module FGR.Naive
type Item =
    { Name: string
      SellIn: int
      Quality: int }


let updateQuality(items : array<Item>) =
        for i = 0 to items.Length - 1 do
            if items[i].Name <> "Aged Brie" && items[i].Name <> "Backstage passes to a TAFKAL80ETC concert" then
                if items[i].Quality > 0 then
                    if items[i].Name <> "Sulfuras, Hand of Ragnaros" then
                        items[i] <- { items[i] with Quality = (items[i].Quality - 1) }
            else
               if items[i].Quality < 50 then
                    items[i] <- { items[i] with Quality = (items[i].Quality + 1) }
                    if items[i].Name = "Backstage passes to a TAFKAL80ETC concert" then
                        if items[i].SellIn < 11 then
                            if items[i].Quality < 50 then
                                items[i] <- { items[i] with Quality = (items[i].Quality + 1) }
                        if items[i].SellIn < 6 then
                            if items[i].Quality < 50 then
                                items[i] <- { items[i] with Quality = (items[i].Quality + 1) }
            if items[i].Name <> "Sulfuras, Hand of Ragnaros" then
                items[i] <- { items[i] with SellIn  = (items[i].SellIn - 1) }
            if items[i].SellIn < 0 then
                if items[i].Name <> "Aged Brie" then
                    if items[i].Name <> "Backstage passes to a TAFKAL80ETC concert" then
                        if items[i].Quality > 0 then
                            if items[i].Name <> "Sulfuras, Hand of Ragnaros" then
                                items[i] <- { items[i] with Quality   = (items[i].Quality  - 1) }
                    else
                        items[i] <- { items[i] with Quality   = (items[i].Quality  - items[i].Quality) }
                else
                    if items[i].Quality < 50 then
                        items[i] <- { items[i] with Quality   = (items[i].Quality + 1) }
        items


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