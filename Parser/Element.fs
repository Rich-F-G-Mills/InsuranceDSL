
namespace Parser


[<RequireQualifiedAccess>]
module internal Element =   

    open FParsec


    let private parseElementType =
        let mappings =
            [
                "ENUM", ENUM, Enumeration.parse
                "STRUCTURE", STRUCTURE, Structure.parse
                "PRODUCT", PRODUCT, Product.parse
            ]
            |> List.map (fun ((_, pType, _) as t) -> pType >>% t)

        choiceL mappings "Invalid element type specified."            

    let internal parse =
        parse {
            let! keyword, parseClosing, parseInner =
                (BEGIN <?> "BEGIN keyword expected to open element.")
                >|+>. parseElementType
                .>> WHITESPACE                

            do! parseInner
                >.> (END <?> "END keyword expected.")
                >|+> (parseClosing <?> sprintf "%s keyword expected to close element." keyword)
                >.> (WHITESPACE <|> eof)
        }
