namespace Sample

open WebSharper
open WebSharper.JavaScript
open WebSharper.React
open WebSharper.Feliz.Engine.React

[<JavaScript>]
module Client =
    
    [<SPAEntryPoint>]
    let Main () =
        html.div [
            prop.children [
                html.div [
                    prop.styles [
                        style.displayFlex
                        style.flexDirectionColumn
                        style.backgroundColor Feliz.color.aliceBlue
                    ]
                    prop.children [
                        JS.jsx """<h1>Hello there!</h1>""" |> asNode
                    ]
                    html.evt (Html.on.progress ignore)
                    prop.text "Sample text"
                ]
            ]
        ]
        |> toReact
        |> ReactHelpers.Mount (JS.Document.GetElementById("root"))

