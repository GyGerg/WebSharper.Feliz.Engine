namespace Sample

open Feliz
open WebSharper
open WebSharper.JavaScript
open WebSharper.React
open WebSharper.Feliz.Engine.React

[<JavaScript>]
module Client =
    let CounterFunctionExample() =
        React.CreateElement((fun _ -> 

            let count, setCount = React.UseState 0
            html.div [
                prop.children [
                    html.button [
                        prop.text "Increment"
                        html.evt (Html.on.click (fun _ -> setCount.Invoke(count+1)))
                    ]
                    html.span [
                        prop.text $"{count}"
                    ]
                    html.button [
                        prop.text "Decrement"
                        html.evt (Html.on.click (fun _ -> setCount.Invoke(count-1)))
                    ]
                ]
            ] |> toReact), ())
        

    [<SPAEntryPoint>]
    let Main () =
        let root = ReactDOM.ReactDomClient.CreateRoot (JS.Document.GetElementById("root"))

        html.div [
            prop.children [
                html.div [
                    prop.styles [
                        style.displayFlex
                        style.flexDirectionColumn
                        style.justifyContentSpaceBetween
                        style.backgroundColor color.aliceBlue
                    ]
                    prop.children [
                        JS.jsx """<h1>Hello there!</h1>""" |> asNode
                        CounterFunctionExample() |> asNode
                    ]
                ]
            ]
        ]
        |> toReact
        |> root.Render

