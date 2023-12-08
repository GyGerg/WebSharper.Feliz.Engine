namespace WebSharper.Feliz.Engine

open Feliz
open WebSharper
open WebSharper.JavaScript
open WebSharper.React


[<JavaScript>]
module React =
    type Style = string * obj
    type AttrVal = obj
    type EventFn = obj
    
    
    type Node =
        | Text of string
        | El of React.Element
        | Styles of Style seq
        | Attr of string * AttrVal
        | Event of string * EventFn

    let toReact (node:Node) =
        match node with
        | El el -> el 
        | Text txt -> Html.text txt
        | _ -> failwithf "Not a React element"
        
    let asNode = El
    let fragment x =
        x
        |> Seq.choose (fun node ->
            match node with
            | Text _ | El _ -> Some <| toReact node
            | _ -> None)
        |> ReactHelpers.Fragment
        |> El
    
    open System
    let private toReactKey = function
    | "class" -> "className"
    | key when key.Contains("-") -> 
            let arr = key.Split('-')
            let uppers = 
                arr[1..arr.Length]
                |> Array.map (fun k ->
                    let arr = k.ToCharArray()
                    String.Concat(arr[0].ToString().ToUpper(), new String(arr[1..arr.Length]))
                )
            String.Join("", Array.append [|arr[0]|] uppers)
    | key -> key

    let private (|ToReactKey|)= toReactKey
        
    let private makeStyles (styles: Style seq) =
        let styleObj = JSObject()
        for (ToReactKey key,value) in styles do
                Console.Log key
                styleObj[key] <- value
        styleObj
    let makeElt (name:string) (children:Node seq) =
        let rec addElts (props:System.Collections.Generic.Dictionary<string,obj> ) (children: ResizeArray<_>) (nodes: Node seq) =
            let inline onElement node =  toReact node |> children.Add
            
            nodes 
            |> Seq.iter (fun node ->
                match node with
                | El _ | Text _ -> onElement node
                | Styles styles -> props["style"] <- makeStyles styles
                | Attr(ToReactKey key,value)
                | Event(ToReactKey key,value) ->
                    props[key] <- value
                )

        let props = System.Collections.Generic.Dictionary()
        let childrenArr = ResizeArray()
        addElts props childrenArr children
        let wsProps = 
            props |> Seq.map (fun a -> (toReactKey a.Key,a.Value))
        ReactHelpers.Elt name wsProps childrenArr
        |> El
        
    type ReactHtmlEngine() =
        inherit HtmlEngine<Node>(makeElt, (Text), (fun () -> fragment []))
            with
                member _.evt = Event
            
    let private Html = ReactHtmlEngine()
    let private Attr =
        AttrEngine((fun (ToReactKey a) b -> Attr(a,b)), fun (ToReactKey a) b -> Attr(a,b))

    let private Css =
        CssEngine((fun (ToReactKey a) b -> Style(a,b)))

    let html = Html
    let style = Css
    
    type prop =
        static member attr = Attr
        static member styles = Styles
        static member children = fragment
        static member text = Text
    