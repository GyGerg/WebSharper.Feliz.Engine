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
    
    type ElementNode =
        | Text of string
        | Elt of React.Element
    
    type Node =
        | Element of ElementNode
        | Children of ElementNode seq
        | Styles of Style seq
        | Attr of string * AttrVal
        | Event of string * EventFn

    let inline private elToReact (node:ElementNode) =
        match node with
        | Text txt -> Html.text txt
        | Elt el -> el
    let toReact (node:Node) =
        match node with
        | Element elt -> elToReact elt
        | _ -> failwithf "Not a React element"
        
    let asNode = Elt >> Element
    let fragment x =
        x
        |> Seq.choose (function
            | Element (Text txt) -> (Html.text >> Some) txt
            | Element (Elt e) -> Some e
            | _ -> None)
        |> ReactHelpers.Fragment
        |> (Elt >> Element)
    
    let private (|ToReactKey|) (key:string) =
        match key with
        | "class" -> "className"
        | _ when key.Contains("-") ->
            // functional-ish example
            // let chars =
            //     (ResizeArray<char>(),(key.ToCharArray() |> Array.pairwise))
            //     ||> Array.fold (fun (curr) (a,b) -> 
            //         match a with
            //         | '-' -> if b >= 'a' && b <= 'z' then b+'a' else b
            //         | _ -> a
            //         |> curr.Add
            //         curr
            //         )
            // new System.String(chars.ToArray())
            let mutable newKey' = key.Clone() :?> string
            while newKey'.Contains("-") do
                let idx = newKey'.IndexOf('-')
                newKey' <- newKey'.Remove(idx).Replace(newKey'[idx], (newKey'[idx]+'a'))
            newKey'
        | _ -> key
    let private makeStyles (styles: Style seq) =
        let styleObj = JSObject()
        for (ToReactKey key,value) in styles do
                styleObj[key] <- value
        styleObj
    let makeElt (name:string) (children:Node seq) =
        let rec addElts (props:System.Collections.Generic.Dictionary<string,obj> ) (children: ResizeArray<_>) (nodes: Node seq) =
            let inline onElement node =  elToReact node |> children.Add
            
            nodes 
            |> Seq.iter (fun node ->
                match node with
                | Element ele -> onElement ele
                | Children _children ->
                    Seq.iter onElement _children
                | Styles styles -> props["style"] <- makeStyles styles
                | Attr(ToReactKey key,value)
                | Event(ToReactKey key,value) ->
                    props[key] <- value
                )

        let props = System.Collections.Generic.Dictionary()
        let childrenArr = ResizeArray()
        addElts props childrenArr children
        let wsProps = 
            props |> Seq.map (fun a -> (a.Key,a.Value))
        ReactHelpers.Elt name wsProps childrenArr
        |> (Elt >> Element)
        
    type ReactHtmlEngine() =
        inherit HtmlEngine<Node>(makeElt, (Text >> Element), (fun () -> fragment []))
            with
                member _.evt = Event
            
    let private Html = ReactHtmlEngine()
    let private Attr =
        AttrEngine((fun a b -> Attr(a,b)), fun a b -> Attr(a,b))

    let private Css =
        CssEngine((fun a b -> Style(a,b)))
    let style = Css
    let html = Html
        
    type prop =
        static member attr = Attr
        static member styles = Styles
        static member children = fragment
        static member text = (Text >> Element)

        
        

    // let Event =
    //     EventEngine(fun a b -> Event(a,b))
    