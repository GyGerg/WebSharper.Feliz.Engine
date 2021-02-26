﻿module Feliz.Snabbdom

open System
open Fable.Core
open Fable.Core.JsInterop
open Feliz
open Snabbdom

[<RequireQualifiedAccess>]
type StyleHook =
    | None
    | Delayed
    | Remove
    | Destroy

type Node =
    | Key of Guid
    | Text of string
    | El of VNode
    | Hook of string * obj
    | Style of string * obj * StyleHook
    | Attr of string * obj
    | Event of string * obj
    | Fragment of Node list
    static member AsVNode = function
        | El vnode -> vnode
        | _ -> failwith "not a vnode"

type Helper() =
    interface HtmlHelper<Node> with
        member _.MakeNode(tag, nodes) =
            let rec add (o: obj) keys (v: obj) =
                match keys with
                | [] -> failwith "Empty key list"
                | [key] -> o?(key) <- v
                | key::keys ->
                    if isNull o?(key) then o?(key) <- obj()
                    add (o?(key)) keys v

            let rec addNodes (props: obj) (children: ResizeArray<_>) (nodes: Node seq) =
                nodes |> Seq.iter (function
                    | Key k -> props?key <- k
                    | Text s -> children.Add(Helper.Text s)
                    | El vnode -> children.Add(vnode)
                    | Hook(k, v) -> add props ["hook"; k] v
                    | Style(k, v, StyleHook.None) -> add props ["style"; k] v
                    | Style(k, v, StyleHook.Delayed) -> add props ["style"; "delayed"; k] v
                    | Style(k, v, StyleHook.Remove) -> add props ["style"; "remove"; k] v
                    | Style(k, v, StyleHook.Destroy) -> add props ["style"; "destroy"; k] v
                    | Attr(k, v) -> add props ["attrs"; k] v
                    | Event(k, v) -> add props ["on"; k] v
                    | Fragment nodes -> addNodes props children nodes
                )

            let props = obj()
            let children = ResizeArray()
            addNodes props children nodes
            Snabbdom.h(tag, props, children) |> El

        member _.StringToNode(v) = Text v
        member _.EmptyNode = Fragment []

    interface AttrHelper<Node> with
        member _.MakeAttr(key, value) = Attr(key, value)
        member _.MakeBooleanAttr(key, value) = Attr(key, value)

    interface CssHelper<Node> with
        member _.MakeStyle(k, v) = Style(k, v, StyleHook.None)

    interface EventHelper<Node> with
        member _.MakeEvent(k, f) = Event(k.ToLowerInvariant(), f)

open System.Runtime.CompilerServices

[<Extension>]
type Extensions() =
    static let withStyleHook hook nodes =
        nodes |> Seq.choose (function
            | Style(k, v, _) -> Some(Style(k, v, hook))
            | _ -> None // error?
        ) |> Seq.toList |> Fragment

    [<Extension>]
    static member delayed(e: CssEngine<Node>, nodes: Node seq) =
        withStyleHook StyleHook.Delayed nodes

    [<Extension>]
    static member remove(e: CssEngine<Node>, nodes: Node seq) =
        withStyleHook StyleHook.Remove nodes

    [<Extension>]
    static member destroy(e: CssEngine<Node>, nodes: Node seq) =
        withStyleHook StyleHook.Destroy nodes

let private h = Helper()

let Html = HtmlEngine(h)
let Attr = AttrEngine(h)
let Css = CssEngine(h)
let Ev = EventEngine(h)

// TODO: Other hooks https://github.com/snabbdom/snabbdom#hooks
module Hook =
    let insert (f: VNode -> unit) = Hook("insert", f)
    let remove (f: VNode -> unit) = Hook("remove", f)
    let destroy (f: VNode -> unit) = Hook("destroy", f)

module internal Util =
    let inline getKey x = (^a: (member Id: Guid) x)

    let patch oldVNode node =
        let newVNode = node |> Node.AsVNode
        Helper.Patch(oldVNode, newVNode)
        newVNode

module Elmish =
    let mount (init: unit -> 'Model) update view node =
        let event = new Event<'Msg>()
        let trigger e = event.Trigger(e)
        let mutable state = init()
        let mutable tree = view state trigger |> Node.AsVNode
        Helper.Patch(node, tree)

        let handleEvent evt =
            state <- update evt state
            tree <- view state trigger |> Util.patch tree

        event.Publish.Add(handleEvent)

    let app id (init: unit -> 'Model) update view =
        Browser.Dom.document.getElementById(id)
        |> Helper.AsNode
        |> mount init update view

let key k = Key k

let inline memoize (render: 'Model -> Node) model =
    Helper.Thunk("memo", (Util.getKey model), (fun m -> render m |> Node.AsVNode), [|model|]) |> El
