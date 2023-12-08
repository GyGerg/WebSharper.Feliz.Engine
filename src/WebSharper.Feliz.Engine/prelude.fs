namespace Feliz
// open System.Runtime.CompilerServices
open WebSharper
open WebSharper.JavaScript

[<AutoOpen>]
module internal Prelude =
    [<Proxy(typeof<System.DateTime>)>]
    type private DateTimeProxy2 =
        [<Inline "new Date($this).toISOString()">]
        member this.ToString(format:string) = X<string>
        
    [<Proxy(typeof<System.String>)>]
    type private StringProxy2 =
        [<Inline "$this.toLocaleUpperCase()">]
        member this.ToUpperInvariant() = X<string>
