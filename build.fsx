#load "node_modules/fable-publish-utils/PublishUtils.fs"
open PublishUtils

match args with
| IgnoreCase "publish"::_ ->
    pushNuget "src/Feliz.Engine.fsproj" [] doNothing
| _ -> ()
