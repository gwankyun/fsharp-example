module AttributeCommon
open Argu

type PathAttribute() =
    inherit AltCommandLineAttribute([|"-p"|])

type DestAttribute() =
    inherit AltCommandLineAttribute([|"-d"; "--dest"|])

type PrevAttribute() =
    inherit AltCommandLineAttribute([|"--prev"|])

type NameAttribute() =
    inherit AltCommandLineAttribute([|"-n"|])

type NoPrefixAttribute() =
    inherit CliPrefixAttribute(CliPrefix.None)
