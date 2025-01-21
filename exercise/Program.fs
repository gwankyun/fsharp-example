// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

type Color =
    | Red = 1
    | Green = 2
    | Blue = 3

let red = enum<Color> 1

let green : Color = enum 2

printfn $"red: %A{red}"
printfn $"green: %A{green}"

// type Base() =
//     abstract member F : uint -> unit
//     default u.F() : unit =
//         printfn $"F Base"

// type Derived() =
//     inherit Base()
//     override u.F() = 
//         printfn $"F Derived"

// let d : Derived = Derived()

// d.F()
