namespace CounterApp

open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Themes.Fluent
open Avalonia.FuncUI.Hosts
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Avalonia.Platform

module Main =

    let view () =
        Component(fun ctx ->
            let state = ctx.useState 0

            DockPanel.create [
                DockPanel.children [
                    Button.create [
                        Button.dock Dock.Bottom
                        Button.onClick (fun _ -> state.Set(state.Current - 1))
                        Button.content "-"
                        Button.horizontalAlignment HorizontalAlignment.Stretch
                        Button.horizontalContentAlignment HorizontalAlignment.Center
                    ]
                    Button.create [
                        Button.dock Dock.Bottom
                        Button.onClick (fun _ -> state.Set(state.Current + 1))
                        Button.content "+"
                        Button.horizontalAlignment HorizontalAlignment.Stretch
                        Button.horizontalContentAlignment HorizontalAlignment.Center
                    ]
                    TextBlock.create [
                        TextBlock.dock Dock.Top
                        TextBlock.fontSize 48.0
                        TextBlock.verticalAlignment VerticalAlignment.Center
                        TextBlock.horizontalAlignment HorizontalAlignment.Center
                        TextBlock.text (string state.Current)
                    ]
                ]
            ]
        )

    let controlList: Types.IView list = [
        Button.create  [
            Button.content "Top"
            Button.dock Dock.Top
        ]
        Button.create  [
            Button.content "Bottom"
            Button.dock Dock.Bottom
        ]
        Button.create  [
            Button.content "Left"
            Button.dock Dock.Left
        ]
        Button.create  [
            Button.content "Right"
            Button.dock Dock.Right
        ]
    ]

    let dockControl =
        DockPanel.create [
            DockPanel.children controlList
        ]

    let dock () =
        Component(fun ctx ->
            dockControl
        )

    let stackControl =
        StackPanel.create [
            StackPanel.children controlList
        ]


    let wrapControl =
        WrapPanel.create [
            WrapPanel.children controlList
        ]

    let stack () =
        Component(fun ctx ->
            stackControl
        )

    //let gridControl =
    //    Grid.create [
    //        Grid.columnDefinitions "*,*"
    //        Grid.rowDefinitions "*,*"
    //        Grid.children [
    //            Button.create  [
    //                Button.content "Top"
    //                Grid.column 0
    //                Grid.row 0
    //            ]
    //            Button.create  [
    //                Button.content "Bottom"
    //                Grid.column 0
    //                Grid.row 1
    //            ]
    //            Button.create  [
    //                Button.content "Left"
    //                Grid.column 1
    //                Grid.row 0
    //            ]
    //            Button.create  [
    //                Button.content "Right"
    //                Grid.column 1
    //                Grid.row 1
    //            ]
    //        ]
    //    ]

    let tab () =
        Component(fun ctx ->
            TabControl.create [
                TabControl.tabStripPlacement Dock.Left
                TabControl.viewItems [
                    TabItem.create [
                        TabItem.header "DockPanel"
                        TabItem.content dockControl
                    ]
                    TabItem.create [
                        TabItem.header "StackPanel"
                        TabItem.content stackControl
                    ]
                    TabItem.create [
                        TabItem.header "WrapPanel"
                        TabItem.content wrapControl
                    ]
                    //TabItem.create [
                    //    TabItem.header "Grid"
                    //    TabItem.content gridControl
                    //]
                ]
            ]
        )

type MainWindow() =
    inherit HostWindow()
    do
        base.Title <- "Counter Example"
        //base.Content <- Main.view ()
        base.Content <- Main.tab ()

type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Add (FluentTheme())
        this.RequestedThemeVariant <- Styling.ThemeVariant.Dark

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
            desktopLifetime.MainWindow <- MainWindow()
        | _ -> ()

module Program =

    [<EntryPoint>]
    let main(args: string[]) =
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .UseSkia()
            .StartWithClassicDesktopLifetime(args)
