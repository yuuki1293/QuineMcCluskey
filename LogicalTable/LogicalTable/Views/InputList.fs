namespace LogicalTable.Views

open System
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Markup.Xaml

type MainView() as this =
    inherit UserControl()

    let definition n =
        Seq.init n (fun _ -> "20") |> String.concat ","

    do this.InitializeComponent()

    member private this.InitializeComponent() = AvaloniaXamlLoader.Load(this)

    member this.Update(_: obj, _: RangeBaseValueChangedEventArgs) =
        let slider = this.GetControl "N" :> Slider
        let n = slider.Value |> int

        if n <= 1 then
            raise (Exception $"Unexpected n: {n}")
        else
            let column = n
            let row = pown 2 n
            let grid: Grid = this.GetControl "InputGrid"

            grid.ColumnDefinitions <- ColumnDefinitions(definition column)
            grid.RowDefinitions <- RowDefinitions(definition row)
            
            grid.Children.RemoveAll grid.Children

            for r in 0 .. row - 1 do
                for c in 0 .. column - 1 do
                    let label = Label()
                    label.Content <- r * c
                    label.SetValue(Grid.RowProperty, r) |> ignore
                    label.SetValue(Grid.ColumnProperty, c) |> ignore
                    grid.Children.Add label
