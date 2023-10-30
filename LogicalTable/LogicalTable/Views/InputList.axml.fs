namespace LogicalTable.Views

open System
open Avalonia
open Avalonia.Controls
open Avalonia.Markup.Xaml

type InputList() as this =
    inherit UserControl()

    let definition n =
        Seq.init n (fun _ -> "20") |> String.concat ","

    let getBit i n digit = $"%B{n}".PadLeft(digit, '0').[i]

    static let DigitProperty: StyledProperty<int> =
        AvaloniaProperty.Register<InputList, int>(nameof Unchecked.defaultof<InputList>.Digit, defaultValue = 2)

    do this.InitializeComponent()

    member private this.InitializeComponent() = AvaloniaXamlLoader.Load(this)

    member this.Update =
        let digit = this.Digit

        if digit <= 1 then
            raise (Exception $"Unexpected n: {digit}")
        else
            let column = digit
            let row = pown 2 digit
            let grid: Grid = this.GetControl "InputGrid"

            grid.ColumnDefinitions <- ColumnDefinitions(definition column)
            grid.RowDefinitions <- RowDefinitions(definition row)

            grid.Children.RemoveAll grid.Children

            for r in 0 .. row - 1 do
                for c in 0 .. column - 1 do
                    let label = Label()
                    let bit = getBit c r digit
                    label.Content <- bit
                    label.SetValue(Grid.RowProperty, r) |> ignore
                    label.SetValue(Grid.ColumnProperty, c) |> ignore
                    grid.Children.Add label

    member this.Digit
        with get () = this.GetValue DigitProperty
        and set value = this.SetValue(DigitProperty, value) |> ignore

    override this.OnPropertyChanged(change: AvaloniaPropertyChangedEventArgs) =
        base.OnPropertyChanged(change)

        if $"%A{change.Property}" = "Digit" then
            change.Property.Equals DigitProperty |> ignore

        if change.Property = DigitProperty then
            this.Digit <- change.GetNewValue<int>()
            this.Update
