
#r @".\packages\XPlot.Plotly\lib\netstandard2.0\XPlot.Plotly.dll"

open XPlot.Plotly

let chart name items = Scatter(name = name, y = items)

let showAll (x: Scatter list) =
    x
    |> Chart.Plot
    |> Chart.WithWidth 1400
    |> Chart.WithHeight 900
    |> Chart.Show

let show x = showAll [x]
