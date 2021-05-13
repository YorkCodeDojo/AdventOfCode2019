
let fuelRequired (mass:int): int = int (System.Math.Floor(decimal mass / 3.0m) - 2m)


let rec fuelRequired2 (mass:int) =
  let extra = fuelRequired mass
  match extra with
    | x when x > 0 -> extra + fuelRequired2 extra
    | _ -> 0

let data = System.IO.File.ReadAllLines("day1-puzzle-input.txt")
            |> Seq.map int
            |> Seq.map fuelRequired2
            |> Seq.sum

fuelRequired 12
fuelRequired 14
fuelRequired 12


fuelRequired2 1969


