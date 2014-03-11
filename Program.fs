module GameOfLife.Main
type Cell = Alive | Dead

type RowOfCells = Cell array
type World = RowOfCells array

let world_size = 10
let underworld = [| for i in 1..world_size -> [| for i in 1..world_size -> Dead |] |]
let worldAtLevelZero aliveCells = 
    let world = Array.copy underworld 
    List.iter (fun (x,y) -> world.[y].[x] <- Alive) aliveCells 
    world

let printWorld world = 
    Array.iter (fun rowOfCells -> 
        Array.iter (fun cell -> printf "%s" (match cell with Alive -> "#" | _ -> "-")) rowOfCells
        printfn ""
    ) world
    printfn ""


let liveOrDie spirit countOfNeighbours = 
    match spirit, countOfNeighbours with
        | Alive, x when x = 2 || x = 3 -> Alive
        | Dead, 3 -> Alive
        | _, _ -> Dead

let nextLevel (world:World) = 
    let neighbours x y = List.fold (fun acc offset_y -> 
        let (>=<) x (min, max) = (x >= min) && (x <= max)
        List.fold (fun neighbours_this_far offset_x -> 
            let neighbourX = x + offset_x
            let neighbourY = y + offset_y
            if (not (offset_x = 0 && offset_y = 0)) && (neighbourX >=< (0, world_size-1) && neighbourY >=< (0, world_size-1)) && (world.[neighbourY].[neighbourX] = Alive) then neighbours_this_far+1
            else neighbours_this_far 
                    ) acc [-1..1]) 0 [-1..1] 
    let nextGenRow (y, row) = 
        Array.mapi (fun x cell -> neighbours x y |> liveOrDie world.[y].[x]) row
    Array.mapi (fun y row -> nextGenRow (y, row)) world

let aliveCells = [1,1 ; 2,1; 3,1; 3,2; 3,0] // x,y tuples
let levelZero = worldAtLevelZero aliveCells

let life = Seq.unfold (fun prevLevel -> 
    Some(prevLevel, nextLevel prevLevel)) levelZero

Seq.take 10 life  |>  Seq.iter printWorld


