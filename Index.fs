let rec merge left right =
    match left, right with
    | [], _ -> right
    | _, [] -> left
    | x :: xs, y :: ys ->
        if x < y then
            x :: merge xs right
        else
            y :: merge left ys

let rec mergeSort list =
    match list with
    | [] | [_] -> list
    | _ ->
        let mid = List.length list / 2
        let left = list |> List.take mid
        let right = list |> List.skip mid
        merge (mergeSort left) (mergeSort right)

// Example usage
let unsortedList = [38; 27; 43; 3; 9; 82; 10]
let sortedList = mergeSort unsortedList

printfn "Sorted list: %A" sortedList
