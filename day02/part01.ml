type move = Rock | Paper | Scissors

let move_bonus = function Rock -> 1 | Paper -> 2 | Scissors -> 3

let strat =
  let rec read_game_strat data_so_far =
    try
      let game = String.split_on_char ' ' (read_line ()) in
      match
        List.map
          (fun s ->
            if s = "A" || s = "X" then Rock
            else if s = "B" || s = "Y" then Paper
            else if s = "C" || s = "Z" then Scissors
            else failwith "Invalid character")
          game
      with
      | [ x; y ] -> read_game_strat ((x, y) :: data_so_far)
      | _ -> failwith "Invalid line"
    with End_of_file -> data_so_far
  in
  read_game_strat []

let move_bonus = function Rock -> 1 | Paper -> 2 | Scissors -> 3

let round_score = function
  | Rock, (Paper as x) | Paper, (Scissors as x) | Scissors, (Rock as x) ->
      move_bonus x + 6
  | y, x when x = y -> move_bonus x + 3
  | _, x -> move_bonus x

let _ =
  strat |> List.map round_score |> List.fold_left ( + ) 0 |> print_int
  |> print_newline
