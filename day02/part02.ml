type move = Rock | Paper | Scissors
type outcome = Win | Lose | Tie
type input = Move of move | Outcome of outcome

let move_bonus = function Rock -> 1 | Paper -> 2 | Scissors -> 3

let determine_move = function
  | Rock, Win | Paper, Tie | Scissors, Lose -> Paper
  | Paper, Win | Scissors, Tie | Rock, Lose -> Scissors
  | Scissors, Win | Rock, Tie | Paper, Lose -> Rock

let strat =
  let rec read_game_strat data_so_far =
    try
      let game = String.split_on_char ' ' (read_line ()) in
      match
        List.map
          (fun s ->
            if s = "A" then Move Rock
            else if s = "B" then Move Paper
            else if s = "C" then Move Scissors
            else if s = "X" then Outcome Lose
            else if s = "Y" then Outcome Tie
            else if s = "Z" then Outcome Win
            else failwith "Invalid character")
          game
      with
      | [ Move x; Outcome y ] ->
          read_game_strat ((x, determine_move (x, y)) :: data_so_far)
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
