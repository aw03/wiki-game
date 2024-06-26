open! Core

module Grid = struct
  module T = struct
    type t =
      { height : int
      ; width : int
      ; wall_cords : (int * int) list
      ; start_cord : int * int
      ; end_cord : int * int
      }
    [@@deriving compare, sexp, fields]
  end

  include T
  include Comparable.Make (T)
end

module Coord = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp, hash]
  end

  include T
  include Comparable.Make (T)
end

let parse_maze ~(input_file : File_path.t) : Grid.t =
  let maze_lines = In_channel.read_lines (File_path.to_string input_file) in
  let start = ref (0, 0) in
  let end_c = ref (0, 0) in
  let wall_list =
    List.mapi maze_lines ~f:(fun row line ->
      let char_list = String.to_list line in
      List.filter_mapi char_list ~f:(fun col letter ->
        match letter with
        | '#' -> Some (row, col)
        | 'S' ->
          start := row, col;
          None
        | 'E' ->
          end_c := row, col;
          None
        | _ -> None))
  in
  let h = List.length maze_lines in
  let w = String.length (List.nth_exn maze_lines 0) in
  { Grid.height = h
  ; width = w
  ; wall_cords = List.concat wall_list
  ; start_cord = !start
  ; end_cord = !end_c
  }
;;

let free_spot (row, col) ~walls =
  not
    (List.mem walls (row, col) ~equal:(fun (r, c) (row, col) ->
       r = row && c = col))
;;

let check_in_bounds (row, col) ~height ~width ~walls =
  row < height
  && row >= 0
  && col < width
  && col >= 0
  && free_spot (row, col) ~walls
;;

let get_neighbors (row, col) ~height ~width ~walls : (int * int) list =
  let possibilities =
    [ row + 1, col; row - 1, col; row, col + 1; row, col - 1 ]
  in
  List.filter possibilities ~f:(fun coord ->
    check_in_bounds coord ~height ~width ~walls)
;;

let not_visited (row, col) ~(visited : Coord.t Hash_set.t) =
  not (Hash_set.mem visited (row, col))
;;

let check_reach_end (row, col) (end_r, end_c) = row = end_r && col = end_c

let print_solution (path : (int * int) list) : unit =
  List.iter path ~f:(fun (r, c) ->
    print_endline ("(" ^ string_of_int r ^ "," ^ string_of_int c ^ ")"))
;;

let solve_maze input_file : unit =
  let maze = parse_maze ~input_file in
  let visited = Hash_set.create (module Coord) in
  let to_do = Stack.create () in
  Stack.push to_do [ maze.start_cord ];
  let rec solve () =
    let curr = Stack.pop to_do in
    match curr with
    | None -> print_endline "No possible solution!"
    | _ ->
      let spot = List.last_exn (Option.value_exn curr) in
      Hash_set.add visited spot;
      if check_reach_end spot (Grid.end_cord maze)
      then print_solution (Option.value_exn curr)
      else (
        let new_spots =
          List.filter
            (get_neighbors
               spot
               ~height:(Grid.height maze)
               ~width:maze.width
               ~walls:maze.wall_cords)
            ~f:(fun s -> not_visited s ~visited)
        in
        List.iter new_spots ~f:(fun s ->
          Stack.push to_do (Option.value_exn curr @ [ s ]));
        solve ())
  in
  solve ()
;;

let solve_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file containing a maze and find a solution"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file containing a maze"
      in
      fun () -> solve_maze input_file]
;;

let command =
  Command.group ~summary:"maze commands" [ "solve", solve_command ]
;;
