open! Core

(* [get_credits] should take the contents of an IMDB page for an actor and
   return a list of strings containing that actor's main credits. *)
let get_credits (contents : string) : string list =
  (* let open Soup in let divs = parse contents $$ "div" in let div_list =
     to_list divs in let class_div = List.filter_map div_list ~f:(fun x ->
     (attribute "class" x)) in let List.filter class_div ~f:(String.equal
     "ipc-primary-image-list-card__title") *)
  let open Soup in
  parse contents
  $$ "a[class=ipc-primary-image-list-card__title]"
  |> to_list
  |> List.filter_map ~f:leaf_text
;;

let%expect_test "get IMB Credits" =
  let cont =
    File_fetcher.fetch_exn
      Remote
      ~resource:"https://www.imdb.com/name/nm0000706/?ref_=fn_al_nm_1"
  in
  print_endline (String.concat ~sep:" " (get_credits cont));
  [%expect
    {| 
  Everything Everywhere All at Once Crouching Tiger, Hidden Dragon Crazy Rich Asians Tomorrow Never Dies
  |}]
;;

let print_credits_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "given an IMDB page for an actor, print out a list of their main \
       credits"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_credits contents) ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"imdb commands"
    [ "print-credits", print_credits_command ]
;;
