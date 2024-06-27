open! Core

(* [get_linked_articles] should return a list of wikipedia article lengths contained in
   the input.

   Note that [get_linked_articles] should ONLY return things that look like wikipedia
   articles. In particular, we should discard links that are:
   - Wikipedia pages under special namespaces that are not articles (see
     https://en.wikipedia.org/wiki/Wikipedia:Namespaces)
   - other Wikipedia internal URLs that are not articles
   - resources that are external to Wikipedia
   - page headers

   One nice think about Wikipedia is that stringent content moderation results in
   uniformity in article format. We can expect that all Wikipedia article links parsed
   from a Wikipedia page will have the form "/wiki/<TITLE>". *)
let get_linked_articles (contents:string ): string list =
  let open Soup in 
  (* parse contents && "a" |> to_list |> List.filter_map ~f:(fun x -> attribute "href" x ) |> List.filter ~f:(fun x -> match Wikipedia_namespace.namespace x with |None -> true |_ -> false) *)
  let cont = parse contents in let nodes = cont $$ "a" in let node_list = to_list nodes in let attr_name = List.filter_map ~f:(fun x -> attribute "href" x ) node_list in
   List.filter ~f:(fun x -> match Wikipedia_namespace.namespace x with |None -> true |_ -> false) attr_name |> List.filter ~f:(fun x -> String.length x >= 6 && String.equal (String.sub x ~pos:0 ~len:6) "/wiki/") |> List.dedup_and_sort ~compare:(String.compare)
;;

let%expect_test "get linked artices cat" = 
  let cont = File_fetcher.fetch_exn (Local (File_path.of_string "../resources")) ~resource:"/wiki/Cat" in  print_endline (String.concat ~sep:" " (get_linked_articles cont));
  [%expect
  {| 
  /wiki/Carnivore /wiki/Domestication_of_the_cat /wiki/Mammal /wiki/Species
  |}
  ]

let print_links_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Print all of the valid wiki page links on a page"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_linked_articles contents) ~f:print_endline]
;;
(* module Link = struct
  module T = struct
  type t = {
    address:string
  } [@@deriving compare, sexp]
  end
include T
include Comparable.Make (T)
end *)

module Link = String

module Connection = struct
  module T = struct
  type t = {
    link1: Link.t;
    link2: Link.t
  } [@@deriving compare, sexp]
  end
include T
  include Comparable.Make (T)
end

module G = Graph.Imperative.Graph.Concrete (Link)

(* We extend our [Graph] structure with the [Dot] API so that we can easily
   render constructed graphs. Documentation about this API can be found here:
   https://github.com/backtracking/ocamlgraph/blob/master/src/dot.mli *)
module Dot = Graph.Graphviz.Dot (struct
    include G

    (* These functions can be changed to tweak the appearance of the
       generated graph. Check out the ocamlgraph graphviz API
       (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli)
       for examples of what values can be set here. *)
    let edge_attributes _ = [ `Dir `None ]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
    let vertex_name v = v
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

let get_article_name (address:string) ~(how_to_fetch:File_fetcher.How_to_fetch.t):string = 
  let open Soup in let contents = File_fetcher.fetch_exn how_to_fetch ~resource:address in 
  parse contents $$ "title" |> to_list  |> (fun x -> List.nth_exn x 0) |> texts |> String.concat |> String.split_on_chars ~on:['-';' ';'(';')';'.'] |> String.concat

(* [visualize] should explore all linked articles up to a distance of [max_depth] away
   from the given [origin] article, and output the result as a DOT file. It should use the
   [how_to_fetch] argument along with [File_fetcher] to fetch the articles so that the
   implementation can be tested locally on the small dataset in the ../resources/wiki
   directory. *)

let rec all_nodes  max_depth ~(origin:string) ~(how_to_fetch:File_fetcher.How_to_fetch.t) : Connection.t list = 
  let contents = File_fetcher.fetch_exn how_to_fetch ~resource:origin in
  let hyper_links = get_linked_articles contents in 
  let connection_list = List.map hyper_links ~f:(fun other -> {Connection.link1 = origin; link2 =  other }) in
  if max_depth > 0 then (
    let deeper_list = List.concat_map hyper_links ~f:(fun address -> all_nodes (max_depth -1) ~origin:address ~how_to_fetch) in connection_list @ deeper_list
  )
  else
    connection_list

let visualize ?(max_depth = 3) ~(origin:string) ~(output_file:File_path.t) ~(how_to_fetch:File_fetcher.How_to_fetch.t) () : unit =
  let network = all_nodes max_depth ~origin ~how_to_fetch in
        let graph = G.create () in
        List.iter network ~f:(fun connect ->
          let name1 = get_article_name connect.link1 ~how_to_fetch in let name2 = get_article_name connect.link2 ~how_to_fetch in
          (* [G.add_edge] auomatically adds the endpoints as vertices in the
             graph if they don't already exist. *)
          G.add_edge graph name1 name2);
        Dot.output_graph
          (Out_channel.create (File_path.to_string output_file))
          graph;
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing the highway \
       network"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        visualize ~max_depth ~origin ~output_file ~how_to_fetch ();
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

(* [find_path] should attempt to find a path between the origin article and the
   destination article via linked articles.

   [find_path] should use the [how_to_fetch] argument along with [File_fetcher] to fetch
   the articles so that the implementation can be tested locally on the small dataset in
   the ../resources/wiki directory.

   [max_depth] is useful to limit the time the program spends exploring the graph. *)

   let correct_url url = function
   | File_fetcher.How_to_fetch.Local _ -> url
   | Remote -> (
     if not (String.is_prefix ~prefix:"https://en.wikipedia.org" url) then
       "https://en.wikipedia.org" ^ url else url
   )

let check_destination current ~destination ~how_to_fetch= String.equal (correct_url current how_to_fetch) (correct_url destination how_to_fetch)

(* let get_solution url_list ~how_to_fetch = List.map url_list ~f:(fun url -> get_article_name ~how_to_fetch url) *)

let get_parse_solution (url_list: (string * int) list)~how_to_fetch = List.map url_list ~f:(fun (url, _dist) -> get_article_name ~how_to_fetch (correct_url url how_to_fetch))
 let not_visited ~(visited: Link.Hash_set.t) spot = not (Hash_set.mem visited spot)

let fetch_wikis (url:string) ~(how_to_fetch:File_fetcher.How_to_fetch.t) = File_fetcher.fetch_exn ~resource:(correct_url url how_to_fetch) how_to_fetch
  
;;

let find_path ?(max_depth = 3) ~origin ~destination ~how_to_fetch (): string list option =
  let visited = Link.Hash_set.create () in
  let to_do = Queue.create () in
  Queue.enqueue to_do [ (origin,max_depth) ];
  let rec solve () =
    let curr = Queue.dequeue to_do in
    match curr with
    | None -> None
    | _ ->
      let (spot, distance)= List.last_exn (Option.value_exn curr) in
      Hash_set.add visited spot;
      if check_destination spot ~destination ~how_to_fetch
      then Some (get_parse_solution (Option.value_exn curr) ~how_to_fetch)
      else (
        (if max_depth > 0 then (
        let new_spots =
          List.filter
            (get_linked_articles (fetch_wikis ~how_to_fetch spot))
            ~f:(fun s -> not_visited s ~visited)
        in
        List.iter new_spots ~f:(fun s ->
          Queue.enqueue to_do (Option.value_exn curr @ [ (s,(distance-1) )]) ))
        else ()); solve ())
  in
  solve ()
;;

(* let rec solve_path  ~max_depth ~(path:string list) ~destination ~(how_to_fetch:File_fetcher.How_to_fetch.t) ~(visited : Link.Hash_set.t ): string list option = 
  let curr = List.last_exn path in 
  Hash_set.add visited curr;
  if check_destination curr ~destination ~how_to_fetch then Some (get_solution path ~how_to_fetch) else (
  let hyper_links = get_linked_articles (fetch_wikis ~how_to_fetch curr) in 
  if max_depth > 0 then (
    let to_check = List.filter hyper_links ~f:(not_visited ~visited) in 
    let result = List.find_map to_check ~f:(fun lin -> print_endline lin; solve_path ~max_depth:(max_depth-1) ~path:(path @ [lin]) ~destination ~how_to_fetch ~visited) in result
  )
  else
    None
  )

let find_path ?(max_depth = 3) ~origin ~destination ~how_to_fetch (): string list option = 
  let start_path = [origin] in solve_path ~max_depth ~path:start_path ~destination ~how_to_fetch ~visited:(Link.Hash_set.create ()) *)

let find_path_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Play wiki game by finding a link between the origin and destination pages"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and destination = flag "destination" (required string) ~doc:" the destination page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      in
      fun () ->
        match find_path ~max_depth ~origin ~destination ~how_to_fetch () with
        | None -> print_endline "No path found!"
        | Some trace -> List.iter trace ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"wikipedia game commands"
    [ "print-links", print_links_command
    ; "visualize", visualize_command
    ; "find-path", find_path_command
    ]
;;
