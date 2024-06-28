(* Note: This incantation allows us to tell the compiler to temporarily stop
   notifying us when we have unused functions and values. Feel free to delete
   after completing exercise 6. *)
[@@@disable_unused_warnings]

open Core

module Node_id : sig
  (** A [Node_id.t] uniquely identifies a node in a graph. We will using it later for
      looking up and setting the state of nodes in the course of our path search. *)
  type t [@@deriving compare, equal, sexp]

  include Comparable.S with type t := t

  val create : int -> t
end = struct
  module T = struct
    type t = int [@@deriving compare, equal, sexp]
  end

  (* Remember that this is the syntax for include modules such as [Map] and
     [Set] that are provided by [Comparable.Make] to our module. In our case,
     we use [Node_id.Map.t] in the [Nodes.t]. *)
  include T
  include Comparable.Make (T)

  let create id = id
end

module Edge = struct
  (** This type represents an edge between two nodes [a] and [b]. Note that since we are
      working with undirected graphs, the order of [a] and [b] do not matter. That is, an
      [Edge.t] { a = 1; b = 2; ... } is equivalent to { a = 2; b = 1; ... } *)

  module T = struct
    type t =
      { a : Node_id.t
      ; b : Node_id.t
      ; distance : int
      }
    [@@deriving compare, equal, sexp, fields]
  end

  include T
  include Comparable.Make (T)
end

module Edges = struct
  type t = Edge.t list [@@deriving sexp]

  (* Exercise 1: Given a [t] (list of edges) and a [Node_id.t], implement a
     function that returns a list of neighboring nodes with their
     corresponding distances. *)
  let neighbors t node_id : (Node_id.t * int) list =
    List.filter_map t ~f:(fun ed ->
      if Node_id.equal (Edge.a ed) node_id
      then Some (Edge.b ed, Edge.distance ed)
      else if Node_id.equal (Edge.b ed) node_id
      then Some (Edge.a ed, Edge.distance ed)
      else None)
  ;;

  (* We've left all of the tets in this file disabled. As you complete the
     exercises, please make sure to remove `[@tags "disabled"]` and run `dune
     runtest` to ensure that your implementation passes the test. *)
  let%expect_test "neighbors" =
    let n = Node_id.create in
    let n0, n1, n2, n3, n4, n5 = n 0, n 1, n 2, n 3, n 4, n 5 in
    let t =
      Edge.
        [ { a = n0; b = n1; distance = 1 }
        ; { a = n1; b = n2; distance = 3 }
        ; { a = n2; b = n3; distance = 2 }
        ; { a = n2; b = n4; distance = 1 }
        ; { a = n3; b = n5; distance = 5 }
        ; { a = n4; b = n5; distance = 1 }
        ]
    in
    let neighbors = neighbors t n2 in
    print_s [%message (neighbors : (Node_id.t * int) list)];
    [%expect {| (neighbors ((1 3) (3 2) (4 1))) |}]
  ;;
end

module Node = struct
  module State = struct
    type t =
      | Origin (** Used to mark the node where our search starts *)
      | Unseen (** Used to mark unexplored Nodes *)
      | Todo of
          { distance : int
          ; via : Node_id.t
          }
      (** Used to mark nodes that have been encountered but have not been processed yet *)
      | Done of { via : Node_id.t }
      (** Used to mark nodes that we are finished processing *)
    [@@deriving sexp]
  end

  type t = { mutable state : State.t } [@@deriving fields ~getters, sexp]

  let init () = { state = Unseen }
  let set_state t state = t.state <- state
end

module Nodes = struct
  (** This type represents a stateful collection of nodes in our graph. These [Node.t]s
      will be updated in the course of our graph search to keep track of progress. *)
  type t = Node.t Node_id.Map.t [@@deriving sexp]

  (* Exercise 2: Given a list of edges, create a [t] that contains all nodes
     found in the edge list. Note that you can construct [Node.t]s with the
     [Node.init] function. *)
  let of_edges (edges : Edges.t) : Node.t Node_id.Map.t =
    let node_map =
      Node_id.Map.of_alist_exn
        (List.map
           (List.dedup_and_sort
              ~compare:Node_id.compare
              (List.concat
                 (List.map edges ~f:(fun ed -> [ Edge.a ed; Edge.b ed ]))))
           ~f:(fun x -> x, Node.init ()))
    in
    node_map
  ;;

  let find = Map.find_exn
  let state t node_id = find t node_id |> Node.state

  let set_state t id state =
    let node = Map.find_exn t id in
    Node.set_state node state
  ;;

  (* Exercise 3: Given a [t], find the next node to process by selecting the
     node with the smallest distance along with its via route. *)

  let map_min_dist
    (to_do_list :
      (Node_id.t, int * Node_id.t, Node_id.comparator_witness) Map_intf.Map.t)
    : (Node_id.t * (int * Node_id.t)) option
    =
    let smallest = ref (Map.nth to_do_list 0) in
    Map.iteri to_do_list ~f:(fun ~key:nod ~data:(dist, via) ->
      match !smallest with
      | None -> smallest := Some (nod, (dist, via))
      | Some (n, (d, v)) ->
        if dist < d then smallest := Some (nod, (dist, via)) else ());
    !smallest
  ;;

  let next_node (t : t) : (Node_id.t * (int * Node_id.t)) option =
    let to_do_map =
      Map.filter_mapi t ~f:(fun ~key:node_id ~data:nod ->
        match Node.state nod with
        | Todo todos -> Some (todos.distance, todos.via)
        | _ -> None)
    in
    map_min_dist to_do_map
  ;;

  let%expect_test "next_node" =
    let n = Node_id.create in
    let n0, n1, n2, n3, n4, n5 = n 0, n 1, n 2, n 3, n 4, n 5 in
    let t =
      [ n0, { Node.state = Origin }
      ; n1, { Node.state = Done { via = n0 } }
      ; n2, { Node.state = Done { via = n1 } }
      ; n3, { Node.state = Todo { distance = 2; via = n1 } }
      ; n4, { Node.state = Todo { distance = 1; via = n1 } }
      ; n5, { Node.state = Unseen }
      ]
      |> Node_id.Map.of_alist_exn
    in
    let next_node = next_node t in
    print_s [%message (next_node : (Node_id.t * (int * Node_id.t)) option)];
    [%expect {| (next_node ((4 (1 1)))) |}]
  ;;

  let rec dest_to_origin (t : t) (curr_node_id : Node_id.t) : Node_id.t list =
    let curr_node = Map.find_exn t curr_node_id in
    match curr_node.state with
    | Done via ->
      let prev_node = via.via in
      dest_to_origin t prev_node @ [ curr_node_id ]
    | Origin -> [ curr_node_id ]
    | _ -> []
  ;;

  (* Exercise 4: Given a [t] that has already been processed from some origin
     -- that is the origin has been marked as [Origin] and nodes on the
     shortest path have been marked as [Done] -- return the path from the
     origin to the given [distination]. *)
  let path (t : t) (destination : Node_id.t) : Node_id.t list =
    dest_to_origin t destination
  ;;

  (* Excercise 5: Write an expect test for the [path] function above. *)
  let%expect_test "path" =
    let n = Node_id.create in
    let n0, n1, n2, n3, n4, n5 = n 0, n 1, n 2, n 3, n 4, n 5 in
    let t =
      [ n0, { Node.state = Origin }
      ; n1, { Node.state = Done { via = n0 } }
      ; n2, { Node.state = Done { via = n1 } }
      ; n3, { Node.state = Todo { distance = 2; via = n1 } }
      ; n4, { Node.state = Todo { distance = 1; via = n1 } }
      ; n5, { Node.state = Unseen }
      ]
      |> Node_id.Map.of_alist_exn
    in
    let node_p = dest_to_origin t n2 in
    List.iter node_p ~f:(fun node -> print_s [%message (node : Node_id.t)]);
    [%expect {| 
    (node 0)
    (node 1) 
    (node 2)
    |}]
  ;;
end

let make_to_do
  (network : Node.t Node_id.Map.t)
  (nodes_list : (Node_id.t * int) list)
  (parent : Node_id.t)
  (distance : int)
  =
  List.iter nodes_list ~f:(fun (nod, dist) ->
    match Node.state (Nodes.find network nod) with
    | Todo elt ->
      if distance + dist < elt.distance
      then
        Nodes.set_state
          network
          nod
          (Node.State.Todo { distance = distance + dist; via = parent })
      else ()
    | Unseen ->
      Nodes.set_state
        network
        nod
        (Node.State.Todo { distance = distance + dist; via = parent })
    | _ -> ())
;;

let rec find_path
  ~(edges : Edge.t list)
  ~(network : Node.t Node_id.Map.t)
  ~(spot : Node_id.t)
  ~(dist : int)
  ~(destination : Node_id.t)
  : Node_id.t list
  =
  (* print_s ([%sexp_of: Node_id.t] spot); *)
  (match Nodes.state network spot with
   | Origin -> ()
   | Todo elt ->
     Nodes.set_state network spot (Node.State.Done { via = elt.via })
   | _ -> ());
  if Node_id.equal spot destination
  then Nodes.path network destination
  else (
    let neighbors = Edges.neighbors edges spot in
    make_to_do network neighbors spot dist;
    let explore_next = Nodes.next_node network in
    match explore_next with
    | None -> []
    | Some (id, (dist, via)) ->
      find_path ~edges ~network ~spot:id ~dist ~destination)
;;

(* Exercise 6: Using the functions and types above, implement Dijkstras graph
   search algorithm! Remember to reenable unused warnings by deleting the
   first line of this file. *)
let shortest_path
  ~(edges : Edge.t list)
  ~(origin : Node_id.t)
  ~(destination : Node_id.t)
  : Node_id.t list
  =
  let nodes_net = Nodes.of_edges edges in
  Nodes.set_state nodes_net origin Node.State.Origin;
  find_path ~edges ~network:nodes_net ~spot:origin ~dist:0 ~destination
;;

let%expect_test "shortest_path" =
  let n = Node_id.create in
  let n0, n1, n2, n3, n4, n5 = n 0, n 1, n 2, n 3, n 4, n 5 in
  let edges =
    Edge.
      [ { a = n0; b = n1; distance = 1 }
      ; { a = n1; b = n2; distance = 1 }
      ; { a = n2; b = n3; distance = 1 }
      ; { a = n2; b = n4; distance = 1 }
      ; { a = n3; b = n5; distance = 2 }
      ; { a = n4; b = n5; distance = 1 }
      ]
  in
  let origin = n0 in
  let destination = n5 in
  let path = shortest_path ~edges ~origin ~destination in
  print_s ([%sexp_of: Node_id.t list] path);
  [%expect {| (0 1 2 4 5) |}]
;;

(* Exercise 7: Add some more test cases, exploring any corner cases you can
   think of. *)
