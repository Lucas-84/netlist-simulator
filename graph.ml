exception Cycle
type mark = NotVisited | InProgress | Visited

type 'a graph =
  { mutable g_nodes : 'a node list }
and 'a node = {
  n_label : 'a;
  mutable n_mark : mark;
  mutable n_link_to : 'a node list;
  mutable n_linked_by : 'a node list;
}

let mk_graph () = { g_nodes = [] }

let add_node g x =
  let n = { n_label = x; n_mark = NotVisited; n_link_to = []; n_linked_by = [] } in
  g.g_nodes <- n::g.g_nodes

let node_for_label g x =
  List.find (fun n -> n.n_label = x) g.g_nodes

let add_edge g id1 id2 =
  let n1 = node_for_label g id1 in
  let n2 = node_for_label g id2 in
  n1.n_link_to <- n2::n1.n_link_to;
  n2.n_linked_by <- n1::n2.n_linked_by

let clear_marks g =
  List.iter (fun n -> n.n_mark <- NotVisited) g.g_nodes

let find_roots g =
  List.filter (fun n -> n.n_linked_by = []) g.g_nodes

let has_cycle g =
  let rec is_cycle_from_node u = match u.n_mark with
    | NotVisited ->
      u.n_mark <- InProgress;
      let ans = is_cycle_from_list u.n_link_to in
      u.n_mark <- Visited;
      ans
    | InProgress ->
      true
    | Visited ->
      false
  and is_cycle_from_list l =
    List.fold_left (fun ans u -> ans || (is_cycle_from_node u)) false l
  in
  clear_marks g;
  is_cycle_from_list g.g_nodes

(* Do not take cycles into account *)
let topological g = 
  let rec sort_from_node u prev = match u.n_mark with
    | NotVisited ->
      u.n_mark <- InProgress;
      let ans = sort_from_list u.n_link_to prev in
      u.n_mark <- Visited;
      u.n_label :: ans
    | _ ->
      prev 
  and sort_from_list l prev =
    List.fold_left (fun ans u -> sort_from_node u ans) prev l
  in
  clear_marks g;
  sort_from_list g.g_nodes []
