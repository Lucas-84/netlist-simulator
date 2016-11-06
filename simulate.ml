(**
 * Simulateur de netlist 
 *)
open Graph
open Netlist_ast

module Imap = Map.Make(struct type t = int let compare = compare end)

let env = ref Env.empty
let quiet_mode = ref false
let rom_file = ref ""

exception Wrong_input

(* Input/output *)
let bool_of_char c = match c with
  | '1' -> true
  | '0' -> false
  | _ -> raise Wrong_input

let rec read_of_type id exp_type =
  (* %! flushes stdout *)
  if not !quiet_mode then Format.printf "%s ? %!" id;
  let s = read_line () in
  try
    (* MSB representation *)
    begin match exp_type with
      | TBit when String.length s = 1 -> VBit (bool_of_char s.[0])
      | TBitArray n when String.length s = n ->
        let a = Array.make n false in
        for i = 0 to n - 1 do
          a.(i) <- bool_of_char s.[i]
        done;
        VBitArray a
      | _ -> raise Wrong_input
    end
  with
    Wrong_input -> read_of_type id exp_type

let string_of_bool b =
  if b then "1" else "0"

let print_value value = match value with
  | VBit (b) ->
    print_string (string_of_bool b)
  | VBitArray (a) ->
    let s = Array.fold_right (fun b ans -> (string_of_bool b) ^ ans) a "" in
    print_string s

(* DEBUG function *)
let print_env () =
  Printf.printf "---- DEBUG ----\n";
  Env.iter (fun var value -> Printf.printf "Env[%s] = " var; print_value value; print_newline ()) !env

let rec simulate_inputs types input_ids = match input_ids with
  | [] -> ()
  | id :: other_ids -> 
    let exp_type = Env.find id types in
    env := Env.add id (read_of_type id exp_type) !env;
    simulate_inputs types other_ids

(* Simulation phase *)
let value_from_id id =
  Env.find id !env
    
let value_from_arg arg = match arg with
  | Avar id -> value_from_id id
  | Aconst value -> value

let bool_of_val value = match value with
  | VBit true -> true
  | VBit false -> false
  | _ -> failwith "invalid bool_of_val"

let rec int_of_val value = match value with
  | VBit true -> 1
  | VBit false -> 0
  | VBitArray a ->
    let n = Array.length a in
    let ans = ref 0 in
    for i = 0 to n - 1 do
      ans := !ans * 2 + (int_of_val (VBit a.(i)))
    done;
    !ans

let array_of_val value = match value with
  | VBitArray a -> a
  (* polymorphism concat/slice/select operations *)
  | VBit b -> Array.make 1 b

let rec simulate_binop op val1 val2 = match val1, val2 with
  | VBit b1, VBit b2 ->
    VBit (match op with
      | Or -> b1 || b2
      | And -> b1 && b2
      | Xor -> (b1 && not b2) || (not b1 && b2)
      | Nand -> not (b1 && b2))
  | VBitArray a1, VBit b2 ->
    simulate_binop op val1 (VBitArray (Array.make 1 b2))
  | VBit b1, VBitArray a2 ->
    simulate_binop op (VBitArray (Array.make 1 b1)) val2
  | VBitArray a1, VBitArray a2 ->
    let n = max (Array.length a1) (Array.length a2) in
    let a = Array.make n false in
    for i = 0 to n - 1 do
      let b1 = if i < Array.length a1 then a1.(i) else false in
      let b2 = if i < Array.length a2 then a2.(i) else false in
      let b = match simulate_binop op (VBit b1) (VBit b2) with VBit b -> b | _ -> failwith "error: wrong datatype in binop" in
      a.(i) <- b
    done;
    VBitArray a
  
exception Ram_access

let simulate_eq (id_result, exp) ram rom write_phase =
  try
    let result = begin match exp with
      | Earg arg -> value_from_arg arg
      | Ereg id -> value_from_id id 
      | Enot arg -> VBit (not (bool_of_val (value_from_arg arg)))
      | Ebinop (op, arg1, arg2) -> simulate_binop op (value_from_arg arg1) (value_from_arg arg2)
        (* Convention *)
      | Emux (arg1, arg2, arg3) ->
        value_from_arg (if bool_of_val (value_from_arg arg3) then arg2 else arg1)
      | Econcat (arg1, arg2) ->
        VBitArray (Array.append (array_of_val (value_from_arg arg1)) (array_of_val (value_from_arg arg2)))
      | Eslice (i1, i2, arg) ->
        VBitArray (Array.sub (array_of_val (value_from_arg arg)) i1 (i2 - i1 + 1))
      | Eselect (i, arg) ->
        VBit (array_of_val (value_from_arg arg)).(i)
      | Erom (_, word_size, read_addr) ->
        let read_addr = int_of_val (value_from_arg read_addr) in
        VBitArray (Array.sub rom read_addr word_size)
      | Eram (_, word_size, read_addr, write_enable, write_addr, data) ->
        let read_addr = int_of_val (value_from_arg read_addr) in
        let write_enable = match value_from_arg write_enable with VBit b -> b | _ -> failwith "error: wrong datatype in RAM" in
        let write_addr = int_of_val (value_from_arg write_addr) in
        let data = array_of_val (value_from_arg data) in
        if write_phase then begin
          if write_enable then begin
            for i = 0 to word_size - 1 do
              ram.(write_addr + i) <- data.(i) 
            done
          end;
          raise Ram_access
        end
        else VBitArray (Array.sub ram read_addr word_size)
    end in
    env := Env.add id_result result !env
  with _ -> () 

let simulate_eqs eqs ram rom write_phase =
  List.iter (fun eq -> simulate_eq eq ram rom write_phase) eqs

let rec simulate_outputs output_ids = match output_ids with
  | [] -> ()
  | id :: other_ids ->
    if not !quiet_mode then Printf.printf "=> %s = " id;
    print_value (Env.find id !env);
    print_newline ();
    simulate_outputs other_ids

(* Sorting phase *)
let rec split_eqs eqs = match eqs with
  | [] -> [], [] 
  | ((_, first_exp) as first_eq) :: other_eqs ->
    let normal_eqs, time_eqs = split_eqs other_eqs in
    begin match first_exp with
      | Eram _ -> first_eq :: normal_eqs, first_eq :: time_eqs
      | Ereg _ -> normal_eqs, first_eq :: time_eqs
      | _ -> first_eq :: normal_eqs, time_eqs
    end

let get_arg_deps arg = match arg with
  | Avar id -> [id]
  | _ -> []

let get_eq_deps (_, exp) write_phase = match exp with
  | Earg arg -> get_arg_deps arg
  | Ereg id -> [id]
  | Enot arg -> get_arg_deps arg
  | Ebinop (_, arg1, arg2) -> (get_arg_deps arg1) @ (get_arg_deps arg2)
  | Emux (arg1, arg2, arg3) -> (get_arg_deps arg1) @ (get_arg_deps arg2) @ (get_arg_deps arg3)
  | Eram (_, _, _, write_enable, write_addr, data) when write_phase ->
    (get_arg_deps write_enable) @ (get_arg_deps write_addr) @
    (get_arg_deps data)
  | Eram (_, _, read_addr, _, _, _) -> get_arg_deps read_addr
  | Erom (_, _, arg) -> get_arg_deps arg
  | Eselect (_, arg) -> get_arg_deps arg
  | Eslice (_, _, arg) -> get_arg_deps arg
  | Econcat (arg1, arg2) -> (get_arg_deps arg1) @ (get_arg_deps arg2)

let sort_all_eqs eqs =
  let id_to_eq = List.fold_left (fun ans ((id, _) as eq) -> Env.add id eq ans) Env.empty eqs in
  let id_to_ram = List.fold_left (fun ans (id, exp) -> Env.add id (match exp with  Eram _ -> true | _ -> false) ans) Env.empty eqs in
  let sort_eqs eqs write_phase =
    let g = mk_graph () in
    let add_dep eq idlist =
      List.iter (fun id -> try
                  if not write_phase || not (Env.find id id_to_ram) then begin
                    let other_eq = Env.find id id_to_eq in
                    if not write_phase then add_edge g other_eq eq
                    else add_edge g eq other_eq
                  end
                with Not_found -> ()) idlist
    in
    List.iter (fun eq -> add_node g eq) eqs; 
    List.iter (fun eq -> add_dep eq (get_eq_deps eq write_phase)) eqs;
    if not write_phase && has_cycle g then failwith "Combinational cycle"
    else topological g
  in
  let normal_eqs, time_eqs = split_eqs eqs in
  sort_eqs normal_eqs false, sort_eqs time_eqs true

let init_var var ty =
  let value = match ty with
    | TBit -> VBit false
    | TBitArray n -> VBitArray (Array.make n false) 
  in
  env := Env.add var value !env

let init_vars vars = 
  Env.iter (fun var ty -> init_var var ty) vars

let nb_total_cycles = ref max_int 

let init_ram eqs =
  let size = List.fold_left (fun ans (_, exp) -> max ans (match exp with Eram (addr_size, _, _, _, _, _) -> addr_size | _ -> 0)) 0 eqs in
  Array.make (1 lsl size) false 

let load_rom eqs =
  let size = List.fold_left (fun ans (_, exp) -> max ans (match exp with Erom (addr_size, _, _) -> addr_size | _ -> 0)) 0 eqs in 
  let rom = Array.make (1 lsl size) false in
  begin try
    let f = open_in !rom_file in
    let s = input_line f in
    for i = 0 to String.length s - 1 do
      rom.(i) <- if s.[i] = '1' then true else false
    done;
    close_in f;
  with _ -> ()
  end;
  rom

let simulate filename =
  try
    let program = Netlist.read_file filename in
    (* Initialization phase *)
    init_vars program.p_vars;
    let ram = init_ram program.p_eqs in
    let rom = load_rom program.p_eqs in
    (* Sorting phase *)
    let normal_eqs, time_eqs = sort_all_eqs program.p_eqs in
    (* Simulation phase *)
    for id_cycle = 1 to !nb_total_cycles do
      if not !quiet_mode then Printf.printf "Step %d:\n" id_cycle;
      simulate_inputs program.p_vars program.p_inputs;
      simulate_eqs normal_eqs ram rom false;
      simulate_outputs program.p_outputs;
      simulate_eqs time_eqs ram rom true
    done
  with Netlist.Parse_error s ->
    Format.eprintf "%s@." s;
    exit 2

let main () =
  Arg.parse
    ["-n", Arg.Set_int nb_total_cycles, "Number of steps to simulate";
     "-q", Arg.Set quiet_mode, "Quiet mode";
     "-rom", Arg.Set_string rom_file, "ROM file"]
    simulate
    "Netlist simulator"

let () = main ()
