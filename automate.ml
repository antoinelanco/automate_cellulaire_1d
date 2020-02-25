type cel = ON | OFF
type reg = { a : cel; b : cel; c : cel; d : cel; e : cel; f : cel; g : cel; h : cel }

let () = Random.self_init ()


let rec decomp n =
  match n with
    | 0 -> []
    | n -> (decomp (n/2)) @ [if n mod 2 = 0 then OFF else ON]

let int_to_cel n =
  assert ( n >= 0 );
  assert ( 256 >= n );
  let bin = decomp n in
  let len = List.length bin in
  let compl = List.init (8-len) (fun x -> OFF) in
  let bin = compl @ bin in
  {
    a = List.nth bin 0;
    b = List.nth bin 1;
    c = List.nth bin 2;
    d = List.nth bin 3;
    e = List.nth bin 4;
    f = List.nth bin 5;
    g = List.nth bin 6;
    h = List.nth bin 7;
  }

let cel_to_string = function
  | ON -> "○"
  | OFF -> " "

let f l c r a =
  match l,c,r with
  | OFF,OFF,OFF -> a.a
  | OFF,OFF,ON  -> a.b
  | OFF,ON,OFF  -> a.c
  | OFF,ON,ON   -> a.d
  | ON,OFF,OFF  -> a.e
  | ON,OFF,ON   -> a.f
  | ON,ON,OFF   -> a.g
  | ON,ON,ON    -> a.h




let upgrade rule g =

  let rec aux rule g =
    match g with
    | l::c::[] -> [f l c OFF rule]
    | l::c::r::rest -> (f l c r rule) :: (aux rule (c::r::rest))
    | _ -> failwith "too short"
  in
  (f OFF (List.nth g 0) (List.nth g 1) rule) :: aux rule g


let affiche l = List.iter (fun x -> print_string (cel_to_string x)) l; Printf.printf "\n%!"

let rand_cel () = if Random.int 100 > 50 then ON else OFF

let rec start l r =
  let () = affiche l in
  let () = Unix.sleepf 0.05 in
  start (upgrade r l) r


let l = List.init 100 (fun x -> OFF ) @ [ON] @ List.init 100 (fun x -> OFF )
let a = int_to_cel (read_int ())
let () = start l a
