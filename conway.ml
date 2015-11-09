(* Simple implementation of Conways Game of Life.
 * Compile and run with
 * `ocamlc -o conway graphics.cma conway.ml && ./conway`. *)

(* open Graphics so we don't to to preface
 * everything with Graphics. *)
open Graphics;;

(* Creates a new double array square world of size by size. *)
let new_world size =
  Array.make_matrix size size false;;

(* Add a glider to the world with a top left corner of x, y. *)
let add_glider world x y =
  begin
    world.(y).(x + 2) <- true;
    world.(y + 1).(x + 2) <- true;
    world.(y + 2).(x + 2) <- true;
    world.(y + 1).(x) <- true;
    world.(y + 2).(x + 1) <- true;
  end;;

let draw_world world =
  for y = 0; to Array.length world - 1 do
    let slice = world.(y) in
    for x = 0; to Array.length slice - 1 do
      if slice.(x) then fill_rect (x * 8) (y * 8) 8 8;
    done;
  done;;

let rec num_neighbours world cx cy =
  let len = Array.length world in
  let num = ref 0 in
  for i = -1; to 1 do
    for j = -1; to 1 do
      let x = cx + j in
      let y = cy + i in
      let in_bounds = y > 0 && y < len && x > 0 && x < len in
      if in_bounds && (x != cx || y != cy) && world.(y).(x) then num := !num + 1;
    done;
  done;
  !num;;

let new_state world x y =
  let n = num_neighbours world x y in
  let alive = world.(y).(x) in
  if alive && n < 2 then false
  else if alive && (n = 2 || n = 3) then true
  else if alive && n > 3 then false
  else if (not alive) && n = 3 then true
  else false;;

let tick_world world =
  let n = new_world (Array.length world) in
  for y = 0; to Array.length world - 1 do
    let old_slice = world.(y) in
    let new_slice = n.(y) in
    for x = 0; to Array.length world.(y) - 1 do
      let s = new_state world x y in
      new_slice.(x) <- s;
    done;
    n.(y) <- new_slice;
  done;
  n;;

(* Initialize the screen for drawing. *)
open_graph " 512x512";;
set_color (black);;

(* Initialize and draw a world with one glider. *)
let w = new_world 64;;
add_glider w 1 1;;

(* Draw tick loop. *)
let rec loop world count =
  clear_graph ();
  draw_world world;
  let new_world = tick_world world in
  Unix.sleep(1);
  if count > 0 then loop new_world (count - 1) else ();;

loop w 62;;

(* read_line blocks, keeping the window open. *)
read_line ();;
