(* Taxicab numbers

   A taxicab number is a positive integer which can be expressed as
   the sum of two positive cubes in more than one way. (This is
   slightly different than the usual definition.)

   Please implement the function `taxicab` of type `int -> int` which,
   given a positive integer `n`, returns the the number of ways that
   `n` can be expressed as the sum of two positive cubes.

   Examples:
   let _ = assert (taxicab 2 = 1)
   let _ = assert (taxicab 5 = 0)
   let _ = assert (taxicab 1729 = 2)   (* 1729 = 1^3 + 12^3 = 3^3 + 9^3 *)
   let _ = assert (taxicab 4104 = 2)   (* 4104 = 2^3 + 16^3 = 9^3 + 15^3 *)

 *)

 let taxicab (n : int) : int =
  let is_perfect_cube (n1 : int) : bool =
    let cube_root = int_of_float (Float.round (float_of_int n1 ** (1.0 /. 3.0))) in
    (* Printf.printf "cube_root: %d, n1: %d\n" cube_root n1;   *)
    cube_root * cube_root * cube_root = n1  
  in
  let rec go c i =
    if i*i*i > n/2 then 
      c
    else
      if (is_perfect_cube (n - i*i*i)) then
        go (c+1) (i+1)
      else
        go c (i+1)
  in
  go 0 1
    