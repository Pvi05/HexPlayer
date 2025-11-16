(* En utilisant la méthode de Gauss-Jordan / Matrice augmentée -> les matrices à inverser seront de taille 10^2 env *)

type 'a matrix = 'a array array

exception Err_Rang

let id (n : int) : float array array =
  let ret = Array.make_matrix n n 0. in
  for k = 0 to n - 1 do
    ret.(k).(k) <- 1.
  done;
  ret

let swapLines (mat : float array array) (l1 : int) (l2 : int) : unit =
  let n = Array.length mat in
  for j = 0 to n - 1 do
    let c = mat.(l1).(j) in
    mat.(l1).(j) <- mat.(l2).(j);
    mat.(l2).(j) <- c;
  done

let findMaxPiv (mat : float array array) (j : int) (mini : int) : int =
  let n = Array.length mat in
  let maxval = ref 0. in
  let maxi = ref (-1) in
  for i = mini to n - 1 do
    if abs_float(mat.(i).(j)) > !maxval then(
      maxval := mat.(i).(j);
      maxi := i)
  done;
  if !maxval = 0. then raise Err_Rang;
  !maxi

let normaliseLine (mat : float array array) (i : int) (norm : float) : unit =
  assert(norm <> 0.);
  let n = Array.length mat in
  for k = 0 to n - 1 do
    mat.(i).(k) <-  mat.(i).(k) /. norm
  done

let operationLine (mat : float array array) (l1 : int) (l2 : int) (mult : float) : unit =
  let n = Array.length mat in
  for j = 0 to n - 1 do
    mat.(l2).(j) <- mat.(l2).(j) -. (mult *. mat.(l1).(j));
  done


  let output_arr (out : out_channel) (gameMap : float array array) : unit =
    let n = Array.length gameMap in
    Printf.fprintf out "[| \n";
    for i = 0 to n - 1 do
      Printf.fprintf out "   [| ";
      for j = 0 to n - 2 do
        Printf.fprintf out "%f; " gameMap.(i).(j)
      done;
      if i < n - 1 then Printf.fprintf out "%f|]; \n" gameMap.(i).(n - 1)
      else Printf.fprintf out "%f|] \n" gameMap.(i).(n - 1)
    done;
    Printf.fprintf out "|] \n\n";
    flush out



let gJInversion (mat : float array array) : float array array =
  let n = Array.length mat in
  let aug = id n in
  for i = 0 to n - 1 do
    try
      let piv = findMaxPiv mat i i in
      if piv <> i then (swapLines mat i piv; swapLines aug i piv);
      let norm = mat.(i).(i) in
      normaliseLine mat i norm;
      normaliseLine aug i norm;
      for k = 0 to n - 1 do
        if (k <> i) then (
          let mult = mat.(k).(i) in
          operationLine mat i k mult;
          operationLine aug i k mult)
      done;
    with 
    | Err_Rang -> output_arr stdout mat; failwith "Matrice non inversible"
  done;
  aug

(* Ressources de test *)

let test = [|
  [|1.; 0.; 2.|];
  [|0.; -1.; 1.|];
  [|1.; -2.; 0.|]
|]

let transpose (mat : float array array) : float array array =
  let n = Array.length mat in
  let transp = Array.make_matrix n n 0. in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      transp.(i).(j) <- mat.(j).(i);
    done;
  done;
  transp

let produit (mat1 : float array array) (mat2 : float array array) : float array array =
  let n =  Array.length mat1 in
  let prod = Array.make_matrix n n 0. in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      let sum = ref 0. in
      for k = 0 to n - 1 do
        sum := !sum +. mat1.(i).(k)*.mat2.(k).(j);
      done;
      prod.(i).(j) <- !sum;
    done;
  done;
  prod

let genereMat (n : int) : float array array =
  let mat = Array.make_matrix n n 0. in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      mat.(i).(j) <- (float) (Random.int 10)
    done;
  done;
  produit mat (transpose mat)

let multiply (mat : float matrix) (vector : float array) =
  let n = Array.length vector in
  let result = Array.make n 0. in
  for k = 0 to n - 1 do
    for j = 0 to n - 1 do
      result.(k) <- vector.(j) *. mat.(k).(j)
    done;
  done;
  result