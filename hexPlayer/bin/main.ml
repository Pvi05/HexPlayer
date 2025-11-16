(* open HeatMap *)
open HexLib
(* open InvertMat *)

(* let hM (i : int) : int matrix =
  match i with
  |3 -> hM3x3
  |4 -> hM4x4
  |5 -> hM5x5
  |6 -> hM6x6
  |7 -> hM7x7
  |8 -> hM8x8
  |9 -> hM9x9
  |10 -> hM10x10
  |_ -> failwith "duh" *)

(* let () = 
for i = 9 to 9 do
  let test = newGame i in
  let t = ref (Sys.time ()) in
  let k = ref 0 in
  while (Sys.time () -. !t < 40.) do
    t := (Sys.time ());
  ignore(hexy_noTLAndMO !k 20 5 test);
  Printf.printf "; %d; %d; %f \n" i !k (Sys.time () -. !t);
  flush stdout;
    k := !k + 1
  done;
done; *)
  

(* let randomGame2 (n : int) : hexGame =
  (* Setup *)
  Random.self_init ();
  let game = newGame n in
  let avCol_std = Array.make (n + 1) 0 in
  avCol_std.(0) <- n;
  for k = 1 to n do
    avCol_std.(k) <- k - 1
  done;
  let avLines = Array.copy avCol_std in
  let avCol = Array.make n [||] in
  for k = 0 to n - 1 do
    avCol.(k) <- Array.copy avCol_std
  done;
  (* Setup done *)
  while not (isWinningPos game (-1 * game.curPlayer)) do
    (*Verifie que le dernier coup de _l'autre_ joueur ne l'a pas fait gagner*)
    let r1, r2 = randomPlay avLines avCol in
    let i = avLines.(r1) in
    let j = avCol.(i).(r2) in
    play game i j;
    let p = avCol.(i).(0) in
    avCol.(i).(r2) <- avCol.(i).(p);
    avCol.(i).(0) <- p - 1;
    if avCol.(i).(0) = 0 then (
      let pl = avLines.(0) in
      avLines.(r1) <- avLines.(pl);
      avLines.(0) <- pl - 1);
    let t = ref (Sys.time ()) in
    ignore(heur hM10x10 game);
    Printf.printf "%f \n" (Sys.time () -. !t)
  done;
  game;;

ignore (randomGame2 10);; *)
(* 
let () = playGame stdout 6 (hexy_fullOpti 2 20 5 20.) (hexy_noTLAndMO 1 20 5) ;;  *)

(* let test = newGame 3;;
play test 1 1;;
let vc2, vsc2, posl2 =  buildFromGameP2_2 test;;
hsearch test posl2 vc2 vsc2 (20) 5;;
(* ignore(resMat test (-1) vc2);; *)
let k = kirchSystemMatrix test (-1) vc2;;
(* ignore(gJInversion k); *)
output_arrfl stdout (gJInversion k);;
Printf.printf "%f" (equivalentResistor test vc2 (-1)) *)

(* let mat = genereMat 2500;;
let t = Sys.time ();;
ignore (gJInversion mat);;
Printf.printf "Exec time : %f\n" (Sys.time() -. t) *)

(* let _ = InvertMat.produit *)

(* let 
test = newGame 3 in
let  *)

(* 
let test = newGame 5;;
play test 3 3;;
let t = Sys.time ();;
Printf.printf "%d" (evalFromHsearch test);;
(* let (i, j), sc = hexy_noTimeLimit 2 20 5 test;;
Printf.printf "Play : %d, %d, Score : %d" i j sc;; *)
Printf.printf "Exec time : %f\n" (Sys.time() -. t);; *)


(* let test = newGame 10;;
play test 3 3;;
(* let t = Sys.time ();; *)
let (i, j), sc = hexy_fullOpti 20 5 30. test;;
Printf.printf "Play : %d, %d, Score : %d" i j sc;; *)
(* Printf.printf "Exec time : %f" (Sys.time() -. t) *)


(* let test = newGame 5;;
let vc1, vsc1, posl1 =  buildFromGameP1_2 test;;
let vc2, vsc2, posl2 =  buildFromGameP2_2 test;;
hsearch test posl1 vc1 vsc1 (20) 5;
hsearch test posl2 vc2 vsc2 (20) 5;
updateHexyTabl (4, 4) (true) (vc1, vsc1, posl1);
updateHexyTabl (4, 4) (false) (vc2, vsc2, posl2);
play test 4 4;;
(* updateHexyTabl (0, 0) (false) (vc1, vsc1, posl1);
updateHexyTabl (0, 0) (true) (vc2, vsc2, posl2);
play test 0 0;; *)
hsearch test posl1 vc1 vsc1 (20) 5;
hsearch test posl2 vc2 vsc2 (20) 5;
output_arr stdout test.gameSet;
Printf.printf "%d %d " test.curPlayer test.nbPlay;
Printf.printf "%d /" (quickEval test vc1 vc2);;

let test = newGame 5;;
play test 4 4;;
(* play test 0 0;; *)
let vcref1, vscref1, poslref1 =  buildFromGameP1_2 test;;
let vcref2, vscref2, poslref2 =  buildFromGameP2_2 test;;
hsearch test poslref1 vcref1 vscref1 (20) 5;;
hsearch test poslref2 vcref2 vscref2 (20) 5;;
output_arr stdout test.gameSet;
Printf.printf "%d %d " test.curPlayer test.nbPlay;
debug_ListDifferencies vc2 vcref2;
debug_ListDifferencies vcref2 vc2;
debug_ListDifferencies vc1 vcref1;
debug_ListDifferencies vcref1 vc1;
Printf.printf "%d /" (quickEval test vcref1 vcref2);;
let test3 = newGame 5;;
play test3 4 4;;
Printf.printf "%d" (evalFromHsearch 20 5 test3);; *)

(* let test = newGame 10;;
play test 4 4;;
play test 9 1;;
play test 1 6;;


let vc1, vsc1, posl1 =  buildFromGameP1_2 test;;
let vc2, vsc2, posl2 =  buildFromGameP2_2 test;;
hsearch test posl1 vc1 vsc1 (20) 5;
hsearch test posl2 vc2 vsc2 (20) 5;;
let t1 = Sys.time();;
ignore(quickEval test vc1 vc2);;
Printf.printf "Exec time : %f" (Sys.time() -. t1);; *)
(* hsearch test posl vc vsc (20) 5;;
Printf.printf "Exec time : %f" (Sys.time() -. t1);; *)

(* let b = Hashtbl.mem vc (cpl (-1, -1) (0, 9));;
if b then Printf.printf "true" else Printf.printf "false" *)

(* let nb, _ = Hashtbl.find vc (cpl (-1, -1) (0, 9));;
Printf.printf "%d" nb;;  *)

(* 
let test = newGame 5;;
play test 3 3;;
let vcref, vscref, poslref =  buildFromGameP2_2 test;;
hsearch test poslref vcref vscref (20) 5;;

let test = newGame 5;;
let vctest, vsctest, posltest =  buildFromGameP2_2 test;;
hsearch test posltest vctest vsctest (20) 5;;
updateHexyTabl (3, 3) (false) (vctest, vsctest, posltest);;
play test 3 3;;
hsearch test posltest vctest vsctest (20) 5;;

Printf.printf "manquant dans vctest : \n";;
debug_ListDifferencies vcref vctest;;
Printf.printf "manquant dans vcref : \n";;
debug_ListDifferencies vctest vcref;; *)
(* let print_list (l : position list) : unit = 
  List.iter (fun (i, j) -> Printf.printf "| %d, %d |" i j) l


let test = newGame 9;;
test.gameSet.(4).(2) <- 1;;
test.gameSet.(5).(5) <- 1;;
(* test.gameSet.(3).(4) <- 1;;
test.gameSet.(5).(4) <- 1;;
test.gameSet.(7).(6) <- 1;;
test.gameSet.(7).(7) <- 1;; *)
let vc1, vsc1, posl1 = buildFromGameP1_2 test;;
hsearch test posl1 vc1 vsc1 (20) 5;;
let _, carl = Hashtbl.find vc1 (cpl (5,5) (-1, -1));;
List.iter (fun lpos -> print_list lpos; Printf.printf "\n") (List.map (fun (_, set) -> Posset.elements set) carl);; *)


let test = newGame 7;;
play test 2 2;;
play test 5 3;;
let vc1, vsc1, posl1 =  buildFromGameP1_2 test;;
hsearch test posl1 vc1 vsc1 (20) 5;;
let mat = Array.make_matrix (test.size) (test.size) 0;;
Hashtbl.iter (fun (i, j) nb -> if (inboundPos test.size i j) then mat.(i).(j) <- nb) posl1;;
output_arr stdout mat;;
(* let (i, j), sc = hexy_noTimeLimit 0 20 5 test;;
Printf.printf "Play : %d, %d, Score : %d" i j sc;; *)
(* List.iter (fun (i, j) -> Printf.printf "| %d, %d |" i j ) (posMovesListOpti posl1) *)





