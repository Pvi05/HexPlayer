let rec twoListIter (l1 : 'a list) (l2 : 'a list) (f : 'a -> 'a -> unit) : unit =
  match l1 with
  | [] -> ()
  | x::ls -> 
    List.iter (f x) l2;
    twoListIter ls l2 f

let isNew (i : int) (depth : int) : bool =
  depth - i <= 1

(* let areIncl (pset1 : Posset.t) (pset2 : Posset.t) : bool =
  let pint = Posset.inter pset1 pset2 in
  not (Posset.equal pint pset1 || Posset.equal pint pset2)  *)

let areIncl (setList : carrier list) (pset : Posset.t) : bool =
  List.fold_left (fun (prev : bool) (_ , pset1 : carrier) : bool -> prev || Posset.subset pset1 pset) false setList

let rec orDeductionRule (prevd, cSet : carrierList) (scSet : carrier list) (u : Posset.t) (i : Posset.t) (depth : int): carrierList =
    match scSet with
    | [] -> (prevd, cSet)
    | (_, sc1)::scs -> 
      let u1 = Posset.union u sc1 in
      let i1 = Posset.inter i sc1 in
      if (areIncl cSet u1) then 
        orDeductionRule (prevd, cSet) scs u i depth
      else if Posset.is_empty i1 then  
        orDeductionRule (depth, (depth, u1)::cSet) scs u i depth
      else (let nd, ncset = orDeductionRule (prevd, cSet) scs u1 i1 depth in
      orDeductionRule (nd, ncset) scs u i depth)
(* K threshold ? *)

let loop2 (c : cTabl) (sc : scTabl) (empty : bool) (g : position) (g1 : position) (g2 : position) (depth : int): bool =
  let vmodif = ref false in
  let aux_iterOnCarriers (d1, c1 : carrier) (d2, c2 : carrier) : unit =
    if (isNew d1 depth || isNew d2 depth) && Posset.disjoint c1 c2 && not(Posset.mem g1 c2) && not(Posset.mem g2 c1) then (
      if not(empty) then (
        let newc = Posset.union c1 c2 in
          let _, carl = Hashtbl.find c (cpl g1 g2) in
          if not(areIncl carl newc) then (
          Hashtbl.replace c (cpl g1 g2) (depth, (depth, newc)::carl);
          vmodif := true))
      else (
        let newsc = Posset.union (Posset.add g c1) c2 in
        let _, scarl = Hashtbl.find sc (cpl g1 g2) in
        if not(areIncl scarl newsc) then (
          Hashtbl.replace sc (cpl g1 g2) (depth, (depth, newsc)::scarl);
          vmodif := true;
          Hashtbl.replace c (cpl g1 g2) (orDeductionRule (Hashtbl.find c (cpl g1 g2)) scarl newsc newsc depth))))
    in let dG1, carListG1 = Hashtbl.find c (cpl g1 g) in
    let dG2, carListG2 = Hashtbl.find c (cpl g2 g) in
    if not (isNew dG2 depth || isNew dG1 depth) then false
    else (
      twoListIter carListG1 carListG2 aux_iterOnCarriers;
      !vmodif
    )

(* Ajout de doublet dans les c/sc -> FAIT*)

let isEmptyPos (game : hexGame) (i, j : position) : bool =
  if (i < 0) || (j < 0) || (i >= game.size) || (j >= game.size) then false else
  game.gameSet.(i).(j) = 0

let hsearch (game : hexGame) (g : posList) (c : cTabl) (sc : scTabl) : unit =
  let vstop = ref true in
  let depth = ref 0 in
  let n = game.size in
  while !vstop do
    vstop := false;
    Hashtbl.iter (fun pos _ -> 
      Hashtbl.iter (fun pos1 _ -> 
        Hashtbl.iter (fun pos2 _ -> 
            if List.mem pos [(-1, -1); (-1, n); (n , -1); (n , n)] then () else (
            let isE = isEmptyPos game pos in
            if (Position.compare pos1 pos2 > 0) && (isE || (isEmptyPos game pos1 && isEmptyPos game pos2)) then 
              let vmodif = loop2 c sc (isEmptyPos game pos) pos pos1 pos2 !depth in
              vstop := !vstop || vmodif)) g
      ) g
    ) g;
    depth := !depth + 1;
  done




  module Position = struct
    type t = position
    let compare ((i, j) : position) ((k, l) : position) = if (i - k <> 0) then i - k else j - l
  end
  
  module Posset = Set.Make(Position)
  
  type carrier = int * Posset.t
  
  type carrierList = int * (carrier list)
  
  type 'a matrix = 'a array array
  
  type couple = position * position
  
  type cTabl = (couple, carrierList) Hashtbl.t
  type scTabl = (couple, carrierList) Hashtbl.t
  
  type posList = (position, bool) Hashtbl.t
  



(* H-Search *)
let rec twoListIter (l1 : 'a list) (l2 : 'a list) (f : 'a -> 'a -> unit) : unit =
  match l1 with
  | [] -> ()
  | x::ls -> 
    List.iter (f x) l2;
    twoListIter ls l2 f

let isNew (i : int) (depth : int) : bool =
  (depth - i) <= 1

(* let areIncl (pset1 : Posset.t) (pset2 : Posset.t) : bool =
  let pint = Posset.inter pset1 pset2 in
  not (Posset.equal pint pset1 || Posset.equal pint pset2)  *)

let areIncl (setList : carrier list) (pset : Posset.t) : bool =
  List.fold_left (fun (prev : bool) (_ , pset1 : carrier) : bool -> prev || Posset.subset pset1 pset) false setList

let rec addOnlyIfMinimal (newc : Posset.t) (odepth : int) (ndepth : int) (carl : carrier list) (acc : carrier list): bool * carrierList =
    match carl with
    | [] -> true, (max odepth ndepth, (ndepth, newc)::acc)
    | (d, set)::carls -> if (Posset.subset set newc) then (
      (* Printf.printf "happen";
      Posset.iter (fun (i, j) -> Printf.printf "| %d, %d |" i j) (Posset.inter set newc); *)
      false, (odepth, carl@acc))
      else if (Posset.subset newc set) then addOnlyIfMinimal newc odepth ndepth carls acc
      else addOnlyIfMinimal newc odepth ndepth carls ((d, set)::acc)
      (* ajout d'un booléen ? -> FAIT*)

let rec orDeductionRule (prevd, cSet : carrierList) (scSet : carrier list) (u : Posset.t) (i : Posset.t) (depth : int) (mTH : int) (kTH : int) : carrierList =
  if (List.length cSet) >= mTH || kTH < 0 then (prevd, cSet) else (
    match scSet with
    | [] -> (prevd, cSet)
    | (_, sc1)::scs -> (
      let u1 = Posset.union u sc1 in
      assert(not(Posset.is_empty u1));
      let i1 = Posset.inter i sc1 in
      if (areIncl cSet u1) then (
        orDeductionRule (prevd, cSet) scs u i depth mTH kTH
      )
      else if Posset.is_empty i1 then (
        let _, ncSetL = addOnlyIfMinimal u1 prevd depth cSet [] in
        orDeductionRule (ncSetL) scs u i depth mTH kTH
      )
      else ( 
        let nd, ncset = orDeductionRule (prevd, cSet) scs u1 i1 depth mTH (kTH - 1) in
        orDeductionRule (nd, ncset) scs u i depth mTH kTH
      )
    )
  )


let loop2 (c : cTabl) (sc : scTabl) (empty : bool) (g : position) (g1 : position) (g2 : position) (depth : int) (mTH : int) (kTH : int) : bool =
  let vmodif = ref false in
  let aux_iterOnCarriers (d1, c1 : carrier) (d2, c2 : carrier) : unit =
    if (isNew d1 depth || isNew d2 depth) && Posset.disjoint c1 c2 && not(Posset.mem g1 c2) && not(Posset.mem g2 c1) then (
      if not(empty) then (
        let newc = Posset.union c1 c2 in
        let oldDepth, carl = Hashtbl.find c (cpl g1 g2) in
        if (List.length carl < mTH) then (
          let b, ncSetL = addOnlyIfMinimal newc oldDepth depth carl [] in
          if b then (
            Hashtbl.replace c (cpl g1 g2) (ncSetL);
            vmodif := true;
          )
        )
      )
      else (
        let newsc = Posset.union (Posset.add g c1) c2 in
        let oldDepth, scarl = Hashtbl.find sc (cpl g1 g2) in
        if (List.length scarl < mTH) then (
          let b, nscSetL = addOnlyIfMinimal newsc oldDepth depth scarl [] in
          if b then (
            Hashtbl.replace sc (cpl g1 g2) (nscSetL);
            vmodif := true;
            Hashtbl.replace c (cpl g1 g2) (orDeductionRule (Hashtbl.find c (cpl g1 g2)) scarl newsc newsc depth mTH kTH)
          )
        )
      )
    )
  in let dG1, carListG1 = Hashtbl.find c (cpl g1 g) in
  let dG2, carListG2 = Hashtbl.find c (cpl g2 g) in
  if not (isNew dG2 depth || isNew dG1 depth) then false
  else (
    twoListIter carListG1 carListG2 aux_iterOnCarriers;
    !vmodif
  )

(* Ajout de doublet dans les c/sc -> FAIT*)

let isEmptyPos (game : hexGame) (i, j : position) : bool =
  if (i < 0) || (j < 0) || (i >= game.size) || (j >= game.size) then false else
  game.gameSet.(i).(j) = 0

let hsearch (game : hexGame) (g : posList) (c : cTabl) (sc : scTabl) (mTH : int) (kTH : int): unit =
  let vstop = ref true in
  let depth = ref 0 in
  let n = game.size in
  while !vstop do
    vstop := false;
    Hashtbl.iter (fun pos _ -> 
      Hashtbl.iter (fun pos1 _ -> 
        Hashtbl.iter (fun pos2 _ -> 
            if List.mem pos [(-1, -1); (-1, n); (n , -1); (n , n)] then () else (
            let isE = isEmptyPos game pos in
            if (Position.compare pos1 pos2 > 0) (* &&(isE || (isEmptyPos game pos1 || isEmptyPos game pos2))*) then 
              let vmodif = loop2 c sc (isE) pos pos1 pos2 !depth mTH kTH in
              vstop := !vstop || vmodif)) g
      ) g
    ) g;
    depth := !depth + 1;
  done


(* --- Deprecated --- *)

let rec cleanRawNeigh (game : hexGame) (l : (int * int) list) :
(int * int) list =
match l with
| [] -> []
| (i, j) :: ls ->
  if
    i < 0 || i >= game.size || j < 0 || j >= game.size
  then cleanRawNeigh game ls
  else (i, j) :: cleanRawNeigh game ls


let rawNeigh (game : hexGame) (i, j : int * int) : (int * int) list
=
cleanRawNeigh game
[
  (i - 1, j);
  (i - 1, j - 1);
  (i, j - 1);
  (i + 1, j);
  (i + 1, j + 1);
  (i, j + 1);
]

let buildEmptPosList (gameSet : int matrix) : position list =
let n = Array.length gameSet in
let rec aux_parcMat (i : int) (j : int) (acc : position list) : position list =
  if i = n then acc
  else if j = n then aux_parcMat (i + 1) (0) acc 
  else aux_parcMat i (j + 1) (if gameSet.(i).(j) = 0 then (i, j)::acc else acc)
in aux_parcMat 0 0 []

(* Bof : HashTabl ... *)

let buildP1EmptyPosList (gameSet : int matrix) : posList =
let n = Array.length gameSet in
let gpos : posList = Hashtbl.create (n*n) in
for i = 0 to n - 1 do
  for j = 0 to n - 1 do
    if gameSet.(i).(j) >= 0 then
    Hashtbl.add gpos (i, j) 0;
  done;
done; 
gpos

let buildP2EmptyPosList (gameSet : int matrix) : posList =
  let n = Array.length gameSet in
  let gpos : posList = Hashtbl.create (n*n) in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      if gameSet.(i).(j) <= 0 then
      Hashtbl.add gpos (i, j) 0;
    done;
  done; 
  gpos



let buildFromGameP1 (game : hexGame) : cTabl * scTabl * posList =
let n = game.size in
let posList = buildP1EmptyPosList game.gameSet in
let tblVC : cTabl = Hashtbl.create (n*n*n*n) in
let tblVSC : scTabl = Hashtbl.create (n*n*n*n) in
let aux_fVC ((g1, _) : position * int) ((g2, _) : position * int) : unit =
  if (Position.compare g1 g2 >= 0) && Hashtbl.mem posList g1 && Hashtbl.mem posList g2 then
    if List.mem g2 (rawNeigh game g1) then 
      Hashtbl.add tblVC (g1, g2) (0, [0 , Posset.empty])
    else
      Hashtbl.add tblVC (g1, g2) (-1, [])
in 
let aux_fVSC ((g1, _) : position * int) ((g2, _) : position * int) : unit =
  if (Position.compare g1 g2 >= 0) && Hashtbl.mem posList g1 && Hashtbl.mem posList g2 then
    Hashtbl.add tblVSC (g1, g2) (-1, [])
in
iterHashtblCouple posList aux_fVC;
iterHashtblCouple posList aux_fVSC;
tblVC, tblVSC, posList

let buildFromGameP2 (game : hexGame) : cTabl* scTabl * posList =
  let n = game.size in
  let posList = buildP2EmptyPosList game.gameSet in
  let tblVC : cTabl = Hashtbl.create (n*n*n*n) in
  let tblVSC : scTabl = Hashtbl.create (n*n*n*n) in
  let aux_fVC ((g1, _) : position * int) ((g2, _) : position * int) : unit =
    if (Position.compare g1 g2 >= 0) && Hashtbl.mem posList g1 && Hashtbl.mem posList g2 then
      if List.mem g2 (rawNeigh game g1) then 
        Hashtbl.add tblVC (g1, g2) (0, [0 , Posset.empty])
      else
        Hashtbl.add tblVC (g1, g2) (-1, [])
  in 
  let aux_fVSC ((g1, _) : position * int) ((g2, _) : position * int) : unit =
    if (Position.compare g1 g2 >= 0) && Hashtbl.mem posList g1 && Hashtbl.mem posList g2 then
      Hashtbl.add tblVSC (g1, g2) (-1, [])
  in
  iterHashtblCouple posList aux_fVC;
  iterHashtblCouple posList aux_fVSC;
  tblVC, tblVSC, posList

(* Rq : il y a doublet dans les couples à l'ordre près pour l'instant. Un ordre pourra eventuellement etre imposé pour eviter de faire plusieur fois le même travail -> FAIT*)
