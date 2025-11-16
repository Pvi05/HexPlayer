type hexGame = {
  gameSet : int array array;
  size : int;
  mutable curPlayer : int;
  mutable nbPlay : int;
}

let newGame (n : int) : hexGame =
  { gameSet = Array.make_matrix n n 0; size = n; curPlayer = 1; nbPlay = 0 }

let possiblePlay (game : hexGame) (i : int) (j : int) : bool =
  let v1 = i >= 0 && i < game.size in
  let v2 = j >= 0 && j < game.size in
  v1 && v2 && game.gameSet.(i).(j) = 0

let possibleUndo (game : hexGame) (i : int) (j : int) : bool =
  let v1 = i >= 0 && i < game.size in
  let v2 = j >= 0 && j < game.size in
  v1 && v2 && game.gameSet.(i).(j) <> 0

let play (game : hexGame) (i : int) (j : int) : unit =
  assert (possiblePlay game i j);
  game.gameSet.(i).(j) <- game.curPlayer;
  game.curPlayer <- -1 * game.curPlayer;
  game.nbPlay <- game.nbPlay + 1

let undo (game : hexGame) (i : int) (j : int) : unit =
  assert (possibleUndo game i j);
  game.gameSet.(i).(j) <- 0;
  game.curPlayer <- -1 * game.curPlayer;
  game.nbPlay <- game.nbPlay - 1

(* -> Reperage d'un plateau gagnant *)

let rec cleanNeigh (game : hexGame) (player : int) (l : (int * int) list) :
    (int * int) list =
  match l with
  | [] -> []
  | (i, j) :: ls ->
      if
        i < 0 || i >= game.size || j < 0 || j >= game.size
        || game.gameSet.(i).(j) <> player
      then cleanNeigh game player ls
      else (i, j) :: cleanNeigh game player ls

let neigh (game : hexGame) (i : int) (j : int) (player : int) : (int * int) list
    =
  cleanNeigh game player
    [
      (i - 1, j);
      (i - 1, j - 1);
      (i, j - 1);
      (i + 1, j);
      (i + 1, j + 1);
      (i, j + 1);
    ]

let generateFirstPile (game : hexGame) (player : int) : (int * int) list =
  if player = 1 then
    let rec auxPileCreate_1 (k : int) (l : (int * int) list) : (int * int) list
        =
      if k = game.size then l
      else if game.gameSet.(k).(0) <> player then auxPileCreate_1 (k + 1) l
      else auxPileCreate_1 (k + 1) ((k, 0) :: l)
    in
    auxPileCreate_1 0 []
  else
    let rec auxPileCreate_2 (k : int) (l : (int * int) list) : (int * int) list
        =
      if k = game.size then l
      else if game.gameSet.(0).(k) <> player then auxPileCreate_2 (k + 1) l
      else auxPileCreate_2 (k + 1) ((0, k) :: l)
    in
    auxPileCreate_2 0 []

let isWinningPos (game : hexGame) (player : int) : bool =
  (* Parcours en profondeur d'un graphe *)
  let parc_pile = generateFirstPile game player in
  let visite = Array.make_matrix game.size game.size false in
  let rec aux_while (parc_pile : (int * int) list) : bool =
    match parc_pile with
    | [] -> false
    | (_, j) :: _ when player = 1 && j = game.size - 1 -> true
    | (i, _) :: _ when player = -1 && i = game.size - 1 -> true
    | (i, j) :: ls ->
        if not visite.(i).(j) then (
          visite.(i).(j) <- true;
          let ntoppile = neigh game i j player in
          aux_while (ntoppile @ ls)
          (*a améliorer en RT ? Non*))
        else aux_while ls
  in
  aux_while parc_pile
(* OK *)

(* Proposition : structure d'union find pour reperer une position gagante = graphe mis à jour à chaque coup (2 graphe, 1 par joueur) -> Inutile : complexité en temps constant à taille de plateau fixé *)

(* -> Statistique de plateau *)

(* avLines [|#lines av; Line 1 av; Line 2 av; ... ; Last Line p <= n av; ...|] *)

let randomPlay (avLines : int array) (avCol : int array array)
    : int * int =
  let p = avLines.(0) in
  (* print_int p;
     print_string " "; *)
  assert (p != 0);
  let randnum1 = 1 + Random.int p in
  let i = avLines.(randnum1) in
  let avCol_i = avCol.(i) in
  let p' = avCol_i.(0) in
  let randnum2 = 1 + Random.int p' in
  (randnum1, randnum2)

let randomGame (n : int) : hexGame =
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
      avLines.(0) <- pl - 1)
  done;
  game

let randomGame_winningplay (n : int) : int * int =
  (* idem mais renvoie le dernier coup joué *)
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
  let ilast = ref (-1) in
  let jlast = ref (-1) in
  while not (isWinningPos game (-1 * game.curPlayer)) do
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
    ilast := i;
    jlast := j
  done;
  (!ilast, !jlast)

let heatMap (n : int) (count : int) : int array array =
  let heatM = Array.make_matrix n n 0 in
  for _ = 0 to count - 1 do
    let i, j = randomGame_winningplay n in
    heatM.(i).(j) <- heatM.(i).(j) + 1
  done;
  heatM

(* Cette implémentation du random n'est pas 100% satisfaisante : il existe des situations ou il n'y a pas equiprobabilité de choisir une case... ! On peut faire plus simple ! *)

(* Alpha Beta *)

type position = int * int

let posMovesList (game : hexGame) : (int * int) list =
  let rec aux_for (i : int) (j : int) (l : (int * int) list) =
    if i >= game.size then l
    else if j >= game.size then aux_for (i + 1) 0 l
    else if possiblePlay game i j then aux_for i (j + 1) ((i, j) :: l)
    else aux_for i (j + 1) l
  in
  aux_for 0 0 []

let maxCustom (((i1, j1), s1) : position * int)
    (((i2, j2), s2) : position * int) : position * int =
  if s1 > s2 then ((i1, j1), s1) else ((i2, j2), s2)

let minCustom (((i1, j1), s1) : position * int)
    (((i2, j2), s2) : position * int) : position * int =
  if s1 < s2 then ((i1, j1), s1) else ((i2, j2), s2)

let minimumPos = ((-1, -1), min_int)
let maximumPos = ((-1, -1), max_int)

let matrixCopy (matrix : 'a array array) : 'a array array =
  let n = Array.length matrix in
  let matcopy = Array.make n [||] in
  for k = 0 to n - 1 do
    matcopy.(k) <- Array.copy matrix.(k)
  done;
  matcopy

let gameCopy (game : hexGame) : hexGame =
  {
    gameSet = matrixCopy game.gameSet;
    size = game.size;
    curPlayer = game.curPlayer;
    nbPlay = game.nbPlay;
  }

let auxMoveScoreMM (game : hexGame) (curD : int) (bestPlay : hexGame -> int -> position *int) 
      ((i, j) : int * int) : (int * int) * int =
    play game i j;
    let s = ref 0 in
    (if isWinningPos game 1 then s := max_int
     else if isWinningPos game (-1) then s := min_int
     else
       let _, s' = bestPlay game (curD + 1) in
       s := s');
    undo game i j;
    ((i, j), !s)

let minMax (maxDepth : int) (heur : hexGame -> int) (game' : hexGame) : position * int=
  let game = gameCopy game' in
  let rec bestPlay (game : hexGame) (curD : int) : (int * int) * int =
    let movesList = posMovesList game in
    if curD > maxDepth then ((-1, -1), heur game)
    else
      (*pos à bouger dans auxMoveScore*)
      let scoreList = List.map (auxMoveScoreMM game curD bestPlay) movesList in
      List.fold_left
        (if game.curPlayer = 1 then maxCustom else minCustom)
        (if game.curPlayer = 1 then minimumPos else maximumPos)
        scoreList
  in
  let p, s = bestPlay game 0 in
  (p, s)

(* let heatmap3x3 : int array array =
  let map = Array.make_matrix 3 3 0 in
  for i = 0 to 2 do
    for j = 0 to 2 do
      map.(i).(j) <- hM3x3.(i).(j) / 1000
    done
  done;
  map *)

let heur (heatmap : int array array) (game : hexGame) : int =
  let n = game.size in
  assert (n = Array.length heatmap);
  let sum = ref 0 in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      sum := !sum + (game.gameSet.(i).(j) * heatmap.(i).(j))
    done
  done;
  !sum

let auxMoveScoreAB (game : hexGame) (curD : int) (bestPlay : hexGame -> int -> int -> int -> position * int) 
    (alpha : int) (beta : int) ((i, j) : int * int) : (int * int) * int =
  play game i j;
  let s = ref 0 in
  (if isWinningPos game 1 then s := max_int
   else if isWinningPos game (-1) then s := min_int
   else
     let _, s' = bestPlay game (curD + 1) alpha beta in
     s := s');
  undo game i j;
  ((i, j), !s)

let listFoldLeftAndMapAB (l : position list)
    (f : position * int -> position * int -> position * int)
    (init : position * int) (alpha : int) (beta : int) (maxSearch : bool)
    (mapf : int -> int -> position -> position * int) : position * int =
  let rec auxFold (l : position list) (arg : position * int) (alpha : int)
      (beta : int) : position * int =
    match l with
    | [] -> arg
    | x :: ls ->
        let mapx = mapf alpha beta x in
        let newPos, newScore = f arg mapx in
        if maxSearch then
          if newScore > beta then (newPos, newScore)
          else auxFold ls (newPos, newScore) (max newScore alpha) beta
        else (if newScore < alpha then (newPos, newScore)
          else auxFold ls (newPos, newScore) alpha (min newScore beta))
  in
  auxFold l init alpha beta

let alphaBeta (maxDepth : int) (heur : hexGame -> int) (game' : hexGame) : position * int=
  let game = gameCopy game' in
  let rec bestPlay (game : hexGame) (curD : int) (alpha : int) (beta : int) :
      (int * int) * int =
    let movesList = posMovesList game in
    if curD > maxDepth then ((-1, -1), heur game)
    else
      (*pos à bouger dans auxMoveScore*)
      listFoldLeftAndMapAB movesList
        (if game.curPlayer = 1 then maxCustom else minCustom)
        (if game.curPlayer = 1 then minimumPos else maximumPos)
        alpha beta
        (if game.curPlayer = 1 then true else false)
        (auxMoveScoreAB game curD bestPlay)
  in
  let p, s = bestPlay game 0 min_int max_int in
  (p, s)
(* Ok *)
(* Convention : *)
(* alpha : etage recherche d'un max *)
(* beta : etage recherche d'un min *)

(* Generally, the program will tend to play more aggressively if depth is odd. - Queensbee Thesis *)

  let inboundPos (n : int) (i : int) (j : int) : bool =
    let v1 = i >= 0 && i < n in
    let v2 = j >= 0 && j < n in
    v1 && v2

  let indicePos (i, j : position) (n : int) : int =
    if (inboundPos n i j) then
      i*n + j
    else (
      if (i, j) = (-1 ,-1) || (i, j) = (-1, n) then 
        n*n
      else if (i, j) = (n ,-1) || (i, j) = (n, n) then (
        n*n + 1 )
      else (failwith (Printf.sprintf "unknown position %d, %d" i j))
      )
  
  let posIndice (player : int) (i : int) (n : int): position =
    if (i < n*n) then
    (i / n),(i mod n)
    else
      if i = n*n then
        if player = 1 then (-1, -1) else (-1, n)
      else if player = 1 then (n, n) else (n, -1)

let hashGame (game : hexGame) : int =
  let n = game.size in
  let hash = ref 0 in
  for k = 0 to n*n - 1 do
    let i, j = posIndice 1 k game.size in
    hash := (3 * !hash) + (if game.gameSet.(i).(j) = -1 then 2 else game.gameSet.(i).(j)) 
  done;
  !hash

let alphaBetaHashTabl (maxDepth : int) (heur : hexGame -> int) (game' : hexGame) : position * int =
  let cpt = ref 0 in
  let game = gameCopy game' in
  let table = Hashtbl.create (truncate (3. ** float game.size)) in
  let rec bestPlay (game : hexGame) (curD : int) (alpha : int) (beta : int) :
      (int * int) * int =
    let movesList = posMovesList game in
    let auxMoveScore (alpha : int) (beta : int) ((i, j) : int * int) :
        (int * int) * int =
      play game i j;
      let s = ref 0 in
      try
        let s = Hashtbl.find table (hashGame game) in
        undo game i j;
        ((i, j), s)
      with Not_found ->
        (if isWinningPos game 1 then s := max_int
         else if isWinningPos game (-1) then s := min_int
         else
           let _, s' =
             bestPlay game (curD + 1) alpha beta
           in
           s := s');
        Hashtbl.add table (hashGame game) !s;
        undo game i j;
        ((i, j), !s)
    in
    if curD > maxDepth then (cpt := !cpt + 1; ((-1, -1), heur game))
    else
      (*pos à bouger dans auxMoveScore*)
      listFoldLeftAndMapAB movesList
        (if game.curPlayer = 1 then maxCustom else minCustom)
        (if game.curPlayer = 1 then minimumPos else maximumPos)
        alpha beta
        (if game.curPlayer = 1 then true else false)
        auxMoveScore
  in
  let p, s = bestPlay game 0 min_int max_int in
  Printf.printf "%d" !cpt;
  (p, s)
(* peu concluant *)

let minMaxHashtbl (maxDepth : int) (heur : hexGame -> int) (game' : hexGame) : position * int=
  let game = gameCopy game' in
  let table = Hashtbl.create (truncate (3. ** float game.size)) in
  let auxMoveScoreMMbis (game : hexGame) (curD : int) (bestPlay : hexGame -> int -> position *int) 
      ((i, j) : int * int) : (int * int) * int =
    play game i j;
    try
      let s = Hashtbl.find table (hashGame game) in
      undo game i j;
      ((i, j), s)
    with Not_found -> (
    let s = ref 0 in
    (if isWinningPos game 1 then s := max_int
     else if isWinningPos game (-1) then s := min_int
     else
       let _, s' = bestPlay game (curD + 1) in
       s := s');
    Hashtbl.add table (hashGame game) !s;
    undo game i j;
    ((i, j), !s))
    in
  let rec bestPlay (game : hexGame) (curD : int) : (int * int) * int =
    let movesList = posMovesList game in
    if curD > maxDepth then ((-1, -1), heur game)
    else
      (*pos à bouger dans auxMoveScore*)
      let scoreList = List.map (auxMoveScoreMMbis game curD bestPlay) movesList in
      List.fold_left
        (if game.curPlayer = 1 then maxCustom else minCustom)
        (if game.curPlayer = 1 then minimumPos else maximumPos)
        scoreList
  in
  let p, s = bestPlay game 0 in
  (p, s)

let output_arr (out : out_channel) (gameMap : int array array) : unit =
  let n = Array.length gameMap in
  Printf.fprintf out "[| \n";
  for i = 0 to n - 1 do
    Printf.fprintf out "   [| ";
    for j = 0 to n - 2 do
      Printf.fprintf out "%d; " gameMap.(i).(j)
    done;
    if i < n - 1 then Printf.fprintf out "%d|]; \n" gameMap.(i).(n - 1)
    else Printf.fprintf out "%d|] \n" gameMap.(i).(n - 1)
  done;
  Printf.fprintf out "|] \n\n";
  flush out

let playGame (out : out_channel) (n : int) (player1 : hexGame -> position * int) (player2 : hexGame -> position * int) =
  let game = newGame n in
  let v = ref true in
  let cur = ref 0 in
  while !v do
    cur := !cur + 1;
    if game.curPlayer = 1 then (
      let (i, j), s = player1 game in
      play game i j;
      Printf.fprintf out "%d :: Player 1 played in (%d, %d) with score of %d\n" !cur i j s;
      output_arr out game.gameSet;
      if isWinningPos game 1 then(
        Printf.fprintf out "Player 1 won ! \n\n";
        v := false)
    ) else (
      let (i, j), s = player2 game in
      play game i j;
      Printf.fprintf out "%d :: Player 2 played in (%d, %d) with score of %d\n" !cur i j s;
      output_arr out game.gameSet;
      if isWinningPos game (-1) then (
        Printf.fprintf out "Player 2 won ! \n\n";
        v := false)
    )
  done;

(* -> Hexy *)

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

type posList = (position, int) Hashtbl.t

let cpl (g1 : position) (g2 : position) : couple =
  if Position.compare g1 g2 < 0 then (g2 , g1) else (g1 , g2)

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
  
let iterHashtblCouple (hashtbl : ('a, 'b) Hashtbl.t) (f : ('a * 'b) -> ('a * 'b) -> unit) =
  let aux_iter (ka : 'a) (ela : 'b) : unit =
    let iterbis (kb : 'a) (elb : 'b) : unit =
      f (ka, ela) (kb, elb)
    in
    Hashtbl.iter (iterbis) hashtbl
  in Hashtbl.iter (aux_iter) hashtbl

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

(* Convention : couple (g1, g2) tq g1 >= g2 *)


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

let incrPoslCount (critic : bool) (posl : posList) (set : Posset.t) : unit =
  let incr = (if critic then (Printf.printf "jure wola"; 1000) else 1) in
  Posset.iter (fun position -> Hashtbl.replace posl position (Hashtbl.find posl position + incr)) set


let loop2 (size : int) (posl : posList) (c : cTabl) (sc : scTabl) (empty : bool) (g : position) (g1 : position) (g2 : position) (depth : int) (mTH : int) (kTH : int) : bool =
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
            incrPoslCount ((cpl g1 g1 = (cpl (-1, -1) (size, size))) || ( cpl g1 g1 = cpl (-1, size) (size, -1))) posl newsc; 
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
              let vmodif = loop2 n g c sc (isE) pos pos1 pos2 !depth mTH kTH in
              vstop := !vstop || vmodif)) g
      ) g
    ) g;
    depth := !depth + 1;
  done


(* ---- Replacement ---- *)

(* 
        (P2) (-1, n)
(P1) (-1, -1) /_¯/ (n, n) (P1)
        (P2) (n, -1)   
*)
let rec remList (l : position list) (x : position) : 'a list =
  match l with
  | [] -> []
  | y::ls -> if (Position.compare x y = 0) then remList ls x else y::(remList ls x)

let rec cleanRawNeigh2 (game : hexGame) (l : (int * int) list) :
  (int * int) list =
  let n = game.size in
  match l with
  | [] -> []
  | (i, _) :: ls when i < 0 -> (-1, n)::remList (cleanRawNeigh2 game ls) (-1, n)
  | (_, j) :: ls when j < 0 -> (-1, -1)::remList (cleanRawNeigh2 game ls) (-1, -1)
  | (i, _) :: ls when i >= n -> (n, -1)::remList (cleanRawNeigh2 game ls) (n, -1)
  | (_, j) :: ls when j >= n -> (n, n)::remList (cleanRawNeigh2 game ls) (n, n)
  | (i, j) :: ls -> (i, j)::cleanRawNeigh2 game ls

let generateNeighPile (game : hexGame) (pos : position) : (int * int) list =
    if pos = (-1, -1) then
      let rec auxPileCreate_1 (k : int) (l : (int * int) list) : (int * int) list =
        if k = game.size then l
        else auxPileCreate_1 (k + 1) ((k, 0) :: l)
      in
      auxPileCreate_1 0 []
    else if pos = (-1, game.size) then
      let rec auxPileCreate_2 (k : int) (l : (int * int) list) : (int * int) list =
        if k = game.size then l
        else auxPileCreate_2 (k + 1) ((0, k) :: l)
      in
      auxPileCreate_2 0 []
    else if pos = (game.size, -1) then
      let rec auxPileCreate_3 (k : int) (l : (int * int) list) : (int * int) list =
        if k = game.size then l
        else auxPileCreate_3 (k + 1) ((game.size - 1, k) :: l)
      in
      auxPileCreate_3 0 []
    else
      let rec auxPileCreate_4 (k : int) (l : (int * int) list) : (int * int) list =
        if k = game.size then l
        else auxPileCreate_4 (k + 1) ((k, game.size - 1) :: l)
      in
      auxPileCreate_4 0 []

let rawNeigh2 (game : hexGame) (i, j : int * int) : (int * int) list =
  if (inboundPos game.size i j) then
    cleanRawNeigh2 game
    [
      (i - 1, j);
      (i - 1, j - 1);
      (i, j - 1);
      (i + 1, j);
      (i + 1, j + 1);
      (i, j + 1);
    ]
  else generateNeighPile game (i, j)

let buildP1EmptyPosList2 (gameSet : int matrix) : posList =
  let n = Array.length gameSet in
  let gpos : posList = Hashtbl.create (n*n) in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      if gameSet.(i).(j) >= 0 then
      Hashtbl.add gpos (i, j) 0;
    done;
  done; 
  Hashtbl.add gpos (-1, -1) 0;
  Hashtbl.add gpos (n, n) 0;
  gpos

  let buildP2EmptyPosList2 (gameSet : int matrix) : posList =
    let n = Array.length gameSet in
    let gpos : posList = Hashtbl.create (n*n) in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        if gameSet.(i).(j) <= 0 then
        Hashtbl.add gpos (i, j) 0;
      done;
    done; 
    Hashtbl.add gpos (-1, n) 0;
    Hashtbl.add gpos (n, -1) 0;
    gpos

let buildFromGameP1_2 (game : hexGame) : cTabl * scTabl * posList =
  let n = game.size in
  let posList = buildP1EmptyPosList2 game.gameSet in
  let tblVC : cTabl = Hashtbl.create (n*n*n*n) in
  let tblVSC : scTabl = Hashtbl.create (n*n*n*n) in
  let aux_fVC ((g1, _) : position * int) ((g2, _) : position * int) : unit =
    if (Position.compare g1 g2 >= 0) && Hashtbl.mem posList g1 && Hashtbl.mem posList g2 then
      if List.mem g2 (rawNeigh2 game g1) then 
        Hashtbl.add tblVC (g1, g2) (-1, [-1 , Posset.empty])
      else
        Hashtbl.add tblVC (g1, g2) (-2, [])
  in 
  let aux_fVSC ((g1, _) : position * int) ((g2, _) : position * int) : unit =
    if (Position.compare g1 g2 >= 0) && Hashtbl.mem posList g1 && Hashtbl.mem posList g2 then
      Hashtbl.add tblVSC (g1, g2) (-2, [])
  in
  iterHashtblCouple posList aux_fVC;
  iterHashtblCouple posList aux_fVSC;
  tblVC, tblVSC, posList

  let buildFromGameP2_2 (game : hexGame) : cTabl* scTabl * posList =
    let n = game.size in
    let posList = buildP2EmptyPosList2 game.gameSet in
    let tblVC : cTabl = Hashtbl.create (n*n*n*n) in
    let tblVSC : scTabl = Hashtbl.create (n*n*n*n) in
    let aux_fVC ((g1, _) : position * int) ((g2, _) : position * int) : unit =
      if (Position.compare g1 g2 >= 0) && Hashtbl.mem posList g1 && Hashtbl.mem posList g2 then
        if List.mem g2 (rawNeigh2 game g1) then 
          Hashtbl.add tblVC (g1, g2) (-1, [-1 , Posset.empty])
        else
          Hashtbl.add tblVC (g1, g2) (-2, [])
    in 
    let aux_fVSC ((g1, _) : position * int) ((g2, _) : position * int) : unit =
      if (Position.compare g1 g2 >= 0) && Hashtbl.mem posList g1 && Hashtbl.mem posList g2 then
        Hashtbl.add tblVSC (g1, g2) (-2, [])
    in
    iterHashtblCouple posList aux_fVC;
    iterHashtblCouple posList aux_fVSC;
    tblVC, tblVSC, posList

(* Rq : il y a doublet dans les couples à l'ordre près pour l'instant. Un ordre pourra eventuellement etre imposé pour eviter de faire plusieurs fois le même travail -> FAIT*)

(* Convention : couple (g1, g2) tq g1 >= g2 *)

let auxListMap (_, set : int * Posset.t) : position list = Posset.elements set

(* 2 dernières etapes :
   -> Evaluation function (Kirchoff)
   -> (event.) Propagation du g, c, sc pour eviter le recalcul complet du H-search *)

(* Construction de la matrice de resistances *)

let resPos (game : hexGame) (i, j : position) (player : int) : float =
  let n = game.size in
  let col = (if (indicePos (i,j) n) >= n*n then player else game.gameSet.(i).(j)) in
  if col = player then (0.001)
  else if col = 0 then (1.)
  else (1000000.)

let output_arrfl (out : out_channel) (gameMap : float array array) : unit =
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

let resMat (game : hexGame) (player : int) (vc : cTabl): float array array =
  let n = game.size in
  let res = Array.make_matrix (n*n + 2) (n*n + 2) 1000000. in
  for a = 0 to n*n + 1 do
    for b = 0 to n*n + 1 do
        let posa = posIndice player a n in
        let posb = posIndice player b n in
        if (Hashtbl.mem vc (cpl posa posb) && let _, list = Hashtbl.find vc (cpl posa posb) in list <> [] ) then (
          res.(a).(b) <- (resPos game posa player) +. (resPos game posb player))
    done;
  done;
  (* output_arrfl stdout res; *)
  res

(* Notre méthode consiste à effectuer une LNTP (Theoreme de Millmann) en chaque position du graphe, pour en tirer n*n env. 100 equations, et résoudre le système en inversant la matrice.
  On se place dans la situation ou le graphe apparenté à un ensemble de résistances est soumis à une tension de 1V entre les deux cotés appartenant au joueur. On determine les potentiels, puis I qui nous permet d'obtenir Req 
  La these de Anshelevich n'evoquait que les "loi de Kirchoff" en générale, mais leur application naive aurait abouti à env. 100*100 = 10 000 equations (pour autant d'inconnues que de resistances) ce qui semble nettement plus ardu à resoudre dans un temps raisonnable. De plus cette méthode nous garanti un nombre fixé d'inconnues, ce qui simplifie nettement l'implémentation*)

(* ! Cas resistance nulle -> Evité.
   ! liens boucle d'une position vers elle même -> Evité
   ! bout de plateau -> Implémenté en amont*)

   (* Deprecated *)

(* exception ResNull

let findMinAndUpdateLine (resM : float array array) (kirch : float array array) (i : int) : unit =
  let v = ref false in
  let m = Array.length kirch in
  for j = 0 to m - 1 do
    if !v && i < j && resM.(i).(j) = 0. then (
      v := true;
      kirch.(i).(i) <- 1.;
      kirch.(i).(j) <- -1.)
    else (
      kirch.(i).(j) <- 0.;
    )
  done *)

  (* Le cas de la resistance nulle n'est plus à traiter *)
(* 
let addBothEnds (kirchMat : float matrix) (player : int) : float matrix = *)


let kirchSystemMatrix (game : hexGame) (player : int) (vc : cTabl): float matrix =
  let n = game.size in
  let resMat = resMat game player vc in
  let kirch = Array.make_matrix (n*n + 2) (n*n + 2) 0. in
  let m = (n*n + 2) in
  for i = 0 to m - 2 do
    for j = 0 to m - 1 do
        if (j <> i) (* && resMat.(i).(j) <> max_float*) then (
          kirch.(i).(j) <- -1. /. resMat.(i).(j);
          kirch.(i).(i) <- kirch.(i).(i) +. (1. /. resMat.(i).(j)); (* LNTP *)
        )
    done;
  done;
  kirch.(n*n).(n*n) <- kirch.(n*n).(n*n) +. 1.;
  kirch.(n*n + 1).(n*n + 1) <- 1.;
  (* output_arrfl stdout kirch; *)
  kirch

let equivalentResistor (game : hexGame) (vc : cTabl) (player : int) : float =
  let n = game.size in
  let kSM = kirchSystemMatrix game player vc in
  let invksM = InvertMat.gJInversion kSM in
  let conditions = Array.make (n*n + 2) 0. in
  conditions.(n*n + 1) <- 1.;
  let potentials : float array = InvertMat.multiply invksM conditions in
  let currentThrough =  potentials.(n*n) in
  (1. -. potentials.(n*n)) /. currentThrough

(* Evaluation function 1ere version -> peu opti *)

(* hexGame -> int *)

let quickEval (game : hexGame) (vc1 : cTabl) (vc2 : cTabl) : int =
  let r1 = equivalentResistor game vc1 1 in
  let r2 = equivalentResistor game vc2 (-1) in
  let result = Float.log10 (r2/.r1) in
  (* Printf.printf "%f / %f " r1 r2; *)
  (* Printf.printf "%f /" result; *)
  int_of_float(10000. *. result)

  let rec auxListcomp (lref : carrier list) (ltest : carrier list) : bool =
    match lref with
    | [] -> true
    | (_, setref)::reste -> (
      let len = (List.length (List.filter (fun (_, settest) -> Posset.equal settest setref) ltest)) in
      if (len > 1) then (
        Printf.printf "bizarre"; false
      )
      else if len < 1 then (
        Printf.printf "manquant : ";
        Posset.iter (fun (i, j) -> Printf.printf "| %d, %d |" i j) setref; false
      )
      else auxListcomp reste ltest
    )

let debug_ListDifferencies (vcref : cTabl) (vctest : cTabl) : unit =
    (Hashtbl.iter (fun couple (_, carL) -> 
      let ((i, j), (k, l)) = couple in
      let (_, carimage) = Hashtbl.find vcref couple in
      if not(auxListcomp carimage carL) then (Printf.printf "entre %d,%d et %d,%d \n" i j k l)
        ) vctest)

let evalFromHsearch (mTH: int) (kTH : int) (game : hexGame) : int =
  let vcref1, vscref1, poslref1 =  buildFromGameP1_2 game in
  let vcref2, vscref2, poslref2 =  buildFromGameP2_2 game in
  hsearch game poslref1 vcref1 vscref1 (mTH) kTH;
  hsearch game poslref2 vcref2 vscref2 (mTH) kTH;
  (* debug_ListDifferencies aled vcref1;
  debug_ListDifferencies vcref1 aled;
  output_arr stdout game.gameSet;
  Printf.printf "%d %d " game.curPlayer game.nbPlay;
  Printf.printf "%d /" (quickEval game vcref1 vcref2) ; *)
  let r1 = equivalentResistor game vcref1 1 in
  let r2 = equivalentResistor game vcref2 (-1) in
  let result = Float.log10 (r2/.r1) in
  (* Printf.printf "%f / %f " r1 r2; *)
  int_of_float(10000. *. result)

(* Note importante : les connections virtuelles sont evalué de même que des connexions réelles pour l'instant *)

(* Version opti *)



type hexyTabl = cTabl * scTabl * posList

let auxUpdateTabl (posPlayed : position) (nb, setList : carrierList) : carrierList =
  (nb, (List.filter (fun (_, set) -> not(Posset.mem posPlayed set)) setList))


let updateHexyTabl (pos : position) (fromPlayer : bool) (vc, vsc, posl : cTabl * scTabl * posList) : unit =
  if not(fromPlayer) then (
  Hashtbl.remove posl pos;
  Hashtbl.filter_map_inplace (fun (posa, posb) sets -> 
      if pos = posa || pos = posb then None
      else Some (auxUpdateTabl pos sets)) vc;
  Hashtbl.filter_map_inplace (fun (posa, posb) sets -> 
        if pos = posa || pos = posb then None
        else Some (auxUpdateTabl pos sets)) vsc
  ) else (
  Hashtbl.filter_map_inplace (fun (_) sets -> 
      Some (auxUpdateTabl pos sets)) vc;
  Hashtbl.filter_map_inplace (fun (_) sets -> 
      Some (auxUpdateTabl pos sets)) vsc
  (* Hashtbl.filter_map_inplace (fun (posa, posb) sets -> 
        if pos = posa || pos = posb then None
        else Some (auxUpdateTabl pos sets)) vc;
  Hashtbl.filter_map_inplace (fun (posa, posb) sets -> 
          if pos = posa || pos = posb then None
          else Some (auxUpdateTabl pos sets)) vsc *)
  )


let hexy_noTimeLimit (dTH : int) (mTH : int) (kTH : int) (game' : hexGame) : position * int =
    let cpt = ref 0 in
    let game = gameCopy game' in
    let table = Hashtbl.create (truncate (3. ** float game.size)) in
    let vc1, vsc1, posl1 = buildFromGameP1_2 game in
    let vc2, vsc2, posl2 = buildFromGameP2_2 game in
    hsearch game posl1 vc1 vsc1 mTH kTH;
    hsearch game posl2 vc2 vsc2 mTH mTH;
    let rec bestPlay (game : hexGame)  (curD : int) (alpha : int) (beta : int) (vc1b, vsc1b, posl1b : cTabl * scTabl * posList) (vc2b, vsc2b, posl2b : cTabl * scTabl * posList) :
        (int * int) * int = (
      let movesList = posMovesList game in
      let auxMoveScore (vc1b, vsc1b, posl1b : cTabl * scTabl * posList) (vc2b, vsc2b, posl2b : cTabl * scTabl * posList) (alpha : int) (beta : int) ((i, j) : int * int) :
          (int * int) * int = (
        let vc1c = Hashtbl.copy vc1b in
        let vsc1c = Hashtbl.copy vsc1b in
        let posl1c = Hashtbl.copy posl1b in
        let vc2c = Hashtbl.copy vc2b in
        let vsc2c = Hashtbl.copy vsc2b in
        let posl2c = Hashtbl.copy posl2b in
        updateHexyTabl (i, j) (game.curPlayer = 1) (vc1c, vsc1c, posl1c) ;
        updateHexyTabl (i, j) (game.curPlayer = (-1)) (vc2c, vsc2c, posl2c);
        play game i j;
        hsearch game posl1c vc1c vsc1c mTH kTH;
        hsearch game posl2c vc2c vsc2c mTH kTH;
        let s = ref 0 in
        try
          let s = Hashtbl.find table (hashGame game) in
          undo game i j;
          ((i, j), s)
        with Not_found ->
          (if isWinningPos game 1 then s := max_int
           else if isWinningPos game (-1) then s := min_int
           else
             let _, s' =
                (* copie ici *)
                bestPlay game (curD + 1) alpha beta (vc1c, vsc1c, posl1c) (vc2c, vsc2c, posl2c)
             in
             s := s');
          Hashtbl.add table (hashGame game) !s;
          undo game i j;
          ((i, j), !s)
      )
      in
      if curD > dTH then (
        cpt := !cpt + 1; 
      ((-2, -2), quickEval game vc1b vc2b))
      else
        listFoldLeftAndMapAB movesList
          (if game.curPlayer = 1 then maxCustom else minCustom)
          (if game.curPlayer = 1 then minimumPos else maximumPos)
          alpha beta
          (if game.curPlayer = 1 then true else false)
          (auxMoveScore (vc1b, vsc1b, posl1b) (vc2b, vsc2b, posl2b))
    )
    in
    let p, s = bestPlay game 0 min_int max_int (vc1, vsc1,  posl1) ( vc2, vsc2, posl2) in
    Printf.printf "%d" !cpt;
    (p, s)

(* Puis : *)
(* Dernière opti : limite de temps et Move ordering *)

(* Notre stratégie de Move ordering : 
   Basé sur l'hypothèse suivante : plus une position est vitale pour l'ennemi plus elle apparait dans les carriers de ses connections semi-virtuelles. (pas virtuelles puisque jouer là n'empeche pas à l'ennemi de relier..)
   De plus on remarquera qu'il est essentiel de se concentrer sur les position qui appartiennent aus carriers semi-virtuels de l'ennemi entre ses deux bords, si elles existent. De ce fait, dès que la liste de tels carrier devient non vide, hexy ne se préoccupera UNIQUEMENT que de ces positions là (ce qui retire un nombre potientiellement conséquent de positions à evaluer du plateau) *)

let posMovesListOpti (n : int) (posOtherPlayer : posList) : position list =
  let posPrioList = List.sort (fun (_, nb1) (_, nb2) -> nb2 - nb1) (Hashtbl.fold (fun pos nb prevl -> (pos, nb)::prevl) posOtherPlayer []) in
  assert(posPrioList <> []);
  let _, max = List.hd posPrioList in
  let posPrioListfiltered = List.filter 
    (fun ((i, j), nb) -> 
    if max >= 1000 then nb >= 1000 && ((inboundPos n i j) && nb > 0 && Float.log10((float)nb /. (float)max) >= -0.53)
    else
    ((inboundPos n i j) && nb > 0 && Float.log10((float)nb /. (float)max) >= -0.53)
    ) 
    posPrioList 
  in
  List.map (fun (pos, _) -> pos) posPrioListfiltered
  
let decrPoslCount (posl : posList) (set : Posset.t) : unit =
    Posset.iter (fun position -> 
      try
      Hashtbl.replace posl position ((Hashtbl.find posl position) - 1)
      with 
      | Not_found -> ()) set

let auxUpdateTabl2 (posl : posList) (posPlayed : position) (nb, setList : carrierList) : carrierList =
    (nb, (List.filter 
    (fun (_, set) ->
      let b = Posset.mem posPlayed set in
      (if b then decrPoslCount posl set);
      not(b)
    ) 
    setList))
  
  
let updateHexyTabl2 (pos : position) (fromPlayer : bool) (vc, vsc, posl : cTabl * scTabl * posList) : unit =
    if not(fromPlayer) then (
    Hashtbl.filter_map_inplace (fun (posa, posb) sets -> 
        if pos = posa || pos = posb then None
        else Some (auxUpdateTabl pos sets)) vc;
    Hashtbl.filter_map_inplace (fun (posa, posb) sets -> 
          if pos = posa || pos = posb then None
          else Some (auxUpdateTabl2 posl pos sets)) vsc;
    Hashtbl.remove posl pos;
    ) else (
    Hashtbl.replace posl pos 0;
    Hashtbl.filter_map_inplace (fun (_) sets -> 
        Some (auxUpdateTabl pos sets)) vc;
    Hashtbl.filter_map_inplace (fun (_) sets -> 
        Some (auxUpdateTabl2 posl pos sets)) vsc
    )

let hexy_noTLAndMO (dTH : int) (mTH : int) (kTH : int) (game' : hexGame) : position * int =
    let cpt = ref 0 in
    let game = gameCopy game' in
    let table = Hashtbl.create (truncate (3. ** float game.size)) in
    let vc1, vsc1, posl1 = buildFromGameP1_2 game in
    let vc2, vsc2, posl2 = buildFromGameP2_2 game in
    hsearch game posl1 vc1 vsc1 mTH kTH;
    hsearch game posl2 vc2 vsc2 mTH mTH;
    let rec bestPlay (game : hexGame)  (curD : int) (alpha : int) (beta : int) (vc1b, vsc1b, posl1b : cTabl * scTabl * posList) (vc2b, vsc2b, posl2b : cTabl * scTabl * posList) :
        (int * int) * int = (
      let movesList = posMovesListOpti game.size (if game.curPlayer = 1 then posl2b else posl1b) in
      let auxMoveScore (vc1b, vsc1b, posl1b : cTabl * scTabl * posList) (vc2b, vsc2b, posl2b : cTabl * scTabl * posList) (alpha : int) (beta : int) ((i, j) : int * int) :
          (int * int) * int = (
        let vc1c = Hashtbl.copy vc1b in
        let vsc1c = Hashtbl.copy vsc1b in
        let posl1c = Hashtbl.copy posl1b in
        let vc2c = Hashtbl.copy vc2b in
        let vsc2c = Hashtbl.copy vsc2b in
        let posl2c = Hashtbl.copy posl2b in
        updateHexyTabl2 (i, j) (game.curPlayer = 1) (vc1c, vsc1c, posl1c);
        updateHexyTabl2 (i, j) (game.curPlayer = (-1)) (vc2c, vsc2c, posl2c);
        flush stdout;
        play game i j;
        hsearch game posl1c vc1c vsc1c mTH kTH;
        hsearch game posl2c vc2c vsc2c mTH kTH;
        let s = ref 0 in
        try
          let s = Hashtbl.find table (hashGame game) in
          undo game i j;
          ((i, j), s)
        with Not_found ->
          (if isWinningPos game 1 then s := max_int
           else if isWinningPos game (-1) then s := min_int
           else
             let _, s' =
                bestPlay game (curD + 1) alpha beta (vc1c, vsc1c, posl1c) (vc2c, vsc2c, posl2c)
             in
             s := s');
          Hashtbl.add table (hashGame game) !s;
          undo game i j;
          ((i, j), !s)
      )
      in
      if curD > dTH then (
        cpt := !cpt + 1; 
        ((-2, -2), quickEval game vc1b vc2b))
      else
        listFoldLeftAndMapAB movesList
          (if game.curPlayer = 1 then maxCustom else minCustom)
          (if game.curPlayer = 1 then minimumPos else maximumPos)
          alpha beta
          (if game.curPlayer = 1 then true else false)
          (auxMoveScore (vc1b, vsc1b, posl1b) (vc2b, vsc2b, posl2b))
    )
    in
    let p, s = bestPlay game 0 min_int max_int (vc1, vsc1, posl1) ( vc2, vsc2, posl2) in
    Printf.printf "%d" !cpt;
    (p, s)

let listFoldLeftAndMapAB_time (tLIMIT : float) (t : float) (l : position list)
    (f : position * int -> position * int -> position * int)
    (init : position * int) (alpha : int) (beta : int) (maxSearch : bool)
    (mapf : int -> int -> position -> position * int) : position * int =
  let rec auxFold (l : position list) (arg : position * int) (alpha : int)
      (beta : int) : position * int =
    match l with
    | [] -> arg
    | _ when (Sys.time() -. t > tLIMIT) -> arg
    | x :: ls ->
        let mapx = mapf alpha beta x in
        let newPos, newScore = f arg mapx in
        if maxSearch then
          if newScore > beta then (newPos, newScore)
          else auxFold ls (newPos, newScore) (max newScore alpha) beta
        else (if newScore < alpha then (newPos, newScore)
          else auxFold ls (newPos, newScore) alpha (min newScore beta))
  in
  auxFold l init alpha beta


let hexy_wthTL (dTH : int) (mTH : int) (kTH : int) (tLIMIT : float) (game' : hexGame) : position * int =
    let t0 = Sys.time() in
    let cpt = ref 0 in
    let game = gameCopy game' in
    let table = Hashtbl.create (truncate (3. ** float game.size)) in
    let vc1, vsc1, posl1 = buildFromGameP1_2 game in
    let vc2, vsc2, posl2 = buildFromGameP2_2 game in
    hsearch game posl1 vc1 vsc1 mTH kTH;
    hsearch game posl2 vc2 vsc2 mTH mTH;
    let rec bestPlay (game : hexGame)  (curD : int) (alpha : int) (beta : int) (vc1b, vsc1b, posl1b : cTabl * scTabl * posList) (vc2b, vsc2b, posl2b : cTabl * scTabl * posList) :
        (int * int) * int = (
      let movesList = posMovesListOpti game.size (if game.curPlayer = 1 then posl2b else posl1b) in
      let auxMoveScore (vc1b, vsc1b, posl1b : cTabl * scTabl * posList) (vc2b, vsc2b, posl2b : cTabl * scTabl * posList) (alpha : int) (beta : int) ((i, j) : int * int) :
          (int * int) * int = (
        let vc1c = Hashtbl.copy vc1b in
        let vsc1c = Hashtbl.copy vsc1b in
        let posl1c = Hashtbl.copy posl1b in
        let vc2c = Hashtbl.copy vc2b in
        let vsc2c = Hashtbl.copy vsc2b in
        let posl2c = Hashtbl.copy posl2b in
        updateHexyTabl2 (i, j) (game.curPlayer = 1) (vc1c, vsc1c, posl1c);
        updateHexyTabl2 (i, j) (game.curPlayer = (-1)) (vc2c, vsc2c, posl2c);
        flush stdout;
        play game i j;
        hsearch game posl1c vc1c vsc1c mTH kTH;
        hsearch game posl2c vc2c vsc2c mTH kTH;
        let s = ref 0 in
        try
          let s = Hashtbl.find table (hashGame game) in
          undo game i j;
          ((i, j), s)
        with Not_found ->
          (if isWinningPos game 1 then s := max_int
           else if isWinningPos game (-1) then s := min_int
           else
             let _, s' =
                bestPlay game (curD + 1) alpha beta (vc1c, vsc1c, posl1c) (vc2c, vsc2c, posl2c)
             in
             s := s');
          Hashtbl.add table (hashGame game) !s;
          undo game i j;
          ((i, j), !s)
      )
      in
      if curD > dTH then (
        cpt := !cpt + 1; 
        ((-2, -2), quickEval game vc1b vc2b))
      else
        listFoldLeftAndMapAB_time (t0) tLIMIT movesList
          (if game.curPlayer = 1 then maxCustom else minCustom)
          (if game.curPlayer = 1 then minimumPos else maximumPos)
          alpha beta
          (if game.curPlayer = 1 then true else false)
          (auxMoveScore (vc1b, vsc1b, posl1b) (vc2b, vsc2b, posl2b))
    )
    in
    let p, s = bestPlay game 0 min_int max_int (vc1, vsc1, posl1) ( vc2, vsc2, posl2) in
    Printf.printf "node visited : %d , Time : %f" !cpt (Sys.time() -. t0);
    (p, s)

(* IDDFS ici *)
let hexy_fullOpti (mTH : int) (kTH : int) (tLIMIT : float) (game : hexGame) : position * int =
  let t0 = Sys.time() in
  let p = ref (-2, -2) in
  let s = ref (if game.curPlayer = 1 then min_int else max_int) in
  let dcur = ref 0 in
  while (Sys.time() -. t0 < tLIMIT) do
    let p', s' = hexy_wthTL !dcur mTH kTH (tLIMIT -. (Sys.time() -. t0)) game in
    if game.curPlayer = 1 && s' > !s then (p := p'; s := s');
    if game.curPlayer = -1 && s' < !s then (p := p'; s := s');
    dcur := !dcur + 1;
  done;
  (!p, !s)