
(* Pour représenter les champs de bits : INT *)
type bitfield = int

let zero = 0
let un = 1
let inc x = x+1        (* Ajoute 1 *)
let lshift x = 2*x     (* Multiplie par 2 *)
let or_if_disj x y =   (* retourne Some (a||b) si a&&b=0 et None sinon *)
  if x land y = 0 then Some (x lor y) else None
  
let rec print x =
  if x<>0 then begin
    print_string (if x mod 2 = 1 then "|" else ".");
    print (x/2)
  end;;

(* Pour représenter les champs de bits : BIGINT *)
type bitfield = int list
let nbbits = 60 (* Nombre de bits utilisables *)
let vmax = 1 lsl nbbits
let mask = vmax - 1

let zero = [0]
let un = [1]
let rec inc = function (* Ajoute 1 *)
  | [] -> [1]
  | t::q when t+1 < vmax -> t+1::q
  | t::q -> 0 :: inc q
let rec lshift = function (* Multiplie par 2 *)
  | [] -> []
  | t::q when 2*t < vmax -> 2*t :: lshift q
  | t::q -> (2*t) land mask :: inc (lshift q)
let rec or_if_disj a b =  (* retourne Some (a||b) si a&&b=0 et None sinon *)
  match a, b with
    | (l, []) | ([], l) -> Some l
    | (t1::q1, t2::q2)
        -> match or_if_disj q1 q2 with
             | None -> None
             | Some x -> if t1 land t2 = 0
                           then Some (t1 lor t2::x)
                           else None

let rec print = function
  | []  -> ()
  | [0] -> ()
  | [x] -> print_string (if x mod 2 = 1 then "|" else ".");
           print [x/2]
  | t::q -> let x = ref t in
              for i = 0 to nbbits - 1 do
                print_string (if !x mod 2 = 1 then "|" else ".");
                x := !x / 2
              done;
              print q;;





type state = {
  nbmarks: int;   (* Nombre de marques                 *)
  lastmark: int;  (* Index de la marque la plus grande *)
  list: bitfield; (* Champ de bit des marques 01...n   *)
  dist: bitfield; (* Champ de bit des distances n...21 *)
}

let rec next s maxi =
  if s.lastmark >= maxi then [] else
    let cand = next { nbmarks = s.nbmarks;
                      list = lshift s.list;
                      dist = s.dist;
                      lastmark = s.lastmark+1 } maxi
    in match or_if_disj s.dist s.list with
         | None -> cand
         | Some l -> { nbmarks = s.nbmarks+1;
                       dist = l;
                       list = inc (lshift s.list);
                       lastmark = s.lastmark+1 }::cand

let select { nbmarks=nbmarks;
             lastmark=lastmark;
             dist=dist; list=list }
  = true

let golomb nbmarks maxi =
  let rec expl s =
    if s.nbmarks = nbmarks then
      (print s.list; print_newline ())
    else List.iter expl (List.filter select (next s maxi))  
  in expl {list=un; dist=zero; nbmarks=1; lastmark=0};;

golomb 5 11;;
golomb 6 17;;
golomb 7 25;;
golomb 8 34;;
golomb 9 44;;
golomb 10 55;;
golomb 11 72;;
golomb 12 85;;
golomb 13 106;;
golomb 14 127;;
golomb 15 151;;
golomb 16 177;;