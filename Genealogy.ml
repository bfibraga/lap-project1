(* Genealogy module body *)

(* 
Aluno 1: ????? mandatory to fill
Aluno 2: ????? mandatory to fill

Comment:

?????????????????????????
?????????????????????????
?????????????????????????
?????????????????????????
?????????????????????????
?????????????????????????

*)

(*
0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
   100 columns
*)


(* COMPILATION - How to build this module (used by Mooshak))
         ocamlc -c Genealogy.mli Genealogy.ml
*)


(* AUXILIARY BASIC FUNCTIONS - you can add more *)

let rec uniq l =
	match l with
	|  [] -> []
	| [x] -> [x]
	| x::y::xs ->
		if x = y then uniq (y::xs)
		else x::uniq (y::xs)

let clean l = (* removes repetitions *)
	uniq (List.sort compare l)

let len =
	List.length

let map =
	List.map

let filter =
	List.filter

let mem =
	List.mem

let flatMap f l =
	List.flatten (map f l)

let partition =
	List.partition

let exists =
	List.exists

let for_all =
	List.for_all

let cFlatMap f l =
	clean (flatMap f l)

let union l1 l2 =
	clean (l1 @ l2)

let inter l1 l2 =
	filter (fun x -> mem x l2) l1

let diff l1 l2 =
	filter (fun a -> not (mem a l2)) l1


(* TYPES *)

type item = string * string list
type repository = item list

type aTree = ANil | ANode of string * aTree * aTree
type dTree = DNil | DNode of string * dTree list


(* EXAMPLES - you can add more *)

let example = [
           ("a", ["f";"g"]);
           ("b", ["f";"h"]);
           ("c", ["h";"i"]);
           ("f", ["g"; "j"]);
           ("g", ["j"]);
           ("h", []);
           ("i", []);
           ("j", [])
          ]


(* BASIC REPOSITORY FUNCTIONS - you can add more *)

let size rep = (* number of individuals *)
	len rep

let all1 rep = (* all the individuals *)
	map fst rep

let all2 rep = (* all the children (of anyone) *)
	cFlatMap snd rep

let roots rep = (* individuals without any parents *)
	diff (all1 rep) (all2 rep)

let inners rep = (* individuals with children *)
	let xs = filter (fun (p,cs) -> cs <> []) rep in
		all1 xs

let leaves rep = (* individuals without any children *)
	let xs = filter (fun (p,cs) -> cs = []) rep in
		all1 xs

let cut1 rep l = (* partition based on first component of the repository *)
	partition (fun (p,cs) -> mem p l) rep

let cut2 rep l = (* partition based on second component of the repository *)
	partition (fun (p,cs) -> inter cs l <> []) rep

let cut rep = (* partition -> (root pairs, rest pairs) *)
	cut1 rep (roots rep)

let children rep l = (* get all the children of the list l *)
	let (a,b) = cut1 rep l in
		all2 a

let rec parents rep l = (* get all the parents of the list l *)
	let (a,b) = cut2 rep l in
		all1 a


(* FUNCTION height *)

let rec height rep =
	if rep = [] then 0
	else let (_, tail) = cut rep in
		1 + height tail



let rec belongsList rep a = match rep with
  |[] -> false
  |(x, _)::xs -> if x = a then true else belongsList xs a

(* FUNCTION belongsRep - goes through the repository, grabbing a list of individuals and comparing them 
to the given element, to check if the element does in fact exist in the repo. *)
let rec belongsRep rep a = 
  if rep = [] then false
  else let (head, tail) = cut rep in
    if belongsList head a then true
    else belongsRep tail a
    
(* FUNCTION buildATree - checks if given element has parents.
	If so, we call the function recursivily on the subtrees of the new node with that parent*)
let rec buildATree rep a = 
  let parentsList = parents rep [a] in 
  match parentsList with
  | [] -> ANode(a, ANil, ANil)
  | x::[] -> ANode(a, buildATree rep x, ANil) 
  | x::y::_ -> ANode(a, buildATree rep x, buildATree rep y)
  
(* FUNCTION makeATree *)
let makeATree rep a =
  if belongsRep rep a then ANode(a, ANil, ANil)
		else buildATree rep a


(* FUNCTION repOfATree *)

let repOfATree t = match t with
	| ANil -> []
	| ANode(a, lft, rgt) -> a::lft::rgt


(* FUNCTION makeDTree *)

let makeDTree rep a =
	DNil


(* FUNCTION repOfDTree *)

let repOfDTree t = []


(* FUNCTION descendantsN *)

let rec descendantsN rep n lst =
	if n = 0 then lst
	else let child = children rep lst in
	descendantsN rep (n-1) child


(* FUNCTION siblings *)

let siblings rep lst =
	let parent = parents rep lst in
	children rep parent


(* FUNCTION siblingsInbreeding *)

let siblingsInbreeding rep =
	[]


(* FUNCTION waveN *)

let waveN rep n lst =
	[]


(* FUNCTION merge *)

let merge rep1 rep2 =
	[]


(* FUNCTION supremum *)

let supremum rep s =
	[]


(* FUNCTION validStructural *)

let validStructural rep =
	false


(* FUNCTION validSemantic *)

let validSemantic rep =
	false
