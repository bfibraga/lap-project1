(* Genealogy module body *)

(* 
Aluno 1: 57833
Aluno 2: 57747

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
		  
let example2 = [
			("a", ["f";"g"]);
			("a", []);
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

(* FUNCTION makeATree *)
let rec makeATree rep a =
  if not (mem a (all1 rep)) then ANode(a, ANil, ANil) 
	else let parentList = parents rep [a] in
	match parentList with
	| [] -> ANode(a, ANil, ANil)
	| x::[] -> ANode(a, makeATree rep x, ANil) 
	| x::y::_ -> ANode(a, makeATree rep x, makeATree rep y)
	

(* FUNCTION repOfATree *)

let repOfATree t = 
	match t with
	| ANil -> []
	| ANode(a, lft, rgt) -> []


(* FUNCTION makeDTree *)

let rec nbuildDTree rep d = 
	let childrenList = children rep [d] in
	match childrenList with 
	| [] -> DNode(d, [])
 	| x::xs -> DNode(d, lnbuildDTree (x::xs) rep)
and lnbuildDTree cl rep = match cl with 
	| [] -> []
	| c::cs -> clean ([nbuildDTree rep c] @ (lnbuildDTree cs rep)) (*maybe change @ to append function*)

let makeDTree rep d =
	if not( mem d (all1 rep)) then DNode(d, [])
	else nbuildDTree rep d


(* FUNCTION repOfDTree *)


let rec repOfDTree t = 
  match t with
  | DNil -> []
  | DNode(x, xs) -> clean ((x, listOfDNodesElements (xs))::getDNode (xs))
and listOfDNodesElements l =
  match l with
  | [] -> []
  | DNil::_ -> failwith "repOfDTree: DNil inside the DNode list"
  | DNode(x, _)::xs -> x::listOfDNodesElements xs 
                         
and getDNode tList =
  match tList with
  | [] -> []
  | x::xs -> repOfDTree x @ getDNode xs


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
let rec waveNParents rep n lst =
  if n = 0 then lst
  else let parent = diff (parents rep lst) lst in
    waveNParents rep (n-1) parent 
	
let rec waveNChildren rep n lst =
  if n = 0 then lst
  else let childs = diff (children rep lst) lst in
    waveNChildren rep (n-1) childs 

let waveN rep n lst =
  if n = 0 then lst
  else let parent = parents rep lst in
    let childs = children rep lst in
    (waveNParents rep (n-1) parent) @ (waveNChildren rep (n-1) childs) 


(* FUNCTION merge *)

let merge rep1 rep2 =
	[]


(* FUNCTION supremum *)

let supremum rep s =
	[]


(* FUNCTION validStructural *)


let rec checkOccurence rep1 rep2 =
  match rep2 with
  | [] -> true 
  | (_, sons)::xs -> if inter (sons) (all1 rep1) = sons then checkOccurence rep1 xs else false

let validStructural rep =
  let individuals = all1 rep in
  if (clean individuals) <> individuals then false
  else (checkOccurence rep rep) 

(* FUNCTION validSemantic *)

let rec validSemanticRec rep1 rep2  =       
  match rep2 with
  | [] -> true
  | (x, xs)::tail -> if mem x xs then false
      else if (len (parents rep1 [x]) > 2) then false
      else validSemanticRec rep1 tail
          
let validSemantic rep =
  validSemanticRec rep rep
