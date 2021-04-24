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
			("c", ["h";"i"]);
			("f", ["g"; "j"]);
			("g", ["j"]);
			("h", []);
			("i", []);
			("j", [])
			]
			
let example3 = [
    ("a",["d";"e"]);
    ("b",["e";"f"]);
    ("c",["g";"h"]);
    ("d",["i"]);
    ("e", ["i";"j";"d"]);
    ("f", ["g";"m"]);
    ("g", ["h";"m"]);
    ("h", ["n"]);
    ("i", ["j"]);
    ("j", ["k"]);
    ("k", []);
    ("m", ["k"; "n"]);
    ("n", [])
    ]
	
let example4 = [
	("a",["b"]);
	("b",["c";"d"]);
	("c",["d"]);
	("d",["e"]);
	("e",["f"]);
	("f",[])
	]

let ultimateexample = [
        ("a",["g";"h"]);
        ("b",["h";"i"]);
        ("c",["i";"j";"k"]);
        ("d",["k";"l"]);
        ("e",["l";"m"]);
        ("f",["m";"n"]);
        ("g",["o";"p"]);
        ("h",["q";"r"]);
        ("i",["s";"t"]);
        ("j",["u"]);
        ("k",["v";"w"]);
        ("l",["x";"y"]);
        ("m",["y";"z"]);
        ("n",["z"]);
        ("o",["p";"2";"1"]);
        ("p",["q";"2"]);
        ("q",["3"]);
        ("r",["4";"5"]);
        ("s",["r";"6";"t";"4"]);
        ("t",["6";"u"]);
        ("u",["v";"8"]);
        ("v",[]);
        ("w",["9";"19"]);
        ("x",["10"]);
        ("y",["11";"x"]);
        ("z",["11"]);
        ("1",["12";"13"]);
        ("2",["1";"12";"13"]);
        ("3",["15"]);
        ("4",["3";"5";"16"]);
        ("5",["17"]);
        ("6",["7";"17"]);
        ("7",["18"]);
        ("8",["7";"19"]);
        ("9",["20"]);
        ("10",["9";"21"]);
        ("11",["10";"21"]);
        ("12",["14"]);
        ("13",["14"]);
        ("14",["15"]);
        ("15",["16"]);
        ("16",[]);
        ("17",[]);
        ("18",[]);
        ("19",["18";"20"]);
        ("20",["22"]);
        ("21",["22"]);
        ("22",[]);
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

(*Returns the height of a given repository*)
(* pre: validStructural rep && validSemantic rep *)
(* post: none *)
let rec height rep =
	if rep = [] then 0
	else let (_, tail) = cut rep in
		1 + height tail

(* FUNCTION makeATree *)

(*Returns a ATree product type which is a representation of a given repository and a string*)
(* pre: validStructural rep && validSemantic rep *)
(* post: none *)
let rec makeATree rep a =
  if not (mem a (all1 rep)) then ANode(a, ANil, ANil) 
	else let parentList = parents rep [a] in
	match parentList with
	| [] -> ANode(a, ANil, ANil)
	| x::[] -> ANode(a, makeATree rep x, ANil) 
	| x::y::_ -> ANode(a, makeATree rep x, makeATree rep y)
	

(* FUNCTION merge *)

(*Auxiliar function that append *)
let rec mergeRec l rep1 rep2 =
	match l with
	| [] -> []
	| x::xs -> (x, union (children rep1 [x]) (children rep2 [x]))::mergeRec xs rep1 rep2

(*Returns a repository that is a merge from two different given repositories*)
(* pre: validStructural rep1 && validSemantic rep1 && validStructural rep2 && validSemantic rep2*)
(* post: validStructural result *)
let merge rep1 rep2 =
	let individuals = union (all1 rep1) (all1 rep2) in
		mergeRec individuals rep1 rep2 


(* FUNCTION repOfATree *)

let getElementFromANode t = 
  match t with 
  | ANil -> failwith "getElementFromANode: can't get element from empty tree"
  | ANode(x, _, _) -> x
                   
let rec repOfATreeRec t rep =
  match t with
  | ANil -> rep
  | ANode(a, ANil, ANil) -> rep
  | ANode(a, lft, ANil) -> 
      let leftParent = getElementFromANode (lft) in
      repOfATreeRec lft ((leftParent, [a])::rep)
  | ANode (a, ANil, rgt) -> 
      let rightParent = getElementFromANode (rgt) in
      repOfATreeRec rgt ((rightParent, [a])::rep)
  | ANode(a, lft, rgt) -> 
      let leftParent = getElementFromANode (lft) in
      let rightParent = getElementFromANode (rgt) in
      merge (repOfATreeRec lft ((leftParent, [a])::rep)) (repOfATreeRec rgt ((rightParent, [a])::rep))

(*Returns **)
(* pre: saneATree t *)
(* post: validStructural result *)
let repOfATree at = repOfATreeRec at []

(* FUNCTION makeDTree *)

let rec nbuildDTree rep d = 
	let childrenList = children rep [d] in
	match childrenList with 
	| [] -> DNode(d, [])
 	| x::xs -> DNode(d, lnbuildDTree (x::xs) rep)
and lnbuildDTree cl rep = match cl with 
	| [] -> []
	| c::cs -> clean ([nbuildDTree rep c] @ (lnbuildDTree cs rep))

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
	
(*We should reconsider this function*)
let siblingsInbreeding rep = []


(* FUNCTION waveN *)
let rec waveN rep n lst =
  waveNRec rep n lst lst
and waveNRec rep n lst discarded =
  if n = 0 then lst
  else let parent = parents rep lst in
    let childs = children rep lst in
    waveNRec rep (n-1) (diff (parent @ childs) (discarded)) (union (parent @ childs) (discarded))


(* FUNCTION supremum *)
	
let rec getAllAncestors rep l = 
	match l with
	| [] -> []
	| x::xs -> let parentsList = parents rep [x] in 
				if parentsList = [] then getAllAncestors rep xs
				else parentsList@(getAllAncestors rep parentsList)@(getAllAncestors rep xs)
	
let getAscendentPath rep e = getAllAncestors rep [e]

let rec getPathList rep l = 
	match l with 
	| [] -> []
	| x::xs ->  (getAscendentPath rep x)::(getPathList rep xs)

let rec nodeInCommon pl = 
	match pl with 
	| [] -> []
	| [x] -> []
	| x::y::xs -> (inter x y) @ nodeInCommon xs

let rec supremumRec rep s = 
	let pathList = getPathList rep s in 
	let res = nodeInCommon pathList in
	if res = [] then supremumRec rep (clean (getAllAncestors rep s))
	else res
	
let supremum rep s = 
	if s = [] then []
	(*else let childNodes = all2 rep in 
	if len (diff (childNodes) s ) == len (childNodes) then []*)
	else supremumRec rep s
	

(* FUNCTION validStructural *)


let rec checkOccurence rep1 rep2 = 
  match rep2 with
  | [] -> true 
  | (_, sons)::xs -> if inter (sons) (all1 rep1) = sons then checkOccurence rep1 xs 
      else false

let validStructural rep =
  let individuals = all1 rep in
 
  if inter ((clean individuals))  (individuals) = individuals then false 
  else (checkOccurence rep rep)

(* FUNCTION validSemantic *)

let rec checkLoop rep x = 
  let ancestors = getAllAncestors rep [x] in
  diff [x] ancestors  = []
    

let rec validSemanticRec rep1 rep2  =       
  match rep2 with
  | [] -> true
  | (x, _)::tail -> if checkLoop rep1 x then false 
      else if (len (parents rep1 [x]) > 2) then false
      else validSemanticRec rep1 tail
          
let validSemantic rep =
  validSemanticRec rep rep