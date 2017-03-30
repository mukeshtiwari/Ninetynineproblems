#load "unix.cma";;
open Unix


let rec last (xs : 'a list) : 'a option =
  match xs with
      | [] -> None
      | [x] -> Some x
      | _ :: ys -> last ys

let rec last_two (xs : 'a list) : ('a * 'a) option =
  match xs with
  | [] | [_] -> None
  | [x; y] -> Some (x, y)
  | _ :: ys -> last_two ys

let rec at (k : int) : 'a list -> 'a option =
  function
  | [] -> None
  | h :: t -> if k = 1 then Some h else at (k - 1) t

let length (xs : 'a list) : int =
  let rec acc_length acc = function
    | [] -> acc
    | h :: t -> acc_length (acc + 1) t
  in acc_length 0 xs

let rev (xs : 'a list) : 'a list =
  let rec rev_list acc = function
    | [] -> acc
    | h :: t -> rev_list (h :: acc) t
  in rev_list [] xs

let is_palindrome (xs : 'a list) : bool =
  xs = rev xs

type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten (xs : 'a node list) : 'a list =
  let rec acc_flatten acc = function
    | [] -> acc
    | One x :: t -> acc_flatten (x :: acc) t
    | Many ys :: t -> acc_flatten (acc_flatten acc ys) t
  in List.rev (acc_flatten [] xs)

let compress (xs : 'a list) : 'a list =
  let rec acc_compress acc = function
    | [] -> acc
    | [x] -> (x :: acc)
    | (x :: y :: t) ->
       if x = y then acc_compress acc (y :: t)
       else acc_compress (x :: acc) (y :: t)
  in List.rev (acc_compress [] xs)


(* bit verbose but clean solution *)
let rec similar (xs : 'a list) : ('a list * 'a list) =
  match xs with
  | [] -> ([], [])
  | [x] -> ([x], [])
  | x :: ((y :: t) as v) ->
     if not (x = y) then ([x], v)
     else let (w, z) = similar v in (x :: w, z)

let rec pack (xs : 'a list) : 'a list list =
  match xs with
  | [] -> []
  | xs ->
     let (u, v) = similar xs in u :: pack v

let encode (xs : 'a list) : (int * 'a) list =
  List.map (fun x -> (List.length x, List.hd x)) (pack xs)

let encodeList (xs : 'a list) : (int * 'a) list =
  let rec acc_encode acc count = function
    | [] -> []
    | [x] ->(count + 1, x) :: acc
    | x :: y :: t ->
       if x = y then acc_encode acc (count + 1) (y :: t)
       else acc_encode ((count + 1, x) :: acc) 0 (y :: t)
  in List.rev (acc_encode [] 0 xs)

type 'a rle =
  |One of 'a
  |Many of int * 'a

let encodeRle (xs : 'a list) : 'a rle list =
  List.map (fun x ->
      let t = List.length x in
      match t with
      | 1 -> One (List.hd x)
      | _ -> Many (t, List.hd x)) (pack xs)

let rec decode : 'a rle list -> 'a list =
  function
  | [] -> []
  | One x :: t -> x :: decode t
  | Many (k, x) :: t ->
     let rec iterate x = function
       | 0 -> []
       | w -> x :: iterate x (pred w)
     in List.append (iterate x k) (decode t)

let encodeDirectSolution (xs : 'a list) : 'a rle list =
  let rle count x = if count = 0 then One x else Many (count, x) in
  let rec acc_encode count acc = function
    | [] -> []
    | [x] -> rle count x :: acc
    | x :: (y :: _ as t) ->
       if x = y then acc_encode (count + 1) acc t
       else acc_encode 0 (rle count x :: acc) t
  in List.rev (acc_encode 0 [] xs)

let rec duplicate : 'a list -> 'a list = function
  | [] -> []
  | x :: t -> x :: x :: duplicate t

let duplicateAcc (xs : 'a list) : 'a list =
  let rec acc_duplicate acc = function
    | [] -> acc
    | x :: t -> acc_duplicate (x :: x :: acc) t
  in List.rev (acc_duplicate [] xs)

let rec replicate (xs : 'a list) (k : int) : 'a list =
  match xs with
  | [] -> []
  | x :: t ->
     let rec iterate x = function
       | 0 -> []
       | w -> x :: iterate x (pred w)
     in List.append (iterate x k) (replicate t k)

let rec drop (xs : 'a list) (k : int) : 'a list =
  let rec drop_k count = function
    | [] -> []
    | x :: t ->
       if count = k then drop_k 1 t
       else x :: drop_k (count + 1) t
  in drop_k 1 xs

let rec split (xs : 'a list) (k : int) : ('a list * 'a list) =
  let rec split_two ys acc cnt =
    if List.length ys = 0 then (List.rev acc, [])
    else if cnt = k then (List.rev acc, ys)
    else split_two (List.tl ys) (List.hd ys :: acc) (cnt + 1)
  in split_two xs [] 0

let slice (xs : 'a list) (n : int) (m : int) : 'a list =
  let rec slice_nm cnt acc = function
    | [] -> List.rev acc
    | h :: t ->
       if cnt < n then slice_nm (succ cnt) acc t
       else if cnt > m then List.rev acc
       else slice_nm (succ cnt) (h :: acc) t
  in slice_nm 0 [] xs

let rotate (xs : 'a list) (k : int) : 'a list =
  let len = List.length xs in
  let k' = (len + k) mod len in
  let (u, v) = split xs k' in List.append v u

let remove_at (k : int) (xs : 'a list) : 'a list =
  let rec remove_kth cnt acc = function
    | [] -> []
    | h :: t ->
       if cnt = k then List.append (List.rev acc) t
       else remove_kth (succ cnt) (h :: acc) t
  in remove_kth 0 [] xs

let rec insert_at (elem : 'a) (k : int) (xs : 'a list) : 'a list =
  let rec insert_kth cnt acc = function
    | [] -> List.rev (elem :: acc)
    | h :: t ->
       if cnt = k then (List.rev (h :: elem :: acc)) @ t
       else insert_kth (succ cnt) (h :: acc) t
  in insert_kth 0 [] xs

let rec range (n : int) (m : int) : int list =
  let parity = if n < m then 1 else -1 in
  let rec range_rec a b =
    if a = b then [a] else a :: range_rec (a + parity) b
  in range_rec n m

               (* Arithmetic *)
let rec prime : int -> bool = function
  | n when n <= 1 -> false
  | 2 -> true
  | n ->
    let lst =
      List.filter (fun x -> n mod x = 0)
                (range 2 (1 + int_of_float (sqrt (float_of_int n))))
    in List.length lst = 0

let rec gcd (n : int) (m : int) : int =
  if n mod m = 0 then m
  else gcd m (n mod m)

let coprime (n : int) (m : int) : bool =
  gcd n m = 1

let rec phi (n : int) : int =
  List.length (List.filter (fun x -> gcd x n = 1) (range 1 n))

let all_primes (n : int) (m : int) : int list =
  let upperbound =
    List.filter (fun x -> m mod x = 0)
                (range 2 (1 + int_of_float (sqrt (float_of_int m)))) in
  let isprime = function
    | v when v <= 1 -> false
    | 2 -> true
    | v ->
       List.length (List.filter (fun x -> v mod x = 0) upperbound) = 0 in
  List.filter isprime (range n m)

let rec factors (n : int) : int list =
  let rec factors_acc acc m = function
    | [] -> List.rev (m :: acc) (* this case will not happen *)
    | (x :: rest) as t ->
       if x * x > m then List.rev (m :: acc)
       else if m mod x = 0 then factors_acc (x :: acc) (m / x) t
       else factors_acc acc m rest
  in factors_acc [] n (all_primes 2 (int_of_float (sqrt (float_of_int n))))

let rec factorslist (n : int) : int list =
  let rec factorlist acc m d =
    if m = 1 then List.rev acc
    else if d * d > m then List.rev (m :: acc)
    else if m mod d = 0 then factorlist (d :: acc) (m / d) d
    else factorlist acc m (d + 1)
  in factorlist [] n 2

let factors_improved (n : int) : (int * int) list =
  let rec factors d m =
    if m = 1 then []
    else if m mod d = 0 then
      match factors d (m / d) with
      | (h, cnt) :: t when h = d -> (h, cnt + 1) :: t
      | t -> (d, 1) :: t
    else factors (d + 1) m
  in factors 2 n


let phi_improved (n : int) : int =
  let primelist = factors_improved n in
  let rec pow a b =
    match b with
    | 0 -> 1
    | 1 -> a
    | _ when b mod 2 = 0 -> pow (a * a) (b / 2)
    | _ when b mod 2 <> 0 -> a * pow (a * a) (b / 2) in
  List.fold_left
    (fun x (p, cnt) -> x * (p - 1) * (pow p (pred cnt))) 1 primelist

let timeit f a =
  let t0 = Unix.gettimeofday() in
  ignore(f a);
  let t1 = Unix.gettimeofday() in
  t1 -. t0

(* n should be greater than 2 *)
let rec goldbach (n : int) : int * int =
  let rec goldbach_help d =
    if prime d && prime (n - d) then (d, n - d)
    else goldbach_help (d + 1) in
  goldbach_help 2

let goldbach_list (n : int) (m : int) : (int * (int * int)) list =
  let rec goldbachhelp a b acc =
    if a > b then List.rev acc
    else if a mod 2 <> 0 then goldbachhelp (succ a) b acc
    else goldbachhelp (succ a) b ((a, goldbach a) :: acc) in
  goldbachhelp n m []

let goldbach_limit a b lim =
  List.filter (fun (_,(x,y)) -> x > lim && y > lim) (goldbach_list a b);;

type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr


let rec generate_table : 'a list -> ('a * 'b) list list =
  function
  | [] -> []
  | [x] -> [[(x, true)]; [(x, false)]]
  | x :: xs ->
     let ret = generate_table xs in
     List.append (List.map (fun ys -> (x, true) :: ys) ret)
                 (List.map (fun ys -> (x, false) :: ys) ret)

let rec evaluate (lst : ('a * bool) list) :
          bool_expr -> bool
  = function
  | Var x -> List.assoc x lst
  | Not exp -> not (evaluate lst exp)
  | And (eone, etwo) -> (evaluate lst eone) && (evaluate lst etwo)
  | Or (eone, etwo) -> (evaluate lst eone) || (evaluate lst etwo)

let rec table lst expr =
  let ret = generate_table lst in
  List.map (fun x -> (x, evaluate x expr)) ret

let greycode n =
  let rec greyinner =
    function
    | 1 -> [["0"]; ["1"]]
    | m ->
       let ret = greyinner (m - 1) in
       List.append (List.map (fun p -> "0" :: p) ret)
                   (List.map (fun p -> "1" :: p) (List.rev ret))
  in List.map (String.concat "") (greyinner n)

let rec subset : 'a list -> 'a list list  = function
  | [] -> [[]]
  | x :: xs ->
     let ret = subset xs in
     List.append ret (List.map (fun y -> x :: y) ret)

let delete x xs = List.filter (fun y -> x <> y) xs

let concatMap (f : 'a -> 'b list) (l : 'a list) : 'b list =
  List.map f l |> List.concat
(*List.(concat (map f l))*)

let rec perm (xs : 'a list) : 'a list list =
  match xs with
  | [] -> [[]]
  | ts ->
     concatMap (fun x -> List.map (fun y -> x :: y) (perm (delete x ts))) ts

(* leave huffman for moment. Implement your own priority queue
to understand module system *)



(* This code is taken from https://www.lri.fr/~filliatr/ftp/ocaml/ds/leftistheap.ml.html *)
module type Ordered = sig
  type t
  val le : t -> t -> bool
end

exception Empty

module Make (X : Ordered) :
sig
  type t
  val empty : t
  val is_empty : t -> bool
  val insert : X.t -> t -> t
  val min : t -> X.t
  val extract_min : t -> X.t * t
  val merge : t -> t -> t
  val num_element : t -> int
end
  =
  struct
    type t =
      | E
      | T of int * X.t * t * t

    let rank = function
      | E -> 0
      | T (r, _, _, _) -> r

    let make x a b =
      let ra = rank a and rb = rank b in
      if ra >= rb then T (rb + 1, x, a, b) else T (ra + 1, x, b, a)

    let empty = E

    let is_empty = function
      | E -> true
      | T _ -> false

    let rec merge h1 h2 =
      match h1, h2 with
      | E, h |h, E -> h
      | T (_, x, a1, b1), T (_, y, a2, b2) ->
         if X.le x y then make x a1 (merge b1 h2) else make y a2 (merge h1 b2)

    let insert x h = merge (T (1, x, E, E)) h

    let min = function 
      | E -> raise Empty
      | T (_, x, _, _) -> x

    let extract_min = function
      | E -> raise Empty
      | T (_, x, a, b) -> x, merge a b

    let rec num_element = function
      | E -> 0
      | T(_, _, l, r) -> 1 + num_element l + num_element r

  end

    
type tree =
  | Leaf of string
  | Node of tree * tree
  
module Heap = Make (struct
                     type t = tree * int
                     let le = fun a b -> snd a < snd b
                   end)

(* convert string * int as heap *)
let convert_heap  =
  List.fold_left (fun x (f, s) -> Heap.insert (Leaf f, s) x) Heap.empty

let rec huffman_tree ht =
  let c = Heap.num_element ht in
  if c = 1 then ht else
    let ((f, x), ht1) = Heap.extract_min ht in
    let ((s, y), ht2) = Heap.extract_min ht1 in
    huffman_tree (Heap.insert (Node (f, s), x + y) ht2) 


let rec traverse_tree pfix = function
  | Leaf s -> [(s, pfix)]
  | Node(l, r) ->
     traverse_tree (pfix ^ "0") l
     @ traverse_tree (pfix ^ "1") r  

(* glue code *)
let huffman fs =
  let hp = convert_heap fs in
  let ((tr, _), _) = Heap.extract_min (huffman_tree hp) in
  traverse_tree "" tr

let t = [ ("a", 45); ("b", 13); ("c", 12); ("d", 16);
          ("e", 9); ("f", 5) ]  

type 'a binary_tree =
  | Empty : 'a binary_tree
  | Node : 'a * 'a binary_tree * 'a binary_tree -> 'a binary_tree

let example_tree =
  Node('a', Node('b', Node('d', Empty, Empty), Node('e', Empty, Empty)),
       Node('c', Empty, Node('f', Node('g', Empty, Empty), Empty)))

let add_trees_with left right all =
  let add_right_tree all l =
    List.fold_left (fun a r -> Node('x', l, r) :: a) all right in
  List.fold_left add_right_tree all left
                 
let rec cbal_tree n =
  match n with
  | 0 -> [Empty]
  | _ when n mod 2 = 1 ->
     let t = cbal_tree (n / 2) in
     add_trees_with t t []
  | _ ->
     let t1 = cbal_tree (n / 2 - 1) in
     let t2 = cbal_tree (n / 2) in
     add_trees_with t1 t2 (add_trees_with t2 t1 [])
