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
  if
