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
