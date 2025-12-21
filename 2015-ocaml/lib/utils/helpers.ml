(** Common helper functions available throughout the project *)

module Array = struct
  include Stdlib.Array (* This brings in map, filter, fold_left, etc. *)

  let count p = Array.fold_left (fun acc x -> acc + if p x then 1 else 0) 0
end

module List = struct
  include Stdlib.List (* This brings in map, filter, fold_left, etc. *)

  let rec combination_pairs = function
    | [] -> []
    | x :: xs ->
        let pairs = List.map (fun y -> (x, y)) xs in
        pairs @ combination_pairs xs

  let combinations k list =
    let rec aux k list acc =
      if k <= 0 then [ acc ]
      else
        match list with
        | [] -> []
        | h :: t -> aux (k - 1) t (h :: acc) @ aux k t acc
    in
    aux k list []

  let count p = List.fold_left (fun acc x -> acc + if p x then 1 else 0) 0
  let min = List.fold_left min max_int
  let max = List.fold_left max min_int

  let min_by f = function
    | [] -> invalid_arg "List.min_by: empty list"
    | h :: t -> List.fold_left (fun acc x -> if f x < f acc then x else acc) h t

  let rec window_by_2 = function
    | x :: (y :: _ as rest) -> (x, y) :: window_by_2 rest
    | _ -> []
end

module Int = struct
  include Stdlib.Int (* This brings in min, max, succ, pred, etc. *)

  let product = List.fold_left (fun acc x -> acc * x) 1
  let sum = List.fold_left ( + ) 0
  let even n = n mod 2 = 0
  let odd n = not (even n)
end

module Set = struct
  include Stdlib.Set

  module type S_with_permutations = sig
    include S

    val permutations : t -> elt list list
  end

  module Make (Ord : OrderedType) : S_with_permutations with type elt = Ord.t =
  struct
    include Make (Ord)

    let rec permutations set =
      if is_empty set then [ [] ]
      else
        fold
          (fun elem acc ->
            let remaining = remove elem set in
            let sub_perms = permutations remaining in
            let with_elem = List.map (fun perm -> elem :: perm) sub_perms in
            with_elem @ acc)
          set []
  end
end

module String = struct
  include Stdlib.String

  let is_vowel = function 'a' | 'e' | 'i' | 'o' | 'u' -> true | _ -> false
end

let ( >> ) f g x = g (f x)

(** Placeholder for unimplemented code - raises a clear error message *)
let todo () = failwith "Not yet implemented"

(** Like todo but with a custom message *)
let todo_msg msg = failwith ("TODO: " ^ msg)

(** Debug print - prints a value and returns it (useful for pipeline debugging)
*)
let debug ?(label = "DEBUG") value =
  Printf.eprintf "[%s] %s\n%!" label
    ( Printexc.get_callstack 0 |> fun _ ->
      if Obj.is_block (Obj.repr value) then "<value>"
      else string_of_int (Obj.magic value : int) );
  value

(** Tap - apply a function for side effects and return the original value *)
let tap f x =
  f x;
  x

(** Flip function arguments *)
let flip f x y = f y x

(** Identity function *)
let id x = x

(** Const function - always returns the same value *)
let const x _ = x
