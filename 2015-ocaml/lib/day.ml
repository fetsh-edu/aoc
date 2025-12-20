(** The interface that every day's solution must implement. *)
module type Solver = sig
  type t
  (** [t] represents the parsed input type for the day's puzzle. Each day can
      define its own input format. *)

  val parse_input : string -> t
  (** [parse_input input] parses the raw input string into the puzzle's input
      format. This is separate from solving to allow profiling parsing vs
      solving. *)

  val solve_part1 : t -> string
  (** [solve_part1 input] solves the first part of the day's puzzle. Returns the
      answer as a string. *)

  val solve_part2 : t -> string
  (** [solve_part2 input] solves the second part of the day's puzzle. Returns
      the answer as a string. *)
end
