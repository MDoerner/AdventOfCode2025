type part = Part1 | Part2

module type DaySolverImpl = sig
  type input_t
  val parse: string -> input_t option
  val solve_part: part -> input_t -> string
end

module type S = sig
  val solve_part: part -> string -> string option
end

module Make : functor  (_ : DaySolverImpl) -> S