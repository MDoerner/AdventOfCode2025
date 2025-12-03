type part = Part1 | Part2

module type DaySolverImpl = sig
  type input_t
  val parse: string -> input_t option
  val solve_part: part -> input_t -> string
end

module type S = sig
  val solve_part: part -> string -> string option
end

module Make(Impl : DaySolverImpl) : S = struct
  let solve_part = fun p s -> s |> Impl.parse |> (Option.map @@ (Impl.solve_part @@ p))
end