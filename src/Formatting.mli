
(**
  {1 Formatting and Pretty-printing}

  When implementing a formatting interface for your own type, you will have to
  implement a function with the type:

{[
type 'a fmt = Format.formatter -> 'a -> unit
]}

  {2 Inspect vs Display}

  These two formatting interfaces have distinct purposes:

  {ul
  {- {!modtype:Display} implementations assert that the type can be faithfully
   represented as a human-readable string. It is not expected that all types
   implement this interface.}
  {- {!modtype:Inspect} should be implemented by {b all} public types. Output
   will represent the internal state of the type as faithfully as possible. The
   purpose of this interface is to facilitate debugging. }

  {Type}  -> Type.display
  {?Type} -> Type.inspect

  {2 Examples}

{[
module Point2D = struct
  type t = {
    x : int;
    y : int;
  } [@@deriving inspect]

  let inspect f t =
    Fmt.pf f "Point.{@[x = %d;@ y = %d@]}" t.x t.y

  let display f t =
    Fmt.pf f "(%d, %d)" t.x t.y
end

let point = Point2D.{ x = 42; y = 10 }

let () =
  print "{Point2D}" point;  (* => "(3, 4)" *)
  print "{?Point2D}" point  (* => "Point2D.{x = 3; y = 4}" *)
]}
*)

type 'a fmt = Format.formatter -> 'a -> unit
(** The type for formatters of values of type ['a]. *)


(* Minimal interface for formattable types. *)
module type Fmt = sig
  type t

  val fmt : t fmt
end


module type Inspect = sig
  type t

  val inspect : t fmt
end


module type Display = sig
  type t

  val display : t fmt
end


module Inspect (B : Fmt) : Inspect with type t := B.t
module Inspect (B : Fmt) : Inspect with type t := B.t

