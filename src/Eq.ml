

module type Eq_base = sig
  type t

  val eq : t -> t -> bool
  (** [eq t1 t2] tests if the values [t1] and [t2] are equal and is used by [=].

      This function is required to implement the [Eq] interface. *)
end


module type Eq = sig
  include Eq_base

  val ( = ) : t -> t -> bool

  val ( <> ) : t -> t -> bool
end

module Eq (B : Eq_base) = struct
  let ( = ) a b = B.eq a b
  let ( <> ) a b = not (B.eq a b)
end


module Eq_int = Eq(struct
  type t = int

  let eq (a: int) (b: int) : bool =
    Pervasives.(=) a b
end)



