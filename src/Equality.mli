
module type Eq = sig
  type t
end

module type Eq1 = sig
  type 'a t
end

module type Eq2 = sig
  type ('a, 'b) t
end

