
module P = Pervasives

type 'a equal = 'a -> 'a -> bool

module type Equatable = sig
  type t

  val equal : t -> t -> bool
  val (==)  : t -> t -> bool
  val (!=)  : t -> t -> bool
end


module Equatable = struct
  module type Base = sig
    type t

    val equal : t -> t -> bool
  end

  module Make(B : Base) : (Equatable with type t := B.t) = struct
    include B

    let (==)  a b = equal a b
    let (!=) a b = not (equal a b)
  end
end


module type Equatable1 = sig
  type 'a t

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
end


module type Equatable2 = sig
  type ('a, 'b) t

  val equal :
    ('a -> 'a -> bool) ->
    ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool
end


type order =
  | Less
  | Equal
  | Greater

type 'a comparator = 'a -> 'a -> order

module type Comparable = sig
  type t

  include Equatable with type t := t

  val compare : t -> t -> order
  val ( <  ) : t -> t -> bool
  val ( >  ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( >= ) : t -> t -> bool
  val min : t -> t -> t
  val max : t -> t -> t
end


module Comparable = struct
  module type Base = sig
    type t

    val compare : t -> t -> order
  end

  module Make(B : Base) : (Comparable with type t := B.t) = struct
    include Equatable.Make(struct
        type nonrec t = B.t
        let equal a b =
          match B.compare a b with
          | Equal -> true
          | Less | Greater -> false
      end)

    let compare = B.compare

    let ( < ) a b =
      match B.compare a b with
      | Less -> true
      | Equal | Greater -> false

    let ( > ) a b =
      match B.compare a b with
      | Greater -> true
      | Less | Equal -> false

    let ( <= ) a b = not (a > b)
    let ( >= ) a b = not (a < b)

    let min a b = if a < b then a else b
    let max a b = if a > b then a else b
  end
end


module type Comparable1 = sig
  type 'a t

  val compare :
    'a comparator -> 'a t -> 'a t -> order

  val min : 'a comparator -> 'a t -> 'a t -> 'a t

  val max : 'a comparator -> 'a t -> 'a t -> 'a t
end


module Comparable1 = struct
  module type Base = sig
    type 'a t

    val compare : 'a comparator -> 'a t -> 'a t -> order
  end

  module Make(B : Base) : (Comparable1 with type 'a t := 'a B.t) = struct
    let compare = B.compare

    let min cmp a b =
      match B.compare cmp a b with
      | Less -> a
      | Equal | Greater -> b

    let max cmp a b =
      match B.compare cmp a b with
      | Greater -> a
      | Equal | Less -> b
  end
end


module type Comparable2 = sig
  type ('a, 'b) t

  val compare :
    'a comparator -> 'b comparator ->
    ('a, 'b) t -> ('a, 'b) t ->
    order

  val min :
    'a comparator -> 'b comparator ->
    ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t

  val max :
    'a comparator -> 'b comparator ->
    ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
end


module Comparable2 = struct
  module type Base = sig
    type ('a, 'b) t

    val compare :
      'a comparator -> 'b comparator ->
      ('a, 'b) t -> ('a, 'b) t ->
      order
  end

  module Make(B : Base) : Comparable2 with type ('a, 'b) t := ('a, 'b) B.t = struct
    let compare = B.compare

    let min cmp_a cmp_b a b =
      match B.compare cmp_a cmp_b a b with
      | Less -> a
      | Equal | Greater -> b

    let max cmp_a cmp_b a b =
      match B.compare cmp_a cmp_b a b with
      | Greater -> a
      | Equal | Less -> b
  end
end

let is = P.( == )


(* Formatting and Pretty-printing *)

type 'a printer = Format.formatter -> 'a -> unit

let format = Format.asprintf

let print ?(channel = Pervasives.stdout) ?(break = "\n") fmt =
  let open Format in
  let formatter = formatter_of_out_channel channel in
  let pp_break formatter =
    pp_print_string formatter break;
    pp_print_flush formatter () in
  kfprintf pp_break formatter fmt


module type Printable = sig
  type t

  val printer : t printer

  val to_string : t -> string

  val print : t -> unit
end


module Printable = struct
  module type Base = sig
    type t

    val printer : t printer
  end

  module Make(B : Base) : (Printable with type t := B.t) = struct
    include B

    let to_string self =
      Format.asprintf "%a" B.printer self

    let print self =
      Format.fprintf Format.std_formatter "%a@." B.printer self
  end
end


module type Printable1 = sig
  type 'a t

  val printer : 'a printer -> 'a t printer

  val to_string : 'a printer -> 'a t -> string

  val print : 'a printer -> 'a t -> unit
end


module Printable1 = struct
  module type Base = sig
    type 'a t

    val printer : 'a printer -> Format.formatter -> 'a t -> unit
  end

  module Make(B : Base) : (Printable1 with type 'a t := 'a B.t) = struct
    include B

    let to_string : 'a . 'a printer -> 'a B.t -> string =
      fun printer1 self ->
      Format.asprintf "%a" (B.printer printer1) self

    let print : 'a . 'a printer -> 'a B.t -> unit =
      fun printer1 self ->
        Format.fprintf Format.std_formatter "%a@." (B.printer printer1) self
  end
end


module type Printable2 = sig
  type ('a, 'b) t

  val printer : 'a printer -> 'b printer -> ('a, 'b) t printer

  val to_string : 'a printer -> 'b printer -> ('a, 'b) t -> string

  val print : 'a printer -> 'b printer -> ('a, 'b) t -> unit
end


module Printable2 = struct
  module type Base = sig
    type ('a, 'b) t

    val printer : 'a printer -> 'b printer -> ('a, 'b) t printer
  end

  module Make(B : Base) : (Printable2 with type ('a, 'b) t := ('a, 'b) B.t) = struct

    include B

    let to_string : 'a 'b . 'a printer -> 'b printer -> ('a, 'b) B.t -> string =
      fun printer1 printer2 self ->
        Format.asprintf "%a" (B.printer printer1 printer2) self

    let print : 'a 'b. 'a printer -> 'b printer -> ('a, 'b) B.t -> unit =
      fun printer1 printer2 self ->
        Format.fprintf Format.std_formatter "%a@." (B.printer printer1 printer2) self
  end
end


type 'a hasher = 'a -> int

module Hasher = struct
  type 'a t = 'a hasher

  let int x =
    Pervasives.(x land max_int)

  let char x =
    Char.code x

  let string x =
    Hashtbl.hash x

  let pair hash_a hash_b (a, b) =
    Hashtbl.seeded_hash (hash_a a) (hash_b b)
end

module type Hashable = sig
  type t

  val hash : t -> int
end

module type Hashable1 = sig
  type 'a t

  val hash : ('a -> int) -> 'a t -> int
end

module type Hashable2 = sig
  type ('a, 'b) t

  val hash : ('a -> int) -> ('b -> int) -> ('a, 'b) t -> int
end

module type Bounded = sig
  type t

  val min_value : t
  val max_value : t
end

module type Default = sig
  type t

  val default : t
end

module type Default1 = sig
  type 'a t

  val default : 'a t
end

module type Enumerable = sig
  type t

  val predecessor : t -> t
  val successor : t -> t
end

module type Numeric = sig
  type t

  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val (~- ) : t -> t
  val (~+ ) : t -> t
  val abs : t -> t
  val signum : t -> t
  val of_int : int -> t
end

module Exception = struct
  type t = exn

  let raise ?(trace = true) self =
    if trace then
      P.raise self
    else
    P.raise_notrace self

  let fail = P.failwith

  let bracket make (free : 'a -> unit) f =
    let x = make () in
    try
      let r = f x in
      free x;
      r
    with exn ->
      free x;
      raise exn

  include Printable.Make(struct
      type nonrec t = t

      let printer formatter self =
        Format.fprintf formatter "%s" (Printexc.to_string self)
    end)
end


(* Exceptions *)
let raise = Exception.raise

let raises ?only:exn f =
  try
    P.ignore (f ());
    false
  with e -> begin
      match exn with
      | Some x -> e = x
      | None -> true
    end

let fail = Exception.fail

exception Undefined

let undefined () =
  raise Undefined

let bracket = Exception.bracket



(* Unit type and operations *)

module Unit = struct
  type t = unit

  let try_of_string = function "()" -> Some () | _ -> None

  (* Bounded *)

  let min_value = ()

  let max_value = ()

  (* Comparable *)
  include Comparable.Make(struct
      type nonrec t = t
      let compare _ _ = Equal
    end)

  (* Default *)
  let default = ()

  (* Equatable *)
  include Equatable.Make(struct
      type nonrec t = t
      let equal : t -> t -> bool = Pervasives.(=)
    end)

  (* Hashable *)
  let hash = Hashtbl.hash

  (* Printable *)
  include Printable.Make(struct
      type nonrec t = t
      let printer fmt () = Format.fprintf fmt "()"
    end)
end


(* Boolean type and operations *)

module Bool = struct
  type t = bool

  let not = P.not

  let ( && ) = P.( && )
  let ( || ) = P.( || )

  let to_int = function false -> 0 | true -> 1
  let of_int = function 0 -> false | _ -> true

  let try_of_string str =
    try
      Some (P.bool_of_string str)
    with _ ->
      None

  let to_option default self =
    match self with
    | true -> Some default
    | false -> None

  (* Bounded *)
  let min_value = false
  let max_value = true

  (* Comparable *)
  include Comparable.Make(struct
      type nonrec t = t

      let compare a b =
        let legacy_cmp : t -> t -> int = Pervasives.compare in
        let order = legacy_cmp a b in
        if order < 0 then Less
        else if order > 0 then Greater
        else Equal
    end)

  (* Default *)
  let default = false

  (* Enumerable *)
  let predecessor = function
    | true  -> false
    | false -> fail "Bool.predecessor"

  let successor = function
    | true  -> fail "Bool.successor"
    | false -> true

  (* Equatable *)
  include Equatable.Make(struct
      type nonrec t = t

      let equal : t -> t -> bool = P.(=)
    end)

  (* Hashable *)
  let hash = Hashtbl.hash

  (* Printable *)
  include Printable.Make(struct
      type nonrec t = t
      let printer = Format.pp_print_bool
    end)
end

let not    = Bool.not
let ( && ) = Bool.( && )
let ( || ) = Bool.( || )


(* Integer type and arithmetic operations *)

module Int = struct
  type t = int

  let ( / ) = P.( / )

  let ( mod ) = P.( mod)

  let to_string self =
    P.string_of_int self

  let of_char = P.int_of_char

  let of_float = P.int_of_float

  let try_of_string s =
    try
      Some (P.int_of_string s)
    with _ ->
      None

  module Unsafe = struct
    let of_string = P.int_of_string
  end

  (* Bounded *)
  let min_value = P.min_int
  let max_value = P.max_int

  (* Comparable *)
  include Comparable.Make(struct
      type nonrec t = t

      let compare a b =
        let legacy_cmp : t -> t -> int = Pervasives.compare in
        let order = legacy_cmp a b in
        if order < 0 then Less
        else if order > 0 then Greater
        else Equal
    end)

  (* Default *)
  let default = 0

  (* Enumerable *)
  let predecessor x =
    if x > min_value then
      x - 1
    else
    fail "Int.predecessor"

  let successor x =
    if x < max_value then
      x + 1
    else
    fail "Int.successor"

  (* Equatable *)
  include Equatable.Make(struct
      type nonrec t = t
      let equal : t -> t -> bool = P.(=)
    end)

  (* Hashable *)
  let hash = Hasher.int

  (* Numeric *)
  let ( + ) = P.( + )
  let ( - ) = P.( - )
  let ( * ) = P.( * )
  let (~- ) = P.(~- )
  let (~+ ) = P.(~+ )
  let abs   = P.abs

  let signum x =
    if x > 0 then +1
    else
    if x < 0 then -1
    else
    0

  let of_int x = x

  (* Printable *)
  include Printable.Make(struct
      type nonrec t = t
      let printer = Format.pp_print_int
    end)
end

include (Int : Numeric)
include (Int : Comparable)


module Float = struct

  type t = float

  let ( / ) = P.( /. )
  let ( mod ) = P.mod_float
  let infinity = P.infinity
  let neg_infinity = P.neg_infinity

  let nan = P.nan

  let epsilon = P.epsilon_float

  let round_nearest_lb = -.(2. ** 52.)
  let round_nearest_ub =    2. ** 52.
  let round self =
    if self >= round_nearest_lb && self <= round_nearest_ub then
      floor (self +. 0.49999999999999994)
    else
      self

  let exp   = P.exp
  let frexp = P.frexp
  let ldexp = P.ldexp
  let modf  = P.modf

  type fpclass = P.fpclass
  let classify = P.classify_float

  let of_int x = float_of_int x

  let to_int self = int_of_float self

  let to_string self =
    string_of_float self

  let try_of_string str =
    try
      Some (float_of_string str)
    with _ ->
      None

  module Unsafe = struct
    let of_string = P.float_of_string
  end

  (* Bounded *)
  let min_value = P.min_float
  let max_value = P.max_float

  (* Comparable *)
  include Comparable.Make(struct
      type nonrec t = t

      let compare a b =
        let legacy_cmp : t -> t -> int = Pervasives.compare in
        let order = legacy_cmp a b in
        if order < 0 then Less
        else if order > 0 then Greater
        else Equal
    end)

  (* Default *)
  let default = 0.0

  (* Equatable *)
  include Equatable.Make(struct
      type nonrec t = t
      let equal : t -> t -> bool = P.(=)
    end)

  (* Hashable *)
  let hash self = Hashtbl.hash self

  (* Numeric *)
  let ( + ) = P.( +. )
  let ( - ) = P.( -. )
  let ( * ) = P.( *. )
  let (~- ) = P.(~-. )
  let (~+ ) = P.(~+. )
  let abs   = P.abs_float

  let signum self =
    if self > 0.0 then
      1.0
    else
    if self < 0.0 then
      -1.0
    else
    if self = 0.0 then
      0.0
    else
      nan

  (* Printable *)
  include Printable.Make(struct
      type nonrec t = t
      let printer = Format.pp_print_float
    end)
end


(* Char type and operations *)

module Char = struct
  type t = char

  let try_of_int x =
    try Some (Pervasives.char_of_int x)
    with Invalid_argument _ -> None

  let to_int self = P.int_of_char self

  let try_of_string s =
    if String.length s = 1 then
      Some (String.get s 0)
    else
      None

  module Unsafe = struct
    let of_int = P.char_of_int
  end

  (* Bounded *)
  let min_value = Unsafe.of_int 0x00
  let max_value = Unsafe.of_int 0xFF

  (* Comparable *)
  include Comparable.Make(struct
      type nonrec t = t
      let compare a b =
        let legacy_cmp : t -> t -> int = Pervasives.compare in
        let order = legacy_cmp a b in
        if order < 0 then Less
        else if order > 0 then Greater
        else Equal
    end)

  (* Default *)
  let default = P.char_of_int 0xFF

  (* Enumerable *)
  let predecessor self =
    if self > min_value then
      P.char_of_int (to_int self - 1)
    else
    fail "Char.predecessor"

  let successor self =
    if self < max_value then
      P.char_of_int (to_int self + 1)
    else
    fail "Char.successor"

  (* Hashable *)
  let hash x = Hasher.char x

  (* Printable *)
  include Printable.Make(struct
      type nonrec t = t
      let printer = Format.pp_print_char
    end)
end


let char = Char.try_of_int
let code = Char.to_int


module Pair = struct
  type ('a, 'b) t = 'a * 'b
    [@@deriving show]
    [@@public]

  let first  = Pervasives.fst
  let second = Pervasives.snd

  include Comparable2.Make(struct
      type nonrec ('a, 'b) t = ('a, 'b) t

      let compare cmp_a cmp_b (self_a, self_b) (other_a, other_b) =
        match cmp_a self_a other_a with
        | Equal -> cmp_b self_b other_b
        | other -> other
    end)

  let hash = Hasher.pair

  include Printable2.Make(struct
      type nonrec ('a, 'b) t = ('a, 'b) t

      let printer printer_a printer_b formatter (a, b) =
        Format.pp_print_string formatter "(";
        (printer_a formatter) a;
        Format.pp_print_string formatter ",";
        (printer_b formatter) b;
        Format.pp_print_string formatter ")"
    end)
end

type ('a, 'b) pair = ('a, 'b) Pair.t

let first  = Pair.first
let second = Pair.second

module Tuple2 = Pair


(** String operations *)
let (++) str1 str2 = Pervasives.(str1 ^ str2)

module Bitwise = struct

  let (<<<)  = P.(lsl)
  let (>>>)  = P.(asr)
  let (>>>!) = P.(lsr)
  let (~~~)  = P.(lnot)
  let (|||)  = P.(lor)
  let (&&&)  = P.(land)
  let (^^^)  = P.(lxor)
end


(* Function operations *)

let pass () = ()
let ( |> ) = P.( |> )
let ( @ ) = P.( @@ )
let identity x = x
let constantly x _ = x
let flip f x y = f y x
let curry f x y = f (x, y)
let uncurry f (x, y) = f x y
let compose f g x = f (g x)
let ( << ) f g = compose f g
let ( >> ) g f = compose f g


let int = Int.of_float
let float = Float.of_int


module Generic = struct
  let compare a b =
    let legacy_cmp : 'a -> 'a -> int = Pervasives.compare in
    let order = legacy_cmp a b in
    if order < 0 then
      Less
    else
    if order > 0 then
      Greater
    else
      Equal

  let equal = Pervasives.( = )

  let ( == ) = Pervasives.( =  )
  let ( != ) = Pervasives.( <> )
  let ( <  ) = Pervasives.( <  )
  let ( >  ) = Pervasives.( >  )
  let ( <= ) = Pervasives.( <= )
  let ( >= ) = Pervasives.( >= )

  let min = Pervasives.min
  let max = Pervasives.max
end


