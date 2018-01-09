
module P = Pervasives

(* Exceptions *)

let raise ?(trace = true) self =
  if trace then
    P.raise self
  else
  P.raise_notrace self


let raises ?only:exn f =
  try
    P.ignore (f ());
    false
  with e -> begin
      match exn with
      | Some x -> e = x
      | None -> true
    end

let fail = P.failwith

exception Undefined

let undefined () =
  raise Undefined

let bracket init (free : 'a -> unit) f =
  let x = init () in
  try
    let r = f x in
    free x;
    r
  with exn ->
    free x;
    raise exn


(* Comparisons and Ordering *)

module type Equatable = sig
  type self

  val equal : self -> self -> bool
  val (==)  : self -> self -> bool
  val (!=)  : self -> self -> bool
end


module Equatable = struct
  module type Base = sig
    type self

    val equal : self -> self -> bool
  end

  module Make(B : Base) : (Equatable with type self := B.self) = struct
    include B

    let (==)  a b = equal a b
    let (!=) a b = not (equal a b)
  end
end


module type Equatable1 = sig
  type 'a self

  val equal : ('a -> 'a -> bool) -> 'a self -> 'a self -> bool
end


module type Equatable2 = sig
  type ('a, 'b) self

  val equal :
    ('a -> 'a -> bool) ->
    ('b -> 'b -> bool) -> ('a, 'b) self -> ('a, 'b) self -> bool
end


type order =
  | Less
  | Equal
  | Greater

type 'a comparator = 'a -> 'a -> order

module type Comparable = sig
  type self

  include Equatable with type self := self

  val compare : self -> self -> order
  val ( <  ) : self -> self -> bool
  val ( >  ) : self -> self -> bool
  val ( <= ) : self -> self -> bool
  val ( >= ) : self -> self -> bool
  val min : self -> self -> self
  val max : self -> self -> self
end


module Comparable = struct
  module type Base = sig
    type self

    val compare : self -> self -> order
  end

  module Make(B : Base) : (Comparable with type self := B.self) = struct
    include Equatable.Make(struct
        type nonrec self = B.self
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
  type 'a self

  val compare :
    'a comparator -> 'a self -> 'a self -> order

  val min : 'a comparator -> 'a self -> 'a self -> 'a self

  val max : 'a comparator -> 'a self -> 'a self -> 'a self
end


module Comparable1 = struct
  module type Base = sig
    type 'a self

    val compare : 'a comparator -> 'a self -> 'a self -> order
  end

  module Make(B : Base) : (Comparable1 with type 'a self := 'a B.self) = struct
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
  type ('a, 'b) self

  val compare :
    'a comparator -> 'b comparator ->
    ('a, 'b) self -> ('a, 'b) self ->
    order

  val min :
    'a comparator -> 'b comparator ->
    ('a, 'b) self -> ('a, 'b) self -> ('a, 'b) self

  val max :
    'a comparator -> 'b comparator ->
    ('a, 'b) self -> ('a, 'b) self -> ('a, 'b) self
end


module Comparable2 = struct
  module type Base = sig
    type ('a, 'b) self

    val compare :
      'a comparator -> 'b comparator ->
      ('a, 'b) self -> ('a, 'b) self ->
      order
  end

  module Make(B : Base) : Comparable2 with type ('a, 'b) self := ('a, 'b) B.self = struct
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

let print ?(output = Pervasives.stdout) ?(break = "\n") fmt =
  let open Format in
  let formatter = formatter_of_out_channel output in
  let pp_break formatter =
    pp_print_string formatter break;
    pp_print_flush formatter () in
  kfprintf pp_break formatter fmt


module type Printable = sig
  type self

  val pp : self printer

  val to_string : self -> string

  val print : self -> unit
end


module Printable = struct
  module type Base = sig
    type self

    val pp : self printer
  end

  module Make(B : Base) : (Printable with type self := B.self) = struct
    include B

    let to_string self =
      Format.asprintf "%a" B.pp self

    let print self =
      Format.fprintf Format.std_formatter "%a" B.pp self
  end
end


module type Printable1 = sig
  type 'a self

  val pp : 'a printer -> 'a self printer

  val to_string : 'a printer -> 'a self -> string

  val print : 'a printer -> 'a self -> unit
end


module Printable1 = struct
  module type Base = sig
    type 'a self

    val pp : 'a printer -> Format.formatter -> 'a self -> unit
  end

  module Make(B : Base) : (Printable1 with type 'a self := 'a B.self) = struct
    include B

    let to_string : 'a . 'a printer -> 'a B.self -> string =
      fun pp1 self -> Format.asprintf "%a" (B.pp pp1) self

    let print : 'a . 'a printer -> 'a B.self -> unit =
      fun pp1 self -> Format.fprintf Format.std_formatter "%a" (B.pp pp1) self
  end
end


module type Printable2 = sig
  type ('a, 'b) self

  val pp : 'a printer -> 'b printer -> ('a, 'b) self printer

  val to_string : 'a printer -> 'b printer -> ('a, 'b) self -> string

  val print : 'a printer -> 'b printer -> ('a, 'b) self -> unit
end


module Printable2 = struct
  module type Base = sig
    type ('a, 'b) self

    val pp : 'a printer -> 'b printer -> ('a, 'b) self printer
  end

  module Make(B : Base) : (Printable2 with type ('a, 'b) self := ('a, 'b) B.self) = struct

    include B

    let to_string : 'a 'b . 'a printer -> 'b printer -> ('a, 'b) B.self -> string =
      fun pp1 pp2 self ->
        Format.asprintf "%a" (B.pp pp1 pp2) self

    let print : 'a 'b. 'a printer -> 'b printer -> ('a, 'b) B.self -> unit =
      fun pp1 pp2 self ->
        Format.fprintf Format.std_formatter "%a" (B.pp pp1 pp2) self
  end
end


type 'a hasher = 'a -> int

module Hasher = struct
  type 'a self = 'a hasher

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
  type self

  val hash : self -> int
end

module type Hashable1 = sig
  type 'a self

  val hash : ('a -> int) -> 'a self -> int
end

module type Hashable2 = sig
  type ('a, 'b) self

  val hash : ('a -> int) -> ('b -> int) -> ('a, 'b) self -> int
end

module type Bounded = sig
  type self

  val min_value : self
  val max_value : self
end

module type Default = sig
  type self

  val default : self
end

module type Default1 = sig
  type 'a self

  val default : 'a self
end

module type Enumerable = sig
  type self

  val predecessor : self -> self
  val successor : self -> self
end

module type Numeric = sig
  type self

  val ( + ) : self -> self -> self
  val ( - ) : self -> self -> self
  val ( * ) : self -> self -> self
  val (~- ) : self -> self
  val (~+ ) : self -> self
  val abs : self -> self
  val signum : self -> self
  val of_int : int -> self
end

type 'a parser = string -> 'a option

module type Parsable = sig
  type self

  val parse : string -> self option
end

module type Parsable1 = sig
  type 'a self

  val parse : 'a parser -> string -> 'a self option
end

module type Parsable2 = sig
  type ('a, 'b) self

  val parse : 'a parser -> 'b parser -> string -> ('a, 'b) self option
end

(* TODO: compound types (lists, tuples, records) may require a custom parser. *)
module Parser = struct
  type 'a self = 'a parser

  let expression_desc str =
    let lexbuf = Lexing.from_string str in
    let expr = Parse.expression lexbuf in
    expr.pexp_desc

  open Parsetree

  let int str =
    match expression_desc str with
    | Pexp_constant (Pconst_integer (val_str, _)) ->
      Some (Pervasives.int_of_string val_str)
    | _ -> None

  let float str =
    match expression_desc str with
    | Pexp_constant (Pconst_float (val_str, _)) ->
      Some (Pervasives.float_of_string val_str)
    | _ -> None

  let char str =
    match expression_desc str with
    | Pexp_constant (Pconst_char c) ->
      Some c
    | _ -> None

  let bool str =
    match expression_desc str with
    | Pexp_construct ({txt = Lident "true"}, None) ->
      Some true
    | Pexp_construct ({txt = Lident "false"}, None) ->
      Some false
    | _ -> None
end


module Exception = struct
  type self = exn

  include Printable.Make(struct
      type nonrec self = self

      let pp formatter self =
        Format.fprintf formatter "%s" (Printexc.to_string self)
    end)
end


(* Unit type and operations *)

module Unit = struct
  type self = unit

  let try_of_string = function "()" -> Some () | _ -> None

  (* Bounded *)

  let min_value = ()

  let max_value = ()

  (* Comparable *)
  include Comparable.Make(struct
      type nonrec self = self
      let compare _ _ = Equal
    end)

  (* Default *)
  let default = ()

  (* Equatable *)
  include Equatable.Make(struct
      type nonrec self = self
      let equal : self -> self -> bool = Pervasives.(=)
    end)

  (* Hashable *)
  let hash = Hashtbl.hash

  (* Parsable *)
  let parse = function
  | "()" -> Some ()
  | _ -> None

  (* Printable *)
  include Printable.Make(struct
      type nonrec self = self
      let pp fmt () = Format.fprintf fmt "()"
    end)
end


(* Boolean type and operations *)

module Bool = struct
  type self = bool

  let not = P.not

  let ( && ) = P.( && )
  let ( || ) = P.( || )

  let to_int = function false -> 0 | true -> 1
  let of_int = function 0 -> false | _ -> true

  let to_option default self =
    match self with
    | true -> Some default
    | false -> None

  (* Bounded *)
  let min_value = false
  let max_value = true

  (* Comparable *)
  include Comparable.Make(struct
      type nonrec self = self

      let compare a b =
        let legacy_cmp : self -> self -> int = Pervasives.compare in
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
      type nonrec self = self

      let equal : self -> self -> bool = P.(=)
    end)

  (* Hashable *)
  let hash = Hashtbl.hash

  (* Parsable *)
  let parse str =
    try
      Some (P.bool_of_string str)
    with _ ->
      None

  (* Printable *)
  include Printable.Make(struct
      type nonrec self = self
      let pp = Format.pp_print_bool
    end)
end

let not    = Bool.not
let ( && ) = Bool.( && )
let ( || ) = Bool.( || )


(* Integer type and arithmetic operations *)

module Int = struct
  type self = int

  let ( / ) = P.( / )

  let ( mod ) = P.( mod)

  let to_string self =
    P.string_of_int self

  let of_char = P.int_of_char

  let of_float = P.int_of_float

  module Unsafe = struct
    let of_string = P.int_of_string
  end

  (* Bounded *)
  let min_value = P.min_int
  let max_value = P.max_int

  (* Comparable *)
  include Comparable.Make(struct
      type nonrec self = self

      let compare a b =
        let legacy_cmp : self -> self -> int = Pervasives.compare in
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
      type nonrec self = self
      let equal : self -> self -> bool = P.(=)
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

  (* Parsable *)
  let parse s =
    try
      Some (Unsafe.of_string s)
    with _ ->
      None

  (* Printable *)
  include Printable.Make(struct
      type nonrec self = self
      let pp = Format.pp_print_int
    end)
end

let (~- )   = Int.(~- )
let (~+ )   = Int.(~+ )
let ( + )   = Int.( + )
let ( - )   = Int.( - )
let ( * )   = Int.( * )
let ( / )   = Int.( / )
let ( mod ) = Int.( mod )
let abs     = Int.abs


module Float = struct

  type self = float

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

  module Unsafe = struct
    let of_string = P.float_of_string
  end

  (* Bounded *)
  let min_value = P.min_float
  let max_value = P.max_float

  (* Comparable *)
  include Comparable.Make(struct
      type nonrec self = self

      let compare a b =
        let legacy_cmp : self -> self -> int = Pervasives.compare in
        let order = legacy_cmp a b in
        if order < 0 then Less
        else if order > 0 then Greater
        else Equal
    end)

  (* Default *)
  let default = 0.0

  (* Equatable *)
  include Equatable.Make(struct
      type nonrec self = self
      let equal : self -> self -> bool = P.(=)
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

  (* Parsable *)
  let parse str =
    try
      Some (float_of_string str)
    with _ ->
      None

  let try_of_string = parse

  (* Printable *)
  include Printable.Make(struct
      type nonrec self = self
      let pp = Format.pp_print_float
    end)

end


(* Char type and operations *)

module Char = struct
  type self = char

  let try_of_int x =
    try Some (Pervasives.char_of_int x)
    with Invalid_argument _ -> None

  let to_int self = P.int_of_char self

  module Unsafe = struct
    let of_int = P.char_of_int
  end

  (* Bounded *)
  let min_value = Unsafe.of_int 0x00
  let max_value = Unsafe.of_int 0xFF

  (* Parsable *)
  let parse s =
    if String.length s = 1 then
      Some (String.get s 0)
    else
    None

  (* Comparable *)
  include Comparable.Make(struct
      type nonrec self = self
      let compare a b =
        let legacy_cmp : self -> self -> int = Pervasives.compare in
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
      type nonrec self = self
      let pp = Format.pp_print_char
    end)
end


let char = Char.try_of_int
let code = Char.to_int


module Pair = struct
  type ('a, 'b) self = 'a * 'b
    [@@deriving show]
    [@@public]

  let first  = Pervasives.fst
  let second = Pervasives.snd

  include Comparable2.Make(struct
      type nonrec ('a, 'b) self = ('a, 'b) self

      let compare cmp_a cmp_b (self_a, self_b) (other_a, other_b) =
        match cmp_a self_a other_a with
        | Equal -> cmp_b self_b other_b
        | other -> other
    end)

  let hash = Hasher.pair

  let parse str =
    undefined ()

  include Printable2.Make(struct
      type nonrec ('a, 'b) self = ('a, 'b) self

      let pp pp_a pp_b formatter (a, b) =
        Format.pp_print_string formatter "(";
        (pp_a formatter) a;
        Format.pp_print_string formatter ",";
        (pp_b formatter) b;
        Format.pp_print_string formatter ")"
    end)
end

type ('a, 'b) pair = ('a, 'b) Pair.self

let first  = Pair.first
let second = Pair.second

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


type nonrec 'a option = 'a option

let some x = Some x
let none   = None

let is_some = function Some _ -> true  | None -> false
let is_none = function Some _ -> false | None -> true

let option f default self =
  match self with
  | Some x -> f x
  | None   -> default


type ('a, 'e) result = ('a, 'e) Pervasives.result

let ok     x = Ok x
let error  e = Error e

let is_ok self =
  match self with
  | Ok _    -> true
  | Error _ -> false

let is_error self =
  match self with
  | Ok _    -> false
  | Error _ -> true

let result f default self =
  match self with
  | Ok x    -> f x
  | Error e -> default e



(* Either type *)
type ('a, 'b) either = Left of 'a | Right of 'b

let left  a = Left  a
let right b = Right b


let int = Int.of_float
let float = Float.of_int


let open_file ?(binary = true) path =
  let flags = [Open_rdonly] in
  let flags = if binary then Open_binary :: flags else flags in
  open_in_gen flags 0o000 path

let create_file
    ?(binary = true) ?(append = false)
    ?(exclusive = false) ?(perm = 00666) path =
  let flags = [Open_wronly; Open_creat] in
  let flags = (if binary then Open_binary else Open_text) :: flags in
  let flags = (if append then Open_append else Open_trunc) :: flags in
  let flags = (if exclusive then Open_excl :: flags else flags) in
  open_out_gen flags perm path


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


