(*---------------------------------------------------------------------------
   Copyright (c) 2017 Rizo Isrof. All rights reserved.
   Distributed under the ISC license, see terms in the LICENSE file.
  ---------------------------------------------------------------------------*)

(** {e Ether} is small base library for OCaml designed from the ground up for
    consistent, safe and user-friendly development experience.

    {3:goals Goals}

    {ul
    {- {b Consistency and predictability} on all levels is enforced by
    following {{: #naming-conventions} naming} and structural conventions.}
    {- {b Simplicity} at the point of usage. Interfaces are defined once but
    used repeatedly. The exported API must be simple and understandable,
    optimized for the context in which it will be used.}
    {- {b Composability} of the components. The modules, types and interfaces
    are expected to be flexible and mixed in new ways.}
    {- {b Portability:} no external dependencies apart from the standard
    library. Stable language features are preferred.}}

    This documentation includes the implemented API reference and describes the
    design conventions adopted and recommended by the library.

    {3 Contents}

    {ol
    {- {{: #interfaces} Interfaces}}
    {- {{: #equality} Equality}}
    {- {{: #ordering} Ordering}}
    {- {{: #formatting_and_pretty_printing} Formatting and Pretty-printing}}
    {- {{: #hashing} Hashing}}
    {- {{: #data_types} Data Types}}
    {- {{: #control_abstractions} Control Abstractions}}
    {- {{: #generic_definitions} Generic Definitions}}
    {- {{: #comparison_to_pervasives} Comparison to Pervasives}}}

    {b Note:} Ether is not compatible with the standard library and should be
    seen as an experimental modern replacement for the {e Pervasives} module.
    The main differences between {e Pervasives} and {e Ether} are highlighted
    in the {{: #comparison_to_pervasives} Comparison to Pervasives} section. *)


(** {1:interfaces Interfaces}

    Interfaces, also known as {i signatures} or {i module types}, in OCaml can
    be used to express behaviours and properties shared by types. Instead of
    working with a particular type interfaces allow us to focus on the
    functionality that we expect a class of types to implement.

    This is a very powerful abstraction mechanism used pervasively in {e Ether}
    to make types more composable and understandable.

    {2:defining_an_interface Defining an Interface}

    As an example we will redefine a simplified version of the interface
    already included in {e Ether} called {!modtype:Comparable}. A type is
    comparable if its values form an order relation which can be {i "less
    than"}, {i "equal to"} or {! "greater than"} ({i i.e.} the {!type:order}
    type).

    Given a [compare] function that describes the ordering relation of two
    values of the same type, we can define useful derived operations as part of
    the [Comparable] interface for monomorphic types ({i i.e.} types without
    generic arguments):

{[
module type Comparable = sig
  type t

  (* Required operation. *)
  val compare : t -> t -> order

  (* Derived operations. *)
  val equal  : t -> t -> bool
  val ( == ) : t -> t -> bool
  val ( != ) : t -> t -> bool
  val ( <  ) : t -> t -> bool
  val ( >  ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( >= ) : t -> t -> bool
  val min    : t -> t -> t
  val max    : t -> t -> t
end
]}

    Comparable types only have to implement the required [compare] function, all
    derived operations will be included automatically. To make the required
    operation(s) more explicit we can copy it into a separate [Base] interface
    along with the [Make] functor that will implement the derived operations.

{[
module Comparable : sig
  (* The minimal required implementation. *)
  module type Base = sig
    type t

    val compare : t -> t -> order
  end

  (* Generates the derived operations given the [Base] module. *)
  module Make(B : Base) : (Comparable with type t := B.t)
end
]}

   This is an example of an interface with an extension. Other interfaces, like
   {!modtype:Hashable} or {!modtype:Default}, have plain definitions without
   [Base] and [Make].

   The actual implementation of this module is left as an exercise to the reader
   (or can be consulted in Ether's {{:#todo} source code}).

   {2:implementing_an_interface Implementing an Interface}

    In the following example we define the type and the module to represent the
    concept of a person. The [Person] module implements the [Comparable]
    interface to allow comparisons based on the age.

{[
type person = {
  name : string;
  age : int
}

module Person : sig
  type t = person

  val say_hello : t -> unit

  include Comparable with type t := t
end = struct
  type t = person

  let say_hello self =
    print "Hello, %s!" self.name

  include Comparable.Make(struct
      type nonrec t = t

      let compare self other =
        Int.compare self.age other.age
    end)
end

let alice = { name = "Alice"; age = 24 }
let bob   = { name = "Bob";   age = 30 }

(* Who is younger, Alice or Bob? *)
let () =
  let younger = Person.min alice bob in
  print "%s is younger!" younger;
  Person.say_hello younger
]}

    By supplying the [compare] function to the [Comparable.Make] functor
    all functions in the {!modtype:Comparable} interface were automatically
    added to the [Person] module. *)


(** {1:equality Equality}

    This section defines interfaces and operations for equality comparison
    between values. Equality is an {{:
    https://en.wikipedia.org/wiki/Equivalence_relation} equivalence relation},
    which means that it must be: {i reflexive}, {i symmetric} and {i
    transitive}.

    User-defined types can implement the {!Equatable}, {!Equatable1} or
    {!Equatable2} interfaces (according to the arity of the main type) to
    include a specialized equality comparison functions.

*)

type 'a equal = 'a -> 'a -> bool
(** The type for equality functions. *)

(** Nullary signature for equality comparisons which are equivalence relations.

    [Equatable.Base] is the minimal implementation of the [Equatable] instance.
    Some examples of nullary equatable types, {i i.e.}, types without
    parameters include: [int], [float], [char]. *)
module type Equatable = sig
  type t
  (** The type of the equatable monomorphic values, {i i.e.}, non-polymorphic
      nullary values, such as [int], [char], etc. *)

  val equal : t equal

  val ( == ) : t equal

  val ( != ) : t equal
end


(** Structure defining the [Base] implementation for nullary [Equatable]
    signature and a functor to build the extended signature. *)
module Equatable : sig
  module type Base = sig
    type t

    val equal : t equal
  end

  module Make(B : Base) : (Equatable with type t := B.t)
  (** Functor building an instance of {!Equatable} given a {!Equatable.Base}
      implementation. *)
end


(** Unary signature for equality comparisons which are equivalence relations.

    The extended version of [Equatable] for polymorphic types is not possible
    since the included infix functions are only useful with two arguments and
    [Equatable1] would require an extra [equal] argument for each type
    parameter.

    If desired, the generic infix functions can be used with the default
    {!equal} implementation. *)
module type Equatable1 = sig
  type 'a t

  val equal : 'a equal -> 'a t -> 'a t -> bool
end


(** Binary signature for equality comparisons which are equivalence relations. *)
module type Equatable2 = sig
  type ('a, 'b) t

  val equal :
    'a equal -> 'b equal ->
    ('a, 'b) t -> ('a, 'b) t -> bool
end


val is : 'a -> 'a -> bool
(** [is a b] tests for physical equality of [a] and [b].

    On mutable types such as references, arrays, byte sequences, records with
    mutable fields and objects with mutable instance variables, [is a b] is
    true if and only if physical modification of [a] also affects [b].

    On non-mutable types, the behavior of [is] is implementation-dependent;
    however, it is guaranteed that [is a b] implies [a == b].

    To check if two values are physically distinct use [not (is a b)]. *)


(** {1:ordering Ordering}

    This section defines types, interfaces and operations for values that form
    a total order relation.

    An order is a total order if it is (for all [a], [b] and [c]):

    - {i total} and {i antisymmetric}: exactly one of [a < b], [a == b] or [a >
    b] is true;
    - {i transitive}, [a < b] and [b < c] implies [a < c].
      The same must hold for [==] and [>].

    User-defined types can implement the {!Comparable}, {!Comparable1} or
    {!Comparable2} interfaces (based on to the arity of the main type) to
    included a specialized comparison functions. *)

(** Defines the relative ordering of two values. *)
type order =
  | Less
  | Equal
  | Greater

type 'a comparator = 'a -> 'a -> order
(** The type for comparison functions. *)

(** Interface for nullary types that form a total order.

    {!Comparable.Base} is the minimal implementation of the {!modtype:Comparable}
    instance. Some examples of nullary comparable types, i.e., types with one
    parameter, include: [int], [char], [string], {i etc}. *)
module type Comparable = sig
  type t

  include Equatable with type t := t

  val compare : t comparator
  val ( <  ) : t -> t -> bool
  val ( >  ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( >= ) : t -> t -> bool
  val min : t -> t -> t
  val max : t -> t -> t
end


(** Structure defining the [Base] implementation for nullary [Comparable]
    signature and a functor to build the extended signature. *)
module Comparable : sig
  module type Base = sig
    type t

    val compare : t comparator
  end

  module Make(B : Base) : (Comparable with type t := B.t)
  (** Functor building an instance of {!Comparable} given a {!Comparable.Base}
      implementation. *)
end


(** Unary comparable signature for types that form a total order.

    The infix functions for the polymorphic versions of [Comparable] are not
    included since it would requires extra [compare] arguments for each type
    parameter and the infix functions are only useful with two arguments.

    If desired, the generic infix functions can be used with the default
    {!compare} implementation. *)
module type Comparable1 = sig
  type 'a t

  val compare : 'a comparator -> 'a t -> 'a t -> order

  val min : 'a comparator -> 'a t -> 'a t -> 'a t

  val max : 'a comparator -> 'a t -> 'a t -> 'a t
end


(** Structure defining the [Base] implementation for unary [Comparable1]
    signature and a functor to build the extended signature. *)
module Comparable1 : sig
  module type Base = sig
    type 'a t

    val compare : 'a comparator -> 'a t -> 'a t -> order
  end

  module Make(B : Base) : (Comparable1 with type 'a t := 'a B.t)
  (** Functor building an instance of {!Comparable1} given a
      {!Comparable1.Base} implementation. *)
end


(** Binary comparable signature for types that form a total order. *)
module type Comparable2 = sig
  type ('a, 'b) t

  val compare :
    'a comparator -> 'b comparator ->
    ('a, 'b) t -> ('a, 'b) t -> order

  val min :
    'a comparator -> 'b comparator ->
    ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t

  val max :
    'a comparator -> 'b comparator ->
    ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
end


(** Structure defining the [Base] implementation for the binary [Comparable2]
    signature and a functor to build the extended signature. *)
module Comparable2 : sig
  module type Base = sig
    type ('a, 'b) t

    val compare :
      'a comparator -> 'b comparator ->
      ('a, 'b) t -> ('a, 'b) t -> order
  end

  module Make(B : Base) : (Comparable2 with type ('a, 'b) t := ('a, 'b) B.t)
  (** Functor building an instance of {!Comparable2} given a
      {!Comparable2.Base} implementation. *)
end


(** {2:formatting_and_pretty_printing Formatting and Pretty-printing}

    This section contains the primitives for formatting and pretty-printing. *)

type 'a printer = Format.formatter -> 'a -> unit
(** The type for pretty-printers of values of type ['a]. *)

val print : ?channel:out_channel -> ?break:string ->
  ('a, Format.formatter, unit) format -> 'a
(** [print ?output ?break fmt] prints the text formatted accordingly to [fmt]
    on the [output] channel followed by [break].

    {3 Parameters}

    @param channel The output channel to write on (default = [stdout]).
    @param break String to be appended at the end (default = ["\n"]).

    {3 Examples}
{[
print "hello";
print "hello, world" ~break:"!\n";
print "hello, %s!" "world";
print ~channel:stderr "hello";
]} *)

val format : ('a, Format.formatter, unit, string) format4 -> 'a
(** [format fmt arg1 ... argN] formats the arguments [arg1] to [arnN]
    according to the format string [fmt] and returns the result as a string.

    The format string is a character string which contains two types of
    objects: plain characters, which are simply copied to the output channel,
    and conversion specifications, each of which causes conversion and printing
    of arguments.

{[
assert (format "x = %d" 42 = "x = 42");
assert (format "Hello, %s!" world = "Hello, world!");
]} *)


(** Interface for the monomorphic types that can be printed.

    [Printable.Base] is the minimal implementation of the [Printable] instance.
    Some examples of nullary printable types, i.e., types without parameters
    include: [Int.t], [Float.t], [Char.t], [Unit.t], etc. *)
module type Printable = sig
  type t
  (** The type of the printable monomorphic values, i.e., non-polymorphic
      nullary values, such as [int], [string], etc. *)

  val printer : t printer
  (** [printer] is the pretty-printer for the type {!t}. *)

  val to_string : t -> string
  (** [to_string self] converts [self] to a string representation using the
      {!printer} pretty-printer. *)

  val print : t -> unit
  (** [print self] writes [self] to the standard output using the {!printer}
      pretty-printer. *)
end


(** Structure defining the [Base] implementation for nullary [Printable]
    signature and a functor to build the extended signature. *)
module Printable : sig
  module type Base = sig
    type t
    (** The type of the printable monomorphic values, i.e., non-polymorphic
        nullary values, such as [int], [string], etc. *)

    val printer : t printer
    (** [printer] is the pretty-printer for the type {!t}. *)
  end

  module Make(B : Base) : (Printable with type t := B.t)
  (** Functor building an instance of {!Printable} given a {!Printable.Base}
      implementation. *)
end


(** Interface for the unary types that can be printed.

    [Printable1.Base] is the minimal implementation of the [Printable]
    instance. Some examples of unary printable types, {i i.e.}, types with one
    parameter, include: {!option}, {!list}, {!array}. *)
module type Printable1 = sig
  type 'a t
  (** The type of the printable polymorphic values of arity one, i.e., values
      with one type parameter, such as ['a list], ['a option], etc. *)

  val printer : 'a printer -> 'a t printer
  (** [printer printer_a] is the pretty-printer for the unary type ['a t].
      [printer_a] is the pretty-printer for the wrapped type ['a]. *)

  val to_string : 'a printer -> 'a t -> string
  (** [to_string printer_a self] converts unary {!'a t} to a string representation
      using {!printer_a} for the wrapped type. *)

  val print : 'a printer -> 'a t -> unit
  (** [print printer_a self] writes [self] to the standard output using
      [printer_a] pretty-printer for the wrapped type and {!Printable1.printer}
      for [self].
  *)
end


(** Structure defining the [Base] implementation for unary [Printable1]
    signature and a functor to build the extended signature. *)
module Printable1 : sig
  module type Base = sig
    type 'a t
    (** The type of the printable polymorphic values of arity one, i.e., values
        with one type parameter, such as ['a list], ['a option], etc. *)

    val printer : 'a printer -> Format.formatter -> 'a t -> unit
    (** [printer printer_a] is the pretty-printer for the type {!'a t}. *)
  end

  module Make(B : Base) : (Printable1 with type 'a t := 'a B.t)
  (** Functor building an instance of {!Printable1} given a {!Printable1.Base}
      implementation. *)
end


(** Interface for the binary types that can be printed.

    [Printable2.Base] is the minimal implementation of the [Printable2]
    instances. Some examples of binary printable types, i.e., types with two
    parameters, include: {!('a, 'e) Result.t} and {!('a, 'b) Either.t}. *)
module type Printable2 = sig
  type ('a, 'b) t
  (** The type of the printable polymorphic values of arity two, i.e., values
      with two type parameters, such as [('a, 'e) Result.t] and [('a, 'b)
      Either.t]. *)

  val printer : 'a printer -> 'b printer -> ('a, 'b) t printer
  (** [printer printer_a printer_b] is the pretty-printer for the binary type
      [('a, 'b) t]. [printer_a] and [printer_b] are the pretty-printers for
      the wrapped types ['a] and ['b]. *)

  val to_string : 'a printer -> 'b printer -> ('a, 'b) t -> string
  (** [to_string pp_a pp_b self] converts binary [('a, 'b) t] to a string
      representation using [pp_a] and [pp_b] for the wrapped types ['a] and
      ['b]. *)

  val print : 'a printer -> 'b printer -> ('a, 'b) t -> unit
  (** [print pp_a pp_b self] writes [self] to the standard output using [pp_a]
      and [pp_b] pretty-printers for the wrapped types and {!Printable1.pp} for
      [self]. *)
end


(** Structure defining the [Base] implementation for binary [Printable2]
    signature and a functor to build the extended signature. *)
module Printable2 : sig
  module type Base = sig
    type ('a, 'b) t
    (** The type of the printable polymorphic values of arity two, i.e., values
        with two type parameters, such as [('a, 'e) Result.t] and [('a, 'b)
        Either.t]. *)

    val printer : 'a printer -> 'b printer -> ('a, 'b) t printer
    (** [printer printer_a printer_b] is the pretty-printer for the binary type
        [('a, 'b) t]. [printer_a] and [printer_b] are the pretty-printers for
        the wrapped types ['a] and ['b]. *)
  end

  module Make(B : Base) : (Printable2 with type ('a, 'b) t := ('a, 'b) B.t)
  (** Functor building an instance of {!Printable2} given a {!Printable2.Base}
      implementation. *)
end


(** {1:hashing Hashing}

    Values which compare equal should have the same hash value. If a type does
    not implement the [Comparable] interface it should not implement [Hashable]
    either.

    Compound types (like records) should mix the hash values of the components
    that participate in comparison to generate their hash. For example:

{[
type person =
  { name : string; age : int }

module Person = struct
  type t = person

  let hash self =
    Pair.hash String.hash Int.hash (self.name, self.age)
end
]}

    The [Hasher] module includes helper combinators to generate hashes for
    compound types. *)

type 'a hasher = 'a -> int
(** The type of hashing functions, {i i.e.}, functions that given a value of
    type ['a] generate an integer representing a hash. *)

module Hasher : sig
  type 'a t = 'a hasher

  val int : int hasher
  val char : char hasher
  val string : string hasher
  val pair : 'a hasher -> 'b hasher -> ('a * 'b) hasher
  val list : 'a hasher -> 'a list hasher
end

(** Interface for nullary hashable types. *)
module type Hashable = sig
  type t

  val hash : t hasher
end


(** Interface for unary hashable types. *)
module type Hashable1 = sig
  type 'a t

  val hash : 'a hasher -> 'a t hasher
end


(** Interface for binary hashable types. *)
module type Hashable2 = sig
  type ('a, 'b) t

  val hash : 'a hasher -> 'b hasher -> ('a, 'b) t hasher
end


(** {2:other_interfaces Other Interfaces} *)

(* Bounded is used to name the upper and lower limits of a type. *)
module type Bounded = sig
  type t

  val min_value : t
  val max_value : t
end


(** Interface for nullary types with a default value. *)
module type Default = sig
  type t

  val default : t
end


(** Interface for unary types with a default value. *)
module type Default1 = sig
  type 'a t

  val default : 'a t
end


(** Enumerable defines operations on sequentially ordered types. *)
module type Enumerable = sig
  type t

  val predecessor : t -> t
  (** [pred self] is the predecessor of [self].

      @raise No_value if [self] has no predecessor. *)

  val successor : t -> t
  (** [succ self] is the successor of [self].

      @raise No_value if [self] has no successor. *)
end


(** Basic numeric interface. *)
module type Numeric = sig
  type t

  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val (~- ) : t -> t
  val (~+ ) : t -> t
  val abs : t -> t
  val signum : t -> t
end


(** {1:exn Exceptions} *)

(** Module definition for the exception type. *)
module Exception : sig
  type t = exn

  val raise : ?trace: bool -> exn -> 'a
  (** [raise ?trace exn] raises the exception [exn]. If [trace] is [false] no
      backtrace will be recorded, resulting in faster execution.

{[
let ensure_negative x =
  if x < 0 then
    raise ~trace:false (Invalid_argument "negative value")
  else x
]} *)

  val raises : ?only: exn -> (unit -> 'a) -> bool
  (** [raises ?only:exn f] is [true] if the exception [exn] is raised while calling
      [f], [false] otherwise. If no [exn] is given, will return [true] on any
      exception.

{[
assert (raises (fun () -> fail "yes"));
assert (raises (fun () -> Option.force None) ~only:No_value);
assert (not (raises (fun () -> "no")))
]} *)

  val fail : string -> 'a
  (** [fail msg] raises the [Failure] exception with given [msg]. Should be used
      for generic failures with a simple message.

{[
if List.length a = 0 then
  fail "empty list"
else
  print "non-empty"
]} *)

  val bracket : (unit -> 'a) -> ('a -> unit) -> ('a -> 'b) -> 'b
  (** Provides a safe way to acquire, use and release a resource.

      [bracket make free f] is [f (make ())] where the value produced by [make]
      is released with [free]. The resource will be closed even if [f] raises an
      exception.

{[
bracket
  (open_file "data.csv") close_input
  (fun input -> ...)
]} *)

  include Printable with type t := t
end

val raise : ?trace: bool -> exn -> 'a
(** [raise ?trace exn] raises the exception [exn]. If [trace] is [false] no
    backtrace will be recorded, resulting in faster execution.

{[
let ensure_negative x =
  if x < 0 then
    raise ~trace:false (Invalid_argument "negative value")
  else x
]} *)

val raises : ?only: exn -> (unit -> 'a) -> bool
(** [raises ?only:exn f] is [true] if the exception [exn] is raised while calling
    [f], [false] otherwise. If no [exn] is given, will return [true] on any
    exception.

{[
assert (raises (fun () -> fail "yes"));
assert (raises (fun () -> Option.force None) ~only:No_value);
assert (not (raises (fun () -> "no")))
]} *)

val fail : string -> 'a
(** [fail msg] raises the [Failure] exception with given [msg]. Should be used
    for generic failures with a simple message.

{[
if List.length a = 0 then
  fail "empty list"
else
  print "non-empty"
]} *)

val bracket : (unit -> 'a) -> ('a -> unit) -> ('a -> 'b) -> 'b
(** Provides a safe way to acquire, use and release a resource.

    [bracket make free f] is [f (make ())] where the value produced by [make]
    is released with [free]. The resource will be closed even if [f] raises an
    exception.

{[
bracket
  (open_file "data.csv") close_input
  (fun input -> ...)
]} *)


exception Undefined
(** An exception for undefined or not implemented code branches. *)

val undefined : unit -> 'a
(** [undefined ()] raises the [Undefined] exception.

{[
let x = 42 in
match x with
| x when x = 0 -> " "
| x when x > 0 -> "+"
| x when x < 0 -> "-"
| otherwise    -> undefined ()
]} *)


(** {1:data_types Data Types}

    Ether includes the following primitives data types: {{: #module-Unit}
    unit}, {{: #module-Bool} bool}, {{: #module-Int} int}, {{: #module-Float}
    float} and {{: #module-Char} char}. All types provide module definitions
    with common interfaces implemented.

    {b Note:} Implementation of other data types such as {{: #TODO} strings},
    {{: #TODO} arrays}, {{: #TODO} optionals} and {{: #TODO} lists} are
    distributed as separate packages.

    {2:defining_data_types Defining Data Types}

    When creating a new data type it is recommended to have a module definition
    for it that includes the implementation of common interfaces.

    Global type names should be used ({i i.e.} types outside modules) unless
    the module is used as a namespace. For example, [request] is preferred to
    [Request.t], but [Request.status] is acceptable. In this case the global
    [request] type should also be aliased as [Request.t]. This convention
    makes the usage of types more consistent (avoids mixing module-defined
    types like [Some_type.t] with simple types [some_type]) and allows direct
    access to the field and variant members of the frequently used types.

    {b Note:} Globally defined types can pollute the global namespace with
    conflicting fields/variants. If this happens the types should be moved into
    modules containing accessor functions for record fields and constructor
    functions for vairants. *)


(** Unit type and operations. *)
module Unit : sig
  type t = unit

  val try_of_string : string -> t option
  (** [try_of_string str] is a unit value converted from the string [str] or
      [None] if the string does not represent a valid unit. *)

  (** {2 Implemented instances} *)
  include Bounded    with type t := t
  include Comparable with type t := t
  include Default    with type t := t
  include Hashable   with type t := t
  include Printable  with type t := t
end


(** Boolean type and operations. *)
module Bool : sig
  type t = bool
  (** Standard boolean type. *)

  val not : t -> t
  (** [not a] is the boolean negation of [a]. *)

  val ( && ) : t -> t -> t
  (** [a && b] is the boolean 'and' of [a] and [b]. Evaluation is sequential,
      left-to-right, [a] is evaluated first, and if it returns [false], [b] is
      not evaluated at all. *)

  val ( || ) : t -> t -> t
  (** [a || b] is the boolean 'or' of [a] and [b]. Evaluation is sequential,
      left-to-right, [a] is evaluated first, and if it returns [true], [b] is
      not evaluated at all. *)

  val of_int : int -> t
  val to_int : t -> int

  val try_of_string : string -> t option
  (** [try_of_string str] is a boolean value converted from the string [str] or
      [None] if the string does not represent a valid boolean. *)

  val to_option : 'a -> t -> 'a option
  (** [to_option x self] is an optional value containing [x] if [self] is
      [true] and [None] otherwise.

{[
assert (Bool.to_option 42 false == None);
assert (Bool.to_option 42 true  == Some 42);
]} *)

  (** {6 Implemented instances} *)
  include Bounded    with type t := t
  include Comparable with type t := t
  include Default    with type t := t
  include Enumerable with type t := t
  include Equatable  with type t := t
  include Hashable   with type t := t
  include Printable  with type t := t
end

val not : bool -> bool
(** [not a] is the boolean negation of [a]. *)

val ( && ) : bool -> bool -> bool
(** [a && b] is the boolean 'and' of [a] and [b]. Evaluation is sequential,
    left-to-right, [a] is evaluated first, and if it returns [false], [b] is
    not evaluated at all. *)

val ( || ) : bool -> bool -> bool
(** [a || b] is the boolean 'or' of [a] and [b]. Evaluation is sequential,
    left-to-right, [a] is evaluated first, and if it returns [true], [b] is
    not evaluated at all. *)


(** Integer type and arithmetic operations.

    Integer numbers with 31 bits on 32-bit processors or 63 bits on 64-bit
    processors. *)
module Int : sig
  type t = int
  (** Standard integer type. *)

  val ( / ) : t -> t -> t
  (* Integer devision. *)

  val ( mod ) : t -> t -> t
  (** Integer reminder.

      If [y] is not zero, the result of [x mod y] satisfies the following
      properties: [x = (x / y) * y + x mod y] and [abs(x mod y) <= abs(y) - 1].
      If [y = 0], [x mod y] raises [Division_by_zero].

      {b Note:} [x mod y] is negative only if [x < 0].
      @raise Division_by_zero if [y] is zero. *)

  (** {6:conv Conversion } *)

  val to_string : t -> string
  (** [to_string x] is a string representation of the integer [x]. *)

  val of_float : float -> t
  (** [of_float x] is an integer representation of the float value [x]. *)

  val of_char : char -> t
  (** [of_char x] is an integer representation of the char value [x].

      See [Char.try_of_int] *)

  val try_of_string : string -> t option
  (** [try_of_string str] is a int value converted from the string [str] or
      [None] if the string does not represent a valid int. *)

  module Unsafe : sig
    val of_string : string -> t
    (** [of_string str] is an integer created by parsing the string [str].

        @raise Failure if the string does not represent a valid integer. *)
  end

  (** {6 Implemented instances} *)
  include Bounded    with type t := t
  include Comparable with type t := t
  include Default    with type t := t
  include Enumerable with type t := t
  include Equatable  with type t := t
  include Hashable   with type t := t
  include Numeric    with type t := t
  include Printable  with type t := t
end

val int : float -> int
(** [int x] is an integer representation of the float value [x]. *)

val ( / ) : int -> int -> int
(* Integer devision. *)

val ( mod ) : int -> int -> int
(** Integer reminder.

    If [y] is not zero, the result of [x mod y] satisfies the following
    properties: [x = (x / y) * y + x mod y] and [abs(x mod y) <= abs(y) - 1].
    If [y = 0], [x mod y] raises [Division_by_zero].

    {b Note:} [x mod y] is negative only if [x < 0].
    @raise Division_by_zero if [y] is zero. *)

include Numeric with type t := int
include Comparable with type t := int


(** Float type and arithmetic operations.

    Floating-point numbers according to IEEE 754, using double precision 64
    bits numbers.

    When working with floating-point values. *)
module Float : sig
  type t = float

  val ( / ) : t -> t -> t

  val ( mod ) : t -> t -> t

  val infinity : t
  (** Positive infinity. *)

  val neg_infinity : t
  (** Negative infinity. *)

  val nan : t
  (** A special floating-point value denoting the result of an undefined
      operation such as [0.0 /. 0.0]. Stands for 'not a number'. Any
      floating-point operation with [nan] as argument returns [nan] as result.
      As for floating-point comparisons, [=], [<], [<=], [>] and [>=] return
      [false] and [<>] returns [true] if one or both of their arguments is [nan]. *)

  val epsilon : t
  (** The difference between [1.0] and the smallest exactly representable
      floating-point number greater than [1.0]. *)

  val round : t -> t
  (** [round self] rounds the float value to the nearest integer float. *)

  val exp : t -> t
  (** Exponential. *)

  val frexp : t -> t * int
  (** [frexp self] returns the pair of the significant and the exponent of
      [self]. When [self] is zero, the significant [x] and the exponent [n] of
      [self] are equal to zero.  When [self] is non-zero, they are defined by
      [self = x *. 2 ** n] and [0.5 <= x < 1.0]. *)

  val ldexp : t -> int -> t
  (** [ldexp self n] returns [self *. 2 ** n]. *)

  val modf : t -> t * t
  (** [modf self] returns the pair of the fractional and integral part of [self]. *)

  type fpclass = Pervasives.fpclass
  (** The five classes of floating-point numbers, as determined by
      the {!classify} function. *)

  val classify : float -> fpclass
  (** Return the class of the given floating-point number:
      normal, subnormal, zero, infinite, or not a number. *)

  (** {2:conv Conversion } *)

  val to_int : t -> int
  (** Truncate the given floating-point number to an integer. The result is
      unspecified if the argument is [nan] or falls outside the range of
      representable integers. *)

  val of_int : int -> t
  (** Converts an integer to floating-point. *)

  val to_string : t -> string

  val try_of_string : string -> t option
  (** [try_of_string str] is a float value converted from the string [str] or
      [None] if the string does not represent a valid float. *)

  module Unsafe : sig
    val of_string : string -> t
    (** [of_string str] is a float created by parsing the string [str].

        @raise Failure if the string does not represent a valid integer. *)
  end

  (** {6 Implemented instances} *)
  include Bounded    with type t := t
  include Comparable with type t := t
  include Default    with type t := t
  include Equatable  with type t := t
  include Hashable   with type t := t
  include Numeric    with type t := t
  include Printable  with type t := t
end

val float : int -> Float.t
(** Converts an integer to floating-point. *)


(** Char type and operations.

    Character values represented as bytes. *)
module Char : sig
  (** Char type and operations. *)

  type t = char
  (** The type for bytes. *)

  (** {2:conversions Conversions} *)

  val try_of_int : int -> t option
  (** [try_of_int b] is a byte from [b]. [None] is returned if [b] is not in
      the range \[[0x00];[0xFF]\]. *)

  val to_int : t -> int
  (** [to_int self] is the byte [self] as an integer. *)

  val try_of_string : string -> t option
  (** [try_of_string str] is a char value converted from the string [str] or
      [None] if the string does not represent a valid char. *)

  module Unsafe : sig
    val of_int : int -> t
    (** [of_int b] is a byte from [b].

        {e See also:} [Char.of_int]
        @raise Failure if the byte [b] does not represent a valid char. *)
  end


  (** {2 Implemented instances} *)

  include Bounded    with type t := t
  include Comparable with type t := t
  include Default    with type t := t
  include Enumerable with type t := t
  include Equatable  with type t := t
  include Hashable   with type t := t
  include Printable  with type t := t
end

val char : int -> char option
(** Public alias for {!Char.try_of_int}. *)

val code : char -> int
(** Public alias for {!Char.to_int}. *)


(** {2:string_operations String Operations} *)

val (++) : string -> string -> string
(** [str1 ++ str2] a string obtained by concatenating [str1] and [str2].

{[
assert ("hello" ++ " " ++ "world" == "hello world");
assert ("a" ++ "" == "a");
assert ("" ++ "a" == "a");
]} *)


(** Bitwise integer operations

    This module defines prefix and infix operators and should be opened
    locally. *)
module Bitwise : sig
  val (~~~) : int -> int
  (** Bitwise logical negation. *)

  val (|||)  : int -> int -> int
  (** Bitwise logical or. *)

  val (&&&)  : int -> int -> int
  (** Bitwise logical and. *)

  val (^^^)  : int -> int -> int
  (** Bitwise logical exclusive or. *)

  val (<<<)  : int -> int -> int
  (** [n <<< m] shifts n to the left by m bits. The result is unspecified if m
      < 0 or m >= bitsize, where bitsize is 32 on a 32-bit platform and 64 on a
      64-bit platform. *)

  val (>>>) : int -> int -> int
  (** [n >>> m] shifts n to the right by m bits. This is an arithmetic shift:
      the sign bit of n is replicated. The result is unspecified if m < 0 or m
      >= bitsize. *)

  val (>>>!) : int -> int -> int
  (** [n >>>! m] shifts n to the right by m bits. This is a logical shift:
      zeroes are inserted regardless of the sign of n. The result is
      unspecified if m < 0 or m >= bitsize. *)
end


(** {2:fun Function Type and Operations} *)

val ( |> ) : 'a -> ('a -> 'b) -> 'b
(** [x |> f] reads as "apply forward" or "pipe" and similarly to [<|] is
    equivalent to [f x]. Can be used to create long chains of forward
    transformation in the style of Unix pipes:

{[
x |> f |> g |> h <=> h (g (f x))
]} *)

val ( @ ) : ('a -> 'b) -> 'a -> 'b
(** [f @ x] reads as "apply" and is equivalent to [f x]. Can be used to
    create long chains of transformation and since [@] has low,
    right-associative precedence, it sometimes allows parentheses to be
    omitted.
{[
f @ g @ h @ x <=> f (g (h x))
]} *)

val identity : 'a -> 'a
(** [identity a] always returns [a]. This is called the identity function. *)

val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
(** [flip f x y] is [f y x], i.e., the application of [f] with its first two
    arguments reversed.*)

val constantly : 'a -> 'b -> 'a
(** [constantly x y] is a function that produces [x] for any input value [y]. *)

val curry : (('a * 'b) -> 'c) -> 'a -> 'b -> 'c
(* [curry f] converts an uncurried function [f] to a curried function. *)

val uncurry : ('a -> 'b -> 'c) -> ('a * 'b) -> 'c
(* [uncurry f] converts a curried function [f] to a function on pairs. *)

val compose : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
(** [compose f g] is the mathematical composition of functions [f] and [g].

    [( << )] and [( >> )] are the infix versions of [compose].

{[
let compose f g =
  fun x -> f (g x)
]} *)

val ( << )  : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
(** [f << g] is infix version of [compose f g]. *)

val ( >> )  : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(** [g >> f] is infix version of [compose f g]. *)


(** {2:tuples Tuple types and Operations} *)

(** A tuple with two values. *)
module Pair : sig
  type ('a, 'b) t = 'a * 'b
  (** The type of tuples with two values. *)

  val first : ('a, 'b) t -> 'a
  (** [first self] is the first elements of [self].

{[
assert (first (1, "x") = 1)
]} *)

  val second : ('a, 'b) t -> 'b
  (** [second self] is the second elements of [self].

{[
assert (second (1, "x") = "x")
]} *)

  include Comparable2 with type ('a, 'b) t := ('a, 'b) t
  include Hashable2   with type ('a, 'b) t := ('a, 'b) t
  include Printable2  with type ('a, 'b) t := ('a, 'b) t
end

module Tuple2 = Pair
(** Alias for [Pair]. *)

type ('a, 'b) pair = ('a, 'b) Pair.t
(** The type of tuples with two values. *)

val first : ('a, 'b) pair -> 'a
(** [first self] is the first elements of [self].

{[
assert (first (1, "x") = 1)
]} *)

val second : ('a, 'b) pair -> 'b
(** [second self] is the second elements of [self].

{[
assert (second (1, "x") = "x")
]} *)


(** {1:generic_definitions Generic Definitions}

    Polymorphic functions for structural equality and comparison are included
    in the {!Generic} module.

{[
open Ether.Generic

let () =
  assert ('A' < 'Z');
  assert (max "abc" "xyz" == "xyz")
]}

    {b Warning:} The polymorphic functions in {!Generic} are provided for
    convenience and should not be used in performance-sensitive code. For
    example, instead of using {!val:Generic.min} prefer the monomorphic
    {!val:Int.min} if you are workign with integer values.

    The data types defined in {e Ether} do {e not} rely on polymorphic functions.

    Custom data types can implement their own specialized {{: #equality}
    equality} and {{: #ordering} ordering} intefaces. *)

module Generic : sig

  (** {1:generic_equality Generic Equality} *)

  val ( == ) : 'a -> 'a -> bool
  (** [a == b] tests for structural equality of [a] and [b]. Mutable
      structures ({i e.g.} references and arrays) are equal if and only if
      their current contents are structurally equal, even if the two mutable
      objects are not the same physical object (as tested with {!val:is}).

      @raise Invalid_argument if function values are compared for equality.

      {b Warning:} Equality between cyclic data structures may not terminate.

      {b See also: } {{: #val-equal} [equal]}, {{: #val-is} [is]} *)

  val ( != ) : 'a -> 'a -> bool
  (** [a != b] is [not (a == b)], {i i.e.}, the negation of {!(==)}. *)

  val equal : 'a -> 'a -> bool
  (** [equal a b] is equivalent to [a == b]. *)


  (** {1:generic_ordering Generic Ordering} *)

  val compare : 'a -> 'a -> order
  (** [compare a b] returns [0] if [a] is equal to [b], [-1] if [a] is less than
      [b], and [1] if [a] is greater than [b].

      The ordering implemented by [compare] is compatible with the comparison
      predicates [==], [<] and [>] defined above, with one difference on the
      treatment of the float value {!nan}. Namely, the comparison predicates
      treat [nan] as different from any other float value, including itself;
      while [compare] treats [nan] as equal to itself and less than any other
      float value. This treatment of [nan] ensures that [compare] defines a
      total ordering relation.

      @raise Invalid_argument if function values are compared for equality.

      {b Warning:} Equality between cyclic data structures may not terminate. *)

  val ( <  ) : 'a -> 'a -> bool
  val ( >  ) : 'a -> 'a -> bool
  val ( <= ) : 'a -> 'a -> bool
  val ( >= ) : 'a -> 'a -> bool
  (** Structural ordering functions.

      These functions coincide with the usual orderings over integers,
      characters, strings, byte sequences and floating-point numbers, and extend
      them to a total ordering over all types.  The ordering is compatible with
      {!(==)}. As in the case of {!(==)}, mutable structures are compared by
      contents.

      @raise Invalid_argument if function values are compared for equality.

      {b Warning:} Equality between cyclic data structures may not terminate. *)

  val min : 'a -> 'a -> 'a
  (** [min a b] returns the smaller of the two arguments.

      The result is unspecified if one of the arguments contains
      the float value [nan].

      @raise Invalid_argument if function values are compared for equality.

      {b Warning:} Equality between cyclic data structures may not terminate.

{[
assert (min 2 5 = 2);
assert (min [1; 2; 3] [2; 3; 4] = [1; 2; 3])
]} *)

  val max : 'a -> 'a -> 'a
  (** [max a b] returns the greater of the two arguments.

      The result is unspecified if one of the arguments contains
      the float value [nan].

      @raise Invalid_argument if function values are compared for equality.

      {b Warning:} Equality between cyclic data structures may not terminate.

{[
assert (max 2 5 = 5);
assert (max [1; 2; 3] [2; 3; 4] = [2; 3; 4])
]} *)

end

(** {1:comparison_to_pervasives Comparison to Pervasives}

    Ether is not compatible with [Pervasives] and uses different naming and
    structural conventions.

    {ul
    {- Polymorphic operators are not exported. See {{: #generic_definitions}
    Generic Definitions} for more details.}
    {- Structural equality operators are named [==] and [!=] and physical
    equality can be tested with {{: #val-is} [is]}.}
    {- Simple type names are preferred, {i e.g.}, [data] instead of [Data.t].}
    {- Interfaces are never named [S], instead a full name is used, like
    [Comparable] or [Hashable]. Additionally a module with the same name is
    defined containing a [Base] definition and a functor [Make] to generate the
    full interface. See {{: #interfaces} Interfaces} for more details.}
    {- No global operator for list appending is exported. List concatenation is
    inefficient and is discouraged (but is still defined in the [List] module).}
    {- The global {!val:(@)} operator is reused for function application.}
    {- String concatenation operator is named {!val:(++)} for consistency with
    the [Monoid] interface implemented in a separate package.}} *)

