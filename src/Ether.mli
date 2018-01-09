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
    {- {{: #io} Input/Output}}
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
  type self

  (* Required operation. *)
  val compare : self -> self -> order

  (* Derived operations. *)
  val equal  : self -> self -> bool
  val ( == ) : self -> self -> bool
  val ( != ) : self -> self -> bool
  val ( <  ) : self -> self -> bool
  val ( >  ) : self -> self -> bool
  val ( <= ) : self -> self -> bool
  val ( >= ) : self -> self -> bool
  val min    : self -> self -> self
  val max    : self -> self -> self
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
    type self

    val compare : self -> self -> order
  end

  (* Generates the derived operations given the [Base] module. *)
  module Make(B : Base) : (Comparable with type self := B.self)
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
  type self = person

  val say_hello : self -> unit

  (* Age-based comparison. *)
  include Comparable with type self := self
end = struct
  type self = person

  let say_hello self =
    print "Hello, %s!" self.name

  include Comparable.Make(struct
      type nonrec self = self

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

(** Nullary signature for equality comparisons which are equivalence relations.

    [Equatable.Base] is the minimal implementation of the [Equatable] instance.
    Some examples of nullary equatable types, {i i.e.}, types without
    parameters include: [int], [float], [char]. *)
module type Equatable = sig
  type self
  (** The type of the equatable monomorphic values, {i i.e.}, non-polymorphic
      nullary values, such as [int], [char], etc. *)

  val equal : self -> self -> bool

  val ( == ) : self -> self -> bool

  val ( != ) : self -> self -> bool
end


(** Structure defining the [Base] implementation for nullary [Equatable]
    signature and a functor to build the extended signature. *)
module Equatable : sig
  module type Base = sig
    type self

    val equal : self -> self -> bool
  end

  module Make(B : Base) : (Equatable with type self := B.self)
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
  type 'a self

  val equal : ('a -> 'a -> bool) -> 'a self -> 'a self -> bool
end


(** Binary signature for equality comparisons which are equivalence relations. *)
module type Equatable2 = sig
  type ('a, 'b) self

  val equal :
    ('a -> 'a -> bool) ->
    ('b -> 'b -> bool) ->
    ('a, 'b) self ->
    ('a, 'b) self -> bool
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
  type self

  include Equatable with type self := self

  val compare : self comparator
  val ( <  ) : self -> self -> bool
  val ( >  ) : self -> self -> bool
  val ( <= ) : self -> self -> bool
  val ( >= ) : self -> self -> bool
  val min : self -> self -> self
  val max : self -> self -> self
end


(** Structure defining the [Base] implementation for nullary [Comparable]
    signature and a functor to build the extended signature. *)
module Comparable : sig
  module type Base = sig
    type self

    val compare : self comparator
  end

  module Make(B : Base) : (Comparable with type self := B.self)
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
  type 'a self

  val compare : 'a comparator -> 'a self -> 'a self -> order

  val min : 'a comparator -> 'a self -> 'a self -> 'a self

  val max : 'a comparator -> 'a self -> 'a self -> 'a self
end


(** Structure defining the [Base] implementation for unary [Comparable1]
    signature and a functor to build the extended signature. *)
module Comparable1 : sig
  module type Base = sig
    type 'a self

    val compare : 'a comparator -> 'a self -> 'a self -> order
  end

  module Make(B : Base) : (Comparable1 with type 'a self := 'a B.self)
  (** Functor building an instance of {!Comparable1} given a
      {!Comparable1.Base} implementation. *)
end


(** Binary comparable signature for types that form a total order. *)
module type Comparable2 = sig
  type ('a, 'b) self

  val compare :
    'a comparator -> 'b comparator ->
    ('a, 'b) self -> ('a, 'b) self -> order

  val min :
    'a comparator -> 'b comparator ->
    ('a, 'b) self -> ('a, 'b) self -> ('a, 'b) self

  val max :
    'a comparator -> 'b comparator ->
    ('a, 'b) self -> ('a, 'b) self -> ('a, 'b) self
end


(** Structure defining the [Base] implementation for the binary [Comparable2]
    signature and a functor to build the extended signature. *)
module Comparable2 : sig
  module type Base = sig
    type ('a, 'b) self

    val compare :
    'a comparator -> 'b comparator ->
      ('a, 'b) self -> ('a, 'b) self -> order
  end

  module Make(B : Base) : (Comparable2 with type ('a, 'b) self := ('a, 'b) B.self)
  (** Functor building an instance of {!Comparable2} given a
      {!Comparable2.Base} implementation. *)
end


(** {2:formatting_and_pretty_printing Formatting and Pretty-printing}

    This section contains the primitives for formatting and pretty-printing. *)

type 'a printer = Format.formatter -> 'a -> unit
(** The type for pretty-printers of values of type ['a]. *)

val print : ?output: out_channel -> ?break:string ->
  ('a, Format.formatter, unit, unit) format4 -> 'a
(** [print ?output ?break fmt] prints the text formatted accordingly to [fmt]
    on the [output] channel followed by [break].

    {3 Parameters}

    @param output The output channel to write on (default = [stdout]).
    @param break String to be appended at the end (default = ["\n"]).

    {3 Examples}
{[
print "hello";
print "hello, world" ~break:"!!!\n";
print "hello, %s!" "world";
print ~output:stderr "hello";
]} *)

val format : ('a, Format.formatter, unit, string) format4 -> 'a
(** [format fmt arg1 ... argN] formats the arguments [arg1] to [arnN]
    according to the format string [fmt] and returns the result as a string.

    The format string is a character string which contains two types of
    objects: plain characters, which are simply copied to the output channel,
    and conversion specifications, each of which causes conversion and printing
    of arguments. For more details see the {!Format} module.

    It is an alias for [Format.asprintf].

{[
assert (format "x = %d" 42 = "x = 42");
assert (format "Hello, %s!" world = "Hello, world!");
]} *)


(** Interface for the monomorphic types that can be printed.

    [Printable.Base] is the minimal implementation of the [Printable] instance.
    Some examples of nullary printable types, i.e., types without parameters
    include: [Int.t], [Float.t], [Char.t], [Unit.t], etc. *)
module type Printable = sig
  type self
  (** The type of the printable monomorphic values, i.e., non-polymorphic
      nullary values, such as [int], [string], etc. *)

  val pp : self printer
  (** [pp] is the pretty-printer for the type {!t}. *)

  val to_string : self -> string
  (** [to_string self] converts [self] to a string representation using the
      {!pp} pretty-printer. *)

  val print : self -> unit
  (** [print self] writes [self] to the standard output using the {!pp}
      pretty-printer. *)
end


(** Structure defining the [Base] implementation for nullary [Printable]
    signature and a functor to build the extended signature. *)
module Printable : sig
  module type Base = sig
    type self
    (** The type of the printable monomorphic values, i.e., non-polymorphic
        nullary values, such as [int], [string], etc. *)

    val pp : self printer
    (** [pp] is the pretty-printer for the type {!self}. *)
  end

  module Make(B : Base) : (Printable with type self := B.self)
  (** Functor building an instance of {!Printable} given a {!Printable.Base}
      implementation. *)
end


(** Interface for the unary types that can be printed.

    [Printable1.Base] is the minimal implementation of the [Printable]
    instance. Some examples of unary printable types, {i i.e.}, types with one
    parameter, include: {!option}, {!list}, {!array}. *)
module type Printable1 = sig
  type 'a self
  (** The type of the printable polymorphic values of arity one, i.e., values
      with one type parameter, such as ['a list], ['a option], etc. *)

  val pp : 'a printer -> 'a self printer
  (** [pp pp_a] is the pretty-printer for the unary type ['a t]. [pp_a] is the
      pretty-printer for the wrapped type ['a]. *)

  val to_string : 'a printer -> 'a self -> string
  (** [to_string pp_a self] converts unary {!'a t} to a string representation
      using {!pp_a} for the wrapped type. *)

  val print : 'a printer -> 'a self -> unit
  (** [print pp_a self] writes [self] to the standard output using [pp_a]
      pretty-printer for the wrapped type and {!Printable1.pp} for [self]. *)
end


(** Structure defining the [Base] implementation for unary [Printable1]
    signature and a functor to build the extended signature. *)
module Printable1 : sig
  module type Base = sig
    type 'a self
    (** The type of the printable polymorphic values of arity one, i.e., values
        with one type parameter, such as ['a list], ['a option], etc. *)

    val pp : 'a printer -> Format.formatter -> 'a self -> unit
    (** [pp] is the pretty-printer for the type {!t}. *)
  end

  module Make(B : Base) : (Printable1 with type 'a self := 'a B.self)
  (** Functor building an instance of {!Printable1} given a {!Printable1.Base}
      implementation. *)
end


(** Interface for the binary types that can be printed.

    [Printable2.Base] is the minimal implementation of the [Printable2]
    instances. Some examples of binary printable types, i.e., types with two
    parameters, include: {!('a, 'e) Result.t} and {!('a, 'b) Either.t}. *)
module type Printable2 = sig
  type ('a, 'b) self
  (** The type of the printable polymorphic values of arity two, i.e., values
      with two type parameters, such as [('a, 'e) Result.t] and [('a, 'b)
      Either.t]. *)

  val pp : 'a printer -> 'b printer -> ('a, 'b) self printer
  (** [pp pp_a pp_b] is the pretty-printer for the binary type [('a, 'b) t].
      [pp_a] and [pp_b] are the pretty-printers for the wrapped types ['a] and
      ['b]. *)

  val to_string : 'a printer -> 'b printer -> ('a, 'b) self -> string
  (** [to_string pp_a pp_b self] converts binary [('a, 'b) t] to a string
      representation using [pp_a] and [pp_b] for the wrapped types ['a] and
      ['b]. *)

  val print : 'a printer -> 'b printer -> ('a, 'b) self -> unit
  (** [print pp_a pp_b self] writes [self] to the standard output using [pp_a]
      and [pp_b] pretty-printers for the wrapped types and {!Printable1.pp} for
      [self]. *)
end


(** Structure defining the [Base] implementation for binary [Printable2]
    signature and a functor to build the extended signature. *)
module Printable2 : sig
  module type Base = sig
    type ('a, 'b) self
    (** The type of the printable polymorphic values of arity two, i.e., values
        with two type parameters, such as [('a, 'e) Result.t] and [('a, 'b)
        Either.t]. *)

    val pp : 'a printer -> 'b printer -> ('a, 'b) self printer
    (** [pp pp_a pp_b] is the pretty-printer for the binary type [('a, 'b) t].
        [pp_a] and [pp_b] are the pretty-printers for the wrapped types ['a] and
        ['b]. *)
  end

  module Make(B : Base) : (Printable2 with type ('a, 'b) self := ('a, 'b) B.self)
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
  type self = person

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
  type 'a self = 'a hasher

  val int : int hasher
  val char : char hasher
  val string : string hasher
  val pair : 'a hasher -> 'b hasher -> ('a * 'b) hasher
end

(** Interface for nullary hashable types. *)
module type Hashable = sig
  type self

  val hash : self hasher
end


(** Interface for unary hashable types. *)
module type Hashable1 = sig
  type 'a self

  val hash : 'a hasher -> 'a self hasher
end


(** Interface for binary hashable types. *)
module type Hashable2 = sig
  type ('a, 'b) self

  val hash : 'a hasher -> 'b hasher -> ('a, 'b) self hasher
end


(** {1:parsing Parsing}

    Valid OCaml literals can be read from their textual representation with the
    [Parsable] interface.

    The [Parser] module includes helper combinators to parse compound types
    such as records and lists. *)

type 'a parser = string -> 'a option
(** The type of parsing functions, {i i.e.}, functions that given a string
    produce values of type ['a], if the string represents a valid value. *)

module Parser : sig
  type 'a self = 'a parser

  val int : int parser

  val float : float parser

  val char : char parser
end

(** Interface for nullary types that can be parsed. *)
module type Parsable = sig
  type self

  val parse : self parser
end


(** Interface for unary types that can be parsed. *)
module type Parsable1 = sig
  type 'a self

  val parse : 'a parser -> 'a self parser
end


(** Interface for binary types that can be parsed. *)
module type Parsable2 = sig
  type ('a, 'b) self

  val parse : 'a parser -> 'b parser -> ('a, 'b) self parser
end


(** {2:other_interfaces Other Interfaces} *)

(* Bounded is used to name the upper and lower limits of a type. *)
module type Bounded = sig
  type self

  val min_value : self
  val max_value : self
end


(** Interface for nullary types with a default value. *)
module type Default = sig
  type self

  val default : self
end


(** Interface for unary types with a default value. *)
module type Default1 = sig
  type 'a self

  val default : 'a self
end


(** Enumerable defines operations on sequentially ordered types. *)
module type Enumerable = sig
  type self

  val predecessor : self -> self
  (** [pred self] is the predecessor of [self].

      @raise No_value if [self] has no predecessor. *)

  val successor : self -> self
  (** [succ self] is the successor of [self].

      @raise No_value if [self] has no successor. *)
end


(** Basic numeric interface. *)
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


(** {1:exn Exceptions} *)

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

(** Module definition for the exception type. *)
module Exception : sig
  type self = exn

  include Printable  with type self := self
end


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
    [request] type should also be aliased as [Request.self]. This convention
    makes the usage of types more consistent (avoids mixing module-defined
    types like [Some_type.t] with simple types [some_type]) and allows direct
    access to the field and variant members of the frequently used types.

    {b Meta:} This needs to be reviewed since globally defined types can also
    pollute the global namespace with conflicting fields/variants. An
    alternative is to start by defining the type in a module and export it
    after the definition (potentially by redefining publicly the body). *)


(** Unit type and operations. *)
module Unit : sig
  type self = unit

  (** {2:implemented_instances Implemented instances} *)

  include Bounded    with type self := self
  include Comparable with type self := self
  include Default    with type self := self
  include Hashable   with type self := self
  include Parsable   with type self := self
  include Printable  with type self := self
end


(** Boolean type and operations. *)
module Bool : sig
  type self = bool
  (** Standard boolean type. *)

  val not : self -> self
  (** [not a] is the boolean negation of [a]. *)

  val ( && ) : self -> self -> self
  (** [a && b] is the boolean 'and' of [a] and [b]. Evaluation is sequential,
      left-to-right, [a] is evaluated first, and if it returns [false], [b] is
      not evaluated at all. *)

  val ( || ) : self -> self -> self
  (** [a || b] is the boolean 'or' of [a] and [b]. Evaluation is sequential,
      left-to-right, [a] is evaluated first, and if it returns [true], [b] is
      not evaluated at all. *)

  val of_int : int -> bool
  val to_int : bool -> int

  val to_option : 'a -> self -> 'a option
  (** [to_option x self] is an optional value containing [x] if [self] is
      [true] and [None] otherwise.

{[
assert (Bool.to_option 42 false == None);
assert (Bool.to_option 42 true  == Some 42);
]} *)

  (** {6 Implemented instances} *)
  include Bounded    with type self := self
  include Comparable with type self := self
  include Default    with type self := self
  include Enumerable with type self := self
  include Equatable  with type self := self
  include Hashable   with type self := self
  include Parsable   with type self := self
  include Printable  with type self := self
end


(** Integer type and arithmetic operations.

    Integer numbers with 31 bits on 32-bit processors or 63 bits on 64-bit
    processors. *)
module Int : sig
  type self = int
  (** Standard integer type. *)

  val ( / ) : self -> self -> self
  (* Integer devision. *)

  val ( mod ) : self -> self -> self

  (** {6:conv Conversion } *)

  val to_string : self -> string
  (** [to_string x] is a string representation of the integer [x]. *)

  val of_float : float -> int
  (** [of_float x] is an integer representation of the float value [x]. *)

  val of_char : char -> int
  (** [of_char x] is an integer representation of the char value [x].

      See [Char.try_of_int] *)

  module Unsafe : sig
    val of_string : string -> int
    (** [of_string str] is an integer created by parsing the string [str].

        @raise Failure if the string does not represent a valid integer. *)
  end

  (** {6 Implemented instances} *)
  include Bounded    with type self := self
  include Comparable with type self := self
  include Default    with type self := self
  include Enumerable with type self := self
  include Equatable  with type self := self
  include Hashable   with type self := self
  include Numeric    with type self := self
  include Parsable   with type self := self
  include Printable  with type self := self
end


(** Float type and arithmetic operations.

    Floating-point numbers according to IEEE 754, using double precision 64
    bits numbers. *)
module Float : sig
  type self = float

  val ( / ) : self -> self -> self

  val ( mod ) : self -> self -> self

  val infinity : float
  (** Positive infinity. *)

  val neg_infinity : float
  (** Negative infinity. *)

  val nan : float
  (** A special floating-point value denoting the result of an undefined
      operation such as [0.0 /. 0.0]. Stands for 'not a number'. Any
      floating-point operation with [nan] as argument returns [nan] as result.
      As for floating-point comparisons, [=], [<], [<=], [>] and [>=] return
      [false] and [<>] returns [true] if one or both of their arguments is [nan]. *)

  val epsilon : float
  (** The difference between [1.0] and the smallest exactly representable
      floating-point number greater than [1.0]. *)

  val round : float -> float
  (** [round f] rounds the float value to the nearest integer float. *)

  val exp : float -> float
  (** Exponential. *)

  val frexp : float -> float * int
  (** [frexp f] returns the pair of the significant and the exponent of [f]. When
      [f] is zero, the significant [x] and the exponent [n] of [f] are equal to
      zero.  When [f] is non-zero, they are defined by [f = x *. 2 ** n] and [0.5
      <= x < 1.0]. *)

  val ldexp : float -> int -> float
  (** [ldexp x n] returns [x *. 2 ** n]. *)

  val modf : float -> float * float
  (** [modf f] returns the pair of the fractional and integral part of [f]. *)

  type fpclass = Pervasives.fpclass
  (** The five classes of floating-point numbers, as determined by
      the {!Pervasives.classify_float} function. *)

  val classify : float -> fpclass
  (** Return the class of the given floating-point number:
      normal, subnormal, zero, infinite, or not a number. *)

  (** {2:conv Conversion } *)

  val to_int : float -> int
  (** Truncate the given floating-point number to an integer. The result is
      unspecified if the argument is [nan] or falls outside the range of
      representable integers. *)

  val to_string : float -> string

  val try_of_string : string -> float option
  (** [try_of_string str] is a float value converted from the string [str] or
      [None] if the string does not represent a valid float. *)

  module Unsafe : sig
    val of_string : string -> float
    (** [of_string str] is a float created by parsing the string [str].

        @raise Failure if the string does not represent a valid integer. *)
  end

  (** {6 Implemented instances} *)
  include Bounded    with type self := self
  include Comparable with type self := self
  include Default    with type self := self
  include Equatable  with type self := self
  include Hashable   with type self := self
  include Numeric    with type self := self
  include Parsable   with type self := self
  include Printable  with type self := self
end


(** Char type and operations.

    Character values represented as bytes. *)
module Char : sig
  (** Char type and operations. *)

  type self = char
  (** The type for bytes. *)

  (** {2:conversions Conversions} *)

  val try_of_int : int -> self option
  (** [try_of_int b] is a byte from [b]. [None] is returned if [b] is not in
      the range \[[0x00];[0xFF]\]. *)

  val to_int : self -> int
  (** [to_int b] is the byte [b] as an integer. *)


  module Unsafe : sig
    val of_int : int -> self
    (** [of_int b] is a byte from [b].

        {e See also:} [Char.of_int]
        @raise Failure if the byte [b] does not represent a valid char. *)
  end


  (** {2 Implemented instances} *)

  include Bounded    with type self := self
  include Comparable with type self := self
  include Default    with type self := self
  include Enumerable with type self := self
  include Equatable  with type self := self
  include Hashable   with type self := self
  include Parsable   with type self := self
  include Printable  with type self := self
end


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


(** {2:pair Pair Type and Operations} *)

(** A tuple with two values. *)
module Pair : sig
  type ('a, 'b) self = 'a * 'b
  (** The type of tuples with two values. *)

  val first : ('a, 'b) self -> 'a
  (** [first pair] is the first elements of the [pair]

{[
assert (first (1, "x") = 1)
]} *)

  val second : ('a, 'b) self -> 'b
  (** [second pair] is the second elements of the [pair]

{[
assert (second (1, "x") = "x")
]} *)

  include Comparable2 with type ('a, 'b) self := ('a, 'b) self
  include Hashable2   with type ('a, 'b) self := ('a, 'b) self
  include Parsable2   with type ('a, 'b) self := ('a, 'b) self
  include Printable2  with type ('a, 'b) self := ('a, 'b) self
end

type ('a, 'b) pair = ('a, 'b) Pair.self
(** Public alias for [Pair] type. *)

val first : ('a, 'b) pair -> 'a
(** Public alias for {!Pair.first}. *)

val second : ('a, 'b) pair -> 'b
(** Public alias for {!Pair.second}. *)


(* {2:option Option Type and Operations}

    Option represents an optional value: an [option] can be either [Some] and
    contain a value or a [None], and do not.

type nonrec 'a option = 'a option
(** The type of optional values ['a]. *)

val some : 'a -> 'a option
(** [some x] wraps [x] in a [Some] option value. *)

val none : 'a option
(** [none] is the [None] option value. *)

val is_some : 'a option -> bool
(** [is_some opt] is [true] if the option is a [Some] value. *)

val is_none : 'a option -> bool
(** [is_none opt] is [true] if the option is a [None] value. *)

val option : ('a -> 'b) -> 'b -> 'a option -> 'b
(** [case f default self] is [f] applied to the [Some] value of [self],
    or [default] if [self] is [None].

{[
assert (None    |> option ((+) 1) 0 = 0);
assert (Some 42 |> option ((+) 1) 0 = 43);
]} *)


(** {2:result Result Type and Operations} *)

type nonrec ('a, 'e) result = ('a, 'e) result
(** A result is either [Ok] meaning the computation succeeded, or it is an
    [Error] meaning that there was some failure. *)

val ok : 'a -> ('a, 'e) result
(** [ok x] creates an [Ok] result with value [x]. *)

val error : 'e -> ('a, 'e) result
(** [error x] creates an [Error] result with value [x]. *)

val is_ok : ('a, 'e) result -> bool
(** [is_ok res] is [true] if the result value is an [Ok]. *)

val is_error : ('a, 'e) result -> bool
(** [is_error res] is [true] if the result value is an [Error]. *)

val result : ('a -> 'b) -> ('e -> 'b) -> ('a, 'e) result -> 'b
(** A view function for the {!type:result} type.

    [result on_ok on_error res] is the function application [on_ok x] if [res]
    is [Ok x], or [on_error e] if [res] is [Error e].

    This function is equivalent to pattern-matching on the structure of the
    result value. For more information on [view] functions see the
    {!view_functions} section.

{[
let use_result =
  result Int.to_string (fun err -> "Error: " ^ err) in
assert (Ok 42 |> use_result = "42");
assert (Error "no value" |> use_result = "Error: no value")
]} *) *)


(** {2:public_exports Public Exports}

    This section exports public definitions from the defined modules aliasing
    some of the names. *)

val int : float -> int
(** Public alias for {!val:Int.of_float}. *)

val float : int -> float
(** Public alias for {!val:Float.of_int}. *)

val char : int -> char option
(** Public alias for {!Char.try_of_int}. *)

val code : char -> int
(** Public alias for {!Char.to_int}. *)

val not : bool -> bool
(** Public alias for {!Bool.not}. *)

val ( && ) : bool -> bool -> bool
(** Public alias for {!Bool.(&&)}. *)

val ( || ) : bool -> bool -> bool
(** Public alias for {!Bool.(||)}. *)


(** {1:io Input/Output} *)

val open_file : ?binary:bool -> string -> in_channel
(** [open_file ?binary path] is an input channel for a file at [path].

    Open a file for reading in textual mode:
{[
open_file ~binary:false (path "/var/log/system.log")
]} *)

val create_file : ?binary:bool -> ?append:bool -> ?exclusive:bool ->
  ?perm:int -> string -> out_channel
(** [create_file ?binary ?append ?exclusive ?perm path] is an output channel
    for a file at [path].

    Open a file for appending, create if doesn't exist:
{[
create_file ~append:true (path "/tmp/data.txt")
]}

    Open a file for writing and truncate its content:
{[
create_file (path "/tmp/data.txt")
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
    {- The main type of modules/interface is called [self] and not [t].}
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

