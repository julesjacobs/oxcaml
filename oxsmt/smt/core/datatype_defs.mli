(** The structural registry for declared algebraic datatypes (GOALS: Datatypes).

    Datatype {e sorts} live in {!Sort.t}; datatype {e values} are ordinary {!Term.t}
    applications — a constructor / selector / tester is an [App (sym, args)] over a symbol
    declared in {!Env} with a rank, so the term and rank machinery is unchanged. What is
    NOT expressible there is the datatype's {e shape}: which constructors a sort has,
    which selectors and tester each constructor has, and the field a selector projects.
    This registry holds exactly that, keyed by symbol, and is the single source the
    datatype theory reads to fire its four rules (clash / injectivity / selector-eval /
    occurs).

    Not frozen (deliberately): it is theory-facing plumbing, additive, and outside the
    ADR-0003 core freeze. Built by the parser (and any future front end) as datatypes are
    declared; read by the theory and the printer. Stdlib-only (I3). *)

type t

(** One selector: its function symbol, the constructor field index it projects (0-based),
    and the field's sort. The selector's rank in {!Env} is [(dt_sort) -> field_sort]. *)
type selector =
  { sym : Symbol.t
  ; index : int
  ; field_sort : Sort.t
  }

(** One constructor of a datatype: its function symbol, its selectors in field order, and
    its tester symbol ([(_ is C)], a [(dt_sort) -> Bool] predicate). A nullary constructor
    (an enum case) has [selectors = []]. The constructor's rank in {!Env} is
    [(field_sort ...) -> dt_sort]. *)
type constructor =
  { sym : Symbol.t
  ; selectors : selector list
  ; tester : Symbol.t
  }

(** One datatype: the sort symbol it was declared under and its constructors in
    declaration order. [sort_sym] is the {!Symbol.t} returned by the sort declaration; the
    corresponding {!Sort.t} is recovered by the front end (representation of a datatype
    sort is a core decision, see [sort.mli]). *)
type datatype =
  { sort_sym : Symbol.t
  ; constructors : constructor list
  }

val empty : t

(** [true] iff no datatype has been declared — the DT theory is not installed in this
    case, so a non-datatype problem is unaffected. *)
val is_empty : t -> bool

(** [add t datatype] registers [datatype] and all its constructor/selector/tester symbols,
    building the reverse indices. Raises [Invalid_argument] on a duplicate datatype sort
    symbol or a symbol already registered in another role (a construction bug — every
    constructor/selector/tester symbol belongs to exactly one datatype). *)
val add : t -> datatype -> t

(** [datatype_of_sort t sort_sym] is the datatype declared under [sort_sym], or [None] if
    [sort_sym] names no datatype (e.g. a plain uninterpreted sort). This is how the
    theory/combination layer answers "is this sort a datatype, and what is its shape". *)
val datatype_of_sort : t -> Symbol.t -> datatype option

(** [constructor_of_sym t sym] is [Some (dt, c)] when [sym] is a constructor, giving its
    owning datatype and its own descriptor; [None] otherwise. *)
val constructor_of_sym : t -> Symbol.t -> (datatype * constructor) option

(** [selector_of_sym t sym] is [Some (dt, c, sel)] when [sym] is a selector, giving the
    owning datatype, the constructor it projects from, and the selector descriptor; [None]
    otherwise. The theory uses this to fire selector evaluation once [c]'s constructor is
    known on the argument's class. *)
val selector_of_sym : t -> Symbol.t -> (datatype * constructor * selector) option

(** [tester_of_sym t sym] is [Some (dt, c)] when [sym] is a tester [(_ is c)]; [None]
    otherwise. *)
val tester_of_sym : t -> Symbol.t -> (datatype * constructor) option

(** [is_datatype_sym t sym] iff [sym] is a registered datatype sort symbol. Convenience
    over {!datatype_of_sort}. *)
val is_datatype_sym : t -> Symbol.t -> bool
