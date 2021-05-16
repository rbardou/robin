(** Encode values to binary. *)

(** {2 Types} *)

(** Full values with their header. *)
type value

(** Record field tags. *)
type tag

(** Record fields made of a tag and a value. *)
type field

(** {2 Encoding Functions} *)

(** Values can have different encodings which are equivalent.
    The functions below choose a canonical, smallest encoding.
    Signed integers which are positive are encoded as unsigned integers,
    which sometimes reduces the size. *)

(** Encode an unsigned 32-bit integer. *)
val uint32: Int32.t -> value

(** Encode a signed 32-bit integer. *)
val int32: Int32.t -> value

(** Encode an unsigned 64-bit integer. *)
val uint64: Int64.t -> value

(** Encode a signed 64-bit integer. *)
val int64: Int64.t -> value

(** Encode an integer. *)
val int: int -> value

(** Encode an unsigned little endian integer of arbitrary size. *)
val uintle: string -> value

(** Encode a signed little endian integer of arbitrary size. *)
val intle: string -> value

(** Encode an unsigned big endian integer of arbitrary size. *)
val uintbe: string -> value

(** Encode a signed big endian integer of arbitrary size. *)
val intbe: string -> value

(** Encode a boolean. *)
val bool: bool -> value

(** Encode a 3-bit float.

    Not very useful in practice. *)
val float3: char -> value

(** Encode a 64-bit float. *)
val float: float -> value

(** Encode a float of arbitrary size. *)
val float_other: string -> value

(** Encode a string. *)
val string: string -> value

(** Encode an array. *)
val array: value array -> value

(** Encode an array represented by a list. *)
val list: value list -> value

(** Encode an 32-bit tag. *)
val tag_int32: Int32.t -> tag

(** Encode an 64-bit tag. *)
val tag_int64: Int64.t -> tag

(** Encode a tag represented using an OCaml integer. *)
val tag_int: int -> tag

(** Encode a tag of arbitrary size. *)
val tag_large: string -> tag

(** Encode a record field. *)
val field: tag -> value -> field

(** Encode a record represented by an array of fields. *)
val record_array: field array -> value

(** Encode a record represented by a list of fields. *)
val record_list: field list -> value

(** {2 Values From The Value Module} *)

(** These functions are a bit higher-level than the ones of the previous section.
    Using them to encode is less efficient since you need to build values of types
    [Value.tag] or [Value.t], but if you already have such values, these functions
    are convenient. *)

(** Encode a tag. *)
val tag: Value.tag -> tag

(** Encode a value. *)
val value: Value.t -> value

(** {2 Output} *)

(** Get the length of the binary representation of a value. *)
val length: value -> int

(** Output the binary representation of a value.

    The first argument is an output function, such as [print_string] to output on
    standard output.  *)
val output: value -> (string -> unit) -> unit

(** Output the binary representation of a value to a new string. *)
val to_string: value -> string
