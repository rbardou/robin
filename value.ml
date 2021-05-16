(** Low-level values. *)

(** These values are returned by the [Decode] module when decoding.
    They can also be [Encode]d. *)

(** Tags are integers with an arbitrary size.

    When a tag is decoded, we choose the smallest representation in memory from
    [int], [int64] or [string] (a sequence of bytes representing an unsigned
    little endian integer). *)
type tag =
  | Tag of int
  | Tag64 of int64
  | Tag_large of string

(** Values.

    Integer values are decoded into [Int], [UInt64], [Int64], [UIntLE], [IntLE],
    [UIntBE] or [IntBE] depending on the size of the integer. If it is signed and
    fits on 31 bits, or if it is unsigned and fits on 30 bits, it is decoded into
    an [Int]. If it is unsigned and fits on 64 bits, it is decoded into [UInt64].
    If it is signed and fits on 64 bits, it is decoded into [Int64].
    If it is too large, it is decoded into [UIntLE] (unsigned little endian),
    [IntLE] (signed little endian), [UIntBE] (unsigned big endian) or [IntBE]
    (signed big endian).

    Floats are decoded into [Float3] for 3-bit floats. They use the 3 lowest bits
    of the [char]. Doubles are decoded into [Float], and other float sizes are
    decoded into [Float_other] (a sequence of bytes using the IEEE 754 standard).
    Note that [Encode] always encodes floats using 64 bits, so if you know that
    what you are decoding was encoded using [Encode], you can expect to only read
    64-bit floats. *)
type t =
  | UInt64 of int64
  | Int64 of int64
  | UIntLE of string
  | IntLE of string
  | UIntBE of string
  | IntBE of string
  | Bool of bool
  | Float3 of char
  | Float of float
  | Float_other of string
  | String of string
  | Array of t array
  | List of t list
  | Record_array of (tag * t) array
  | Record_list of (tag * t) list
