(** Decode binary-encoded values. *)

(** Errors that can be encountered while decoding. *)
type error =
  | End_of_file
  | Unsupported_data_type of char (* header byte *)
  | Unsupported_data_size of Int64.t
  | Array_item_too_long of int (* offset of the end of the item *)
  | Record_field_too_long of int (* offset of the end of the field *)

(** Convert an error into text. *)
val show_error: error -> string

(** Decode a value.

    The argument is a function that, given a suggested [size], shall return the
    next bytes to read. It can return less than [size] bytes, but it will be called
    again later for the remaining bytes; and if it returns [0] bytes this will result
    in an end-of-file error. It can return more bytes, but it is possible that those
    additional bytes are ignored if they are after the value being decoded;
    if you want to check boundaries, or if you want to read a sequence of values,
    you may not want those bytes to be ignored. *)
val input: (int -> string) -> (Value.t, error) result

(** Decode a value from a string. *)
(* TODO: return the end offset, or check that we read the whole string? *)
val from_string: string -> (Value.t, error) result
