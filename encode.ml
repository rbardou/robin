type t =
  | Char of char
  | String of string
  | Array of { total_size: int; items: t array }
  | List of { total_size: int; items: t list }

type value = t
type tag = t
type field = t

(* Helpers *)

let get_size = function
  | Char _ -> 1
  | String s -> String.length s
  | Array { total_size; _ } | List { total_size; _ } -> total_size

let make_char (i: int) = Char (Char.chr i)

(* Only call this if [b] will no longer be modified. *)
let make_bytes (b: bytes) = String (Bytes.unsafe_to_string b)

let make_pair a b =
  let total_size = get_size a + get_size b in
  Array { total_size; items = [| a; b |] }

(* [data_type land (lnot 0b1111_0000)] must be 0. *)
let with_header data_type data =
  let header =
    match get_size data with
      | 0 ->
          make_char data_type
      | 1 ->
          (* In theory here we could try to use tiny sizes.
             But it may not be worth it since [with_header] is mostly used with
             larger values. *)
          make_char (data_type lor 0b1000)
      | 2 ->
          make_char (data_type lor 0b1001)
      | 4 ->
          make_char (data_type lor 0b1010)
      | 8 ->
          make_char (data_type lor 0b1011)
      | data_size ->
          if data_size <= 0xFF then
            let bytes = Bytes.create 2 in
            Bytes.set bytes 0 (Char.chr (data_type lor 0b1100));
            Bytes.set_uint8 bytes 1 data_size;
            make_bytes bytes
          else if data_size <= 0xFFFF then
            let bytes = Bytes.create 3 in
            Bytes.set bytes 0 (Char.chr (data_type lor 0b1101));
            Bytes.set_uint16_le bytes 1 data_size;
            make_bytes bytes
          else
            (* Convert to int64 now in case we are on a 32-bit platform and
               the data size already uses the 32th bit. *)
            let data_size = Int64.of_int data_size in
            if data_size <= 0xFFFF_FFFFL then
              let bytes = Bytes.create 5 in
              Bytes.set bytes 0 (Char.chr (data_type lor 0b1110));
              Bytes.set_int32_le bytes 1 (Int64.to_int32 data_size);
              make_bytes bytes
            else
              let bytes = Bytes.create 9 in
              Bytes.set bytes 0 (Char.chr (data_type lor 0b1111));
              Bytes.set_int64_le bytes 1 data_size;
              make_bytes bytes
  in
  make_pair header data

(* Constructors *)

let uint32 value =
  if Int32.logand value 0b1111_1111_1111_1111_1111_1111_1111_1000l = 0l then
    Char (Char.chr (Int32.to_int value))
  else if Int32.logand value 0xFFFF_FF00l = 0l then
    let bytes = Bytes.create 2 in
    Bytes.set bytes 0 (Char.chr 0b0000_1000);
    Bytes.set_uint8 bytes 1 (Int32.to_int value);
    make_bytes bytes
  else if Int32.logand value 0xFFFF_0000l = 0l then
    let bytes = Bytes.create 3 in
    Bytes.set bytes 0 (Char.chr 0b0000_1001);
    Bytes.set_uint16_le bytes 1 (Int32.to_int value);
    make_bytes bytes
  else
    let bytes = Bytes.create 5 in
    Bytes.set bytes 0 (Char.chr 0b0000_1010);
    Bytes.set_int32_le bytes 1 value;
    make_bytes bytes

let int32 value =
  if value >= 0l then
    if Int32.logand value 0b1111_1111_1111_1111_1111_1111_1111_1100l = 0l then
      Char (Char.chr (0b0001_0000 lor (Int32.to_int value)))
    else if Int32.logand value 0xFFFF_FF00l = 0l then
      let bytes = Bytes.create 2 in
      Bytes.set bytes 0 (Char.chr 0b0000_1000);
      Bytes.set_uint8 bytes 1 (Int32.to_int value);
      make_bytes bytes
    else if Int32.logand value 0xFFFF_0000l = 0l then
      let bytes = Bytes.create 3 in
      Bytes.set bytes 0 (Char.chr 0b0000_1001);
      Bytes.set_uint16_le bytes 1 (Int32.to_int value);
      make_bytes bytes
    else
      let bytes = Bytes.create 5 in
      Bytes.set bytes 0 (Char.chr 0b0001_1010);
      Bytes.set_int32_le bytes 1 value;
      make_bytes bytes
  else (
    if
      Int32.logand value 0b1111_1111_1111_1111_1111_1111_1111_1100l
      = 0b1111_1111_1111_1111_1111_1111_1111_1100l
    then
      Char (Char.chr (0b0001_0000 lor (Int32.to_int value land 0b111)))
    else if Int32.logand value 0xFFFF_FF80l = 0xFFFF_FF80l then
      let bytes = Bytes.create 2 in
      Bytes.set bytes 0 (Char.chr 0b0001_1000);
      Bytes.set_uint8 bytes 1 (Int32.to_int value);
      make_bytes bytes
    else if Int32.logand value 0xFFFF_8000l = 0xFFFF_8000l then
      let bytes = Bytes.create 3 in
      Bytes.set bytes 0 (Char.chr 0b0001_1001);
      Bytes.set_uint16_le bytes 1 (Int32.to_int value);
      make_bytes bytes
    else
      let bytes = Bytes.create 5 in
      Bytes.set bytes 0 (Char.chr 0b0001_1010);
      Bytes.set_int32_le bytes 1 value;
      make_bytes bytes
  )

let uint64 value =
  if Int64.logand value (Int64.lognot 0b111L) = 0L then
    Char (Char.chr (Int64.to_int value))
  else if Int64.logand value 0xFFFF_FFFF_FFFF_FF00L = 0L then
    let bytes = Bytes.create 2 in
    Bytes.set bytes 0 (Char.chr 0b0000_1000);
    Bytes.set_uint8 bytes 1 (Int64.to_int value);
    make_bytes bytes
  else if Int64.logand value 0xFFFF_FFFF_FFFF_0000L = 0L then
    let bytes = Bytes.create 3 in
    Bytes.set bytes 0 (Char.chr 0b0000_1001);
    Bytes.set_uint16_le bytes 1 (Int64.to_int value);
    make_bytes bytes
  else if Int64.logand value 0xFFFF_FFFF_0000_0000L = 0L then
    let bytes = Bytes.create 5 in
    Bytes.set bytes 0 (Char.chr 0b0000_1010);
    Bytes.set_int32_le bytes 1 (Int64.to_int32 value);
    make_bytes bytes
  else
    let bytes = Bytes.create 9 in
    Bytes.set bytes 0 (Char.chr 0b0000_1011);
    Bytes.set_int64_le bytes 1 value;
    make_bytes bytes

let int64_large_negative value =
  if Int64.logand value 0xFFFF_FFFF_8000_0000L = 0xFFFF_FFFF_8000_0000L then
    let bytes = Bytes.create 5 in
    Bytes.set bytes 0 (Char.chr 0b0001_1010);
    Bytes.set_int32_le bytes 1 (Int64.to_int32 value);
    make_bytes bytes
  else
    let bytes = Bytes.create 9 in
    Bytes.set bytes 0 (Char.chr 0b0001_1011);
    Bytes.set_int64_le bytes 1 value;
    make_bytes bytes

let int64_large_positive value =
  if Int64.logand value 0xFFFF_FFFF_0000_0000L = 0L then
    let bytes = Bytes.create 5 in
    Bytes.set bytes 0 (Char.chr 0b0000_1010);
    Bytes.set_int32_le bytes 1 (Int64.to_int32 value);
    make_bytes bytes
  else
    let bytes = Bytes.create 9 in
    Bytes.set bytes 0 (Char.chr 0b0000_1011);
    Bytes.set_int64_le bytes 1 value;
    make_bytes bytes

let int64 value =
  if value >= 0L then
    if
      Int64.logand value
        0b1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1000L
      = 0L
    then
      Char (Char.chr (0b0000_0000 lor (Int64.to_int value)))
    else if Int64.logand value 0xFFFF_FFFF_FFFF_FF00L = 0L then
      let bytes = Bytes.create 2 in
      Bytes.set bytes 0 (Char.chr 0b0000_1000);
      Bytes.set_uint8 bytes 1 (Int64.to_int value);
      make_bytes bytes
    else if Int64.logand value 0xFFFF_FFFF_FFFF_0000L = 0L then
      let bytes = Bytes.create 3 in
      Bytes.set bytes 0 (Char.chr 0b0000_1001);
      Bytes.set_uint16_le bytes 1 (Int64.to_int value);
      make_bytes bytes
    else
      int64_large_positive value
  else (
    if
      Int64.logand value
        0b1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1100L
      = 0b1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1100L
    then
      Char (Char.chr (0b0001_0000 lor ((Int64.to_int value) land 0b111)))
    else if Int64.logand value 0xFFFF_FFFF_FFFF_FF80L = 0xFFFF_FFFF_FFFF_FF80L then
      let bytes = Bytes.create 2 in
      Bytes.set bytes 0 (Char.chr 0b0001_1000);
      Bytes.set_uint8 bytes 1 (Int64.to_int value);
      make_bytes bytes
    else if Int64.logand value 0xFFFF_FFFF_FFFF_8000L = 0xFFFF_FFFF_FFFF_8000L then
      let bytes = Bytes.create 3 in
      Bytes.set bytes 0 (Char.chr 0b0001_1001);
      Bytes.set_uint16_le bytes 1 (Int64.to_int value);
      make_bytes bytes
    else
      int64_large_negative value
  )

let int value =
  if value >= 0 then
    if value land (lnot 0b111) = 0 then
      Char (Char.chr value)
    else if value land (lnot 0xFF) = 0 then
      let bytes = Bytes.create 2 in
      Bytes.set bytes 0 (Char.chr 0b0001_1000);
      Bytes.set bytes 1 (Char.chr value);
      make_bytes bytes
    else if value land (lnot 0xFFFF) = 0 then
      let bytes = Bytes.create 2 in
      Bytes.set bytes 0 (Char.chr 0b0001_1001);
      Bytes.set_uint16_le bytes 1 value;
      make_bytes bytes
    else
      int64_large_positive (Int64.of_int value)
  else (
    if value land lnot 0b11 = lnot 0b11 then
      Char (Char.chr (0b0001_0000 lor (value land 0b111)))
    else if value land lnot 0x7F = lnot 0x7F then
      let bytes = Bytes.create 2 in
      Bytes.set bytes 0 (Char.chr 0b0001_1000);
      Bytes.set bytes 1 (Char.chr value);
      make_bytes bytes
    else if value land lnot 0x7FFF = lnot 0x7FFF then
      let bytes = Bytes.create 2 in
      Bytes.set bytes 0 (Char.chr 0b0001_1001);
      Bytes.set_uint16_le bytes 1 value;
      make_bytes bytes
    else
      int64_large_positive (Int64.of_int value)
  )

let uintle value =
  with_header 0b0000 (String value)

let intle value =
  with_header 0b0001 (String value)

let uintbe value =
  with_header 0b0010 (String value)

let intbe value =
  with_header 0b0011 (String value)

let bool value =
  Char (Char.chr (if value then 0b0100_0001 else 0b0100_0000))

let float3 value =
  Char (Char.chr (0b0101_0000 lor (Char.code value land 0b111)))

let float value =
  let bytes = Bytes.create 9 in
  Bytes.set bytes 0 (Char.chr 0b0101_1011);
  (* TODO: is little endian the right endianness here? (same for decoding floats) *)
  Bytes.set_int64_le bytes 1 (Int64.bits_of_float value);
  make_bytes bytes

let float_other value =
  with_header 0b0101_0000 (String value)

let string value =
  with_header 0b0110_0000 (String value)

let array items =
  let total_size = Array.fold_left (fun acc value -> acc + get_size value) 0 items in
  with_header 0b1000_0000 (Array { total_size; items })

let list items =
  let total_size = List.fold_left (fun acc value -> acc + get_size value) 0 items in
  with_header 0b1000_0000 (List { total_size; items })

let tag_int32 value =
  if value < 0b1111_1000l then
    make_char (Int32.to_int value)
  else if value < 0xFFl then
    let bytes = Bytes.create 2 in
    Bytes.set bytes 0 (Char.chr 0b1111_1000);
    Bytes.set_uint8 bytes 1 (Int32.to_int value);
    make_bytes bytes
  else if value < 0xFFFFl then
    let bytes = Bytes.create 3 in
    Bytes.set bytes 0 (Char.chr 0b1111_1001);
    Bytes.set_uint16_le bytes 1 (Int32.to_int value);
    make_bytes bytes
  else
    let bytes = Bytes.create 5 in
    Bytes.set bytes 0 (Char.chr 0b1111_1010);
    Bytes.set_int32_le bytes 1 value;
    make_bytes bytes

let tag_int64_large value =
  if value < 0xFFFF_FFFFL then
    let bytes = Bytes.create 5 in
    Bytes.set bytes 0 (Char.chr 0b1111_1010);
    Bytes.set_int32_le bytes 1 (Int64.to_int32 value);
    make_bytes bytes
  else
    let bytes = Bytes.create 9 in
    Bytes.set bytes 0 (Char.chr 0b1111_1011);
    Bytes.set_int64_le bytes 1 value;
    make_bytes bytes

let tag_int64 value =
  if value < 0b1111_1000L then
    make_char (Int64.to_int value)
  else if value < 0xFFL then
    let bytes = Bytes.create 2 in
    Bytes.set bytes 0 (Char.chr 0b1111_1000);
    Bytes.set_uint8 bytes 1 (Int64.to_int value);
    make_bytes bytes
  else if value < 0xFFFFL then
    let bytes = Bytes.create 3 in
    Bytes.set bytes 0 (Char.chr 0b1111_1001);
    Bytes.set_uint16_le bytes 1 (Int64.to_int value);
    make_bytes bytes
  else
    tag_int64_large value

let tag_int value =
  if value < 0b1111_1000 then
    make_char value
  else if value < 0xFF then
    let bytes = Bytes.create 2 in
    Bytes.set bytes 0 (Char.chr 0b1111_1000);
    Bytes.set_uint8 bytes 1 value;
    make_bytes bytes
  else if value < 0xFFFF then
    let bytes = Bytes.create 3 in
    Bytes.set bytes 0 (Char.chr 0b1111_1001);
    Bytes.set_uint16_le bytes 1 value;
    make_bytes bytes
  else
    tag_int64_large (Int64.of_int value)

let tag_large value =
  let len = String.length value in
  if len = 0 then
    make_char 0
  else if len = 1 then
    let c = value.[0] in
    if Char.code c < 0b1111_1000 then
      Char c
    else
      let bytes = Bytes.create 2 in
      Bytes.set bytes 0 (Char.chr 0b1111_1000);
      Bytes.set_uint8 bytes 1 (Char.code c);
      make_bytes bytes
  else if len = 2 then
    let bytes = Bytes.create 3 in
    Bytes.set bytes 0 (Char.chr 0b1111_1001);
    String.blit value 0 bytes 1 2;
    make_bytes bytes
  else if len = 4 then
    let bytes = Bytes.create 5 in
    Bytes.set bytes 0 (Char.chr 0b1111_1010);
    String.blit value 0 bytes 1 4;
    make_bytes bytes
  else if len = 8 then
    let bytes = Bytes.create 9 in
    Bytes.set bytes 0 (Char.chr 0b1111_1011);
    String.blit value 0 bytes 1 8;
    make_bytes bytes
  else if len <= 0xFF then
    let header =
      let bytes = Bytes.create 2 in
      Bytes.set bytes 0 (Char.chr 0b1111_1100);
      Bytes.set_uint8 bytes 1 len;
      make_bytes bytes
    in
    make_pair header (String value)
  else if len <= 0xFFFF then
    let header =
      let bytes = Bytes.create 3 in
      Bytes.set bytes 0 (Char.chr 0b1111_1101);
      Bytes.set_uint16_le bytes 1 len;
      make_bytes bytes
    in
    make_pair header (String value)
  else
    let len = Int64.of_int len in
    if len <= 0xFFFF_FFFFL then
      let header =
        let bytes = Bytes.create 5 in
        Bytes.set bytes 0 (Char.chr 0b1111_1110);
        Bytes.set_int32_le bytes 1 (Int64.to_int32 len);
        make_bytes bytes
      in
      make_pair header (String value)
    else
      let header =
        let bytes = Bytes.create 9 in
        Bytes.set bytes 0 (Char.chr 0b1111_1111);
        Bytes.set_int64_le bytes 1 len;
        make_bytes bytes
      in
      make_pair header (String value)

let field = make_pair

let record_array fields =
  let total_size = Array.fold_left (fun acc value -> acc + get_size value) 0 fields in
  with_header 0b1010_0000 (Array { total_size; items = fields })

let record_list fields =
  let total_size = List.fold_left (fun acc value -> acc + get_size value) 0 fields in
  with_header 0b1010_0000 (List { total_size; items = fields })

let tag (t: Value.tag) =
  match t with
    | Tag t -> tag_int t
    | Tag64 t -> tag_int64 t
    | Tag_large t -> tag_large t

let rec value (v: Value.t) =
  match v with
    | UInt64 i -> uint64 i
    | Int64 i -> int64 i
    | UIntLE i -> uintle i
    | IntLE i -> intle i
    | UIntBE i -> uintbe i
    | IntBE i -> intbe i
    | Bool b -> bool b
    | Float3 f -> float3 f
    | Float f -> float f
    | Float_other f -> float_other f
    | String s -> string s
    | Array l -> array (Array.map value l)
    | List l -> list (List.map value l)
    | Record_array l -> record_array (Array.map (fun (t, v) -> field (tag t) (value v)) l)
    | Record_list l -> record_list (List.map (fun (t, v) -> field (tag t) (value v)) l)

let length = get_size

let rec output out = function
  | Char c -> out (String.make 1 c)
  | String s -> out s
  | Array { items; _ } -> Array.iter (output out) items
  | List { items; _ } -> List.iter (output out) items

let output value out = output out value

let to_string value =
  let result = Bytes.create (get_size value) in
  let offset = ref 0 in
  let rec output = function
    | Char c ->
        Bytes.set result !offset c;
        incr offset
    | String s ->
        let len = String.length s in
        String.blit s 0 result !offset len;
        offset := !offset + len
    | Array { items; _ } ->
        Array.iter output items
    | List { items; _ } ->
        List.iter output items
  in
  output value;
  Bytes.unsafe_to_string result
