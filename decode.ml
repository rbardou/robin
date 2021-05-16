type error =
  | End_of_file
  | Unsupported_data_type of char (* header byte *)
  | Unsupported_data_size of Int64.t
  | Array_item_too_long of int (* offset of the end of the item *)
  | Record_field_too_long of int (* offset of the end of the field *)

let show_error = function
  | End_of_file -> "end of file"
  | Unsupported_data_type c -> Printf.sprintf "unsupported data type: %C" c
  | Unsupported_data_size i -> Printf.sprintf "unsupported data size: %i" (Int64.to_int i)
  | Array_item_too_long i -> Printf.sprintf "array item too long (ends at %d)" i
  | Record_field_too_long i -> Printf.sprintf "record field too long (ends at %d)" i

exception Error of error

let () =
  Printexc.register_printer @@ function
  | Error error -> Some (show_error error)
  | _ -> None

let error e = raise (Error e)

let value_exn read =
  let buffer = ref "" in
  let offset = ref 0 in
  let total_read = ref 0 in
  let read_char () =
    if !offset < String.length !buffer then
      (* Buffer already contains enough bytes. *)
      let c = !buffer.[!offset] in
      incr offset;
      incr total_read;
      c
    else
      (* Buffer is empty, read at least one byte. *)
      let chunk = read 1 in
      let len = String.length chunk in
      if len = 0 then error End_of_file else
      if len = 1 then
        let c = chunk.[0] in
        incr total_read;
        c
      else
        (* We read too much, split. *)
        let c = chunk.[0] in
        buffer := chunk;
        offset := 1;
        incr total_read;
        c
  in
  let read_string count =
    if String.length !buffer - !offset >= count then (
      (* Buffer already contains enough bytes. *)
      let s = String.sub !buffer !offset count in
      offset := !offset + count;
      total_read := !total_read + count;
      s
    ) else (
      (* Buffer does not contain enough bytes. *)
      (* Allocate enough bytes to contain the full result. *)
      let bytes = Bytes.create count in
      (* Copy what we have. *)
      let current_count = ref (String.length !buffer - !offset) in
      String.blit !buffer !offset bytes 0 !current_count;
      offset := 0;
      (* Read until we have all that we need. *)
      while !current_count < count do
        let missing_count = count - !current_count in
        let chunk = read missing_count in
        let chunk_len = String.length chunk in
        if chunk_len > missing_count then (
          (* We read too much, split. *)
          String.blit chunk 0 bytes !current_count missing_count;
          current_count := !current_count + missing_count;
          buffer := chunk;
          offset := missing_count
        ) else (
          (* Use the whole chunk and continue. *)
          String.blit chunk 0 bytes !current_count chunk_len;
          current_count := !current_count + chunk_len
        )
      done;
      total_read := !total_read + count;
      Bytes.unsafe_to_string bytes
    )
  in
  let read_string_64 data_size =
    (* TODO: may raise Unsupported_data_size? *)
    read_string (Int64.to_int data_size)
  in
  let read_uint8 () = Char.code (read_char ()) in
  let read_int8 () =
    let i = read_uint8 () in
    if i >= 0b1000_0000 then
      (* By writing (-1) the literal makes sense both on 32-bit and on 64-bit hardware. *)
      i lor ((-1) lxor 0b1111_1111)
    else
      i
  in
  let read_uint16le () = Bytes.get_uint16_le (Bytes.unsafe_of_string (read_string 2)) 0 in
  let read_int16le () = Bytes.get_int16_le (Bytes.unsafe_of_string (read_string 2)) 0 in
  let read_uint16be () = Bytes.get_uint16_be (Bytes.unsafe_of_string (read_string 2)) 0 in
  let read_int16be () = Bytes.get_int16_be (Bytes.unsafe_of_string (read_string 2)) 0 in
  let read_uint32le_as64 () =
    let i = Bytes.get_int32_le (Bytes.unsafe_of_string (read_string 4)) 0 in
    Int64.logand (Int64.of_int32 i) 0xFFFF_FFFFL
  in
  let read_uint32be_as64 () =
    let i = Bytes.get_int32_be (Bytes.unsafe_of_string (read_string 4)) 0 in
    Int64.logand (Int64.of_int32 i) 0xFFFF_FFFFL
  in
  let read_int32le () = Bytes.get_int32_le (Bytes.unsafe_of_string (read_string 4)) 0 in
  let read_int32be () = Bytes.get_int32_be (Bytes.unsafe_of_string (read_string 4)) 0 in
  let read_int64le () = Bytes.get_int64_le (Bytes.unsafe_of_string (read_string 8)) 0 in
  let read_int64be () = Bytes.get_int64_be (Bytes.unsafe_of_string (read_string 8)) 0 in
  let read_uint64le = read_int64le in
  let read_uint64be = read_int64be in
  let read_float64 () =
    Int64.float_of_bits (Bytes.get_int64_le (Bytes.unsafe_of_string (read_string 8)) 0)
  in
  let read_tag (): Value.tag =
    let first_byte = read_char () in
    if Char.code first_byte < 0b1111_1000 then
      Tag (Char.code first_byte)
    else
      match Char.code first_byte with
        | 0b1111_1000 -> Tag (read_uint8 ())
        | 0b1111_1001 -> Tag (read_uint16le ())
        | 0b1111_1010 -> Tag64 (read_uint32le_as64 ())
        | 0b1111_1011 -> Tag64 (read_uint64le ())
        | _ ->
            let size =
              match Char.code first_byte with
                | 0b1111_1100 -> read_uint8 () |> Int64.of_int
                | 0b1111_1101 -> read_uint16le () |> Int64.of_int
                | 0b1111_1110 -> read_uint32le_as64 ()
                | 0b1111_1111 -> read_uint64le ()
                | _ -> assert false (* we went through all possible values for a byte *)
            in
            (
              match size with
                | 0L -> Tag 0
                | 1L -> Tag (read_uint8 ())
                | 2L -> Tag (read_uint16le ())
                | 4L -> Tag64 (read_uint32le_as64 ())
                | 8L -> Tag64 (read_uint64le ())
                | _ -> Tag_large (read_string_64 size)
            )
  in
  let rec read_value (): Value.t =
    let header = read_char () in
    (* Match on data size. *)
    match (Char.code header) land 0b1111 with
      | 0b0000 | 0b0001 | 0b0010 | 0b0011
      | 0b0100 | 0b0101 | 0b0110 | 0b0111 as value ->
          (* Tiny size: the value is the size itself.
             How to interpret it depends on the type. *)
          (
            match Char.code header land 0b1111_0000 with
              | 0b0000_0000 | 0b0010_0000 -> (* Unsigned Integer (endianness irrelevant) *)
                  UInt64 (Int64.of_int value)
              | 0b0001_0000 | 0b0011_0000 -> (* Signed Integer (endianness irrelevant) *)
                  if value land 0b100 = 0 then
                    Int64 (Int64.of_int value)
                  else (
                    match value with
                      | 0b111 -> Int64 (-1L)
                      | 0b110 -> Int64 (-2L)
                      | 0b101 -> Int64 (-3L)
                      | 0b100 -> Int64 (-4L)
                      | _ -> assert false (* not one of the values we matched on earlier *)
                  )
              | 0b0100_0000 -> (* Boolean *)
                  Bool (value <> 0)
              | 0b0101_0000 -> (* Float *)
                  Float3 (Char.chr value)
              | 0b0110_0000 -> (* String *)
                  String ""
              | 0b1000_0000 -> (* Array *)
                  List []
              | 0b0111_0000 | 0b1011_0000 | 0b1100_0000 | 0b1101_0000
              | 0b1110_0000 | 0b1111_0000 -> (* Reserved *)
                  error (Unsupported_data_type header)
              | _ ->
                  assert false (* we went through all possible type values *)
          )
      | size ->
          let data_size =
            match size with
              | 0b1000 -> 1L
              | 0b1001 -> 2L
              | 0b1010 -> 4L
              | 0b1011 -> 8L
              | 0b1100 -> read_uint8 () |> Int64.of_int
              | 0b1101 -> read_uint16le () |> Int64.of_int
              | 0b1110 -> read_uint32le_as64 ()
              | 0b1111 ->
                  let i = read_uint64le () in
                  if i < 0L then error (Unsupported_data_size i);
                  i
              | _ -> assert false (* we handled all 4-bit values *)
          in
          (* TODO: for large ints (and large tags), if the size is 3, 5, 6 or 7,
             we can still make it fit in a fixed-size integer *)
          match (Char.code header) land 0b1111_0000 with
            | 0b0000_0000 -> (* Unsigned Integer (Little Endian) *)
                (
                  match data_size with
                    | 0L -> UInt64 0L
                    | 1L -> UInt64 (read_uint8 () |> Int64.of_int)
                    | 2L -> UInt64 (read_uint16le () |> Int64.of_int)
                    | 4L -> UInt64 (read_uint32le_as64 ())
                    | 8L -> UInt64 (read_uint64le ())
                    | _ -> UIntLE (read_string_64 data_size)
                )
            | 0b0001_0000 -> (* Signed Integer (Little Endian) *)
                (
                  match data_size with
                    | 0L -> Int64 0L
                    | 1L -> Int64 (read_int8 () |> Int64.of_int)
                    | 2L -> Int64 (read_int16le () |> Int64.of_int)
                    | 4L -> Int64 (read_int32le () |> Int64.of_int32)
                    | 8L -> Int64 (read_int64le ())
                    | _ -> IntLE (read_string_64 data_size)
                )
            | 0b0010_0000 -> (* Unsigned Integer (Big Endian) *)
                (
                  match data_size with
                    | 0L -> UInt64 0L
                    | 1L -> UInt64 (read_int8 () |> Int64.of_int)
                    | 2L -> UInt64 (read_uint16be () |> Int64.of_int)
                    | 4L -> UInt64 (read_uint32be_as64 ())
                    | 8L -> UInt64 (read_uint64be ())
                    | _ -> UIntBE (read_string_64 data_size)
                )
            | 0b0011_0000 -> (* Signed Integer (Big Endian) *)
                (
                  match data_size with
                    | 0L -> Int64 0L
                    | 1L -> Int64 (read_int8 () |> Int64.of_int)
                    | 2L -> Int64 (read_int16be () |> Int64.of_int)
                    | 4L -> Int64 (read_int32be () |> Int64.of_int32)
                    | 8L -> Int64 (read_int64be ())
                    | _ -> IntBE (read_string_64 data_size)
                )
            | 0b0100_0000 -> (* Boolean *)
                (
                  match data_size with
                    | 0L -> Bool false
                    | 1L -> Bool (read_int8 () <> 0)
                    | 2L -> Bool (read_int16le () <> 0)
                    | 4L -> Bool (read_int32le () <> 0l)
                    | 8L -> Bool (read_int64le () <> 0L)
                    | _ ->
                        let s = read_string_64 data_size in
                        let rec loop n =
                          if n >= String.length s then
                            Value.Bool false
                          else if s.[n] <> '\000' then
                            Value.Bool true
                          else
                            loop (n + 1)
                        in
                        loop 0
                )
            | 0b0101_0000 -> (* Float *)
                (
                  match data_size with
                    | 8L ->
                        Float (read_float64 ())
                    | _ ->
                        Float_other (read_string_64 data_size)
                )
            | 0b0110_0000 -> (* String *)
                String (read_string_64 data_size)
            | 0b1000_0000 -> (* Array *)
                (* TODO: fail if data_size too big for an int *)
                let expected_total_read = !total_read + (Int64.to_int data_size) in
                let rec loop acc =
                  if !total_read > expected_total_read then
                    error (Array_item_too_long !total_read)
                  else if !total_read = expected_total_read then
                    Value.List (List.rev acc)
                  else
                    let item = read_value () in
                    loop (item :: acc)
                in
                loop []
            | 0b1010_0000 -> (* Record *)
                (* TODO: fail if data_size too big for an int *)
                let expected_total_read = !total_read + (Int64.to_int data_size) in
                let rec loop acc =
                  if !total_read > expected_total_read then
                    error (Record_field_too_long !total_read)
                  else if !total_read = expected_total_read then
                    Value.Record_list (List.rev acc)
                  else
                    let tag = read_tag () in
                    let value = read_value () in
                    loop ((tag, value) :: acc)
                in
                loop []
            | 0b0111_0000 | 0b1011_0000 | 0b1100_0000 | 0b1101_0000
            | 0b1110_0000 | 0b1111_0000 -> (* Reserved *)
                error (Unsupported_data_type header)
            | _ ->
                assert false (* we went through all possible type values *)
  in
  read_value ()

let input read =
  try
    Ok (value_exn read)
  with Error e ->
    Error e

let from_string string =
  let read =
    let already_read = ref false in
    fun _ -> if !already_read then "" else (already_read := true; string)
  in
  input read
