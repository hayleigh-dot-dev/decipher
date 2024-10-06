// IMPORTS ---------------------------------------------------------------------

import birl.{type Time}
import gleam/bit_array
import gleam/dict
import gleam/dynamic.{type DecodeError, type Decoder, type Dynamic, DecodeError}
import gleam/float
import gleam/int
import gleam/json
import gleam/list
import gleam/pair
import gleam/result
import gleam/set.{type Set}
import gleam/string
import gleam/uri.{type Uri}
import stoiridh/version.{type Version}

// PRIMITIVES ------------------------------------------------------------------

/// Decode an `Int` as long as it is greater than or equal to zero.
///
pub fn non_negative_int(dynamic: Dynamic) -> Result(Int, List(DecodeError)) {
  use int <- result.try(dynamic.int(dynamic))

  case int >= 0 {
    True -> Ok(int)
    False ->
      Error([
        DecodeError(
          expected: "A non-negative int",
          found: int.to_string(int),
          path: [],
        ),
      ])
  }
}

/// Decode an `Int` that has been converted to a string. Some JSON APIs will
/// send numbers as strings, so this decoder can come in handy more often than
/// you'd think!
///
pub fn int_string(dynamic: Dynamic) -> Result(Int, List(DecodeError)) {
  use string <- result.try(dynamic.string(dynamic))

  string
  |> int.parse
  |> result.replace_error([
    DecodeError(expected: "A stringified int", found: string, path: []),
  ])
}

/// Decode a `Float` that has been converted to a string. Some JSON APIs will
/// send numbers as strings, so this decoder can come in handy more often than
/// you'd think!
///
pub fn float_string(dynamic: Dynamic) -> Result(Float, List(DecodeError)) {
  use string <- result.try(dynamic.string(dynamic))

  string
  |> float.parse
  |> result.replace_error([
    DecodeError(expected: "A stringified float", found: string, path: []),
  ])
}

/// This decoder is capable of decoding both `Int` and `Float` values. If the
/// value is an `Int`, it will be converted to a `Float` automatically.
///
pub fn number(dynamic: Dynamic) -> Result(Float, List(DecodeError)) {
  dynamic.any([
    dynamic.float,
    fn(dynamic) {
      dynamic.int(dynamic)
      |> result.map(int.to_float)
    },
  ])(dynamic)
}

/// Decode numbers that have been converted to strings. This decoder is capable
/// of decoding both `Int` and `Float` values converted to strings. Some JSON
/// APIs will send numbers as strings, so this decoder can come in handy more
/// often than you'd think!
///
pub fn number_string(dynamic: Dynamic) -> Result(Float, List(DecodeError)) {
  dynamic.any([
    float_string,
    fn(dynamic) {
      int_string(dynamic)
      |> result.map(int.to_float)
    },
  ])(dynamic)
}

/// Decode a string that represents a YAML-style boolean value. Any of the following
/// values will be decoded as `True`:
///
/// - "true"
/// - "True"
/// - "on"
/// - "On"
/// - "yes"
/// - "Yes"
///
/// Any of the following values will be decoded as `False`:
///
/// - "false"
/// - "False"
/// - "off"
/// - "Off"
/// - "no"
/// - "No"
///
/// Anything else will fail to decode.
///
pub fn bool_string(dynamic: Dynamic) -> Result(Bool, List(DecodeError)) {
  enum([
    #("true", True),
    #("True", True),
    #("on", True),
    #("On", True),
    #("yes", True),
    #("Yes", True),
    #("false", False),
    #("False", False),
    #("off", False),
    #("Off", False),
    #("no", False),
    #("No", False),
  ])(dynamic)
}

/// This decoder will decode a string and then confirm that it is not empty.
///
pub fn non_empty_string(dynamic: Dynamic) -> Result(String, List(DecodeError)) {
  use string <- result.try(dynamic.string(dynamic))

  case string {
    "" ->
      Error([
        DecodeError(
          expected: "A non-empty string",
          found: "An empty string",
          path: [],
        ),
      ])
    _ -> Ok(string)
  }
}

// COLLECTIONS -----------------------------------------------------------------

/// Decode a list or [arraylike](#arraylike) into a `Set`. Any duplicate values
/// will be _dropped_. If you want to ensure that there are no duplicates, use
/// the [exact_set](#exact_set) decoder instead.
///
pub fn set(decoder: Decoder(a)) -> Decoder(Set(a)) {
  fn(dynamic: Dynamic) {
    dynamic
    |> dynamic.any([dynamic.list(decoder), arraylike(decoder)])
    |> result.map(set.from_list)
  }
}

/// Decode a list or [arraylike](#arraylike) into a `Set`. This decoder is slightly
/// slower than the [set](#set) decoder, but it will guarantee that there were no
/// duplicate values in the incoming list.
///
pub fn exact_set(decoder: Decoder(a)) -> Decoder(Set(a)) {
  fn(dynamic: Dynamic) {
    use list <- result.try(
      dynamic.any([dynamic.list(decoder), arraylike(decoder)])(dynamic),
    )
    let length = list.length(list)
    let set = set.from_list(list)

    case set.size(set) == length {
      True -> Ok(set)
      False ->
        Error([
          DecodeError(
            expected: "A list with no duplicate values",
            found: "A list with duplicate values",
            path: [],
          ),
        ])
    }
  }
}

/// Decode a list or [arraylike](#arraylike) with at least one item into a `List`.
/// If the incoming list is empty, decoding will fail.
///
pub fn non_empty_list(decode: Decoder(a)) -> Decoder(List(a)) {
  fn(dynamic: Dynamic) {
    use list <- result.try(dynamic.list(decode)(dynamic))

    case list.is_empty(list) {
      True ->
        Error([
          DecodeError(
            expected: "A non-empty list",
            found: "A list with at least 1 item",
            path: [],
          ),
        ])
      False -> Ok(list)
    }
  }
}

/// In JavaScript certain objects are said to be "arraylike". These are objects
/// that satisfy the following conditions:
///
/// - They have a `length` property that is a non-negative integer.
/// - They have a property for each integer index from `0` up to `length - 1`.
///
/// Operations like `document.querySelectorAll` or `document.getElementsByTagName`
/// return arraylike objects like a [`NodeList`](https://developer.mozilla.org/en-US/docs/Web/API/NodeList).
/// This decoder is capable of decoding such objects into a proper Gleam `List`.
///
pub fn arraylike(decoder: Decoder(a)) -> Decoder(List(a)) {
  fn(dynamic: Dynamic) {
    use length <- result.try(dynamic.field("length", dynamic.int)(dynamic))

    all({
      let list = list.range(0, length - 1)
      use index <- list.map(list)

      dynamic.field(int.to_string(index), decoder)
    })(dynamic)
  }
}

/// Create a decoder for a list of values from a list of decoders to run. Each
/// decoder will run against the input value, and all must succeed for the decoder
/// to succeed.
///
/// Errors from each decoder will be collected, which means the entire list is
/// run even if one decoder fails!
///
pub fn all(decoders: List(Decoder(a))) -> Decoder(List(a)) {
  fn(dynamic: Dynamic) {
    use list, decoder <- list.fold_right(decoders, Ok([]))

    case list, decoder(dynamic) {
      Ok(xs), Ok(x) -> Ok([x, ..xs])
      Ok(_), Error(e) -> Error(e)
      Error(e), Ok(_) -> Error(e)
      Error(e), Error(x) -> Error(list.append(e, x))
    }
  }
}

// OBJECTS & TUPLES ------------------------------------------------------------

/// Decode an object with exactly one field into some value. If additional fields
/// are present, decoding will fail and the additional fields will be reported in
/// the error.
///
pub fn exact_object1(
  constructor: fn(a) -> b,
  field1: #(String, Decoder(a)),
) -> Decoder(b) {
  let expected_keys = set.from_list([field1.0])

  fn(dynamic: Dynamic) {
    dynamic
    |> dynamic.decode1(constructor, dynamic.field(field1.0, field1.1))
    |> result.then(check_exact_object(_, expected_keys, dynamic))
  }
}

/// Decode an object with exactly two fields into some value. If additional fields
/// are present, decoding will fail and the additional fields will be reported in
/// the error.
///
pub fn exact_object2(
  constructor: fn(a, b) -> c,
  field1: #(String, Decoder(a)),
  field2: #(String, Decoder(b)),
) -> Decoder(c) {
  let expected_keys = set.from_list([field1.0])

  fn(dynamic: Dynamic) {
    dynamic
    |> dynamic.decode2(
      constructor,
      dynamic.field(field1.0, field1.1),
      dynamic.field(field2.0, field2.1),
    )
    |> result.then(check_exact_object(_, expected_keys, dynamic))
  }
}

/// Decode an object with exactly three fields into some value. If additional fields
/// are present, decoding will fail and the additional fields will be reported in
/// the error.
///
pub fn exact_object3(
  constructor: fn(a, b, c) -> d,
  field1: #(String, Decoder(a)),
  field2: #(String, Decoder(b)),
  field3: #(String, Decoder(c)),
) -> Decoder(d) {
  let expected_keys = set.from_list([field1.0, field2.0, field3.0])

  fn(dynamic: Dynamic) {
    dynamic
    |> dynamic.decode3(
      constructor,
      dynamic.field(field1.0, field1.1),
      dynamic.field(field2.0, field2.1),
      dynamic.field(field3.0, field3.1),
    )
    |> result.then(check_exact_object(_, expected_keys, dynamic))
  }
}

/// Decode an object with exactly four fields into some value. If additional fields
/// are present, decoding will fail and the additional fields will be reported in
/// the error.
///
pub fn exact_object4(
  constructor: fn(a, b, c, d) -> e,
  field1: #(String, Decoder(a)),
  field2: #(String, Decoder(b)),
  field3: #(String, Decoder(c)),
  field4: #(String, Decoder(d)),
) -> Decoder(e) {
  let expected_keys = set.from_list([field1.0, field2.0, field3.0, field4.0])

  fn(dynamic: Dynamic) {
    dynamic
    |> dynamic.decode4(
      constructor,
      dynamic.field(field1.0, field1.1),
      dynamic.field(field2.0, field2.1),
      dynamic.field(field3.0, field3.1),
      dynamic.field(field4.0, field4.1),
    )
    |> result.then(check_exact_object(_, expected_keys, dynamic))
  }
}

/// Decode an object with exactly five fields into some value. If additional fields
/// are present, decoding will fail and the additional fields will be reported in
/// the error.
///
pub fn exact_object5(
  constructor: fn(a, b, c, d, e) -> f,
  field1: #(String, Decoder(a)),
  field2: #(String, Decoder(b)),
  field3: #(String, Decoder(c)),
  field4: #(String, Decoder(d)),
  field5: #(String, Decoder(e)),
) -> Decoder(f) {
  let expected_keys =
    set.from_list([field1.0, field2.0, field3.0, field4.0, field5.0])

  fn(dynamic: Dynamic) {
    dynamic
    |> dynamic.decode5(
      constructor,
      dynamic.field(field1.0, field1.1),
      dynamic.field(field2.0, field2.1),
      dynamic.field(field3.0, field3.1),
      dynamic.field(field4.0, field4.1),
      dynamic.field(field5.0, field5.1),
    )
    |> result.then(check_exact_object(_, expected_keys, dynamic))
  }
}

/// Decode an object with exactly six fields into some value. If additional fields
/// are present, decoding will fail and the additional fields will be reported in
/// the error.
///
pub fn exact_object6(
  constructor: fn(a, b, c, d, e, f) -> g,
  field1: #(String, Decoder(a)),
  field2: #(String, Decoder(b)),
  field3: #(String, Decoder(c)),
  field4: #(String, Decoder(d)),
  field5: #(String, Decoder(e)),
  field6: #(String, Decoder(f)),
) -> Decoder(g) {
  let expected_keys =
    set.from_list([field1.0, field2.0, field3.0, field4.0, field5.0, field6.0])

  fn(dynamic: Dynamic) {
    dynamic
    |> dynamic.decode6(
      constructor,
      dynamic.field(field1.0, field1.1),
      dynamic.field(field2.0, field2.1),
      dynamic.field(field3.0, field3.1),
      dynamic.field(field4.0, field4.1),
      dynamic.field(field5.0, field5.1),
      dynamic.field(field6.0, field6.1),
    )
    |> result.then(check_exact_object(_, expected_keys, dynamic))
  }
}

fn check_exact_object(
  return: a,
  expected: Set(String),
  dynamic: Dynamic,
) -> Result(a, List(DecodeError)) {
  use keys <- result.try(keys(dynamic))

  let found_keys = set.from_list(keys)
  let difference = set.to_list(set.difference(found_keys, expected))

  case list.is_empty(difference) {
    True -> Ok(return)
    False -> {
      let expected_keys =
        expected
        |> set.to_list
        |> string.join(", ")

      let extra_keys =
        difference
        |> string.join(", ")

      Error([
        DecodeError(
          expected: "An object with exactly these keys: " <> expected_keys,
          found: "An object with these extra keys: " <> extra_keys,
          path: [],
        ),
      ])
    }
  }
}

// CUSTOM TYPES ----------------------------------------------------------------

/// There is no standard way to represent something like Gleam's custom types as
/// JSON or YAML (or most common formats). It's common then to represent them as
/// a _tagged_ or _discriminated_ union where a field is used to signify which
/// variant of the type is being represented.
///
/// This decoder lets you decode things in this format by first decoding the tag
/// and then selecting the appropriate decoder to run based on that tag.
///
/// ```gleam
/// import decipher
/// import gleam/dynamic.{type DecodeError, type Decoder, type Dynamic, DecodeError}
///
/// type Example {
///   Wibble(foo: Int)
///   Wobble(bar: String)
/// }
///
/// fn example_decoder(dynamic: Dynamic) -> Result(Example, List(DecodeError)) {
///   decipher.tagged_union(
///     dynamic.field("$", dynamic.string),
///     [
///       #("wibble", dynamic.decode1(Wibble, dynamic.field("foo", dynamic.int))),
///       #("wobble", dynamic.decode1(Wobble, dynamic.field("bar", dynamic.string))),
///     ]
///   )(dynamic)
/// }
/// ```
///
pub fn tagged_union(
  tag: Decoder(a),
  variants: List(#(a, Decoder(b))),
) -> Decoder(b) {
  let switch = dict.from_list(variants)

  fn(dynamic: Dynamic) {
    use kind <- result.try(tag(dynamic))

    case dict.get(switch, kind) {
      Ok(decoder) -> decoder(dynamic)
      Error(_) -> {
        // We're going to report the possible tags as a TS-style union, so
        // something like:
        //
        //   "A" | "B" | "C"
        //
        let tags =
          dict.keys(switch)
          |> list.map(string.inspect)
          |> string.join(" | ")

        // Recover the path from the user's `tag_decoder`. This is kind of hacky
        // but honestly if they somehow succeed in decoding `Nil` then what are
        // they even playing at.
        //
        let path = case tag(dynamic.from(Nil)) {
          Error([DecodeError(path: path, ..), ..]) -> path
          _ -> []
        }

        Error([
          DecodeError(expected: tags, found: string.inspect(kind), path: path),
        ])
      }
    }
  }
}

/// A simplified version of the [tagged_union](#tagged_union) decoder. First
/// decodes a string, and then attempts to find a corresponding value from a
/// list of variants.
///
/// This is how the [`bool_string`](#bool_string) decoder is implemented:
///
/// ```gleam
/// import decipher
/// import gleam/dynamic.{type DecodeError, type Decoder, type Dynamic, DecodeError}
///
/// pub fn bool_string(dynamic: Dynamic) -> Result(Bool, List(DecodeError)) {
///   decipher.enum([
///     #("true", True),
///     #("True", True),
///     #("on", True),
///     #("On", True),
///     #("yes", True),
///     #("Yes", True),
///     #("false", False),
///     #("False", False),
///     #("off", False),
///     #("Off", False),
///     #("no", False),
///     #("No", False),
///   ])(dynamic)
/// }
/// ```
///
pub fn enum(variants: List(#(String, a))) -> Decoder(a) {
  tagged_union(
    dynamic.string,
    list.map(variants, pair.map_second(_, fn(variant) { fn(_) { Ok(variant) } })),
  )
}

// EXOTICS ---------------------------------------------------------------------

/// Decode a string representing an [ISO 8601 datetime](https://en.wikipedia.org/wiki/ISO_8601)
/// as a [`Time`](https://hexdocs.pm/birl/birl.html#Time) value from the birl
/// package.
///
pub fn iso_8601(dynamic: Dynamic) -> Result(Time, List(DecodeError)) {
  use string <- result.try(dynamic.string(dynamic))

  case birl.parse(string) {
    Ok(time) -> Ok(time)
    Error(_) ->
      Error([
        DecodeError(
          expected: "An ISO 8601 date string",
          found: string,
          path: [],
        ),
      ])
  }
}

/// Decode a [Unix timestamp](https://en.wikipedia.org/wiki/Unix_time) as a
/// [`Time`](https://hexdocs.pm/birl/birl.html#Time) value from the birl package.
///
pub fn unix_timestamp(dynamic: Dynamic) -> Result(Time, List(DecodeError)) {
  dynamic
  |> dynamic.any([dynamic.int, int_string])
  |> result.map(birl.from_unix)
}

/// Decode a string representing a [HTTP-date](https://www.rfc-editor.org/rfc/rfc9110#http.date)
/// as a [`Time`](https://hexdocs.pm/birl/birl.html#Time) value from the birl
/// package.
///
pub fn http_date(dynamic: Dynamic) -> Result(Time, List(DecodeError)) {
  use string <- result.try(dynamic.string(dynamic))

  case birl.from_http(string) {
    Ok(time) -> Ok(time)
    Error(_) ->
      Error([
        DecodeError(expected: "An HTTP date string", found: string, path: []),
      ])
  }
}

/// Decode a string representing a [URI](https://en.wikipedia.org/wiki/Uniform_Resource_Identifier)
/// into a Gleam [`Uri`](https://hexdocs.pm/gleam_stdlib/gleam/uri.html#Uri) value.
///
pub fn uri(dynamic: Dynamic) -> Result(Uri, List(DecodeError)) {
  use string <- result.try(dynamic.string(dynamic))

  case uri.parse(string) {
    Ok(uri) -> Ok(uri)
    Error(_) ->
      Error([
        DecodeError(expected: "A valid Gleam URI", found: string, path: []),
      ])
  }
}

/// Decode a string representing base16-encoded binary data into a `BitArray`.
///
pub fn base16(dynamic: Dynamic) -> Result(BitArray, List(DecodeError)) {
  use string <- result.try(dynamic.string(dynamic))

  case bit_array.base16_decode(string) {
    Ok(bit_array) -> Ok(bit_array)
    Error(_) ->
      Error([
        DecodeError(
          expected: "A valid base16-encoded string",
          found: string,
          path: [],
        ),
      ])
  }
}

/// Decode a string representing base64-encoded binary data into a `BitArray`.
///
pub fn base64(dynamic: Dynamic) -> Result(BitArray, List(DecodeError)) {
  use string <- result.try(dynamic.string(dynamic))

  case bit_array.base64_decode(string) {
    Ok(bit_array) -> Ok(bit_array)
    Error(_) ->
      Error([
        DecodeError(
          expected: "A valid base64-encoded string",
          found: string,
          path: [],
        ),
      ])
  }
}

/// Decode a string representing a valid semantic version into a [`Version`](https://hexdocs.pm/stoiridh_version/stoiridh/version.html#Version)
/// value from the stoiridh_version package.
///
pub fn semver(dynamic: Dynamic) -> Result(Version, List(DecodeError)) {
  use string <- result.try(dynamic.string(dynamic))

  case version.parse(string) {
    Ok(version) -> Ok(version)
    Error(version.InvalidVersion) | Error(version.NegativeValue(_)) ->
      Error([
        DecodeError(
          expected: "A valid semantic version",
          found: string,
          path: [],
        ),
      ])
    Error(version.InvalidMajorVersion) ->
      Error([
        DecodeError(
          expected: "A valid semantic version",
          found: "A semantic version with an invalid major version number",
          path: [],
        ),
      ])
    Error(version.InvalidMinorVersion) ->
      Error([
        DecodeError(
          expected: "A valid semantic version",
          found: "A semantic version with an invalid minor version number",
          path: [],
        ),
      ])
    Error(version.InvalidPatchVersion) ->
      Error([
        DecodeError(
          expected: "A valid semantic version",
          found: "A semantic version with an invalid patch version number",
          path: [],
        ),
      ])
    Error(version.InvalidPrerelease) ->
      Error([
        DecodeError(
          expected: "A valid semantic version",
          found: "A semantic version with an invalid prerelease",
          path: [],
        ),
      ])
    Error(version.InvalidBuildMetadata) ->
      Error([
        DecodeError(
          expected: "A valid semantic version",
          found: "A semantic version with invalid build metadata",
          path: [],
        ),
      ])
  }
}

/// Decode a string representing url-safe base64-encoded binary data into a
/// `BitArray`.
///
pub fn base64_url_encoded(
  dynamic: Dynamic,
) -> Result(BitArray, List(DecodeError)) {
  use string <- result.try(dynamic.string(dynamic))

  case bit_array.base64_url_decode(string) {
    Ok(bit_array) -> Ok(bit_array)
    Error(_) ->
      Error([
        DecodeError(
          expected: "A valid base64-url-encoded string",
          found: string,
          path: [],
        ),
      ])
  }
}

// UTILITIES -------------------------------------------------------------------

/// Run a decoder but only keep the result if it satisfies the given predicate.
/// This is how decoders like [`non_negative_int`](#non_negative_int) can be
/// implemented:
///
/// ```gleam
/// import decipher
/// import gleam/dynamic.{type DecodeError, type Decoder, type Dynamic, DecodeError}
///
/// pub fn non_negative_int(dynamic: Dynamic) -> Result(Int, List(DecodeError)) {
///   decipher.when(dynamic.int, is: fn(x) { x >= 0 })(dynamic)
/// }
/// ```
///
pub fn when(decoder: Decoder(a), is predicate: fn(a) -> Bool) -> Decoder(a) {
  fn(dynamic: Dynamic) {
    use value <- result.try(decoder(dynamic))

    case predicate(value) {
      True -> Ok(value)
      False ->
        Error([
          DecodeError(
            expected: "A value that satisfies the predicate",
            found: string.inspect(value),
            path: [],
          ),
        ])
    }
  }
}

/// Occasionally you might find yourself in the situation where a JSON string is
/// embedded in the dynamic value you're trying to decode. This decoder lets you
/// extract that JSON and then run the decoder against it.
///
pub fn json_string(decoder: Decoder(a)) -> Decoder(a) {
  fn(dynamic: Dynamic) {
    use json <- result.try(dynamic.string(dynamic))

    case json.decode(json, decoder) {
      Ok(a) -> Ok(a)
      Error(json.UnexpectedFormat(errors)) -> Error(errors)
      Error(_) ->
        Error([
          DecodeError(
            expected: "A valid JSON-encoded string",
            found: json,
            path: [],
          ),
        ])
    }
  }
}

/// Decode just the keys of an object as a list of strings.
///
pub fn keys(dynamic: Dynamic) -> Result(List(String), List(DecodeError)) {
  dynamic
  |> dynamic.dict(dynamic.string, dynamic.dynamic)
  |> result.map(dict.keys)
}

/// Decode a value at a given index. This decoder is permissive and will try to
/// decode tuples, objects with string integer keys, and in the worst case will
/// decode a list and index into that.
///
/// For strict tuple access, use the [`element`](https://hexdocs.pm/gleam_stdlib/gleam/dynamic.html#element)
/// decoder from the standard library.
///
pub fn index(idx: Int, decoder: Decoder(a)) -> Decoder(a) {
  dynamic.any([
    dynamic.element(idx, decoder),
    dynamic.field(int.to_string(idx), decoder),
    index_list(idx, decoder),
  ])
}

fn index_list(idx: Int, decoder: Decoder(a)) -> Decoder(a) {
  fn(dynamic: Dynamic) {
    use list <- result.try(dynamic.list(dynamic.dynamic)(dynamic))

    case idx >= 0 {
      True ->
        list
        |> list.drop(idx)
        |> list.first
        |> result.replace_error([
          DecodeError(
            expected: "A list with at least"
              <> int.to_string(idx + 1)
              <> "elements",
            found: "A list with"
              <> int.to_string(list.length(list))
              <> "elements",
            path: [int.to_string(idx)],
          ),
        ])
        |> result.then(decoder)
      False ->
        Error([
          DecodeError(
            expected: "An 'index' decoder with a non-negative index",
            found: int.to_string(idx),
            path: [],
          ),
        ])
    }
  }
}

/// Decode a value at a given key path. This decoder is permissive and will use
/// [`index`](#index) for any numeric string keys where possible.
///
pub fn at(path: List(String), decoder: Decoder(a)) -> Decoder(a) {
  fn(dynamic: Dynamic) { do_at(path, decoder, dynamic) }
}

fn do_at(
  path: List(String),
  decoder: Decoder(a),
  dynamic: Dynamic,
) -> Result(a, List(DecodeError)) {
  case path {
    [] -> decoder(dynamic)
    [head, ..rest] -> {
      case int.parse(head) {
        Ok(idx) ->
          dynamic
          |> index(idx, dynamic.dynamic)
          |> result.then(do_at(rest, decoder, _))
        Error(_) ->
          dynamic
          |> dynamic.field(head, dynamic.dynamic)
          |> result.then(do_at(rest, decoder, _))
      }
    }
  }
}
