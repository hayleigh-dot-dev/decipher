import decipher
import gleam/dynamic.{DecodeError}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

type Example {
  Wibble(foo: Int)
  Wobble(bar: String)
}

pub fn tagged_union_test() {
  let decoder =
    decipher.tagged_union(dynamic.element(0, dynamic.string), [
      #("wibble", dynamic.decode1(Wibble, dynamic.element(1, dynamic.int))),
      #("wobble", dynamic.decode1(Wobble, dynamic.element(1, dynamic.string))),
    ])

  dynamic.from(#("wobble", "bar"))
  |> decoder
  |> should.be_ok
  |> should.equal(Wobble(bar: "bar"))

  dynamic.from(#("jelly"))
  |> decoder
  |> should.be_error
  |> should.equal([DecodeError("\"wibble\" | \"wobble\"", "\"jelly\"", [])])
}
