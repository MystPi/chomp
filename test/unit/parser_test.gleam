// IMPORTS ---------------------------------------------------------------------

import gleam/io
import gleam/list
import gleeunit/should
import chomp.{type Parser, do, return}
import chomp/lexer.{type Token}

// STRESS TESTS ----------------------------------------------------------------

pub fn do_tco_test() {
  use run <- should("Not stack overflow with many chained `do` calls")
  let input = []
  let expected = 0
  let parser =
    list.repeat(return, 1_000_000)
    |> list.fold(return(0), do)

  run(input, expected, parser)
}

// UTILS -----------------------------------------------------------------------

fn should(
  description: String,
  run: fn(fn(List(Token(tok)), a, Parser(a, e, tok, ctx)) -> Nil) -> Nil,
) -> Nil {
  use input, expected, parser <- run

  io.print("should " <> description)

  chomp.run(input, parser)
  |> should.be_ok
  |> should.equal(expected)

  io.println(" ✅")
}
