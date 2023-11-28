module input

import std/os/path
import std/os/file
import std/text/parse

pub fun get-parsed(parsed : parse-error<a>) : exn a
  match parsed.either
    Left(err)      -> err.throw
    Right(success) -> success

pub fun get-input(day : string, parser : () -> <io,parse|e> a) : <io|e> a
  val parsed = read-text-file(cwd()/(day ++ "/input.txt")).slice.parse(parser)
  parsed.get-parsed
