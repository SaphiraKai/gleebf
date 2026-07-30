import gleam/bit_array
import gleam/int
import gleam/io
import gleam/list
import gleam/string

import input
import iv.{type Array}

pub type Memory {
  Memory(data: Array(Int), pointer: Int)
}

pub type Command {
  Right
  Left
  Increment
  Decrement
  Read
  Print
  BeginLoop
  EndLoop
}

pub type Program =
  List(Command)

pub const memory_size: Int = 30_000

pub fn execute(program: Program) -> #(String, Memory) {
  let memory = Memory(data: iv.repeat(0, memory_size), pointer: 0)

  do_execute(program, memory, "")
}

fn do_execute(
  program: Program,
  memory: Memory,
  acc: String,
) -> #(String, Memory) {
  case program {
    // end of program
    [] -> #(acc, memory)

    // next instruction
    [next, ..program] ->
      case next {
        // move data pointer right (>)
        Right -> {
          let memory =
            Memory(..memory, pointer: { memory.pointer + 1 } % memory_size)

          do_execute(program, memory, acc)
        }

        // move data pointer left (<)
        Left -> {
          let memory =
            Memory(..memory, pointer: { memory.pointer - 1 } % memory_size)

          do_execute(program, memory, acc)
        }

        // increment current cell (+)
        Increment -> {
          let assert Ok(data) =
            iv.update(memory.data, memory.pointer, fn(i) { { i + 1 } % 256 })
          let memory = Memory(..memory, data:)

          do_execute(program, memory, acc)
        }

        // decrement current cell (-)
        Decrement -> {
          let assert Ok(data) =
            iv.update(memory.data, memory.pointer, fn(i) {
              case { i - 1 } % 256 {
                i if i < 0 -> i + 256
                i -> i
              }
            })
          let memory = Memory(..memory, data:)

          do_execute(program, memory, acc)
        }

        // read a byte from stdin to the current cell (,)
        // 
        // note: trailing bytes after the first byte per line are truncated
        Read -> {
          case input.input("> ") {
            Error(_) -> panic as "failed to read input"

            Ok(char) ->
              case bit_array.from_string(char) {
                <<value:8, _:bits>> -> {
                  let assert Ok(data) =
                    iv.set(memory.data, memory.pointer, value)
                  let memory = Memory(..memory, data:)

                  do_execute(program, memory, acc)
                }

                _ -> panic as "invalid input"
              }
          }
        }

        // print the value of the current cell (.)
        Print -> {
          let assert Ok(cell) = iv.get(memory.data, memory.pointer)
          let assert Ok(codepoint) = string.utf_codepoint(cell)

          let char = string.from_utf_codepoints([codepoint])

          do_execute(program, memory, acc <> char)
        }

        // enter a while-not-zero loop ([)
        BeginLoop -> {
          let assert Ok(cell) = iv.get(memory.data, memory.pointer)

          case cell {
            // exit condition reached, skip to end of loop body and start executing from there
            0 -> exit_loop(program, 0, do_execute(_, memory, acc))

            // enter loop
            _ -> {
              // extract the section of the program that is contained within this loop
              let within_loop = enter_loop(program, 0, [])

              // execute until the end of the loop body
              let #(acc, memory) = do_execute(within_loop, memory, acc)

              // return to the beginning of the loop
              do_execute([BeginLoop, ..program], memory, acc)
            }
          }
        }

        // exit loop
        //
        // note: the previous case will always consume the end of the loop, so it
        // doesn't need to be handled here
        EndLoop -> panic as "unexpected EndLoop"
      }
  }
}

pub fn enter_loop(program: Program, depth: Int, acc: Program) -> Program {
  case program {
    [] -> []
    [next, ..program] ->
      case next {
        // end of this loop
        EndLoop if depth == 0 -> list.reverse(acc)

        // end of child loop
        EndLoop -> enter_loop(program, depth - 1, [EndLoop, ..acc])

        // beginning of child loop
        BeginLoop -> enter_loop(program, depth + 1, [BeginLoop, ..acc])

        // all other instructions get added to the accumulator
        _ -> enter_loop(program, depth, [next, ..acc])
      }
  }
}

pub fn exit_loop(
  program: Program,
  depth: Int,
  continue: fn(Program) -> a,
) -> a {
  case program {
    [] -> panic as "unterminated loop"
    [next, ..program] ->
      case next {
        // end of this loop
        EndLoop if depth == 0 -> continue(program)

        // end of child loop
        EndLoop -> exit_loop(program, depth - 1, continue)

        // beginning of child loop
        BeginLoop -> exit_loop(program, depth + 1, continue)

        // all other instructions get skipped
        _ -> exit_loop(program, depth, continue)
      }
  }
}

pub fn to_string(program: Program) -> String {
  case program {
    [] -> ""
    [next, ..program] ->
      case next {
        Right -> ">"
        Left -> "<"
        Increment -> "+"
        Decrement -> "-"
        Read -> ","
        Print -> "."
        BeginLoop -> "["
        EndLoop -> "]"
      }
      <> to_string(program)
  }
}

pub fn from_string(program: String) -> List(Command) {
  do_from_string(program, [])
}

pub fn do_from_string(program: String, acc: Program) -> List(Command) {
  case program {
    "" -> list.reverse(acc)

    _ -> {
      let #(command, program) = case program {
        ">" <> program -> #(Right, program)
        "<" <> program -> #(Left, program)
        "+" <> program -> #(Increment, program)
        "-" <> program -> #(Decrement, program)
        "," <> program -> #(Read, program)
        "." <> program -> #(Print, program)
        "[" <> program -> #(BeginLoop, program)
        "]" <> program -> #(EndLoop, program)

        // yes yes i know other characters are supposed to be treated as comments but i'm LAZY
        _ -> panic as "invalid program"
      }

      do_from_string(program, [command, ..acc])
    }
  }
}

pub fn main() -> Nil {
  let assert Ok(code) = input.input("Enter a brainfuck program: ")
  let program = from_string(code)

  let #(output, memory) = execute(program)

  io.println("---\n\n" <> string.inspect(output))

  // skip printing like 30,000 trailing empty cells
  let memory_data =
    iv.to_list(memory.data)
    |> list.reverse
    |> list.drop_while(fn(c) { c == 0 })
    |> list.reverse
    |> string.inspect

  io.println("memory:  " <> memory_data)
  io.println("pointer: " <> int.to_string(memory.pointer))
}
