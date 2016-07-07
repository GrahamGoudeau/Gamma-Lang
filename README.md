# Gamma-Lang
An interpreter for an original programming language named Gamma.  Gamma emphasizes (but does not require) functional purity by requiring functions that are side-effecting to bear an `impure` annotation.  Gamma is statically typed and performs type inference.  Variables in Gamma are not mutable, and cannot be reassigned once set.

A to-do list of desired features (in no particular order):
- [ ] Garbage collection
- [ ] Types
  - [ ] Type inference
  - [ ] Type checking
- [ ] First-class functions
- [ ] Function definition pattern matching
- [ ] Tail call optimization
- [ ] Parsing more complicated values:
  - [ ] Real numbers (floating point)
  - [ ] Strings
  - [ ] Tuples

The syntax of Gamma is influenced by Elixir and Standard ML.  A function in Gamma:

    impure function f(x, y, z) =
      let a := x + y
      let b := if z then 3 else 0
      print("line three")
      b
    end

Is equivalent to (in SML):

    fun f x y z =
    let
      val a = x + y
      val b = if z then 3 else 0
      val _ = print "line three"
    in b
    end
