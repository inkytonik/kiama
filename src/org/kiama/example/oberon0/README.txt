Kiama solution to the LDTA Workshop tool challenge 2011.

(http://ldta.info/ldta_2011_tool_challenge.pdf)

Tasks:

T1 - parsing and pretty printing (1)
T2 - name analysis (2)
T3 - type analysis (3)
T4a - desugar (4)
T4b - optimisation (5) (not implemented)
T5a - C code gen (6)
T5b - RISC code gen (not implemented)

(Numbers in parentheses are the level used in the compiler to denote that task.)

Languages:

- base: basic module structure; empty statements
- L0: const, var, type decls; basic types; expressions; assignment stmts
- L1: if and while statements
- L2: for and case statements
- L3: procedures (restricted non-local access to vars)
- L4: arrays and records
- L5: L4 with arbitrary non-local access to vars (not implemented)

- A1:  L2, T1, T2
Core language, with pretty printing and name binding.

- A2a: L3, T1, T2
Add syntax and name binding for procedures to A1

- A2b: L2, T1, T2, T3
Add add type checking to base language to A1

- A3: L3, T1, T2, T3
Combining the components added in A2a and A2b.  Can build on top of
either A2a or A2b.

- A4: L4, T1-T5a
Add data structure constructs and translation to C to A3.

- A5: L5, T1-T5a (not implemented)
