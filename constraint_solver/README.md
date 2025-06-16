The goal of this project is to generate solutions for the number game 24 by framing the problem as constraint solving.
The result is a toy constraint solver DSL that allows us to express this problem and ones like it in a declarative stlye,
letting the DSL handle the computation to generate a solution. The constraint solving DSL is an example of non-deterministic
programming, where evaluation of certain expressions creates "decision points" that can be backtracked to as the program searches
over the solution space for an answer that meets the given constraints. This non-deterministic evaluation is implemented using
continuation passing style based on the amb evaluator in SICP. 
