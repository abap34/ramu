module ramu

include("types.jl")
include("ast.jl")
include("lexer.jl")
include("values.jl")
include("parser.jl")
include("interpret.jl")
include("inference.jl")

# Export types
export TypeLike, LFType
export BoolType, UnitType, ListType, ProductType, SumType, LFTypeVar, FuncType

# Export AST types and constructors
export ExprHead, ConstBool, Var, FunApply, Let, If, Unit, LFPair, MatchPair
export Inl, Inr, MatchSum, Nil, Cons, Match, MatchPrime
export LFExpr, FunctionDef, Program

# Export lexer
export Token, TokenType
export TK_LET, TK_IN, TK_IF, TK_THEN, TK_ELSE, TK_MATCH, TK_MATCH_PRIME, TK_WITH
export TK_NIL, TK_CONS, TK_INL, TK_INR, TK_TRUE, TK_FALSE, TK_LIST
export TK_BOOL_TYPE, TK_UNIT_TYPE
export TK_LPAREN, TK_RPAREN, TK_COMMA, TK_PIPE, TK_ARROW, TK_COLON, TK_RARROW
export TK_TENSOR, TK_PLUS, TK_EQUAL, TK_STAR, TK_IDENT, TK_EOF
export tokenize

# Export parser
export Parser, parse_type, parse_expr, parse_program, parse_from_file

# Export values
export LFVal, BoolVal, UnitVal, NilVal, LocVal, PairVal, InlVal, InrVal

# Export interpreter
export InterpreterState, eval_expr, run_program, alloc_location

# Export type inference/checking
export TypeEnv, ToplevelEnv, Constraint, Subst, InferResult
export typeinf, typecheck_program, typecheck_function, build_toplevel_env
export fresh_typevar, fresh_resource_var, make_fresh_symbol_generator
export LinearExpression, ResourceConstraint, compute_size
export typeinf_with_resources, typecheck_function_with_resources
export print_resource_constraints, typecheck_program_with_resources

function @main(ARGS)
    @assert length(ARGS) <= 2 "Usage: julia -m ramu <source-file> (<heap-size>)\n Got $ARGS"
    filepath = ARGS[1]
    heap_size = length(ARGS) == 2 ? parse(Int, ARGS[2]) : 1000

    println("="^60)
    println("Parsing and Type Checking: $filepath")
    println("="^60)

    ast = parse_from_file(filepath)

    println("\nType Checking...")
    _, all_constraints = typecheck_program_with_resources(ast)
    println("✓ Type checking passed")

    println("\n" * "="^60)
    println("Resource Constraints Analysis")
    println("="^60)

    for (fname, constraints) in all_constraints
        println("\nFunction: $fname")
        print_resource_constraints(constraints)
    end

    println("\n" * "="^60)
    println("Program Execution")
    println("="^60)

    v, state = run_program(ast, heap_size=heap_size)

    println("\nExecution Finished.")
    println("Final Value: ", v)
    println("="^40)
    println("Final Stack:")
    for (k, val) in state.S
        println("  $k => $val")
    end
    println("="^40)
    println("Final Heap:")
    for (addr, val) in state.σ
        println("  $addr => $val")
    end
    println("(Ram usage: $(heap_size - length(state.σ)) / $heap_size bytes free)")

    return 0
end

end
