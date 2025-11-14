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
export TK_LET, TK_IN, TK_IF, TK_THEN, TK_ELSE, TK_MATCH, TK_WITH
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
export InterpreterState, eval_expr, run_program, compute_size, alloc_location

# Export type inference/checking
export TypeEnv, ToplevelEnv, Constraint, Subst, InferResult
export typeinf, typecheck_program, typecheck_function, build_toplevel_env
export fresh_typevar

function @main(ARGS)
    @assert length(ARGS) == 2 "Usage: julia -m ramu <source-file> <heap-size> . Got $ARGS"
    filepath = ARGS[1]
    heap_size = parse(Int, ARGS[2])
    ast = parse_from_file(filepath)
    v, state = run_program(ast, heap_size=heap_size)
    println("Execution Finished.")
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