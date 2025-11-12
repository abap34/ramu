include("types.jl")

@enum ExprHead begin
    ConstBool   # args: [value]
    Var         # args: [Symbol]
    FunApply    # args: [fun_name::Symbol, arg1::Symbol, arg2::Symbol, ...]
    Let         # args: [var::Symbol, bound_expr::Expr, body::Expr]
    If          # args: [cond_var::Symbol, then_expr::Expr, else_expr::Expr]
    Unit        # args: []
                # The unit value: *
    Pair        # args: [fst::Symbol, snd::Symbol]
    MatchPair   # args: [var::Symbol, fst_var::Symbol, snd_var::Symbol, body::Expr]
                # match x with (x1, x2) => e
    Inl         # args: [var::Symbol]
    Inr         # args: [var::Symbol]
    MatchSum    # args: [var::Symbol, inl_var::Symbol, inl_expr::Expr, inr_var::Symbol, inr_expr::Expr]
                # match x with | inl(y) => e1 | inr(z) => e2
    Nil         # args: []
    Cons        # args: [head::Symbol, tail::Symbol]
    Match       # args: [var::Symbol, nil_expr::Expr, (cons_h::Symbol, cons_t::Symbol), cons_expr::Expr]
                # Destructive match: deallocates the matched node, freelist grows
    MatchPrime  # args: [var::Symbol, nil_expr::Expr, (cons_h::Symbol, cons_t::Symbol), cons_expr::Expr]
                # Non-destructive match: preserves the matched node, freelist unchanged
end

struct LFExpr
    head::ExprHead
    args::Vector{Any}
end

struct FunctionDef
    params::Vector{Symbol}
    param_types::Vector{LFType}
    return_type::LFType
    body::LFExpr
end

struct Program
    functions::Dict{Symbol, FunctionDef}
end
