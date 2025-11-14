@enum ExprHead begin
    ConstBool   # args: [value]
    Var         # args: [Symbol]
    FunApply    # args: [fun_name::Symbol, arg1::Expr, arg2::Expr, ...]
    Let         # args: [var::Symbol, bound_expr::Expr, body::Expr]
    If          # args: [cond_expr::Expr, then_expr::Expr, else_expr::Expr]
    Unit        # args: []
    # The unit value: *
    LFPair      # args: [fst::Expr, snd::Expr]
    MatchPair   # args: [matched_expr::Expr, fst_var::Symbol, snd_var::Symbol, body::Expr]
    # match expr with (x1, x2) => e
    Inl         # args: [expr::Expr]
    Inr         # args: [expr::Expr]
    MatchSum    # args: [matched_expr::Expr, inl_var::Symbol, inl_expr::Expr, inr_var::Symbol, inr_expr::Expr]
    # match expr with | inl(y) => e1 | inr(z) => e2
    Nil         # args: []
    Cons        # args: [head::Expr, tail::Expr]
    Match       # args: [matched_expr::Expr, nil_expr::Expr, (cons_h::Symbol, cons_t::Symbol), cons_expr::Expr]
    # Destructive match: deallocates the matched node, freelist grows
    MatchPrime  # args: [matched_expr::Expr, nil_expr::Expr, (cons_h::Symbol, cons_t::Symbol), cons_expr::Expr]
    # Non-destructive match: preserves the matched node, freelist unchanged
end

struct LFExpr
    head::ExprHead
    args::Vector{Any}
end

struct FunctionDef
    params::Vector{Symbol}
    param_types::Vector{TypeLike}
    return_type::TypeLike
    body::LFExpr
end

# Constructor without type annotations (for interpreter tests)
function FunctionDef(params::Vector{Symbol}, body::LFExpr)
    # Create dummy type variables for untyped functions
    param_types = TypeLike[LFTypeVar(gensym()) for _ in params]
    return_type = LFTypeVar(gensym())
    return FunctionDef(params, param_types, return_type, body)
end

struct Program
    functions::Dict{Symbol, FunctionDef}
end
