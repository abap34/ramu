struct TypeEnv
    types::Dict{Symbol, TypeLike}
end

TypeEnv() = TypeEnv(Dict{Symbol, TypeLike}())

function extend(Γ::TypeEnv, x::Symbol, A::TypeLike)::TypeEnv
    new_types = copy(Γ.types)
    new_types[x] = A
    return TypeEnv(new_types)
end

struct ToplevelEnv
    functions::Dict{Symbol, FuncType}
end

ToplevelEnv() = ToplevelEnv(Dict{Symbol, FuncType}())

struct Constraint
    lhs::TypeLike
    rhs::TypeLike
end

const Subst = Dict{Symbol, TypeLike}
const InferResult = Tuple{TypeLike, Vector{Constraint}}

function apply_subst(s::Subst, t::TypeLike)::TypeLike
    if t isa LFTypeVar
        return haskey(s, t.name) ? apply_subst(s, s[t.name]) : t
    elseif t isa UnitType || t isa BoolType
        return t
    elseif t isa ListType
        return ListType(apply_subst(s, t.elem_type))
    elseif t isa ProductType
        return ProductType(apply_subst(s, t.fst_type), apply_subst(s, t.snd_type))
    elseif t isa SumType
        return SumType(apply_subst(s, t.inl_type), apply_subst(s, t.inr_type))
    elseif t isa FuncType
        return FuncType(
            [apply_subst(s, pt) for pt in t.param_types],
            apply_subst(s, t.return_type)
        )
    else
        error("Unknown type: $(typeof(t))")
    end
end

function apply_subst(s::Subst, c::Constraint)::Constraint
    return Constraint(apply_subst(s, c.lhs), apply_subst(s, c.rhs))
end

function compose(s1::Subst, s2::Subst)::Subst
    result = Dict{Symbol, TypeLike}()
    for (v, t) in s2
        result[v] = apply_subst(s1, t)
    end
    for (v, t) in s1
        if !haskey(result, v)
            result[v] = t
        end
    end
    return result
end

function occurs(tv::Symbol, t::TypeLike)::Bool
    if t isa LFTypeVar
        return t.name == tv
    elseif t isa UnitType || t isa BoolType
        return false
    elseif t isa ListType
        return occurs(tv, t.elem_type)
    elseif t isa ProductType
        return occurs(tv, t.fst_type) || occurs(tv, t.snd_type)
    elseif t isa SumType
        return occurs(tv, t.inl_type) || occurs(tv, t.inr_type)
    elseif t isa FuncType
        return any(occurs(tv, pt) for pt in t.param_types) || occurs(tv, t.return_type)
    else
        return false
    end
end

function unify(t1::TypeLike, t2::TypeLike)::Subst
    if t1 == t2
        return Subst()
    end

    if t1 isa LFTypeVar
        if occurs(t1.name, t2)
            error("Occurs check failed: $(t1.name) occurs in $t2")
        end
        return Subst(t1.name => t2)
    end

    if t2 isa LFTypeVar
        if occurs(t2.name, t1)
            error("Occurs check failed: $(t2.name) occurs in $t1")
        end
        return Subst(t2.name => t1)
    end

    if t1 isa ListType && t2 isa ListType
        return unify(t1.elem_type, t2.elem_type)
    end

    if t1 isa ProductType && t2 isa ProductType
        s1 = unify(t1.fst_type, t2.fst_type)
        s2 = unify(apply_subst(s1, t1.snd_type), apply_subst(s1, t2.snd_type))
        return compose(s2, s1)
    end

    if t1 isa SumType && t2 isa SumType
        s1 = unify(t1.inl_type, t2.inl_type)
        s2 = unify(apply_subst(s1, t1.inr_type), apply_subst(s1, t2.inr_type))
        return compose(s2, s1)
    end

    if t1 isa FuncType && t2 isa FuncType
        if length(t1.param_types) != length(t2.param_types)
            error("Cannot unify function types with different arities: $t1 vs $t2")
        end
        s = Subst()
        for (p1, p2) in zip(t1.param_types, t2.param_types)
            s_new = unify(apply_subst(s, p1), apply_subst(s, p2))
            s = compose(s_new, s)
        end
        s_ret = unify(apply_subst(s, t1.return_type), apply_subst(s, t2.return_type))
        return compose(s_ret, s)
    end

    error("Cannot unify $t1 and $t2")
end

function solve_constraints(constraints::Vector{Constraint})::Subst
    s = Subst()
    for c in constraints
        c_subst = apply_subst(s, c)
        s_new = unify(c_subst.lhs, c_subst.rhs)
        s = compose(s_new, s)
    end
    return s
end

function infer(e::LFExpr, Γ::TypeEnv, Σ::ToplevelEnv)::InferResult
    if e.head == ConstBool
        return (BoolType(), Constraint[])

    elseif e.head == Unit
        return (UnitType(), Constraint[])

    elseif e.head == Var
        x = e.args[1]
        if !haskey(Γ.types, x)
            error("Unbound variable: $x")
        end
        return (Γ.types[x], Constraint[])

    elseif e.head == LFPair
        x1, x2 = e.args[1], e.args[2]
        t1, c1 = infer(LFExpr(Var, [x1]), Γ, Σ)
        t2, c2 = infer(LFExpr(Var, [x2]), Γ, Σ)
        return (ProductType(t1, t2), vcat(c1, c2))

    elseif e.head == Inl
        x = e.args[1]
        t, c = infer(LFExpr(Var, [x]), Γ, Σ)
        α = fresh_typevar()
        return (SumType(t, α), c)

    elseif e.head == Inr
        x = e.args[1]
        t, c = infer(LFExpr(Var, [x]), Γ, Σ)
        α = fresh_typevar()
        return (SumType(α, t), c)

    elseif e.head == Nil
        α = fresh_typevar()
        return (ListType(α), Constraint[])

    elseif e.head == Cons
        xh, xt = e.args[1], e.args[2]
        th, ch = infer(LFExpr(Var, [xh]), Γ, Σ)
        tt, ct = infer(LFExpr(Var, [xt]), Γ, Σ)
        constraints = vcat(ch, ct, [Constraint(tt, ListType(th))])
        return (ListType(th), constraints)

    elseif e.head == Let
        x, e1, e2 = e.args[1], e.args[2], e.args[3]
        t1, c1 = infer(e1, Γ, Σ)
        Γ2 = extend(Γ, x, t1)
        t2, c2 = infer(e2, Γ2, Σ)
        return (t2, vcat(c1, c2))

    elseif e.head == If
        x, et, ef = e.args[1], e.args[2], e.args[3]
        tx, cx = infer(LFExpr(Var, [x]), Γ, Σ)
        tt, ct = infer(et, Γ, Σ)
        tf, cf = infer(ef, Γ, Σ)
        constraints = vcat(
            cx, ct, cf,
            [Constraint(tx, BoolType()), Constraint(tt, tf)]
        )
        return (tt, constraints)

    elseif e.head == MatchPair
        x, x1, x2, body = e.args[1], e.args[2], e.args[3], e.args[4]
        tx, cx = infer(LFExpr(Var, [x]), Γ, Σ)

        α = fresh_typevar()
        β = fresh_typevar()
        expected_pair_type = ProductType(α, β)

        Γ2 = extend(extend(Γ, x1, α), x2, β)
        tbody, cbody = infer(body, Γ2, Σ)

        constraints = vcat(cx, cbody, [Constraint(tx, expected_pair_type)])
        return (tbody, constraints)

    elseif e.head == MatchSum
        x, y_inl, e_inl, y_inr, e_inr = e.args[1], e.args[2], e.args[3], e.args[4], e.args[5]
        tx, cx = infer(LFExpr(Var, [x]), Γ, Σ)

        α = fresh_typevar()
        β = fresh_typevar()
        expected_sum_type = SumType(α, β)

        Γ_inl = extend(Γ, y_inl, α)
        t_inl, c_inl = infer(e_inl, Γ_inl, Σ)

        Γ_inr = extend(Γ, y_inr, β)
        t_inr, c_inr = infer(e_inr, Γ_inr, Σ)

        constraints = vcat(
            cx, c_inl, c_inr,
            [Constraint(tx, expected_sum_type), Constraint(t_inl, t_inr)]
        )
        return (t_inl, constraints)

    elseif e.head == Match
        x, e_nil, (xh, xt), e_cons = e.args[1], e.args[2], e.args[3], e.args[4]
        tx, cx = infer(LFExpr(Var, [x]), Γ, Σ)

        t_nil, c_nil = infer(e_nil, Γ, Σ)

        α = fresh_typevar()
        Γ_cons = extend(extend(Γ, xh, α), xt, ListType(α))
        t_cons, c_cons = infer(e_cons, Γ_cons, Σ)

        constraints = vcat(
            cx, c_nil, c_cons,
            [Constraint(tx, ListType(α)), Constraint(t_nil, t_cons)]
        )
        return (t_nil, constraints)

    elseif e.head == MatchPrime
        x, e_nil, (xh, xt), e_cons = e.args[1], e.args[2], e.args[3], e.args[4]
        tx, cx = infer(LFExpr(Var, [x]), Γ, Σ)

        t_nil, c_nil = infer(e_nil, Γ, Σ)

        α = fresh_typevar()
        Γ_cons = extend(extend(Γ, xh, α), xt, ListType(α))
        t_cons, c_cons = infer(e_cons, Γ_cons, Σ)

        constraints = vcat(
            cx, c_nil, c_cons,
            [Constraint(tx, ListType(α)), Constraint(t_nil, t_cons)]
        )
        return (t_nil, constraints)

    elseif e.head == FunApply
        fname = e.args[1]
        arg_exprs = e.args[2:end]

        if !haskey(Σ.functions, fname)
            error("Undefined function: $fname")
        end

        fsig = Σ.functions[fname]

        if length(arg_exprs) != length(fsig.param_types)
            error("Function $fname expects $(length(fsig.param_types)) arguments, got $(length(arg_exprs))")
        end

        constraints = Constraint[]
        for (arg_expr, expected_type) in zip(arg_exprs, fsig.param_types)
            # Each argument can be an expression or a Symbol
            if arg_expr isa Symbol
                t_arg, c_arg = infer(LFExpr(Var, [arg_expr]), Γ, Σ)
            else
                t_arg, c_arg = infer(arg_expr, Γ, Σ)
            end
            append!(constraints, c_arg)
            push!(constraints, Constraint(t_arg, expected_type))
        end

        return (fsig.return_type, constraints)

    else
        error("Unknown expression head: $(e.head)")
    end
end

function typeinf(e::LFExpr, Γ::TypeEnv = TypeEnv(), Σ::ToplevelEnv = ToplevelEnv())::TypeLike
    t, constraints = infer(e, Γ, Σ)
    subst = solve_constraints(constraints)
    return apply_subst(subst, t)
end


function build_toplevel_env(prog::Program)::ToplevelEnv
    Σ = ToplevelEnv()
    for (fname, func) in prog.functions
        Σ.functions[fname] = FuncType(func.param_types, func.return_type)
    end
    return Σ
end

function typecheck_function(fname::Symbol, func::FunctionDef, Σ::ToplevelEnv)
    if length(func.params) != length(func.param_types)
        error("Function $fname: parameter count ($(length(func.params))) doesn't match type annotation count ($(length(func.param_types)))")
    end

    Γ = TypeEnv(Dict(zip(func.params, func.param_types)))

    t, constraints = infer(func.body, Γ, Σ)
    push!(constraints, Constraint(t, func.return_type))

    subst = solve_constraints(constraints)
    final_type = apply_subst(subst, t)

    if final_type != func.return_type
        error("Function $fname: body type $final_type doesn't match declared return type $(func.return_type)")
    end

    return true
end

function typecheck_program(prog::Program)::Bool
    Σ = build_toplevel_env(prog)

    for (fname, func) in prog.functions
        typecheck_function(fname, func, Σ)
    end

    return true
end
