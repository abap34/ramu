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

const resourcevarnames = ["n", "m", "k", "l", "p", "q"]
fresh_resource_var = make_fresh_symbol_generator(resourcevarnames)

struct TypeConstraint
    lhs::TypeLike
    rhs::TypeLike
end

struct LinearExpression
    terms::Dict{Symbol, Int}
    constant::Int
end

LinearExpression() = LinearExpression(Dict{Symbol, Int}(), 0)
LinearExpression(c::Int) = LinearExpression(Dict{Symbol, Int}(), c)
LinearExpression(var::Symbol, coef::Int=1) = LinearExpression(Dict(var => coef), 0)

function Base.:(+)(le1::LinearExpression, le2::LinearExpression)
    terms = copy(le1.terms)
    for (var, coef) in le2.terms
        terms[var] = get(terms, var, 0) + coef
    end
    return LinearExpression(terms, le1.constant + le2.constant)
end

function Base.:(-)(le1::LinearExpression, le2::LinearExpression)
    terms = copy(le1.terms)
    for (var, coef) in le2.terms
        terms[var] = get(terms, var, 0) - coef
    end
    return LinearExpression(terms, le1.constant - le2.constant)
end

function Base.:(*)(c::Int, le::LinearExpression)
    terms = Dict(var => c * coef for (var, coef) in le.terms)
    return LinearExpression(terms, c * le.constant)
end

struct ResourceConstraint
    lhs::LinearExpression
    rhs::LinearExpression
end

function Base.show(io::IO, le::LinearExpression)
    parts = String[]

    sorted_vars = sort(collect(keys(le.terms)))
    for var in sorted_vars
        coef = le.terms[var]
        if coef == 0
            continue
        elseif coef == 1
            push!(parts, string(var))
        elseif coef == -1
            push!(parts, string("-", var))
        else
            push!(parts, string(coef, "*", var))
        end
    end

    if le.constant != 0 || isempty(parts)
        push!(parts, string(le.constant))
    end

    if isempty(parts)
        print(io, "0")
        return
    end

    result = parts[1]
    for i in 2:length(parts)
        part = parts[i]
        if startswith(part, "-")
            result *= " - " * part[2:end]
        else
            result *= " + " * part
        end
    end

    print(io, result)
end

function Base.show(io::IO, rc::ResourceConstraint)
    print(io, rc.lhs, " ≤ ", rc.rhs)
end

function print_resource_constraints(constraints::Vector{ResourceConstraint})
    if isempty(constraints)
        println("No resource constraints generated.")
        return
    end

    println("Resource Constraints:")
    println("=" ^ 60)
    for (i, c) in enumerate(constraints)
        println("  [$i] ", c)
    end
    println("=" ^ 60)
end

function compute_size(t::TypeLike)::Int
    if t isa UnitType || t isa BoolType
        return 1
    elseif t isa ListType
        return 1
    elseif t isa ProductType
        return compute_size(t.fst_type) + compute_size(t.snd_type)
    elseif t isa SumType
        return 1 + max(compute_size(t.inl_type), compute_size(t.inr_type))
    elseif t isa LFTypeVar
        return 1
    else
        return 1
    end
end

const Subst = Dict{Symbol, TypeLike}
const InferResult = Tuple{TypeLike, Vector{TypeConstraint}, Vector{ResourceConstraint}}

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

function apply_subst(s::Subst, c::TypeConstraint)::TypeConstraint
    return TypeConstraint(apply_subst(s, c.lhs), apply_subst(s, c.rhs))
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

function solve_constraints(constraints::Vector{TypeConstraint})::Subst
    s = Subst()
    for c in constraints
        c_subst = apply_subst(s, c)
        s_new = unify(c_subst.lhs, c_subst.rhs)
        s = compose(s_new, s)
    end
    return s
end

function infer(e::LFExpr, Γ::TypeEnv, Σ::ToplevelEnv, n_in::Symbol, n_out::Symbol)::InferResult
    if e.head == ConstBool
        rc = ResourceConstraint(LinearExpression(n_out), LinearExpression(n_in))
        return (BoolType(), TypeConstraint[], [rc])

    elseif e.head == Unit
        rc = ResourceConstraint(LinearExpression(n_out), LinearExpression(n_in))
        return (UnitType(), TypeConstraint[], [rc])

    elseif e.head == Var
        x = e.args[1]
        if !haskey(Γ.types, x)
            error("Unbound variable: $x")
        end
        rc = ResourceConstraint(LinearExpression(n_out), LinearExpression(n_in))
        return (Γ.types[x], TypeConstraint[], [rc])

    elseif e.head == LFPair
        x1, x2 = e.args[1], e.args[2]
        e1 = x1 isa Symbol ? LFExpr(Var, [x1]) : x1
        e2 = x2 isa Symbol ? LFExpr(Var, [x2]) : x2
        t1, c1, r1 = infer(e1, Γ, Σ, n_in, n_in)
        t2, c2, r2 = infer(e2, Γ, Σ, n_in, n_in)
        rc = ResourceConstraint(LinearExpression(n_out), LinearExpression(n_in))
        return (ProductType(t1, t2), vcat(c1, c2), vcat(r1, r2, [rc]))

    elseif e.head == Inl
        x = e.args[1]
        ex = x isa Symbol ? LFExpr(Var, [x]) : x
        t, c, r = infer(ex, Γ, Σ, n_in, n_in)
        α = fresh_typevar()
        k_l = fresh_resource_var()
        rc = ResourceConstraint(
            LinearExpression(n_out),
            LinearExpression(n_in) - LinearExpression(k_l)
        )
        return (SumType(t, α), c, vcat(r, [rc]))

    elseif e.head == Inr
        x = e.args[1]
        ex = x isa Symbol ? LFExpr(Var, [x]) : x
        t, c, r = infer(ex, Γ, Σ, n_in, n_in)
        α = fresh_typevar()
        k_r = fresh_resource_var()
        rc = ResourceConstraint(
            LinearExpression(n_out),
            LinearExpression(n_in) - LinearExpression(k_r)
        )
        return (SumType(α, t), c, vcat(r, [rc]))

    elseif e.head == Nil
        α = fresh_typevar()
        rc = ResourceConstraint(LinearExpression(n_out), LinearExpression(n_in))
        return (ListType(α), TypeConstraint[], [rc])

    elseif e.head == Cons
        xh, xt = e.args[1], e.args[2]
        eh = xh isa Symbol ? LFExpr(Var, [xh]) : xh
        et = xt isa Symbol ? LFExpr(Var, [xt]) : xt
        th, ch, rh = infer(eh, Γ, Σ, n_in, n_in)
        tt, ct, rt = infer(et, Γ, Σ, n_in, n_in)

        k = fresh_resource_var()
        size = compute_size(ProductType(th, ListType(th)))

        constraints = vcat(ch, ct, [TypeConstraint(tt, ListType(th))])
        rc = ResourceConstraint(
            LinearExpression(n_out),
            LinearExpression(n_in) - LinearExpression(k) - LinearExpression(size)
        )
        return (ListType(th), constraints, vcat(rh, rt, [rc]))

    elseif e.head == Let
        x, e1, e2 = e.args[1], e.args[2], e.args[3]
        n_mid = fresh_resource_var()

        t1, c1, r1 = infer(e1, Γ, Σ, n_in, n_mid)
        Γ2 = extend(Γ, x, t1)
        t2, c2, r2 = infer(e2, Γ2, Σ, n_mid, n_out)

        return (t2, vcat(c1, c2), vcat(r1, r2))

    elseif e.head == If
        x, et, ef = e.args[1], e.args[2], e.args[3]
        ex = x isa Symbol ? LFExpr(Var, [x]) : x
        tx, cx, rx = infer(ex, Γ, Σ, n_in, n_in)
        tt, ct, rt = infer(et, Γ, Σ, n_in, n_out)
        tf, cf, rf = infer(ef, Γ, Σ, n_in, n_out)

        constraints = vcat(
            cx, ct, cf,
            [TypeConstraint(tx, BoolType()), TypeConstraint(tt, tf)]
        )
        return (tt, constraints, vcat(rx, rt, rf))

    elseif e.head == MatchPair
        x, x1, x2, body = e.args[1], e.args[2], e.args[3], e.args[4]
        ex = x isa Symbol ? LFExpr(Var, [x]) : x
        tx, cx, rx = infer(ex, Γ, Σ, n_in, n_in)

        α = fresh_typevar()
        β = fresh_typevar()
        expected_pair_type = ProductType(α, β)

        Γ2 = extend(extend(Γ, x1, α), x2, β)
        tbody, cbody, rbody = infer(body, Γ2, Σ, n_in, n_out)

        constraints = vcat(cx, cbody, [TypeConstraint(tx, expected_pair_type)])
        return (tbody, constraints, vcat(rx, rbody))

    elseif e.head == MatchSum
        x, y_inl, e_inl, y_inr, e_inr = e.args[1], e.args[2], e.args[3], e.args[4], e.args[5]
        ex = x isa Symbol ? LFExpr(Var, [x]) : x
        tx, cx, rx = infer(ex, Γ, Σ, n_in, n_in)

        α = fresh_typevar()
        β = fresh_typevar()
        expected_sum_type = SumType(α, β)

        k_l = fresh_resource_var()
        k_r = fresh_resource_var()
        n_inl_in = fresh_resource_var()
        n_inr_in = fresh_resource_var()

        Γ_inl = extend(Γ, y_inl, α)
        t_inl, c_inl, r_inl = infer(e_inl, Γ_inl, Σ, n_inl_in, n_out)

        Γ_inr = extend(Γ, y_inr, β)
        t_inr, c_inr, r_inr = infer(e_inr, Γ_inr, Σ, n_inr_in, n_out)

        rc_inl = ResourceConstraint(
            LinearExpression(n_in) + LinearExpression(k_l),
            LinearExpression(n_inl_in)
        )
        rc_inr = ResourceConstraint(
            LinearExpression(n_in) + LinearExpression(k_r),
            LinearExpression(n_inr_in)
        )

        constraints = vcat(
            cx, c_inl, c_inr,
            [TypeConstraint(tx, expected_sum_type), TypeConstraint(t_inl, t_inr)]
        )
        return (t_inl, constraints, vcat(rx, r_inl, r_inr, [rc_inl, rc_inr]))

    elseif e.head == Match
        x, e_nil, (xh, xt), e_cons = e.args[1], e.args[2], e.args[3], e.args[4]
        ex = x isa Symbol ? LFExpr(Var, [x]) : x
        tx, cx, rx = infer(ex, Γ, Σ, n_in, n_in)

        t_nil, c_nil, r_nil = infer(e_nil, Γ, Σ, n_in, n_out)

        α = fresh_typevar()
        k = fresh_resource_var()
        size = compute_size(ProductType(α, ListType(α)))
        n_cons_in = fresh_resource_var()

        Γ_cons = extend(extend(Γ, xh, α), xt, ListType(α))
        t_cons, c_cons, r_cons = infer(e_cons, Γ_cons, Σ, n_cons_in, n_out)

        rc_cons = ResourceConstraint(
            LinearExpression(n_in) + LinearExpression(k) + LinearExpression(size),
            LinearExpression(n_cons_in)
        )

        constraints = vcat(
            cx, c_nil, c_cons,
            [TypeConstraint(tx, ListType(α)), TypeConstraint(t_nil, t_cons)]
        )
        return (t_nil, constraints, vcat(rx, r_nil, r_cons, [rc_cons]))

    elseif e.head == MatchPrime
        x, e_nil, (xh, xt), e_cons = e.args[1], e.args[2], e.args[3], e.args[4]
        ex = x isa Symbol ? LFExpr(Var, [x]) : x
        tx, cx, rx = infer(ex, Γ, Σ, n_in, n_in)

        t_nil, c_nil, r_nil = infer(e_nil, Γ, Σ, n_in, n_out)

        α = fresh_typevar()
        k = fresh_resource_var()
        n_cons_in = fresh_resource_var()

        Γ_cons = extend(extend(Γ, xh, α), xt, ListType(α))
        t_cons, c_cons, r_cons = infer(e_cons, Γ_cons, Σ, n_cons_in, n_out)

        rc_cons = ResourceConstraint(
            LinearExpression(n_in) + LinearExpression(k),
            LinearExpression(n_cons_in)
        )

        constraints = vcat(
            cx, c_nil, c_cons,
            [TypeConstraint(tx, ListType(α)), TypeConstraint(t_nil, t_cons)]
        )
        return (t_nil, constraints, vcat(rx, r_nil, r_cons, [rc_cons]))

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

        constraints = TypeConstraint[]
        resource_constraints = ResourceConstraint[]
        for (arg_expr, expected_type) in zip(arg_exprs, fsig.param_types)
            if arg_expr isa Symbol
                t_arg, c_arg, r_arg = infer(LFExpr(Var, [arg_expr]), Γ, Σ, n_in, n_in)
            else
                t_arg, c_arg, r_arg = infer(arg_expr, Γ, Σ, n_in, n_in)
            end
            append!(constraints, c_arg)
            append!(resource_constraints, r_arg)
            push!(constraints, TypeConstraint(t_arg, expected_type))
        end

        rc = ResourceConstraint(LinearExpression(n_out), LinearExpression(n_in))
        return (fsig.return_type, constraints, vcat(resource_constraints, [rc]))

    else
        error("Unknown expression head: $(e.head)")
    end
end

function typeinf(e::LFExpr, Γ::TypeEnv = TypeEnv(), Σ::ToplevelEnv = ToplevelEnv())::TypeLike
    n_dummy_in = fresh_resource_var()
    n_dummy_out = fresh_resource_var()
    t, constraints, _ = infer(e, Γ, Σ, n_dummy_in, n_dummy_out)
    subst = solve_constraints(constraints)
    return apply_subst(subst, t)
end

function typeinf_with_resources(e::LFExpr, Γ::TypeEnv = TypeEnv(), Σ::ToplevelEnv = ToplevelEnv())
    n_in = fresh_resource_var()
    n_out = fresh_resource_var()
    t, type_constraints, resource_constraints = infer(e, Γ, Σ, n_in, n_out)
    subst = solve_constraints(type_constraints)
    final_type = apply_subst(subst, t)
    return (final_type, resource_constraints)
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

    n_dummy_in = fresh_resource_var()
    n_dummy_out = fresh_resource_var()
    t, constraints, _ = infer(func.body, Γ, Σ, n_dummy_in, n_dummy_out)
    push!(constraints, TypeConstraint(t, func.return_type))

    subst = solve_constraints(constraints)
    final_type = apply_subst(subst, t)

    if final_type != func.return_type
        error("Function $fname: body type $final_type doesn't match declared return type $(func.return_type)")
    end

    return true
end

function typecheck_function_with_resources(fname::Symbol, func::FunctionDef, Σ::ToplevelEnv)
    if length(func.params) != length(func.param_types)
        error("Function $fname: parameter count ($(length(func.params))) doesn't match type annotation count ($(length(func.param_types)))")
    end

    Γ = TypeEnv(Dict(zip(func.params, func.param_types)))

    n_in = fresh_resource_var()
    n_out = fresh_resource_var()
    t, type_constraints, resource_constraints = infer(func.body, Γ, Σ, n_in, n_out)
    push!(type_constraints, TypeConstraint(t, func.return_type))

    subst = solve_constraints(type_constraints)
    final_type = apply_subst(subst, t)

    if final_type != func.return_type
        error("Function $fname: body type $final_type doesn't match declared return type $(func.return_type)")
    end

    return (true, resource_constraints)
end

function typecheck_program(prog::Program)::Bool
    Σ = build_toplevel_env(prog)

    for (fname, func) in prog.functions
        typecheck_function(fname, func, Σ)
    end

    return true
end

function typecheck_program_with_resources(prog::Program)
    Σ = build_toplevel_env(prog)
    all_constraints = Dict{Symbol, Vector{ResourceConstraint}}()

    for (fname, func) in prog.functions
        _, rcs = typecheck_function_with_resources(fname, func, Σ)
        all_constraints[fname] = rcs
    end

    return (true, all_constraints)
end
