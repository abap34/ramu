include("ast.jl")
include("values.jl")

struct InterpreterState
    S::Dict{Symbol, Val}
    σ::Dict{Int, Val}
    m::Int
end

function alloc_location(σ::Dict{Int, Val})::Int
    isempty(σ) ? 1 : maximum(keys(σ)) + 1
end

function compute_size(v::Val)::Int
    if v isa BoolVal || v isa UnitVal || v isa NilVal || v isa LocVal
        return 1
    elseif v isa PairVal
        return compute_size(v.fst) + compute_size(v.snd)
    elseif v isa InlVal || v isa InrVal
        return 1 + compute_size(v.value)
    else
        error("Unknown value type: $(typeof(v))")
    end
end

function eval_expr(e::LFExpr, state::InterpreterState, prog::Program)::Tuple{Val, InterpreterState}
    S, σ, m = state.S, state.σ, state.m

    if e.head == ConstBool
        return (BoolVal(e.args[1]), state)

    elseif e.head == Var
        x = e.args[1]
        return (S[x], state)

    elseif e.head == Unit
        return (UnitVal(), state)

    elseif e.head == Nil
        return (NilVal(), state)

    elseif e.head == Pair
        x1, x2 = e.args[1], e.args[2]
        return (PairVal(S[x1], S[x2]), state)

    elseif e.head == Inl
        x = e.args[1]
        return (InlVal(S[x]), state)

    elseif e.head == Inr
        x = e.args[1]
        return (InrVal(S[x]), state)

    elseif e.head == Cons
        xh, xt = e.args[1], e.args[2]
        v = PairVal(S[xh], S[xt])
        size = compute_size(v)

        if m < size
            error("Insufficient heap space: need $size, have $m")
        end

        ℓ = alloc_location(σ)
        new_σ = copy(σ)
        new_σ[ℓ] = v
        new_m = m - size
        new_state = InterpreterState(S, new_σ, new_m)

        return (LocVal(ℓ), new_state)

    elseif e.head == Let
        x, e1, e2 = e.args[1], e.args[2], e.args[3]
        v1, state1 = eval_expr(e1, state, prog)
        new_S = copy(state1.S)
        new_S[x] = v1
        new_state = InterpreterState(new_S, state1.σ, state1.m)
        return eval_expr(e2, new_state, prog)

    elseif e.head == If
        x, et, ef = e.args[1], e.args[2], e.args[3]
        cond = S[x]
        if !(cond isa BoolVal)
            error("Condition must be a boolean value")
        end
        if cond.value != false
            return eval_expr(et, state, prog)
        else
            return eval_expr(ef, state, prog)
        end

    elseif e.head == Match
        x, e_nil, (xh, xt), e_cons = e.args[1], e.args[2], e.args[3], e.args[4]
        val = S[x]

        if val isa NilVal
            return eval_expr(e_nil, state, prog)
        elseif val isa LocVal
            ℓ = val.loc
            pair = σ[ℓ]
            if !(pair isa PairVal)
                error("Expected PairVal at location $ℓ")
            end
            vh, vt = pair.fst, pair.snd

            freed_size = compute_size(pair)
            new_σ = copy(σ)
            delete!(new_σ, ℓ)
            new_m = m + freed_size

            new_S = copy(S)
            new_S[xh] = vh
            new_S[xt] = vt
            new_state = InterpreterState(new_S, new_σ, new_m)

            return eval_expr(e_cons, new_state, prog)
        else
            error("Match expects NilVal or LocVal, got $(typeof(val))")
        end

    elseif e.head == MatchPrime
        x, e_nil, (xh, xt), e_cons = e.args[1], e.args[2], e.args[3], e.args[4]
        val = S[x]

        if val isa NilVal
            return eval_expr(e_nil, state, prog)
        elseif val isa LocVal
            ℓ = val.loc
            pair = σ[ℓ]
            if !(pair isa PairVal)
                error("Expected PairVal at location $ℓ")
            end
            vh, vt = pair.fst, pair.snd

            new_S = copy(S)
            new_S[xh] = vh
            new_S[xt] = vt
            new_state = InterpreterState(new_S, σ, m)

            return eval_expr(e_cons, new_state, prog)
        else
            error("MatchPrime expects NilVal or LocVal, got $(typeof(val))")
        end

    elseif e.head == MatchPair
        x, x1, x2, body = e.args[1], e.args[2], e.args[3], e.args[4]
        val = S[x]

        if !(val isa PairVal)
            error("MatchPair expects PairVal, got $(typeof(val))")
        end

        v1, v2 = val.fst, val.snd
        new_S = copy(S)
        new_S[x1] = v1
        new_S[x2] = v2
        new_state = InterpreterState(new_S, σ, m)

        return eval_expr(body, new_state, prog)

    elseif e.head == MatchSum
        x, y_inl, e_inl, y_inr, e_inr = e.args[1], e.args[2], e.args[3], e.args[4], e.args[5]
        val = S[x]

        if val isa InlVal
            new_S = copy(S)
            new_S[y_inl] = val.value
            new_state = InterpreterState(new_S, σ, m)
            return eval_expr(e_inl, new_state, prog)
        elseif val isa InrVal
            new_S = copy(S)
            new_S[y_inr] = val.value
            new_state = InterpreterState(new_S, σ, m)
            return eval_expr(e_inr, new_state, prog)
        else
            error("MatchSum expects InlVal or InrVal, got $(typeof(val))")
        end

    elseif e.head == FunApply
        f = e.args[1]
        arg_names = e.args[2:end]

        if !haskey(prog.functions, f)
            error("Undefined function: $f")
        end

        func_def = prog.functions[f]

        if length(arg_names) != length(func_def.params)
            error("Function $f expects $(length(func_def.params)) arguments, got $(length(arg_names))")
        end

        new_S = Dict{Symbol, Val}()
        for (param, arg_name) in zip(func_def.params, arg_names)
            if !haskey(S, arg_name)
                error("Undefined variable: $arg_name")
            end
            new_S[param] = S[arg_name]
        end

        new_state = InterpreterState(new_S, σ, m)
        return eval_expr(func_def.body, new_state, prog)

    else
        error("Unkown expression head: $(e.head)")
    end
end

function run_program(
    prog::Program;
    entrypoint::Symbol = :main,
    initial_stack::Dict{Symbol, Val} = Dict{Symbol, Val}(),
    initial_heap::Dict{Int, Val} = Dict{Int, Val}(),
    heap_size::Int = 100
)::Tuple{Val, InterpreterState}
    if !haskey(prog.functions, entrypoint)
        error("Function $entrypoint not found in program")
    end

    func_def = prog.functions[entrypoint]

    for param in func_def.params
        if !haskey(initial_stack, param)
            error("Missing argument for parameter $param")
        end
    end

    state = InterpreterState(initial_stack, initial_heap, heap_size)
    return eval_expr(func_def.body, state, prog)
end


