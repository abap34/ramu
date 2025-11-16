struct InterpreterState
    S::Dict{Symbol, LFVal}
    σ::Dict{Int, LFVal}
    m::Int
end

alloc_location(σ::Dict{Int, LFVal})::Int = isempty(σ) ? 1 : maximum(keys(σ)) + 1

function compute_size(v::LFVal)::Int
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

function eval_expr(
    e::LFExpr,
    state::InterpreterState,
    prog::Program;
    trace_callback::Union{Function, Nothing} = nothing,
    depth::Int = 0
)::Tuple{LFVal, InterpreterState}
    # トレース記録
    !isnothing(trace_callback) && trace_callback(e, state, depth)

    # 再帰用のヘルパー関数（パラメータを自動で引き継ぐ）
    recurse(expr, st) = eval_expr(expr, st, prog; trace_callback, depth=depth+1)

    S, σ, m = state.S, state.σ, state.m

    if e.head == ConstBool
        return (BoolVal(e.args[1]), state)

    elseif e.head == Var
        x = e.args[1]
        if !haskey(S, x)
            error("Undefined variable: $x")
        end
        return (S[x], state)

    elseif e.head == Unit
        return (UnitVal(), state)

    elseif e.head == Nil
        return (NilVal(), state)

    elseif e.head == LFPair
        e1, e2 = e.args[1], e.args[2]
        v1, state1 = recurse(e1, state)
        v2, state2 = recurse(e2, state1)
        return (PairVal(v1, v2), state2)

    elseif e.head == Inl
        e1 = e.args[1]
        v1, state1 = recurse(e1, state)
        return (InlVal(v1), state1)

    elseif e.head == Inr
        e1 = e.args[1]
        v1, state1 = recurse(e1, state)
        return (InrVal(v1), state1)

    elseif e.head == Cons
        eh, et = e.args[1], e.args[2]
        vh, state1 = recurse(eh, state)
        vt, state2 = recurse(et, state1)
        v = PairVal(vh, vt)
        size = compute_size(v)

        σ2, m2 = state2.σ, state2.m
        if m2 < size
            error("Insufficient heap space: need $size, have $m2")
        end

        ℓ = alloc_location(σ2)
        new_σ = copy(σ2)
        new_σ[ℓ] = v
        new_m = m2 - size
        new_state = InterpreterState(state2.S, new_σ, new_m)

        return (LocVal(ℓ), new_state)

    elseif e.head == Let
        x, e1, e2 = e.args[1], e.args[2], e.args[3]
        v1, state1 = recurse(e1, state)
        new_S = copy(state1.S)
        new_S[x] = v1
        new_state = InterpreterState(new_S, state1.σ, state1.m)
        return recurse(e2, new_state)

    elseif e.head == If
        econd, et, ef = e.args[1], e.args[2], e.args[3]
        cond, state1 = recurse(econd, state)
        if !(cond isa BoolVal)
            error("Condition must be a boolean value")
        end
        if cond.value != false
            return recurse(et, state1)
        else
            return recurse(ef, state1)
        end

    elseif e.head == Match
        ematched, e_nil, (xh, xt), e_cons = e.args[1], e.args[2], e.args[3], e.args[4]
        val, state1 = recurse(ematched, state)

        if val isa NilVal
            return recurse(e_nil, state1)
        elseif val isa LocVal
            ℓ = val.loc
            σ1 = state1.σ
            pair = σ1[ℓ]
            if !(pair isa PairVal)
                error("Expected PairVal at location $ℓ")
            end
            vh, vt = pair.fst, pair.snd

            freed_size = compute_size(pair)
            new_σ = copy(σ1)
            delete!(new_σ, ℓ)
            new_m = state1.m + freed_size

            new_S = copy(state1.S)
            new_S[xh] = vh
            new_S[xt] = vt
            new_state = InterpreterState(new_S, new_σ, new_m)

            return recurse(e_cons, new_state)
        else
            error("Match expects NilVal or LocVal, got $(typeof(val))")
        end

    elseif e.head == MatchPrime
        ematched, e_nil, (xh, xt), e_cons = e.args[1], e.args[2], e.args[3], e.args[4]
        val, state1 = recurse(ematched, state)

        if val isa NilVal
            return recurse(e_nil, state1)
        elseif val isa LocVal
            ℓ = val.loc
            σ1 = state1.σ
            pair = σ1[ℓ]
            if !(pair isa PairVal)
                error("Expected PairVal at location $ℓ")
            end
            vh, vt = pair.fst, pair.snd

            new_S = copy(state1.S)
            new_S[xh] = vh
            new_S[xt] = vt
            new_state = InterpreterState(new_S, σ1, state1.m)

            return recurse(e_cons, new_state)
        else
            error("MatchPrime expects NilVal or LocVal, got $(typeof(val))")
        end

    elseif e.head == MatchPair
        ematched, x1, x2, body = e.args[1], e.args[2], e.args[3], e.args[4]
        val, state1 = recurse(ematched, state)

        if !(val isa PairVal)
            error("MatchPair expects PairVal, got $(typeof(val))")
        end

        v1, v2 = val.fst, val.snd
        new_S = copy(state1.S)
        new_S[x1] = v1
        new_S[x2] = v2
        new_state = InterpreterState(new_S, state1.σ, state1.m)

        return recurse(body, new_state)

    elseif e.head == MatchSum
        ematched, y_inl, e_inl, y_inr, e_inr = e.args[1], e.args[2], e.args[3], e.args[4], e.args[5]
        val, state1 = recurse(ematched, state)

        if val isa InlVal
            new_S = copy(state1.S)
            new_S[y_inl] = val.value
            new_state = InterpreterState(new_S, state1.σ, state1.m)
            return recurse(e_inl, new_state)
        elseif val isa InrVal
            new_S = copy(state1.S)
            new_S[y_inr] = val.value
            new_state = InterpreterState(new_S, state1.σ, state1.m)
            return recurse(e_inr, new_state)
        else
            error("MatchSum expects InlVal or InrVal, got $(typeof(val))")
        end

    elseif e.head == FunApply
        f = e.args[1]
        arg_exprs = e.args[2:end]

        if !haskey(prog.functions, f)
            error("Undefined function: $f")
        end

        func_def = prog.functions[f]

        if length(arg_exprs) != length(func_def.params)
            error("Function $f expects $(length(func_def.params)) arguments, got $(length(arg_exprs))")
        end

        # Evaluate arguments and bind to parameters
        new_S = Dict{Symbol, LFVal}()
        current_state = state
        for (param, arg_expr) in zip(func_def.params, arg_exprs)
            # Evaluate the argument expression
            arg_val, new_state = recurse(arg_expr, current_state)
            current_state = new_state
            new_S[param] = arg_val
        end

        func_state = InterpreterState(new_S, current_state.σ, current_state.m)
        result_val, result_state = recurse(func_def.body, func_state)
        # Restore the caller's stack after function call (preserving heap and memory from result)
        final_state = InterpreterState(current_state.S, result_state.σ, result_state.m)
        return (result_val, final_state)

    else
        error("Unkown expression head: $(e.head)")
    end
end

function run_program(
        prog::Program;
        entrypoint::Symbol = :main,
        initial_stack::Dict{Symbol, LFVal} = Dict{Symbol, LFVal}(),
        initial_heap::Dict{Int, LFVal} = Dict{Int, LFVal}(),
        heap_size::Int = 100,
        trace_callback::Union{Function, Nothing} = nothing
    )::Tuple{LFVal, InterpreterState}
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
    return eval_expr(func_def.body, state, prog; trace_callback, depth=0)
end
