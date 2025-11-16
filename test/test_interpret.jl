@testset "Values" begin
    @testset "BoolVal" begin
        v = BoolVal(true)
        @test v.value == true
        @test compute_size(v) == 1
    end

    @testset "UnitVal" begin
        v = UnitVal()
        @test compute_size(v) == 1
    end

    @testset "NilVal" begin
        v = NilVal()
        @test compute_size(v) == 1
    end

    @testset "LocVal" begin
        v = LocVal(5)
        @test v.loc == 5
        @test compute_size(v) == 1
    end

    @testset "PairVal" begin
        v1 = BoolVal(true)
        v2 = BoolVal(false)
        pair = PairVal(v1, v2)
        @test pair.fst == v1
        @test pair.snd == v2
        @test compute_size(pair) == 2
    end

    @testset "InlVal" begin
        v = InlVal(BoolVal(true))
        @test v.value == BoolVal(true)
        @test compute_size(v) == 2
    end

    @testset "InrVal" begin
        v = InrVal(BoolVal(true))
        @test v.value == BoolVal(true)
        @test compute_size(v) == 2
    end
end

@testset "Heap allocation" begin
    @testset "alloc_location - empty heap" begin
        σ = Dict{Int, LFVal}()
        @test alloc_location(σ) == 1
    end

    @testset "alloc_location - non-empty heap" begin
        σ = Dict{Int, LFVal}(1 => NilVal(), 3 => NilVal())
        @test alloc_location(σ) == 4
    end
end

@testset "Basic expressions" begin
    prog = Program(Dict{Symbol, FunctionDef}())

    @testset "ConstBool" begin
        e = LFExpr(ConstBool, [true])
        state = InterpreterState(Dict{Symbol, LFVal}(), Dict{Int, LFVal}(), 100)
        v, new_state = eval_expr(e, state, prog)
        @test v == BoolVal(true)
        @test new_state.m == 100

        e_false = LFExpr(ConstBool, [false])
        v, new_state = eval_expr(e_false, state, prog)
        @test v == BoolVal(false)
        @test new_state.m == 100
    end

    @testset "Unit" begin
        e = LFExpr(Unit, [])
        state = InterpreterState(Dict{Symbol, LFVal}(), Dict{Int, LFVal}(), 100)
        v, new_state = eval_expr(e, state, prog)
        @test v == UnitVal()
        @test new_state.m == 100
    end

    @testset "Nil" begin
        e = LFExpr(Nil, [])
        state = InterpreterState(Dict{Symbol, LFVal}(), Dict{Int, LFVal}(), 100)
        v, new_state = eval_expr(e, state, prog)
        @test v == NilVal()
        @test new_state.m == 100
    end

    @testset "Var" begin
        e = LFExpr(Var, [:x])
        S = Dict{Symbol, LFVal}(:x => BoolVal(true))
        state = InterpreterState(S, Dict{Int, LFVal}(), 100)
        v, new_state = eval_expr(e, state, prog)
        @test v == BoolVal(true)
    end

    @testset "Pair" begin
        e = LFExpr(LFPair, [LFExpr(Var, [:x]), LFExpr(Var, [:y])])
        S = Dict{Symbol, LFVal}(:x => BoolVal(true), :y => BoolVal(false))
        state = InterpreterState(S, Dict{Int, LFVal}(), 100)
        v, new_state = eval_expr(e, state, prog)
        @test v isa PairVal
        @test v.fst == BoolVal(true)
        @test v.snd == BoolVal(false)
    end

    @testset "Inl" begin
        e = LFExpr(Inl, [LFExpr(Var, [:x])])
        S = Dict{Symbol, LFVal}(:x => BoolVal(true))
        state = InterpreterState(S, Dict{Int, LFVal}(), 100)
        v, new_state = eval_expr(e, state, prog)
        @test v isa InlVal
        @test v.value == BoolVal(true)
    end

    @testset "Inr" begin
        e = LFExpr(Inr, [LFExpr(Var, [:x])])
        S = Dict{Symbol, LFVal}(:x => BoolVal(true))
        state = InterpreterState(S, Dict{Int, LFVal}(), 100)
        v, new_state = eval_expr(e, state, prog)
        @test v isa InrVal
        @test v.value == BoolVal(true)
    end
end

@testset "Cons and heap allocation" begin
    prog = Program(Dict{Symbol, FunctionDef}())

    @testset "Cons - basic" begin
        e = LFExpr(Cons, [LFExpr(Var, [:h]), LFExpr(Var, [:t])])
        S = Dict{Symbol, LFVal}(:h => BoolVal(true), :t => NilVal())
        state = InterpreterState(S, Dict{Int, LFVal}(), 100)
        v, new_state = eval_expr(e, state, prog)

        @test v isa LocVal
        @test haskey(new_state.σ, v.loc)
        @test new_state.σ[v.loc] isa PairVal
        @test new_state.σ[v.loc].fst == BoolVal(true)
        @test new_state.σ[v.loc].snd == NilVal()
        @test new_state.m < 100
    end

    @testset "Cons - insufficient heap" begin
        e = LFExpr(Cons, [LFExpr(Var, [:h]), LFExpr(Var, [:t])])
        S = Dict{Symbol, LFVal}(:h => BoolVal(true), :t => NilVal())
        state = InterpreterState(S, Dict{Int, LFVal}(), 0)
        @test_throws ErrorException eval_expr(e, state, prog)
    end

    @testset "Cons - multiple allocations" begin
        e1 = LFExpr(Cons, [LFExpr(Var, [:h1]), LFExpr(Var, [:t1])])
        e2 = LFExpr(Cons, [LFExpr(Var, [:h2]), LFExpr(Var, [:t2])])
        S = Dict{Symbol, LFVal}(
            :h1 => BoolVal(true), :t1 => NilVal(),
            :h2 => BoolVal(false), :t2 => NilVal()
        )
        state = InterpreterState(S, Dict{Int, LFVal}(), 100)

        v1, state1 = eval_expr(e1, state, prog)
        state1.S[:h2] = BoolVal(false)
        state1.S[:t2] = NilVal()
        v2, state2 = eval_expr(e2, state1, prog)

        @test v1 isa LocVal
        @test v2 isa LocVal
        @test v1.loc != v2.loc
        @test length(state2.σ) == 2
    end
end

@testset "Let binding" begin
    prog = Program(Dict{Symbol, FunctionDef}())

    @testset "Let - simple" begin
        e = LFExpr(Let, [:x, LFExpr(ConstBool, [true]), LFExpr(Var, [:x])])
        state = InterpreterState(Dict{Symbol, LFVal}(), Dict{Int, LFVal}(), 100)
        v, new_state = eval_expr(e, state, prog)
        @test v == BoolVal(true)
    end

    @testset "Let - nested" begin
        inner_let = LFExpr(Let, [:y, LFExpr(ConstBool, [false]), LFExpr(Var, [:y])])
        outer_let = LFExpr(Let, [:x, LFExpr(ConstBool, [true]), inner_let])
        state = InterpreterState(Dict{Symbol, LFVal}(), Dict{Int, LFVal}(), 100)
        v, new_state = eval_expr(outer_let, state, prog)
        @test v == BoolVal(false)
    end

    @testset "Let - shadowing" begin
        inner = LFExpr(Let, [:x, LFExpr(ConstBool, [false]), LFExpr(Var, [:x])])
        outer = LFExpr(Let, [:x, LFExpr(ConstBool, [true]), inner])
        state = InterpreterState(Dict{Symbol, LFVal}(), Dict{Int, LFVal}(), 100)
        v, new_state = eval_expr(outer, state, prog)
        @test v == BoolVal(false)
    end
end

@testset "If expression" begin
    prog = Program(Dict{Symbol, FunctionDef}())

    @testset "If - true branch" begin
        e = LFExpr(If, [LFExpr(Var, [:cond]), LFExpr(ConstBool, [true]), LFExpr(ConstBool, [false])])
        S = Dict{Symbol, LFVal}(:cond => BoolVal(true))
        state = InterpreterState(S, Dict{Int, LFVal}(), 100)
        v, new_state = eval_expr(e, state, prog)
        @test v == BoolVal(true)
    end

    @testset "If - false branch" begin
        e = LFExpr(If, [LFExpr(Var, [:cond]), LFExpr(ConstBool, [true]), LFExpr(ConstBool, [false])])
        S = Dict{Symbol, LFVal}(:cond => BoolVal(false))
        state = InterpreterState(S, Dict{Int, LFVal}(), 100)
        v, new_state = eval_expr(e, state, prog)
        @test v == BoolVal(false)
    end

    @testset "If - non-boolean condition" begin
        e = LFExpr(If, [LFExpr(Var, [:cond]), LFExpr(ConstBool, [true]), LFExpr(ConstBool, [false])])
        S = Dict{Symbol, LFVal}(:cond => NilVal())
        state = InterpreterState(S, Dict{Int, LFVal}(), 100)
        @test_throws ErrorException eval_expr(e, state, prog)
    end
end

@testset "MatchPair" begin
    prog = Program(Dict{Symbol, FunctionDef}())

    @testset "MatchPair - basic" begin
        body = LFExpr(LFPair, [LFExpr(Var, [:a]), LFExpr(Var, [:b])])
        e = LFExpr(MatchPair, [LFExpr(Var, [:p]), :a, :b, body])
        pair = PairVal(BoolVal(true), BoolVal(false))
        S = Dict{Symbol, LFVal}(:p => pair)
        state = InterpreterState(S, Dict{Int, LFVal}(), 100)
        v, new_state = eval_expr(e, state, prog)

        @test v isa PairVal
        @test v.fst == BoolVal(true)
        @test v.snd == BoolVal(false)
    end

    @testset "MatchPair - invalid value" begin
        body = LFExpr(Var, [:a])
        e = LFExpr(MatchPair, [LFExpr(Var, [:p]), :a, :b, body])
        S = Dict{Symbol, LFVal}(:p => BoolVal(true))
        state = InterpreterState(S, Dict{Int, LFVal}(), 100)
        @test_throws ErrorException eval_expr(e, state, prog)
    end
end

@testset "MatchSum" begin
    prog = Program(Dict{Symbol, FunctionDef}())

    @testset "MatchSum - Inl branch" begin
        e_inl = LFExpr(Var, [:y])
        e_inr = LFExpr(ConstBool, [false])
        e = LFExpr(MatchSum, [LFExpr(Var, [:x]), :y, e_inl, :z, e_inr])
        S = Dict{Symbol, LFVal}(:x => InlVal(BoolVal(true)))
        state = InterpreterState(S, Dict{Int, LFVal}(), 100)
        v, new_state = eval_expr(e, state, prog)

        @test v == BoolVal(true)
    end

    @testset "MatchSum - Inr branch" begin
        e_inl = LFExpr(ConstBool, [true])
        e_inr = LFExpr(Var, [:z])
        e = LFExpr(MatchSum, [LFExpr(Var, [:x]), :y, e_inl, :z, e_inr])
        S = Dict{Symbol, LFVal}(:x => InrVal(BoolVal(false)))
        state = InterpreterState(S, Dict{Int, LFVal}(), 100)
        v, new_state = eval_expr(e, state, prog)

        @test v == BoolVal(false)
    end

    @testset "MatchSum - invalid value" begin
        e_inl = LFExpr(Var, [:y])
        e_inr = LFExpr(Var, [:z])
        e = LFExpr(MatchSum, [LFExpr(Var, [:x]), :y, e_inl, :z, e_inr])
        S = Dict{Symbol, LFVal}(:x => BoolVal(true))
        state = InterpreterState(S, Dict{Int, LFVal}(), 100)
        @test_throws ErrorException eval_expr(e, state, prog)
    end
end

@testset "Match - destructive" begin
    prog = Program(Dict{Symbol, FunctionDef}())

    @testset "Match - nil case" begin
        e_nil = LFExpr(ConstBool, [false])
        e_cons = LFExpr(Var, [:h])
        e = LFExpr(Match, [LFExpr(Var, [:l]), e_nil, (:h, :t), e_cons])
        S = Dict{Symbol, LFVal}(:l => NilVal())
        state = InterpreterState(S, Dict{Int, LFVal}(), 100)
        v, new_state = eval_expr(e, state, prog)

        @test v == BoolVal(false)
        @test new_state.m == 100
    end

    @testset "Match - cons case" begin
        σ = Dict{Int, LFVal}(1 => PairVal(BoolVal(true), NilVal()))
        e_nil = LFExpr(ConstBool, [false])
        e_cons = LFExpr(Var, [:h])
        e = LFExpr(Match, [LFExpr(Var, [:l]), e_nil, (:h, :t), e_cons])
        S = Dict{Symbol, LFVal}(:l => LocVal(1))
        state = InterpreterState(S, σ, 100)
        v, new_state = eval_expr(e, state, prog)

        @test v == BoolVal(true)
        @test !haskey(new_state.σ, 1)
        @test new_state.m == 102
    end

    @testset "Match - heap freed" begin
        σ = Dict{Int, LFVal}(1 => PairVal(BoolVal(true), NilVal()))
        e_nil = LFExpr(ConstBool, [false])
        e_cons = LFExpr(ConstBool, [true])
        e = LFExpr(Match, [LFExpr(Var, [:l]), e_nil, (:h, :t), e_cons])
        S = Dict{Symbol, LFVal}(:l => LocVal(1))
        initial_m = 50
        state = InterpreterState(S, σ, initial_m)
        v, new_state = eval_expr(e, state, prog)

        freed_size = compute_size(PairVal(BoolVal(true), NilVal()))
        @test new_state.m == initial_m + freed_size
    end
end

@testset "MatchPrime - non-destructive" begin
    prog = Program(Dict{Symbol, FunctionDef}())

    @testset "MatchPrime - nil case" begin
        e_nil = LFExpr(ConstBool, [false])
        e_cons = LFExpr(Var, [:h])
        e = LFExpr(MatchPrime, [LFExpr(Var, [:l]), e_nil, (:h, :t), e_cons])
        S = Dict{Symbol, LFVal}(:l => NilVal())
        state = InterpreterState(S, Dict{Int, LFVal}(), 100)
        v, new_state = eval_expr(e, state, prog)

        @test v == BoolVal(false)
        @test new_state.m == 100
    end

    @testset "MatchPrime - cons case" begin
        σ = Dict{Int, LFVal}(1 => PairVal(BoolVal(true), NilVal()))
        e_nil = LFExpr(ConstBool, [false])
        e_cons = LFExpr(Var, [:h])
        e = LFExpr(MatchPrime, [LFExpr(Var, [:l]), e_nil, (:h, :t), e_cons])
        S = Dict{Symbol, LFVal}(:l => LocVal(1))
        state = InterpreterState(S, σ, 100)
        v, new_state = eval_expr(e, state, prog)

        @test v == BoolVal(true)
        @test haskey(new_state.σ, 1)
        @test new_state.m == 100
    end

    @testset "MatchPrime - heap preserved" begin
        σ = Dict{Int, LFVal}(1 => PairVal(BoolVal(true), NilVal()))
        e_nil = LFExpr(ConstBool, [false])
        e_cons = LFExpr(ConstBool, [true])
        e = LFExpr(MatchPrime, [LFExpr(Var, [:l]), e_nil, (:h, :t), e_cons])
        S = Dict{Symbol, LFVal}(:l => LocVal(1))
        initial_m = 50
        state = InterpreterState(S, σ, initial_m)
        v, new_state = eval_expr(e, state, prog)

        @test new_state.m == initial_m
        @test haskey(new_state.σ, 1)
        @test new_state.σ[1] == PairVal(BoolVal(true), NilVal())
    end
end

@testset "FunApply" begin
    @testset "FunApply - identity function" begin
        id_body = LFExpr(Var, [:x])
        id_func = FunctionDef([:x], id_body)
        prog = Program(Dict(:id => id_func))

        e = LFExpr(FunApply, [:id, LFExpr(Var, [:arg])])
        S = Dict{Symbol, LFVal}(:arg => BoolVal(true))
        state = InterpreterState(S, Dict{Int, LFVal}(), 100)
        v, new_state = eval_expr(e, state, prog)

        @test v == BoolVal(true)
    end

    @testset "FunApply - constant function" begin
        const_body = LFExpr(ConstBool, [false])
        const_func = FunctionDef([:x], const_body)
        prog = Program(Dict(:const => const_func))

        e = LFExpr(FunApply, [:const, LFExpr(Var, [:arg])])
        S = Dict{Symbol, LFVal}(:arg => BoolVal(true))
        state = InterpreterState(S, Dict{Int, LFVal}(), 100)
        v, new_state = eval_expr(e, state, prog)

        @test v == BoolVal(false)
    end

    @testset "FunApply - two arguments" begin
        fst_body = LFExpr(Var, [:x])
        fst_func = FunctionDef([:x, :y], fst_body)
        prog = Program(Dict(:fst => fst_func))

        e = LFExpr(FunApply, [:fst, LFExpr(Var, [:a]), LFExpr(Var, [:b])])
        S = Dict{Symbol, LFVal}(:a => BoolVal(true), :b => BoolVal(false))
        state = InterpreterState(S, Dict{Int, LFVal}(), 100)
        v, new_state = eval_expr(e, state, prog)

        @test v == BoolVal(true)
    end

    @testset "FunApply - undefined function" begin
        prog = Program(Dict{Symbol, FunctionDef}())
        e = LFExpr(FunApply, [:foo, LFExpr(Var, [:x])])
        S = Dict{Symbol, LFVal}(:x => BoolVal(true))
        state = InterpreterState(S, Dict{Int, LFVal}(), 100)

        @test_throws ErrorException eval_expr(e, state, prog)
    end

    @testset "FunApply - wrong number of arguments" begin
        id_body = LFExpr(Var, [:x])
        id_func = FunctionDef([:x], id_body)
        prog = Program(Dict(:id => id_func))

        e = LFExpr(FunApply, [:id, LFExpr(Var, [:a]), LFExpr(Var, [:b])])
        S = Dict{Symbol, LFVal}(:a => BoolVal(true), :b => BoolVal(false))
        state = InterpreterState(S, Dict{Int, LFVal}(), 100)

        @test_throws ErrorException eval_expr(e, state, prog)
    end

    @testset "FunApply - undefined variable" begin
        id_body = LFExpr(Var, [:x])
        id_func = FunctionDef([:x], id_body)
        prog = Program(Dict(:id => id_func))

        e = LFExpr(FunApply, [:id, LFExpr(Var, [:undefined_var])])
        S = Dict{Symbol, LFVal}()
        state = InterpreterState(S, Dict{Int, LFVal}(), 100)

        @test_throws ErrorException eval_expr(e, state, prog)
    end
end

@testset "Complex programs" begin
    @testset "Length function (using MatchPrime)" begin
        length_body = LFExpr(MatchPrime, [
            LFExpr(Var, [:l]),
            LFExpr(ConstBool, [false]),
            (:h, :t),
            LFExpr(Let, [
                :rest_len,
                LFExpr(FunApply, [:length, LFExpr(Var, [:t])]),
                LFExpr(ConstBool, [true])
            ])
        ])
        length_func = FunctionDef([:l], length_body)
        prog = Program(Dict(:length => length_func))

        σ = Dict{Int, LFVal}(
            1 => PairVal(BoolVal(true), LocVal(2)),
            2 => PairVal(BoolVal(false), NilVal())
        )
        S = Dict{Symbol, LFVal}(:l => LocVal(1))
        state = InterpreterState(S, σ, 100)
        v, new_state = eval_expr(length_body, state, prog)

        @test v == BoolVal(true)
        @test haskey(new_state.σ, 1)
        @test haskey(new_state.σ, 2)
        @test new_state.m == 100
    end

    @testset "Sum of pair" begin
        sum_pair_body = LFExpr(MatchPair, [
            LFExpr(Var, [:p]),
            :a,
            :b,
            LFExpr(Var, [:a])
        ])
        sum_pair_func = FunctionDef([:p], sum_pair_body)
        prog = Program(Dict(:sum_pair => sum_pair_func))

        pair = PairVal(BoolVal(true), BoolVal(false))
        S = Dict{Symbol, LFVal}(:p => pair)
        state = InterpreterState(S, Dict{Int, LFVal}(), 100)

        e = LFExpr(FunApply, [:sum_pair, LFExpr(Var, [:p])])
        v, new_state = eval_expr(e, state, prog)

        @test v == BoolVal(true)
    end

    @testset "Either unwrap (left)" begin
        unwrap_body = LFExpr(MatchSum, [
            LFExpr(Var, [:either]),
            :left_val,
            LFExpr(Var, [:left_val]),
            :right_val,
            LFExpr(Var, [:right_val])
        ])
        unwrap_func = FunctionDef([:either], unwrap_body)
        prog = Program(Dict(:unwrap => unwrap_func))

        either = InlVal(BoolVal(true))
        S = Dict{Symbol, LFVal}(:either => either)
        state = InterpreterState(S, Dict{Int, LFVal}(), 100)

        e = LFExpr(FunApply, [:unwrap, LFExpr(Var, [:either])])
        v, new_state = eval_expr(e, state, prog)

        @test v == BoolVal(true)
    end

    @testset "Either unwrap (right)" begin
        unwrap_body = LFExpr(MatchSum, [
            LFExpr(Var, [:either]),
            :left_val,
            LFExpr(Var, [:left_val]),
            :right_val,
            LFExpr(Var, [:right_val])
        ])
        unwrap_func = FunctionDef([:either], unwrap_body)
        prog = Program(Dict(:unwrap => unwrap_func))

        either = InrVal(BoolVal(false))
        S = Dict{Symbol, LFVal}(:either => either)
        state = InterpreterState(S, Dict{Int, LFVal}(), 100)

        e = LFExpr(FunApply, [:unwrap, LFExpr(Var, [:either])])
        v, new_state = eval_expr(e, state, prog)

        @test v == BoolVal(false)
    end
end

@testset "Memory management" begin
    @testset "Heap allocation tracking" begin
        prog = Program(Dict{Symbol, FunctionDef}())

        S = Dict{Symbol, LFVal}(:h => BoolVal(true), :t => NilVal())
        state = InterpreterState(S, Dict{Int, LFVal}(), 10)

        e = LFExpr(Cons, [LFExpr(Var, [:h]), LFExpr(Var, [:t])])
        initial_m = state.m
        v, new_state = eval_expr(e, state, prog)

        @test new_state.m < initial_m
        @test new_state.m == initial_m - compute_size(new_state.σ[v.loc])
    end

    @testset "Heap deallocation tracking (Match)" begin
        prog = Program(Dict{Symbol, FunctionDef}())

        pair_val = PairVal(BoolVal(true), NilVal())
        σ = Dict{Int, LFVal}(1 => pair_val)
        S = Dict{Symbol, LFVal}(:l => LocVal(1))
        state = InterpreterState(S, σ, 50)

        e = LFExpr(Match, [LFExpr(Var, [:l]), LFExpr(ConstBool, [false]), (:h, :t), LFExpr(ConstBool, [true])])
        initial_m = state.m
        v, new_state = eval_expr(e, state, prog)

        @test new_state.m > initial_m
        @test new_state.m == initial_m + compute_size(pair_val)
    end

    @testset "No deallocation with MatchPrime" begin
        prog = Program(Dict{Symbol, FunctionDef}())

        pair_val = PairVal(BoolVal(true), NilVal())
        σ = Dict{Int, LFVal}(1 => pair_val)
        S = Dict{Symbol, LFVal}(:l => LocVal(1))
        state = InterpreterState(S, σ, 50)

        e = LFExpr(MatchPrime, [LFExpr(Var, [:l]), LFExpr(ConstBool, [false]), (:h, :t), LFExpr(ConstBool, [true])])
        initial_m = state.m
        v, new_state = eval_expr(e, state, prog)

        @test new_state.m == initial_m
        @test haskey(new_state.σ, 1)
    end
end

@testset "Reverse function from paper (Example 1)" begin
    rev_aux_body = LFExpr(Match, [
        LFExpr(Var, [:l]),
        LFExpr(Var, [:acc]),
        (:h, :t),
        LFExpr(Let, [
            :new_acc,
            LFExpr(Cons, [LFExpr(Var, [:h]), LFExpr(Var, [:acc])]),
            LFExpr(FunApply, [:rev_aux, LFExpr(Var, [:t]), LFExpr(Var, [:new_acc])])
        ])
    ])

    rev_aux_func = FunctionDef([:l, :acc], rev_aux_body)

    reverse_body = LFExpr(Let, [
        :nil_val,
        LFExpr(Nil, []),
        LFExpr(FunApply, [:rev_aux, LFExpr(Var, [:input]), LFExpr(Var, [:nil_val])])
    ])

    reverse_func = FunctionDef([:input], reverse_body)

    prog = Program(Dict(
        :rev_aux => rev_aux_func,
        :reverse => reverse_func
    ))

    @testset "Reverse empty list" begin
        S = Dict{Symbol, LFVal}(:input => NilVal())
        state = InterpreterState(S, Dict{Int, LFVal}(), 100)

        v, new_state = eval_expr(reverse_body, state, prog)

        @test v == NilVal()
        @test isempty(new_state.σ)
    end

    @testset "Reverse single element list" begin
        σ = Dict{Int, LFVal}(
            1 => PairVal(BoolVal(true), NilVal())
        )
        S = Dict{Symbol, LFVal}(:input => LocVal(1))
        initial_m = 100
        state = InterpreterState(S, σ, initial_m)

        v, new_state = eval_expr(reverse_body, state, prog)

        @test v isa LocVal
        @test new_state.σ[v.loc] isa PairVal
        @test new_state.σ[v.loc].fst == BoolVal(true)
        @test new_state.σ[v.loc].snd == NilVal()
    end

    @testset "Reverse two element list [true, false]" begin
        σ = Dict{Int, LFVal}(
            1 => PairVal(BoolVal(true), LocVal(2)),
            2 => PairVal(BoolVal(false), NilVal())
        )
        S = Dict{Symbol, LFVal}(:input => LocVal(1))
        initial_m = 100
        state = InterpreterState(S, σ, initial_m)

        v, new_state = eval_expr(reverse_body, state, prog)

        @test v isa LocVal
        result_pair = new_state.σ[v.loc]
        @test result_pair.fst == BoolVal(false)

        @test result_pair.snd isa LocVal
        second_pair = new_state.σ[result_pair.snd.loc]
        @test second_pair.fst == BoolVal(true)
        @test second_pair.snd == NilVal()
    end

    @testset "Reverse three element list [true, false, true]" begin
        σ = Dict{Int, LFVal}(
            1 => PairVal(BoolVal(true), LocVal(2)),
            2 => PairVal(BoolVal(false), LocVal(3)),
            3 => PairVal(BoolVal(true), NilVal())
        )
        S = Dict{Symbol, LFVal}(:input => LocVal(1))
        initial_m = 100
        state = InterpreterState(S, σ, initial_m)

        v, new_state = eval_expr(reverse_body, state, prog)

        @test v isa LocVal

        first = new_state.σ[v.loc]
        @test first.fst == BoolVal(true)

        @test first.snd isa LocVal
        second = new_state.σ[first.snd.loc]
        @test second.fst == BoolVal(false)

        @test second.snd isa LocVal
        third = new_state.σ[second.snd.loc]
        @test third.fst == BoolVal(true)
        @test third.snd == NilVal()
    end

    @testset "Memory management - destructive reverse" begin
        σ = Dict{Int, LFVal}(
            1 => PairVal(BoolVal(true), LocVal(2)),
            2 => PairVal(BoolVal(false), LocVal(3)),
            3 => PairVal(BoolVal(true), NilVal())
        )
        S = Dict{Symbol, LFVal}(:input => LocVal(1))
        initial_m = 10
        state = InterpreterState(S, σ, initial_m)

        v, new_state = eval_expr(reverse_body, state, prog)

        @test !haskey(new_state.σ, 1)
        @test !haskey(new_state.σ, 2)
        @test !haskey(new_state.σ, 3)

        @test length(new_state.σ) == 3
    end
end

function make_list_from_array(arr::Vector, σ::Dict{Int, LFVal})::Tuple{LFVal, Dict{Int, LFVal}}
    if isempty(arr)
        return (NilVal(), σ)
    end

    new_σ = copy(σ)
    tail_val, new_σ = make_list_from_array(arr[2:end], new_σ)

    ℓ = alloc_location(new_σ)
    head_val = arr[1] isa LFVal ? arr[1] : BoolVal(arr[1])
    new_σ[ℓ] = PairVal(head_val, tail_val)

    return (LocVal(ℓ), new_σ)
end

function list_to_array(v::LFVal, σ::Dict{Int, LFVal})::Vector{LFVal}
    if v isa NilVal
        return LFVal[]
    elseif v isa LocVal
        pair = σ[v.loc]
        if !(pair isa PairVal)
            error("Expected PairVal at location $(v.loc)")
        end
        return [pair.fst; list_to_array(pair.snd, σ)]
    else
        error("Expected NilVal or LocVal, got $(typeof(v))")
    end
end

@testset "End-to-end tests using run_program" begin
    @testset "Identity function with main" begin
        main_body = LFExpr(Var, [:x])
        main_func = FunctionDef([:x], main_body)
        prog = Program(Dict(:main => main_func))

        args = Dict{Symbol, LFVal}(:x => BoolVal(true))
        v, final_state = run_program(prog; initial_stack=args)

        @test v == BoolVal(true)
    end

    @testset "Simple arithmetic-like function" begin
        main_body = LFExpr(Let, [
            :a,
            LFExpr(ConstBool, [true]),
            LFExpr(Let, [
                :b,
                LFExpr(ConstBool, [false]),
                LFExpr(Var, [:a])
            ])
        ])
        main_func = FunctionDef(Symbol[], main_body)
        prog = Program(Dict(:main => main_func))

        v, final_state = run_program(prog)

        @test v == BoolVal(true)
    end

    @testset "Reverse function end-to-end" begin
        rev_aux_body = LFExpr(Match, [
            LFExpr(Var, [:l]),
            LFExpr(Var, [:acc]),
            (:h, :t),
            LFExpr(Let, [
                :new_acc,
                LFExpr(Cons, [LFExpr(Var, [:h]), LFExpr(Var, [:acc])]),
                LFExpr(FunApply, [:rev_aux, LFExpr(Var, [:t]), LFExpr(Var, [:new_acc])])
            ])
        ])
        rev_aux_func = FunctionDef([:l, :acc], rev_aux_body)

        main_body = LFExpr(Let, [
            :nil_val,
            LFExpr(Nil, []),
            LFExpr(FunApply, [:rev_aux, LFExpr(Var, [:input]), LFExpr(Var, [:nil_val])])
        ])
        main_func = FunctionDef([:input], main_body)

        prog = Program(Dict(
            :rev_aux => rev_aux_func,
            :main => main_func
        ))

        initial_heap = Dict{Int, LFVal}(
            1 => PairVal(BoolVal(true), LocVal(2)),
            2 => PairVal(BoolVal(false), LocVal(3)),
            3 => PairVal(BoolVal(true), NilVal())
        )
        args = Dict{Symbol, LFVal}(:input => LocVal(1))

        v, final_state = run_program(
            prog;
            initial_stack=args,
            initial_heap=initial_heap,
            heap_size=100
        )

        @test v isa LocVal
        result_arr = list_to_array(v, final_state.σ)
        @test length(result_arr) == 3
        @test result_arr[1] == BoolVal(true)
        @test result_arr[2] == BoolVal(false)
        @test result_arr[3] == BoolVal(true)
    end

    @testset "List length function with MatchPrime" begin
        length_body = LFExpr(MatchPrime, [
            LFExpr(Var, [:l]),
            LFExpr(ConstBool, [false]),
            (:h, :t),
            LFExpr(Let, [
                :rest_len,
                LFExpr(FunApply, [:length, LFExpr(Var, [:t])]),
                LFExpr(ConstBool, [true])
            ])
        ])
        length_func = FunctionDef([:l], length_body)

        main_body = LFExpr(FunApply, [:length, LFExpr(Var, [:input])])
        main_func = FunctionDef([:input], main_body)

        prog = Program(Dict(
            :length => length_func,
            :main => main_func
        ))

        initial_heap = Dict{Int, LFVal}(
            1 => PairVal(BoolVal(true), LocVal(2)),
            2 => PairVal(BoolVal(false), LocVal(3)),
            3 => PairVal(BoolVal(true), NilVal())
        )
        args = Dict{Symbol, LFVal}(:input => LocVal(1))

        v, final_state = run_program(
            prog;
            initial_stack=args,
            initial_heap=initial_heap,
            heap_size=100
        )

        @test v == BoolVal(true)
        @test haskey(final_state.σ, 1)
        @test haskey(final_state.σ, 2)
        @test haskey(final_state.σ, 3)
    end

    @testset "Sum type processing" begin
        process_body = LFExpr(MatchSum, [
            LFExpr(Var, [:value]),
            :left,
            LFExpr(Var, [:left]),
            :right,
            LFExpr(Var, [:right])
        ])
        process_func = FunctionDef([:value], process_body)

        main_body = LFExpr(FunApply, [:process, LFExpr(Var, [:input])])
        main_func = FunctionDef([:input], main_body)

        prog = Program(Dict(
            :process => process_func,
            :main => main_func
        ))

        args_left = Dict{Symbol, LFVal}(:input => InlVal(BoolVal(true)))
        v1, _ = run_program(prog; initial_stack=args_left)
        @test v1 == BoolVal(true)

        args_right = Dict{Symbol, LFVal}(:input => InrVal(BoolVal(false)))
        v2, _ = run_program(prog; initial_stack=args_right)
        @test v2 == BoolVal(false)
    end

    @testset "Custom function name (not main)" begin
        compute_body = LFExpr(ConstBool, [false])
        compute_func = FunctionDef(Symbol[], compute_body)
        prog = Program(Dict(:compute => compute_func))

        v, _ = run_program(prog; entrypoint=:compute)
        @test v == BoolVal(false)
    end

    @testset "Memory management in run_program" begin
        cons_twice_body = LFExpr(Let, [
            :first,
            LFExpr(Cons, [LFExpr(Var, [:x]), LFExpr(Var, [:y])]),
            LFExpr(Let, [
                :second,
                LFExpr(Cons, [LFExpr(Var, [:x]), LFExpr(Var, [:y])]),
                LFExpr(Var, [:second])
            ])
        ])
        main_func = FunctionDef([:x, :y], cons_twice_body)
        prog = Program(Dict(:main => main_func))

        args = Dict{Symbol, LFVal}(:x => BoolVal(true), :y => NilVal())
        v, final_state = run_program(prog; initial_stack=args, heap_size=10)

        @test v isa LocVal
        @test length(final_state.σ) == 2
        @test final_state.m < 10
    end

    @testset "Empty list reverse" begin
        rev_aux_body = LFExpr(Match, [
            LFExpr(Var, [:l]),
            LFExpr(Var, [:acc]),
            (:h, :t),
            LFExpr(Let, [
                :new_acc,
                LFExpr(Cons, [LFExpr(Var, [:h]), LFExpr(Var, [:acc])]),
                LFExpr(FunApply, [:rev_aux, LFExpr(Var, [:t]), LFExpr(Var, [:new_acc])])
            ])
        ])
        rev_aux_func = FunctionDef([:l, :acc], rev_aux_body)

        main_body = LFExpr(Let, [
            :nil_val,
            LFExpr(Nil, []),
            LFExpr(FunApply, [:rev_aux, LFExpr(Var, [:input]), LFExpr(Var, [:nil_val])])
        ])
        main_func = FunctionDef([:input], main_body)

        prog = Program(Dict(
            :rev_aux => rev_aux_func,
            :main => main_func
        ))

        args = Dict{Symbol, LFVal}(:input => NilVal())
        v, final_state = run_program(prog; initial_stack=args, heap_size=10)

        @test v == NilVal()
        @test isempty(final_state.σ)
    end
end

@testset "Complex scoping and nested structures" begin
    prog = Program(Dict{Symbol, FunctionDef}())

    @testset "Let inside Match cons branch" begin
        # match xs with | nil => false | cons(h, t) => let dup = h in dup
        σ = Dict{Int, LFVal}(1 => PairVal(BoolVal(true), NilVal()))
        e_nil = LFExpr(ConstBool, [false])
        e_cons = LFExpr(Let, [
            :dup,
            LFExpr(Var, [:h]),
            LFExpr(Var, [:dup])
        ])
        e = LFExpr(Match, [LFExpr(Var, [:xs]), e_nil, (:h, :t), e_cons])
        S = Dict{Symbol, LFVal}(:xs => LocVal(1))
        state = InterpreterState(S, σ, 100)
        v, new_state = eval_expr(e, state, prog)

        @test v == BoolVal(true)
        @test !haskey(new_state.σ, 1)  # Destructive match freed the location
    end

    @testset "FunApply with variable from Match - the replicate pattern" begin
        # Simulates: match xs with | nil => nil | cons(h, t) => let dup = duplicate(h) in append(dup, process(t))
        duplicate_body = LFExpr(Cons, [LFExpr(Var, [:x]), LFExpr(Cons, [LFExpr(Var, [:x]), LFExpr(Nil, [])])])
        duplicate_func = FunctionDef([:x], duplicate_body)

        process_body = LFExpr(Match, [
            LFExpr(Var, [:xs]),
            LFExpr(Nil, []),
            (:h, :t),
            LFExpr(Let, [
                :dup,
                LFExpr(FunApply, [:duplicate, LFExpr(Var, [:h])]),
                LFExpr(Var, [:dup])  # Simplified: just return dup instead of appending
            ])
        ])
        process_func = FunctionDef([:xs], process_body)

        prog_with_funcs = Program(Dict(:duplicate => duplicate_func, :process => process_func))

        # Test with list [true]
        σ = Dict{Int, LFVal}(1 => PairVal(BoolVal(true), NilVal()))
        S = Dict{Symbol, LFVal}(:xs => LocVal(1))
        state = InterpreterState(S, σ, 100)

        e = LFExpr(FunApply, [:process, LFExpr(Var, [:xs])])
        v, new_state = eval_expr(e, state, prog_with_funcs)

        @test v isa LocVal  # Should return a list [true, true]
        result = list_to_array(v, new_state.σ)
        @test length(result) == 2
        @test result[1] == BoolVal(true)
        @test result[2] == BoolVal(true)
    end

    @testset "Nested Match with variable access" begin
        # match outer with | nil => false | cons(h1, t1) =>
        #   match inner with | nil => h1 | cons(h2, t2) => h2
        σ_outer = Dict{Int, LFVal}(1 => PairVal(BoolVal(true), NilVal()))
        σ_inner = Dict{Int, LFVal}(2 => PairVal(BoolVal(false), NilVal()))

        e_inner = LFExpr(MatchPrime, [
            LFExpr(Var, [:inner]),
            LFExpr(Var, [:h1]),  # nil case: return h1 from outer match
            (:h2, :t2),
            LFExpr(Var, [:h2])   # cons case: return h2
        ])

        e_outer = LFExpr(MatchPrime, [
            LFExpr(Var, [:outer]),
            LFExpr(ConstBool, [false]),
            (:h1, :t1),
            e_inner
        ])

        S = Dict{Symbol, LFVal}(:outer => LocVal(1), :inner => LocVal(2))
        state = InterpreterState(S, merge(σ_outer, σ_inner), 100)
        v, new_state = eval_expr(e_outer, state, prog)

        @test v == BoolVal(false)  # Should return h2 from inner match
    end

    @testset "Multiple function calls with shared variables" begin
        # Simulates: let x = true in let y = id(x) in id(y)
        id_body = LFExpr(Var, [:param])
        id_func = FunctionDef([:param], id_body)
        prog_id = Program(Dict(:id => id_func))

        e = LFExpr(Let, [
            :x,
            LFExpr(ConstBool, [true]),
            LFExpr(Let, [
                :y,
                LFExpr(FunApply, [:id, LFExpr(Var, [:x])]),
                LFExpr(FunApply, [:id, LFExpr(Var, [:y])])
            ])
        ])

        state = InterpreterState(Dict{Symbol, LFVal}(), Dict{Int, LFVal}(), 100)
        v, new_state = eval_expr(e, state, prog_id)

        @test v == BoolVal(true)
    end

    @testset "MatchPair with nested Let" begin
        # match p with (a, b) => let c = a in c
        pair = PairVal(BoolVal(true), BoolVal(false))
        body = LFExpr(Let, [
            :c,
            LFExpr(Var, [:a]),
            LFExpr(Var, [:c])
        ])
        e = LFExpr(MatchPair, [LFExpr(Var, [:p]), :a, :b, body])
        S = Dict{Symbol, LFVal}(:p => pair)
        state = InterpreterState(S, Dict{Int, LFVal}(), 100)
        v, new_state = eval_expr(e, state, prog)

        @test v == BoolVal(true)
    end

    @testset "Complex: Match in Match in Let" begin
        # let outer_list = [true] in
        #   match outer_list with
        #     | nil => false
        #     | cons(h_outer, t_outer) =>
        #         match inner_list with
        #           | nil => h_outer
        #           | cons(h_inner, t_inner) => h_inner
        σ = Dict{Int, LFVal}(
            1 => PairVal(BoolVal(true), NilVal()),
            2 => PairVal(BoolVal(false), NilVal())
        )

        e_inner_match = LFExpr(MatchPrime, [
            LFExpr(Var, [:inner_list]),
            LFExpr(Var, [:h_outer]),
            (:h_inner, :t_inner),
            LFExpr(Var, [:h_inner])
        ])

        e_outer_match = LFExpr(MatchPrime, [
            LFExpr(Var, [:outer_list]),
            LFExpr(ConstBool, [false]),
            (:h_outer, :t_outer),
            e_inner_match
        ])

        e = LFExpr(Let, [
            :outer_list,
            LFExpr(Var, [:list1]),
            e_outer_match
        ])

        S = Dict{Symbol, LFVal}(:list1 => LocVal(1), :inner_list => LocVal(2))
        state = InterpreterState(S, σ, 100)
        v, new_state = eval_expr(e, state, prog)

        @test v == BoolVal(false)
    end

    @testset "Function call with expression argument from Match" begin
        # match xs with | nil => false | cons(h, t) => id(h)
        id_body = LFExpr(Var, [:x])
        id_func = FunctionDef([:x], id_body)
        prog_id = Program(Dict(:id => id_func))

        σ = Dict{Int, LFVal}(1 => PairVal(BoolVal(true), NilVal()))
        e_nil = LFExpr(ConstBool, [false])
        e_cons = LFExpr(FunApply, [:id, LFExpr(Var, [:h])])
        e = LFExpr(MatchPrime, [LFExpr(Var, [:xs]), e_nil, (:h, :t), e_cons])
        S = Dict{Symbol, LFVal}(:xs => LocVal(1))
        state = InterpreterState(S, σ, 100)
        v, new_state = eval_expr(e, state, prog_id)

        @test v == BoolVal(true)
    end

    @testset "Two nested Lets with Match in between" begin
        # let a = true in
        #   match xs with
        #     | nil => a
        #     | cons(h, t) => let b = h in b
        σ = Dict{Int, LFVal}(1 => PairVal(BoolVal(false), NilVal()))

        e_cons_body = LFExpr(Let, [
            :b,
            LFExpr(Var, [:h]),
            LFExpr(Var, [:b])
        ])

        e_match = LFExpr(MatchPrime, [
            LFExpr(Var, [:xs]),
            LFExpr(Var, [:a]),
            (:h, :t),
            e_cons_body
        ])

        e = LFExpr(Let, [
            :a,
            LFExpr(ConstBool, [true]),
            e_match
        ])

        S = Dict{Symbol, LFVal}(:xs => LocVal(1))
        state = InterpreterState(S, σ, 100)
        v, new_state = eval_expr(e, state, prog)

        @test v == BoolVal(false)  # Should get h from cons branch
    end
end
