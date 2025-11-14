@testset "Example Programs" begin
    @testset "bool_ops.ramu" begin
        prog = parse_from_file("examples/bool_ops.ramu")
        @test haskey(prog.functions, :main)
        @test haskey(prog.functions, :not)

        # Run main: not(true) should return false
        result, _ = run_program(prog)
        @test result == BoolVal(false)
    end

    @testset "pair_example.ramu" begin
        prog = parse_from_file("examples/pair_example.ramu")
        @test haskey(prog.functions, :main)
        @test haskey(prog.functions, :fst)
        @test haskey(prog.functions, :makePair)

        # Run main: fst((true, false)) should return true
        result, _ = run_program(prog)
        @test result == BoolVal(true)
    end

    @testset "sum_example.ramu" begin
        prog = parse_from_file("examples/sum_example.ramu")
        @test haskey(prog.functions, :main)
        @test haskey(prog.functions, :getOrDefault)
        @test haskey(prog.functions, :someValue)

        # Run main: getOrDefault(Some(true), false) should return true
        result, _ = run_program(prog)
        @test result == BoolVal(true)
    end

    @testset "list_length.ramu" begin
        prog = parse_from_file("examples/list_length.ramu")
        @test haskey(prog.functions, :main)
        @test haskey(prog.functions, :length)
        @test haskey(prog.functions, :makeList)

        # Run main: length([true, false, true]) should return [*, *, *]
        result, state = run_program(prog; heap_size=100)
        @test result isa LocVal

        # Verify it's a list with 3 elements
        pair1 = state.σ[result.loc]
        @test pair1 isa PairVal
        @test pair1.fst == UnitVal()
        @test pair1.snd isa LocVal

        pair2 = state.σ[pair1.snd.loc]
        @test pair2 isa PairVal
        @test pair2.fst == UnitVal()
        @test pair2.snd isa LocVal

        pair3 = state.σ[pair2.snd.loc]
        @test pair3 isa PairVal
        @test pair3.fst == UnitVal()
        @test pair3.snd == NilVal()
    end

    @testset "list_ops.ramu" begin
        prog = parse_from_file("examples/list_ops.ramu")
        @test haskey(prog.functions, :main)
        @test haskey(prog.functions, :isEmpty)
        @test haskey(prog.functions, :length)
        @test haskey(prog.functions, :prepend)
        @test haskey(prog.functions, :head)
        @test haskey(prog.functions, :tail)
        @test haskey(prog.functions, :append)

        # Run main: isEmpty(cons(true, nil)) should return false
        result, _ = run_program(prog; heap_size=100)
        @test result == BoolVal(false)
    end

    @testset "pair_ops.ramu" begin
        prog = parse_from_file("examples/pair_ops.ramu")
        @test haskey(prog.functions, :main)
        @test haskey(prog.functions, :fst)
        @test haskey(prog.functions, :snd)
        @test haskey(prog.functions, :swap)
        @test haskey(prog.functions, :duplicate)
        @test haskey(prog.functions, :mapBoth)
        @test haskey(prog.functions, :both)
        @test haskey(prog.functions, :makePair)

        # Run main: both((true, true)) should return true
        result, _ = run_program(prog)
        @test result == BoolVal(true)
    end

    @testset "sum_type.ramu" begin
        prog = parse_from_file("examples/sum_type.ramu")
        @test haskey(prog.functions, :main)
        @test haskey(prog.functions, :isSome)
        @test haskey(prog.functions, :isNone)
        @test haskey(prog.functions, :getOrDefault)
        @test haskey(prog.functions, :mapOpt)
        @test haskey(prog.functions, :andOpt)
        @test haskey(prog.functions, :some)
        @test haskey(prog.functions, :none)

        # Run main: isSome(some(true)) should return true
        result, _ = run_program(prog)
        @test result == BoolVal(true)
    end
end
