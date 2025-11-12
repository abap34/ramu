using Test
include("../src/inference.jl")

@testset "Type Inference Tests" begin
    @testset "Basic types" begin
        # Bool constant
        e = LFExpr(ConstBool, [true])
        t = typeinf(e)
        @test t == BoolType()

        # Unit constant
        e = LFExpr(Unit, [])
        t = typeinf(e)
        @test t == UnitType()
    end

    @testset "Variables" begin
        Γ = TypeEnv(Dict(:x => BoolType(), :y => UnitType()))

        # Variable lookup
        e = LFExpr(Var, [:x])
        t = typeinf(e, Γ)
        @test t == BoolType()

        e = LFExpr(Var, [:y])
        t = typeinf(e, Γ)
        @test t == UnitType()
    end

    @testset "Pairs (Product types)" begin
        Γ = TypeEnv(Dict(:x => BoolType(), :y => UnitType()))

        # (x, y) where x:B, y:1
        e = LFExpr(Pair, [:x, :y])
        t = typeinf(e, Γ)
        @test t == ProductType(BoolType(), UnitType())
    end

    @testset "Sum types - Inl and Inr" begin
        Γ = TypeEnv(Dict(:x => BoolType()))

        # inl(x) where x:B should give (B + α) for some α
        e = LFExpr(Inl, [:x])
        t = typeinf(e, Γ)
        @test t isa SumType
        @test t.inl_type == BoolType()
        @test t.inr_type isa TypeVar

        # inr(x) where x:B should give (α + B) for some α
        e = LFExpr(Inr, [:x])
        t = typeinf(e, Γ)
        @test t isa SumType
        @test t.inl_type isa TypeVar
        @test t.inr_type == BoolType()
    end

    @testset "Let binding" begin
        # let x = true in x
        e = LFExpr(Let, [
            :x,
            LFExpr(ConstBool, [true]),
            LFExpr(Var, [:x])
        ])
        t = typeinf(e)
        @test t == BoolType()

        # let x = * in let y = true in (x, y)
        e = LFExpr(Let, [
            :x,
            LFExpr(Unit, []),
            LFExpr(Let, [
                :y,
                LFExpr(ConstBool, [true]),
                LFExpr(Pair, [:x, :y])
            ])
        ])
        t = typeinf(e)
        @test t == ProductType(UnitType(), BoolType())
    end

    @testset "If expression" begin
        Γ = TypeEnv(Dict(:cond => BoolType()))

        # if cond then * else *
        e = LFExpr(If, [
            :cond,
            LFExpr(Unit, []),
            LFExpr(Unit, [])
        ])
        t = typeinf(e, Γ)
        @test t == UnitType()
    end

    @testset "MatchPair" begin
        Γ = TypeEnv(Dict(:p => ProductType(BoolType(), UnitType())))

        # match p with (x, y) => x
        e = LFExpr(MatchPair, [
            :p,
            :x,
            :y,
            LFExpr(Var, [:x])
        ])
        t = typeinf(e, Γ)
        @test t == BoolType()

        # match p with (x, y) => y
        e = LFExpr(MatchPair, [
            :p,
            :x,
            :y,
            LFExpr(Var, [:y])
        ])
        t = typeinf(e, Γ)
        @test t == UnitType()
    end

    @testset "MatchSum" begin
        Γ = TypeEnv(Dict(:s => SumType(BoolType(), UnitType())))

        # match s with | inl(x) => x | inr(y) => y
        # This should fail because branches have different types
        e = LFExpr(MatchSum, [
            :s,
            :x,
            LFExpr(Var, [:x]),  # returns B
            :y,
            LFExpr(Var, [:y])   # returns 1
        ])
        @test_throws ErrorException typeinf(e, Γ)

        # match s with | inl(x) => * | inr(y) => *
        e = LFExpr(MatchSum, [
            :s,
            :x,
            LFExpr(Unit, []),
            :y,
            LFExpr(Unit, [])
        ])
        t = typeinf(e, Γ)
        @test t == UnitType()
    end

    @testset "List types" begin
        # nil should infer List α for some α
        e = LFExpr(Nil, [])
        t = typeinf(e)
        @test t isa ListType
        @test t.elem_type isa TypeVar

        # cons with concrete types
        Γ = TypeEnv(Dict(
            :x => BoolType(),
            :xs => ListType(BoolType())
        ))

        e = LFExpr(Cons, [:x, :xs])
        t = typeinf(e, Γ)
        @test t == ListType(BoolType())
    end

    @testset "List pattern matching" begin
        Γ = TypeEnv(Dict(:list => ListType(BoolType())))

        # match list with | nil => * | cons(h, t) => h
        e = LFExpr(Match, [
            :list,
            LFExpr(Unit, []),         # nil branch
            (:h, :t),
            LFExpr(Var, [:h])         # cons branch
        ])
        # This should fail because nil returns 1 but cons returns B
        @test_throws ErrorException typeinf(e, Γ)

        # match list with | nil => true | cons(h, t) => h
        e = LFExpr(Match, [
            :list,
            LFExpr(ConstBool, [true]),  # nil branch
            (:h, :t),
            LFExpr(Var, [:h])           # cons branch
        ])
        t = typeinf(e, Γ)
        @test t == BoolType()
    end

    @testset "Complex sum type inference" begin
        # let x = inl(true) in
        #   match x with
        #   | inl(a) => a
        #   | inr(b) => b
        # This should infer that both branches have type B
        e = LFExpr(Let, [
            :x,
            LFExpr(Inl, [:t]),
            LFExpr(MatchSum, [
                :x,
                :a,
                LFExpr(Var, [:a]),
                :b,
                LFExpr(Var, [:b])
            ])
        ])
        Γ = TypeEnv(Dict(:t => BoolType()))
        t = typeinf(e, Γ)
        @test t == BoolType()
    end

    @testset "Unification" begin
        # Test that type variables are properly unified
        # let x = nil in cons(true, x)
        # Should infer that x : List B
        e = LFExpr(Let, [
            :x,
            LFExpr(Nil, []),
            LFExpr(Cons, [:t, :x])
        ])
        Γ = TypeEnv(Dict(:t => BoolType()))
        t = typeinf(e, Γ)
        @test t == ListType(BoolType())
    end
end
