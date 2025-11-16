@testset "Type Inference Tests" begin
    @testset "Fresh symbol generators" begin
        tv1 = fresh_typevar()
        tv2 = fresh_typevar()
        tv3 = fresh_typevar()

        @test tv1 isa LFTypeVar
        @test tv2 isa LFTypeVar
        @test tv3 isa LFTypeVar

        @test tv1.name != tv2.name
        @test tv2.name != tv3.name
        @test tv1.name != tv3.name

        rv1 = fresh_resource_var()
        rv2 = fresh_resource_var()
        rv3 = fresh_resource_var()

        @test rv1 isa Symbol
        @test rv2 isa Symbol
        @test rv3 isa Symbol

        @test rv1 != rv2
        @test rv2 != rv3
        @test rv1 != rv3
    end

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
        e = LFExpr(LFPair, [:x, :y])
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
        @test t.inr_type isa LFTypeVar

        # inr(x) where x:B should give (α + B) for some α
        e = LFExpr(Inr, [:x])
        t = typeinf(e, Γ)
        @test t isa SumType
        @test t.inl_type isa LFTypeVar
        @test t.inr_type == BoolType()
    end

    @testset "Let binding" begin
        # let x = true in x
        e = LFExpr(
            Let, [
                :x,
                LFExpr(ConstBool, [true]),
                LFExpr(Var, [:x]),
            ]
        )
        t = typeinf(e)
        @test t == BoolType()

        # let x = * in let y = true in (x, y)
        e = LFExpr(
            Let, [
                :x,
                LFExpr(Unit, []),
                LFExpr(
                    Let, [
                        :y,
                        LFExpr(ConstBool, [true]),
                        LFExpr(LFPair, [:x, :y]),
                    ]
                ),
            ]
        )
        t = typeinf(e)
        @test t == ProductType(UnitType(), BoolType())
    end

    @testset "If expression" begin
        Γ = TypeEnv(Dict(:cond => BoolType()))

        # if cond then * else *
        e = LFExpr(
            If, [
                :cond,
                LFExpr(Unit, []),
                LFExpr(Unit, []),
            ]
        )
        t = typeinf(e, Γ)
        @test t == UnitType()
    end

    @testset "MatchPair" begin
        Γ = TypeEnv(Dict(:p => ProductType(BoolType(), UnitType())))

        # match p with (x, y) => x
        e = LFExpr(
            MatchPair, [
                :p,
                :x,
                :y,
                LFExpr(Var, [:x]),
            ]
        )
        t = typeinf(e, Γ)
        @test t == BoolType()

        # match p with (x, y) => y
        e = LFExpr(
            MatchPair, [
                :p,
                :x,
                :y,
                LFExpr(Var, [:y]),
            ]
        )
        t = typeinf(e, Γ)
        @test t == UnitType()
    end

    @testset "MatchSum" begin
        Γ = TypeEnv(Dict(:s => SumType(BoolType(), UnitType())))

        # match s with | inl(x) => x | inr(y) => y
        # This should fail because branches have different types
        e = LFExpr(
            MatchSum, [
                :s,
                :x,
                LFExpr(Var, [:x]),  # returns B
                :y,
                LFExpr(Var, [:y]),   # returns 1
            ]
        )
        @test_throws ErrorException typeinf(e, Γ)

        # match s with | inl(x) => * | inr(y) => *
        e = LFExpr(
            MatchSum, [
                :s,
                :x,
                LFExpr(Unit, []),
                :y,
                LFExpr(Unit, []),
            ]
        )
        t = typeinf(e, Γ)
        @test t == UnitType()
    end

    @testset "List types" begin
        # nil should infer List α for some α
        e = LFExpr(Nil, [])
        t = typeinf(e)
        @test t isa ListType
        @test t.elem_type isa LFTypeVar

        # cons with concrete types
        Γ = TypeEnv(
            Dict(
                :x => BoolType(),
                :xs => ListType(BoolType())
            )
        )

        e = LFExpr(Cons, [:x, :xs])
        t = typeinf(e, Γ)
        @test t == ListType(BoolType())
    end

    @testset "List pattern matching" begin
        Γ = TypeEnv(Dict(:list => ListType(BoolType())))

        # match list with | nil => * | cons(h, t) => h
        e = LFExpr(
            Match, [
                :list,
                LFExpr(Unit, []),         # nil branch
                (:h, :t),
                LFExpr(Var, [:h]),         # cons branch
            ]
        )
        # This should fail because nil returns 1 but cons returns B
        @test_throws ErrorException typeinf(e, Γ)

        # match list with | nil => true | cons(h, t) => h
        e = LFExpr(
            Match, [
                :list,
                LFExpr(ConstBool, [true]),  # nil branch
                (:h, :t),
                LFExpr(Var, [:h]),           # cons branch
            ]
        )
        t = typeinf(e, Γ)
        @test t == BoolType()
    end

    @testset "Complex sum type inference" begin
        # let x = inl(true) in
        #   match x with
        #   | inl(a) => a
        #   | inr(b) => b
        # This should infer that both branches have type B
        e = LFExpr(
            Let, [
                :x,
                LFExpr(Inl, [:t]),
                LFExpr(
                    MatchSum, [
                        :x,
                        :a,
                        LFExpr(Var, [:a]),
                        :b,
                        LFExpr(Var, [:b]),
                    ]
                ),
            ]
        )
        Γ = TypeEnv(Dict(:t => BoolType()))
        t = typeinf(e, Γ)
        @test t == BoolType()
    end

    @testset "Unification" begin
        # Test that type variables are properly unified
        # let x = nil in cons(true, x)
        # Should infer that x : List B
        e = LFExpr(
            Let, [
                :x,
                LFExpr(Nil, []),
                LFExpr(Cons, [:t, :x]),
            ]
        )
        Γ = TypeEnv(Dict(:t => BoolType()))
        t = typeinf(e, Γ)
        @test t == ListType(BoolType())
    end
end

@testset "Resource Analysis Tests" begin
    @testset "compute_size function" begin
        @test compute_size(BoolType()) == 1
        @test compute_size(UnitType()) == 1
        @test compute_size(ListType(BoolType())) == 1

        @test compute_size(ProductType(BoolType(), UnitType())) == 2
        @test compute_size(ProductType(BoolType(), ProductType(UnitType(), BoolType()))) == 3

        @test compute_size(SumType(BoolType(), UnitType())) == 2
        @test compute_size(SumType(ProductType(BoolType(), BoolType()), UnitType())) == 3
    end

    @testset "LinearExpression display" begin
        le1 = LinearExpression(:n)
        @test string(le1) == "n"

        le2 = LinearExpression(Dict(:n => 2), 0)
        @test string(le2) == "2*n"

        le3 = LinearExpression(:n) + LinearExpression(5)
        @test string(le3) == "n + 5"

        le4 = LinearExpression(:n) - LinearExpression(3)
        @test string(le4) == "n - 3"

        le5 = LinearExpression(10)
        @test string(le5) == "10"

        le6 = -1 * LinearExpression(:n)
        @test string(le6) == "-n"
    end

    @testset "ResourceConstraint display" begin
        rc = ResourceConstraint(
            LinearExpression(:n),
            LinearExpression(:m) + LinearExpression(5)
        )
        @test string(rc) == "n ≤ m + 5"
    end

    @testset "Resource constraints generation - basic" begin
        # true should generate n_out ≤ n_in
        e = LFExpr(ConstBool, [true])
        t, rcs = typeinf_with_resources(e)
        @test t == BoolType()
        @test length(rcs) == 1

        # * should generate n_out ≤ n_in
        e = LFExpr(Unit, [])
        t, rcs = typeinf_with_resources(e)
        @test t == UnitType()
        @test length(rcs) == 1

        # Variable lookup should generate n_out ≤ n_in
        Γ = TypeEnv(Dict(:x => BoolType()))
        e = LFExpr(Var, [:x])
        t, rcs = typeinf_with_resources(e, Γ)
        @test t == BoolType()
        @test length(rcs) == 1
    end

    @testset "Resource constraints generation - pairs" begin
        Γ = TypeEnv(Dict(:x => BoolType(), :y => UnitType()))

        # (x, y) - should generate constraints for x, y lookups and pair construction
        e = LFExpr(LFPair, [:x, :y])
        t, rcs = typeinf_with_resources(e, Γ)
        @test t == ProductType(BoolType(), UnitType())
        @test length(rcs) > 0  # Should have constraints from variable lookups and pair
    end

    @testset "Resource constraints generation - let" begin
        # let x = true in x
        e = LFExpr(
            Let, [
                :x,
                LFExpr(ConstBool, [true]),
                LFExpr(Var, [:x]),
            ]
        )
        t, rcs = typeinf_with_resources(e)
        @test t == BoolType()
        @test length(rcs) >= 2  # Should have constraints from both expressions
    end

    @testset "Resource constraints generation - if" begin
        Γ = TypeEnv(Dict(:cond => BoolType()))

        # if cond then * else *
        e = LFExpr(
            If, [
                :cond,
                LFExpr(Unit, []),
                LFExpr(Unit, []),
            ]
        )
        t, rcs = typeinf_with_resources(e, Γ)
        @test t == UnitType()
        @test length(rcs) > 0  # Should have constraints from all branches
    end

    @testset "Resource constraints generation - lists" begin
        # nil
        e = LFExpr(Nil, [])
        t, rcs = typeinf_with_resources(e)
        @test t isa ListType
        @test length(rcs) == 1  # n_out ≤ n_in

        # cons(x, xs) - should generate constraint with SIZE + k
        Γ = TypeEnv(Dict(:x => BoolType(), :xs => ListType(BoolType())))
        e = LFExpr(Cons, [:x, :xs])
        t, rcs = typeinf_with_resources(e, Γ)
        @test t == ListType(BoolType())
        @test length(rcs) > 0  # Should have cons constraint + variable lookups
    end

    @testset "Resource constraints generation - comprehensive" begin
        # Inl/Inr should have potential constraints
        Γ = TypeEnv(Dict(:x => BoolType()))
        e = LFExpr(Inl, [:x])
        t, rcs = typeinf_with_resources(e, Γ)
        @test t isa SumType
        @test length(rcs) > 0

        # MatchSum should have potential constraints
        Γ = TypeEnv(Dict(:s => SumType(BoolType(), UnitType())))
        e = LFExpr(
            MatchSum, [
                :s,
                :x,
                LFExpr(Unit, []),
                :y,
                LFExpr(Unit, []),
            ]
        )
        t, rcs = typeinf_with_resources(e, Γ)
        @test t == UnitType()
        @test length(rcs) > 0  # Should have potential constraints

        # Match (destructive) should have SIZE + k constraint
        Γ = TypeEnv(Dict(:list => ListType(BoolType())))
        e = LFExpr(
            Match, [
                :list,
                LFExpr(ConstBool, [true]),
                (:h, :t),
                LFExpr(Var, [:h]),
            ]
        )
        t, rcs = typeinf_with_resources(e, Γ)
        @test t == BoolType()
        @test length(rcs) > 0  # Should have match constraint

        # MatchPair
        Γ = TypeEnv(Dict(:p => ProductType(BoolType(), UnitType())))
        e = LFExpr(
            MatchPair, [
                :p,
                :x,
                :y,
                LFExpr(Var, [:x]),
            ]
        )
        t, rcs = typeinf_with_resources(e, Γ)
        @test t == BoolType()
        @test length(rcs) > 0

        # Complex nested expression
        e = LFExpr(
            Let, [
                :x,
                LFExpr(Nil, []),
                LFExpr(
                    Let, [
                        :y,
                        LFExpr(ConstBool, [true]),
                        LFExpr(Cons, [:y, :x]),
                    ]
                ),
            ]
        )
        t, rcs = typeinf_with_resources(e)
        @test t == ListType(BoolType())
        @test length(rcs) > 3  # Multiple expressions should generate multiple constraints
    end

    @testset "LinearExpression operations" begin
        # Test addition
        le1 = LinearExpression(:n)
        le2 = LinearExpression(:m)
        le3 = le1 + le2
        @test haskey(le3.terms, :n)
        @test haskey(le3.terms, :m)

        # Test subtraction
        le4 = le1 - le2
        @test le4.terms[:n] == 1
        @test le4.terms[:m] == -1

        # Test scalar multiplication
        le5 = 3 * LinearExpression(:k)
        @test le5.terms[:k] == 3

        # Test constant operations
        le6 = LinearExpression(:n) + LinearExpression(5)
        @test le6.constant == 5
        @test le6.terms[:n] == 1
    end
end
