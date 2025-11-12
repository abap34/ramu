using Test
include("../src/inference.jl")

@testset "Function Type Checking Tests" begin
    @testset "Simple function" begin
        # id : B -> B
        # id(x) = x
        id_func = FunctionDef(
            [:x],
            [BoolType()],
            BoolType(),
            LFExpr(Var, [:x])
        )

        prog = Program(Dict(:id => id_func))
        @test typecheck_program(prog) == true
    end

    @testset "Constant function" begin
        # const_unit : B -> 1
        # const_unit(x) = *
        const_func = FunctionDef(
            [:x],
            [BoolType()],
            UnitType(),
            LFExpr(Unit, [])
        )

        prog = Program(Dict(:const_unit => const_func))
        @test typecheck_program(prog) == true
    end

    @testset "Pair function" begin
        # make_pair : B -> 1 -> (B ⊗ 1)
        # make_pair(x, y) = (x, y)
        make_pair = FunctionDef(
            [:x, :y],
            [BoolType(), UnitType()],
            ProductType(BoolType(), UnitType()),
            LFExpr(Pair, [:x, :y])
        )

        prog = Program(Dict(:make_pair => make_pair))
        @test typecheck_program(prog) == true
    end

    @testset "Sum type function" begin
        # wrap_inl : B -> (B + 1)
        # wrap_inl(x) = inl(x)
        wrap_inl = FunctionDef(
            [:x],
            [BoolType()],
            SumType(BoolType(), UnitType()),
            LFExpr(Inl, [:x])
        )

        prog = Program(Dict(:wrap_inl => wrap_inl))
        @test typecheck_program(prog) == true
    end

    @testset "List function" begin
        # singleton : B -> List B
        # singleton(x) = cons(x, nil)
        singleton = FunctionDef(
            [:x],
            [BoolType()],
            ListType(BoolType()),
            LFExpr(Let, [
                :xs,
                LFExpr(Nil, []),
                LFExpr(Cons, [:x, :xs])
            ])
        )

        prog = Program(Dict(:singleton => singleton))
        @test typecheck_program(prog) == true
    end

    @testset "Function with let" begin
        # swap : (B ⊗ 1) -> (1 ⊗ B)
        # swap(p) = match p with (x, y) => (y, x)
        swap = FunctionDef(
            [:p],
            [ProductType(BoolType(), UnitType())],
            ProductType(UnitType(), BoolType()),
            LFExpr(MatchPair, [
                :p,
                :x,
                :y,
                LFExpr(Pair, [:y, :x])
            ])
        )

        prog = Program(Dict(:swap => swap))
        @test typecheck_program(prog) == true
    end

    @testset "Function calling another function" begin
        # id : B -> B
        # id(x) = x
        id_func = FunctionDef(
            [:x],
            [BoolType()],
            BoolType(),
            LFExpr(Var, [:x])
        )

        # apply_id : B -> B
        # apply_id(y) = id(y)
        apply_id = FunctionDef(
            [:y],
            [BoolType()],
            BoolType(),
            LFExpr(FunApply, [:id, :y])
        )

        prog = Program(Dict(:id => id_func, :apply_id => apply_id))
        @test typecheck_program(prog) == true
    end

    @testset "Type mismatch error" begin
        # bad : B -> 1
        # bad(x) = x  -- returns B, not 1
        bad_func = FunctionDef(
            [:x],
            [BoolType()],
            UnitType(),  # declared as returning 1
            LFExpr(Var, [:x])  # but returns B
        )

        prog = Program(Dict(:bad => bad_func))
        @test_throws ErrorException typecheck_program(prog)
    end

    @testset "Parameter count mismatch" begin
        # bad : B -> B
        # bad(x, y) = x  -- too many parameters
        bad_func = FunctionDef(
            [:x, :y],
            [BoolType()],  # only one type annotation
            BoolType(),
            LFExpr(Var, [:x])
        )

        prog = Program(Dict(:bad => bad_func))
        @test_throws ErrorException typecheck_program(prog)
    end

    @testset "Sum type with match" begin
        # is_inl : (B + 1) -> B
        # is_inl(x) = match x with | inl(a) => true | inr(b) => false
        is_inl = FunctionDef(
            [:x],
            [SumType(BoolType(), UnitType())],
            BoolType(),
            LFExpr(MatchSum, [
                :x,
                :a,
                LFExpr(ConstBool, [true]),
                :b,
                LFExpr(ConstBool, [false])
            ])
        )

        prog = Program(Dict(:is_inl => is_inl))
        @test typecheck_program(prog) == true
    end

    @testset "List pattern matching" begin
        # is_empty : List B -> B
        # is_empty(xs) = match xs with | nil => true | cons(h, t) => false
        is_empty = FunctionDef(
            [:xs],
            [ListType(BoolType())],
            BoolType(),
            LFExpr(Match, [
                :xs,
                LFExpr(ConstBool, [true]),
                (:h, :t),
                LFExpr(ConstBool, [false])
            ])
        )

        prog = Program(Dict(:is_empty => is_empty))
        @test typecheck_program(prog) == true
    end
end
