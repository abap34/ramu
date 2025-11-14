@testset "Parser Tests" begin
    @testset "Type Parsing" begin
        @testset "Basic Types" begin
            tokens = tokenize("B")
            p = Parser(tokens)
            t = parse_type(p)
            @test t isa BoolType

            tokens = tokenize("1")
            p = Parser(tokens)
            t = parse_type(p)
            @test t isa UnitType
        end

        @testset "List Types" begin
            tokens = tokenize("list B")
            p = Parser(tokens)
            t = parse_type(p)
            @test t isa ListType
            @test t.elem_type isa BoolType

            tokens = tokenize("list (list 1)")
            p = Parser(tokens)
            t = parse_type(p)
            @test t isa ListType
            @test t.elem_type isa ListType
            @test t.elem_type.elem_type isa UnitType
        end

        @testset "Product Types" begin
            tokens = tokenize("B ⊗ 1")
            p = Parser(tokens)
            t = parse_type(p)
            @test t isa ProductType
            @test t.fst_type isa BoolType
            @test t.snd_type isa UnitType

            # Left-associative: B ⊗ B ⊗ 1 parsed as (B ⊗ B) ⊗ 1
            tokens = tokenize("B ⊗ B ⊗ 1")
            p = Parser(tokens)
            t = parse_type(p)
            @test t isa ProductType
            @test t.fst_type isa ProductType
            @test t.snd_type isa UnitType
        end

        @testset "Sum Types" begin
            tokens = tokenize("B + 1")
            p = Parser(tokens)
            t = parse_type(p)
            @test t isa SumType
            @test t.inl_type isa BoolType
            @test t.inr_type isa UnitType

            # Left-associative: B + 1 + list B parsed as (B + 1) + list B
            tokens = tokenize("B + 1 + list B")
            p = Parser(tokens)
            t = parse_type(p)
            @test t isa SumType
            @test t.inl_type isa SumType
            @test t.inr_type isa ListType
        end

        @testset "Parenthesized Types" begin
            tokens = tokenize("(B ⊗ 1) + list B")
            p = Parser(tokens)
            t = parse_type(p)
            @test t isa SumType
            @test t.inl_type isa ProductType
            @test t.inr_type isa ListType
        end
    end

    @testset "Expression Parsing" begin
        @testset "Constants" begin
            tokens = tokenize("true")
            p = Parser(tokens)
            e = parse_expr(p)
            @test e.head == ConstBool
            @test e.args[1] == true

            tokens = tokenize("false")
            p = Parser(tokens)
            e = parse_expr(p)
            @test e.head == ConstBool
            @test e.args[1] == false

            tokens = tokenize("*")
            p = Parser(tokens)
            e = parse_expr(p)
            @test e.head == Unit

            tokens = tokenize("nil")
            p = Parser(tokens)
            e = parse_expr(p)
            @test e.head == Nil
        end

        @testset "Variables" begin
            tokens = tokenize("x")
            p = Parser(tokens)
            e = parse_expr(p)
            @test e.head == Var
            @test e.args[1] == :x

            tokens = tokenize("(x)")
            p = Parser(tokens)
            e = parse_expr(p)
            @test e.head == Var
            @test e.args[1] == :x
        end

        @testset "Pairs - simple variables" begin
            tokens = tokenize("(x, y)")
            p = Parser(tokens)
            e = parse_expr(p)
            @test e.head == LFPair
            @test e.args[1].head == Var
            @test e.args[1].args[1] == :x
            @test e.args[2].head == Var
            @test e.args[2].args[1] == :y
        end

        @testset "Pairs - with expressions" begin
            # Note: According to grammar, pairs should accept expressions
            # (expr, expr), not just (var, var)
            # This test will likely fail with current parser
            tokens = tokenize("(true, false)")
            p = Parser(tokens)
            e = parse_expr(p)
            @test e.head == LFPair
            @test e.args[1].head == ConstBool
            @test e.args[2].head == ConstBool
        end

        @testset "Sum Type Constructors - simple variables" begin
            tokens = tokenize("inl(x)")
            p = Parser(tokens)
            e = parse_expr(p)
            @test e.head == Inl
            @test e.args[1].head == Var
            @test e.args[1].args[1] == :x

            tokens = tokenize("inr(y)")
            p = Parser(tokens)
            e = parse_expr(p)
            @test e.head == Inr
            @test e.args[1].head == Var
            @test e.args[1].args[1] == :y
        end

        @testset "Sum Type Constructors - with expressions" begin
            # According to grammar: inl( <expr> )
            tokens = tokenize("inl(true)")
            p = Parser(tokens)
            e = parse_expr(p)
            @test e.head == Inl
            @test e.args[1].head == ConstBool

            tokens = tokenize("inr(nil)")
            p = Parser(tokens)
            e = parse_expr(p)
            @test e.head == Inr
            @test e.args[1].head == Nil
        end

        @testset "Cons - simple variables" begin
            tokens = tokenize("cons(h, t)")
            p = Parser(tokens)
            e = parse_expr(p)
            @test e.head == Cons
            @test e.args[1].head == Var
            @test e.args[1].args[1] == :h
            @test e.args[2].head == Var
            @test e.args[2].args[1] == :t
        end

        @testset "Cons - with expressions" begin
            # According to grammar: cons( <expr> , <expr> )
            tokens = tokenize("cons(true, nil)")
            p = Parser(tokens)
            e = parse_expr(p)
            @test e.head == Cons
            @test e.args[1].head == ConstBool
            @test e.args[2].head == Nil
        end

        @testset "Let Expressions" begin
            tokens = tokenize("let x = true in x")
            p = Parser(tokens)
            e = parse_expr(p)
            @test e.head == Let
            @test e.args[1] == :x
            @test e.args[2].head == ConstBool
            @test e.args[3].head == Var

            tokens = tokenize("let x = let y = false in y in x")
            p = Parser(tokens)
            e = parse_expr(p)
            @test e.head == Let
            @test e.args[2].head == Let
        end

        @testset "If Expressions - with variable condition" begin
            tokens = tokenize("if x then true else false")
            p = Parser(tokens)
            e = parse_expr(p)
            @test e.head == If
            @test e.args[1].head == Var
            @test e.args[1].args[1] == :x
            @test e.args[2].head == ConstBool
            @test e.args[3].head == ConstBool
        end

        @testset "If Expressions - with expression condition" begin
            # According to grammar: if <expr> then <expr> else <expr>
            # The condition should be an expression, not just a variable
            tokens = tokenize("if true then * else *")
            p = Parser(tokens)
            e = parse_expr(p)
            @test e.head == If
            @test e.args[1].head == ConstBool
            @test e.args[2].head == Unit
            @test e.args[3].head == Unit
        end

        @testset "Match Pair" begin
            tokens = tokenize("match p with (x, y) => x")
            p = Parser(tokens)
            e = parse_expr(p)
            @test e.head == MatchPair
            @test e.args[1].head == Var
            @test e.args[1].args[1] == :p
            @test e.args[2] == :x
            @test e.args[3] == :y
            @test e.args[4].head == Var
        end

        @testset "Match Pair - with expression" begin
            # According to grammar: match <expr> with ...
            tokens = tokenize("match (x, y) with (a, b) => a")
            p = Parser(tokens)
            e = parse_expr(p)
            @test e.head == MatchPair
            @test e.args[1].head == LFPair  # expr, not just var
        end

        @testset "Match Sum" begin
            tokens = tokenize("match s with | inl(x) => x | inr(y) => y")
            p = Parser(tokens)
            e = parse_expr(p)
            @test e.head == MatchSum
            @test e.args[1].head == Var
            @test e.args[1].args[1] == :s
            @test e.args[2] == :x
            @test e.args[3].head == Var
            @test e.args[4] == :y
            @test e.args[5].head == Var
        end

        @testset "Match List" begin
            tokens = tokenize("match xs with | nil => * | cons(h, t) => h")
            p = Parser(tokens)
            e = parse_expr(p)
            @test e.head == Match
            @test e.args[1].head == Var
            @test e.args[1].args[1] == :xs
            @test e.args[2].head == Unit
            @test e.args[3] == (:h, :t)
            @test e.args[4].head == Var
        end

        @testset "Function Calls" begin
            tokens = tokenize("f(x)")
            p = Parser(tokens)
            e = parse_expr(p)
            @test e.head == FunApply
            @test e.args[1] == :f
            @test e.args[2].head == Var
            @test e.args[2].args[1] == :x

            tokens = tokenize("f(x, y)")
            p = Parser(tokens)
            e = parse_expr(p)
            @test e.head == FunApply
            @test e.args[1] == :f
            @test e.args[2].head == Var
            @test e.args[2].args[1] == :x
            @test e.args[3].head == Var
            @test e.args[3].args[1] == :y

            tokens = tokenize("f()")
            p = Parser(tokens)
            e = parse_expr(p)
            @test e.head == FunApply
            @test e.args[1] == :f
            @test length(e.args) == 1
        end

        @testset "Function Calls - with expression arguments" begin
            tokens = tokenize("f(true)")
            p = Parser(tokens)
            e = parse_expr(p)
            @test e.head == FunApply
            @test e.args[1] == :f
            @test e.args[2].head == ConstBool

            tokens = tokenize("f(cons(x, nil))")
            p = Parser(tokens)
            e = parse_expr(p)
            @test e.head == FunApply
            @test e.args[1] == :f
            @test e.args[2].head == Cons
        end
    end

    @testset "Function Definition Parsing" begin
        @testset "Simple Function" begin
            prog = parse_program("id : B -> B\nid(x) = x")
            @test haskey(prog.functions, :id)
            func = prog.functions[:id]
            @test length(func.params) == 1
            @test func.params[1] == :x
            @test func.return_type isa BoolType
            @test func.body.head == Var
        end

        @testset "Multi-parameter Function" begin
            prog = parse_program("and : B -> B -> B\nand(x, y) = x")
            @test haskey(prog.functions, :and)
            func = prog.functions[:and]
            @test length(func.params) == 2
            @test func.params[1] == :x
            @test func.params[2] == :y
        end

        @testset "Zero-parameter Function" begin
            prog = parse_program("getTrue : () -> B\ngetTrue() = true")
            @test haskey(prog.functions, :getTrue)
            func = prog.functions[:getTrue]
            @test length(func.params) == 0
            @test func.body.head == ConstBool
        end

        @testset "Complex Function Body" begin
            prog = parse_program("not : B -> B\nnot(x) = if x then false else true")
            @test haskey(prog.functions, :not)
            func = prog.functions[:not]
            @test func.body.head == If
        end

        @testset "Multiple Functions" begin
            code = """
            f : B -> B
            f(x) = x

            g : 1 -> 1
            g(y) = *
            """
            prog = parse_program(code)
            @test haskey(prog.functions, :f)
            @test haskey(prog.functions, :g)
        end

        @testset "Function with List Type" begin
            prog = parse_program("head : list B -> B\nhead(xs) = xs")
            @test haskey(prog.functions, :head)
            func = prog.functions[:head]
            @test func.param_types[1] isa ListType
            @test func.return_type isa BoolType
        end

        @testset "Function with Product Type" begin
            prog = parse_program("fst : B ⊗ 1 -> B\nfst(p) = p")
            @test haskey(prog.functions, :fst)
            func = prog.functions[:fst]
            @test func.param_types[1] isa ProductType
        end

        @testset "Function with Sum Type" begin
            prog = parse_program("f : B + 1 -> B\nf(x) = x")
            @test haskey(prog.functions, :f)
            func = prog.functions[:f]
            @test func.param_types[1] isa SumType
        end
    end

    @testset "Complex Programs" begin
        @testset "List Operations" begin
            code = """
            isEmpty : list B -> B
            isEmpty(xs) = match xs with
                | nil => true
                | cons(h, t) => false
            """
            prog = parse_program(code)
            @test haskey(prog.functions, :isEmpty)
        end

        @testset "With Comments" begin
            code = """
            -- This is a comment
            id : B -> B
            id(x) = x
            -- Another comment
            """
            prog = parse_program(code)
            @test haskey(prog.functions, :id)
        end

        @testset "Nested Expressions" begin
            code = """
            complex : B -> B
            complex(x) = let y = if x then true else false in
                         let z = cons(y, nil) in
                         match z with
                         | nil => false
                         | cons(h, t) => h
            """
            prog = parse_program(code)
            @test haskey(prog.functions, :complex)
            func = prog.functions[:complex]
            @test func.body.head == Let
        end
    end
end
