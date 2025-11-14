@testset "Lexer Tests" begin
    @testset "Keywords" begin
        @test tokenize("let")[1].type == TK_LET
        @test tokenize("in")[1].type == TK_IN
        @test tokenize("if")[1].type == TK_IF
        @test tokenize("then")[1].type == TK_THEN
        @test tokenize("else")[1].type == TK_ELSE
        @test tokenize("match")[1].type == TK_MATCH
        @test tokenize("with")[1].type == TK_WITH
        @test tokenize("nil")[1].type == TK_NIL
        @test tokenize("cons")[1].type == TK_CONS
        @test tokenize("inl")[1].type == TK_INL
        @test tokenize("inr")[1].type == TK_INR
        @test tokenize("true")[1].type == TK_TRUE
        @test tokenize("false")[1].type == TK_FALSE
        @test tokenize("list")[1].type == TK_LIST
    end

    @testset "Type Tokens" begin
        @test tokenize("B")[1].type == TK_BOOL_TYPE
        @test tokenize("1")[1].type == TK_UNIT_TYPE
    end

    @testset "Symbols" begin
        @test tokenize("(")[1].type == TK_LPAREN
        @test tokenize(")")[1].type == TK_RPAREN
        @test tokenize(",")[1].type == TK_COMMA
        @test tokenize("|")[1].type == TK_PIPE
        @test tokenize("=>")[1].type == TK_ARROW
        @test tokenize(":")[1].type == TK_COLON
        @test tokenize("->")[1].type == TK_RARROW
        @test tokenize("⊗")[1].type == TK_TENSOR
        @test tokenize("+")[1].type == TK_PLUS
        @test tokenize("=")[1].type == TK_EQUAL
        @test tokenize("*")[1].type == TK_STAR
    end

    @testset "Identifiers" begin
        tokens = tokenize("x")
        @test tokens[1].type == TK_IDENT
        @test tokens[1].value == "x"

        tokens = tokenize("myVar")
        @test tokens[1].type == TK_IDENT
        @test tokens[1].value == "myVar"

        tokens = tokenize("foo_bar")
        @test tokens[1].type == TK_IDENT
        @test tokens[1].value == "foo_bar"

        tokens = tokenize("x'")
        @test tokens[1].type == TK_IDENT
        @test tokens[1].value == "x'"
    end

    @testset "Whitespace Handling" begin
        tokens = tokenize("  let   in  ")
        @test tokens[1].type == TK_LET
        @test tokens[2].type == TK_IN
        @test tokens[3].type == TK_EOF

        tokens = tokenize("let\n\tin")
        @test tokens[1].type == TK_LET
        @test tokens[2].type == TK_IN
    end

    @testset "Line Comments" begin
        tokens = tokenize("let -- this is a comment\nin")
        @test tokens[1].type == TK_LET
        @test tokens[2].type == TK_IN
        @test tokens[3].type == TK_EOF

        tokens = tokenize("-- comment at start\nlet")
        @test tokens[1].type == TK_LET
        @test tokens[2].type == TK_EOF
    end

    @testset "Multiple Line Comments" begin
        tokens = tokenize("let -- comment 1\n-- comment 2\nin")
        @test tokens[1].type == TK_LET
        @test tokens[2].type == TK_IN
        @test tokens[3].type == TK_EOF
    end

    @testset "Comments do not interfere with operators" begin
        # Test that (*...*)  pattern in code is not treated as comment
        tokens = tokenize("cons(*, nil)")
        @test tokens[1].type == TK_CONS
        @test tokens[2].type == TK_LPAREN
        @test tokens[3].type == TK_STAR
        @test tokens[4].type == TK_COMMA
        @test tokens[5].type == TK_NIL
        @test tokens[6].type == TK_RPAREN
        @test tokens[7].type == TK_EOF
    end

    @testset "Complex Expressions" begin
        tokens = tokenize("if x then true else false")
        @test tokens[1].type == TK_IF
        @test tokens[2].type == TK_IDENT
        @test tokens[3].type == TK_THEN
        @test tokens[4].type == TK_TRUE
        @test tokens[5].type == TK_ELSE
        @test tokens[6].type == TK_FALSE

        tokens = tokenize("let x = cons(h, t) in x")
        @test tokens[1].type == TK_LET
        @test tokens[2].type == TK_IDENT
        @test tokens[3].type == TK_EQUAL
        @test tokens[4].type == TK_CONS
        @test tokens[5].type == TK_LPAREN
        @test tokens[6].type == TK_IDENT
        @test tokens[7].type == TK_COMMA
        @test tokens[8].type == TK_IDENT
        @test tokens[9].type == TK_RPAREN
        @test tokens[10].type == TK_IN
        @test tokens[11].type == TK_IDENT
    end

    @testset "Type Expressions" begin
        tokens = tokenize("B -> B")
        @test tokens[1].type == TK_BOOL_TYPE
        @test tokens[2].type == TK_RARROW
        @test tokens[3].type == TK_BOOL_TYPE

        tokens = tokenize("list B")
        @test tokens[1].type == TK_LIST
        @test tokens[2].type == TK_BOOL_TYPE

        tokens = tokenize("B ⊗ B")
        @test tokens[1].type == TK_BOOL_TYPE
        @test tokens[2].type == TK_TENSOR
        @test tokens[3].type == TK_BOOL_TYPE

        tokens = tokenize("B + 1")
        @test tokens[1].type == TK_BOOL_TYPE
        @test tokens[2].type == TK_PLUS
        @test tokens[3].type == TK_UNIT_TYPE
    end

    @testset "Match Expressions" begin
        tokens = tokenize("match xs with | nil => * | cons(h, t) => h")
        @test tokens[1].type == TK_MATCH
        @test tokens[2].type == TK_IDENT
        @test tokens[3].type == TK_WITH
        @test tokens[4].type == TK_PIPE
        @test tokens[5].type == TK_NIL
        @test tokens[6].type == TK_ARROW
        @test tokens[7].type == TK_STAR
        @test tokens[8].type == TK_PIPE
        @test tokens[9].type == TK_CONS
    end

    @testset "Sum Type Constructors" begin
        tokens = tokenize("inl(x)")
        @test tokens[1].type == TK_INL
        @test tokens[2].type == TK_LPAREN
        @test tokens[3].type == TK_IDENT
        @test tokens[4].type == TK_RPAREN

        tokens = tokenize("inr(y)")
        @test tokens[1].type == TK_INR
        @test tokens[2].type == TK_LPAREN
        @test tokens[3].type == TK_IDENT
        @test tokens[4].type == TK_RPAREN
    end

    @testset "Line and Column Tracking" begin
        tokens = tokenize("let\nx")
        @test tokens[1].line == 1
        @test tokens[1].col == 1
        @test tokens[2].line == 2
        @test tokens[2].col == 1

        tokens = tokenize("  let  x")
        @test tokens[1].line == 1
        @test tokens[1].col == 3  # after two spaces
        @test tokens[2].line == 1
        @test tokens[2].col == 8  # after "let  "
    end

    @testset "Error Cases" begin
        @test_throws ErrorException tokenize("2")  # Only 1 is allowed
        @test_throws ErrorException tokenize("@")  # Invalid character
    end
end
