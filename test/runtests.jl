using Test
using ramu

@testset "ramu" begin
    @testset "Lexer" begin
        include("test_lexer.jl")
    end

    @testset "Parser" begin
        include("test_parser.jl")
    end

    @testset "Type Inference" begin
        include("test_inference.jl")
    end

    @testset "Type Checking" begin
        include("test_typecheck.jl")
    end

    @testset "Interpreter" begin
        include("test_interpret.jl")
    end

    @testset "Example Programs" begin
        include("test_examples.jl")
    end
end