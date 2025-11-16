@testset "Example Programs" begin
    # Dynamically discover all .ramu files in examples/ directory
    examples_dir = joinpath(@__DIR__, "..", "examples")
    example_files = filter(f -> endswith(f, ".ramu"), readdir(examples_dir))

    if isempty(example_files)
        @warn "No .ramu files found in examples/ directory"
    end

    # Sort for consistent test ordering
    sort!(example_files)

    @testset "$(filename)" for filename in example_files
        filepath = joinpath(examples_dir, filename)

        # Parse the program
        prog = parse_from_file(filepath)

        # Basic sanity checks
        @test !isempty(prog.functions)
        @test haskey(prog.functions, :main) "$(filename) should have a main function"

        # Try to run the program
        # Some programs may require specific heap sizes or fail intentionally
        try
            result, state = run_program(prog; heap_size=100)
            @test result isa LFVal "$(filename) should return a valid value"

            # Check that heap size is reasonable
            @test state.m >= 0 "$(filename) should not have negative heap space"
            @test length(state.σ) <= 100 "$(filename) should not exceed heap allocation"
        catch e
            # If the program fails, it might be intentional (e.g., testing heap exhaustion)
            # or a bug. For now, we'll rethrow to catch bugs
            @warn "$(filename) failed during execution" exception=e
            rethrow(e)
        end
    end
end
