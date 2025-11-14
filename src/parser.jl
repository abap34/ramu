mutable struct Parser
    tokens::Vector{Token}
    pos::Int
end

Parser(tokens::Vector{Token}) = Parser(tokens, 1)

function peek(p::Parser)::Token
    if p.pos <= length(p.tokens)
        return p.tokens[p.pos]
    end
    return p.tokens[end]  # EOF
end

function advance!(p::Parser)::Token
    tok = peek(p)
    if p.pos <= length(p.tokens)
        p.pos += 1
    end
    return tok
end

function expect!(p::Parser, type::TokenType)::Token
    tok = peek(p)
    if tok.type != type
        error("Expected $type but got $(tok.type) at line $(tok.line), col $(tok.col)")
    end
    return advance!(p)
end

function parse_type(p::Parser)::LFType
    return parse_sum_type(p)
end

function parse_sum_type(p::Parser)::LFType
    left = parse_product_type(p)

    while peek(p).type == TK_PLUS
        advance!(p)
        right = parse_product_type(p)
        left = SumType(left, right)
    end

    return left
end

function parse_product_type(p::Parser)::LFType
    left = parse_primary_type(p)

    while peek(p).type == TK_TENSOR
        advance!(p)
        right = parse_primary_type(p)
        left = ProductType(left, right)
    end

    return left
end

function parse_primary_type(p::Parser)::LFType
    tok = peek(p)

    if tok.type == TK_UNIT_TYPE
        advance!(p)
        return UnitType()
    elseif tok.type == TK_BOOL_TYPE
        advance!(p)
        return BoolType()
    elseif tok.type == TK_LIST
        advance!(p)
        elem_type = parse_primary_type(p)
        return ListType(elem_type)
    elseif tok.type == TK_LPAREN
        advance!(p)
        t = parse_type(p)
        expect!(p, TK_RPAREN)
        return t
    else
        error("Expected type at line $(tok.line), col $(tok.col)")
    end
end

function parse_function_type(p::Parser)::Tuple{Vector{LFType}, LFType}
    param_types = LFType[]

    # Check for empty parameter list: () -> ReturnType
    if peek(p).type == TK_LPAREN
        next_pos = p.pos + 1
        if next_pos <= length(p.tokens) && p.tokens[next_pos].type == TK_RPAREN
            # Empty parameter list
            advance!(p)  # consume (
            advance!(p)  # consume )
            expect!(p, TK_RARROW)
            return_type = parse_type(p)
            return (param_types, return_type)
        end
    end

    # Parse parameter types
    push!(param_types, parse_type(p))

    while peek(p).type == TK_RARROW
        advance!(p)
        if peek(p).type in [TK_UNIT_TYPE, TK_BOOL_TYPE, TK_LIST, TK_LPAREN]
            # More parameter types
            next_type = parse_type(p)

            # Check if this is the return type (no more arrows)
            if peek(p).type != TK_RARROW
                return (param_types, next_type)
            end

            push!(param_types, next_type)
        else
            error("Expected type after -> at line $(peek(p).line)")
        end
    end

    error("Function type must have at least one -> at line $(peek(p).line)")
end

function parse_expr(p::Parser)::LFExpr
    tok = peek(p)

    # let expression
    if tok.type == TK_LET
        return parse_let(p)
    end

    # if expression
    if tok.type == TK_IF
        return parse_if(p)
    end

    # match expression
    if tok.type == TK_MATCH
        return parse_match(p)
    end

    # Primary expression
    return parse_primary_expr(p)
end

function parse_let(p::Parser)::LFExpr
    expect!(p, TK_LET)
    var_tok = expect!(p, TK_IDENT)
    var = Symbol(var_tok.value)
    expect!(p, TK_EQUAL)
    e1 = parse_expr(p)
    expect!(p, TK_IN)
    e2 = parse_expr(p)
    return LFExpr(Let, [var, e1, e2])
end

function parse_if(p::Parser)::LFExpr
    expect!(p, TK_IF)
    cond = parse_primary_expr(p)  # Parse expression instead of just identifier
    expect!(p, TK_THEN)
    then_expr = parse_expr(p)
    expect!(p, TK_ELSE)
    else_expr = parse_expr(p)
    return LFExpr(If, [cond, then_expr, else_expr])
end

function parse_match(p::Parser)::LFExpr
    expect!(p, TK_MATCH)
    matched_expr = parse_primary_expr(p)  # Parse expression instead of just identifier
    expect!(p, TK_WITH)

    # Check what kind of match
    tok = peek(p)

    if tok.type == TK_LPAREN
        # MatchPair: match x with (a, b) => e
        advance!(p)
        x1_tok = expect!(p, TK_IDENT)
        x1 = Symbol(x1_tok.value)
        expect!(p, TK_COMMA)
        x2_tok = expect!(p, TK_IDENT)
        x2 = Symbol(x2_tok.value)
        expect!(p, TK_RPAREN)
        expect!(p, TK_ARROW)
        body = parse_expr(p)
        return LFExpr(MatchPair, [matched_expr, x1, x2, body])

    elseif tok.type == TK_PIPE
        # Either MatchSum or Match (list)
        advance!(p)
        tok = peek(p)

        if tok.type == TK_INL
            # MatchSum
            advance!(p)
            expect!(p, TK_LPAREN)
            inl_var_tok = expect!(p, TK_IDENT)
            inl_var = Symbol(inl_var_tok.value)
            expect!(p, TK_RPAREN)
            expect!(p, TK_ARROW)
            inl_expr = parse_expr(p)
            expect!(p, TK_PIPE)
            expect!(p, TK_INR)
            expect!(p, TK_LPAREN)
            inr_var_tok = expect!(p, TK_IDENT)
            inr_var = Symbol(inr_var_tok.value)
            expect!(p, TK_RPAREN)
            expect!(p, TK_ARROW)
            inr_expr = parse_expr(p)
            return LFExpr(MatchSum, [matched_expr, inl_var, inl_expr, inr_var, inr_expr])

        elseif tok.type == TK_NIL
            # Match (list)
            advance!(p)
            expect!(p, TK_ARROW)
            nil_expr = parse_expr(p)
            expect!(p, TK_PIPE)
            expect!(p, TK_CONS)
            expect!(p, TK_LPAREN)
            h_tok = expect!(p, TK_IDENT)
            h = Symbol(h_tok.value)
            expect!(p, TK_COMMA)
            t_tok = expect!(p, TK_IDENT)
            t = Symbol(t_tok.value)
            expect!(p, TK_RPAREN)
            expect!(p, TK_ARROW)
            cons_expr = parse_expr(p)
            return LFExpr(Match, [matched_expr, nil_expr, (h, t), cons_expr])

        else
            error("Expected 'inl' or 'nil' after '|' at line $(tok.line), col $(tok.col)")
        end

    else
        error("Expected '(' or '|' after 'with' at line $(tok.line), col $(tok.col)")
    end
end

function parse_primary_expr(p::Parser)::LFExpr
    tok = peek(p)

    # true/false
    if tok.type == TK_TRUE
        advance!(p)
        return LFExpr(ConstBool, [true])
    elseif tok.type == TK_FALSE
        advance!(p)
        return LFExpr(ConstBool, [false])
    end

    # *
    if tok.type == TK_STAR
        advance!(p)
        return LFExpr(Unit, [])
    end

    # nil
    if tok.type == TK_NIL
        advance!(p)
        return LFExpr(Nil, [])
    end

    # inl(expr)
    if tok.type == TK_INL
        advance!(p)
        expect!(p, TK_LPAREN)
        arg = parse_expr(p)  # Parse expression instead of just identifier
        expect!(p, TK_RPAREN)
        return LFExpr(Inl, [arg])
    end

    # inr(expr)
    if tok.type == TK_INR
        advance!(p)
        expect!(p, TK_LPAREN)
        arg = parse_expr(p)  # Parse expression instead of just identifier
        expect!(p, TK_RPAREN)
        return LFExpr(Inr, [arg])
    end

    # cons(expr, expr)
    if tok.type == TK_CONS
        advance!(p)
        expect!(p, TK_LPAREN)
        h = parse_expr(p)  # Parse expression instead of just identifier
        expect!(p, TK_COMMA)
        t = parse_expr(p)  # Parse expression instead of just identifier
        expect!(p, TK_RPAREN)
        return LFExpr(Cons, [h, t])
    end

    # (expr, expr) or (expr)
    if tok.type == TK_LPAREN
        advance!(p)

        # Parse first expression
        first_expr = parse_expr(p)

        if peek(p).type == TK_COMMA
            # LFPair (expr, expr)
            advance!(p)
            second_expr = parse_expr(p)
            expect!(p, TK_RPAREN)
            return LFExpr(LFPair, [first_expr, second_expr])
        else
            # Just (expr)
            expect!(p, TK_RPAREN)
            return first_expr
        end
    end

    # Variable or function call
    if tok.type == TK_IDENT
        name = Symbol(tok.value)
        advance!(p)

        # Check for function call
        if peek(p).type == TK_LPAREN
            advance!(p)
            args = []

            if peek(p).type != TK_RPAREN
                # Parse first argument as expression
                arg_expr = parse_expr(p)
                push!(args, arg_expr)

                while peek(p).type == TK_COMMA
                    advance!(p)
                    arg_expr = parse_expr(p)
                    push!(args, arg_expr)
                end
            end

            expect!(p, TK_RPAREN)
            return LFExpr(FunApply, [name, args...])
        else
            # Just a variable
            return LFExpr(Var, [name])
        end
    end

    error("Unexpected token $(tok.type) at line $(tok.line), col $(tok.col)")
end

function parse_function(p::Parser)::Tuple{Symbol, FunctionDef}
    # Function name
    name_tok = expect!(p, TK_IDENT)
    fname = Symbol(name_tok.value)

    expect!(p, TK_COLON)

    # Function type
    param_types, return_type = parse_function_type(p)

    # Function definition
    name_tok2 = expect!(p, TK_IDENT)
    if Symbol(name_tok2.value) != fname
        error("Function name mismatch: expected $fname, got $(name_tok2.value)")
    end

    expect!(p, TK_LPAREN)

    # Parameters
    params = Symbol[]
    if peek(p).type != TK_RPAREN
        param_tok = expect!(p, TK_IDENT)
        push!(params, Symbol(param_tok.value))

        while peek(p).type == TK_COMMA
            advance!(p)
            param_tok = expect!(p, TK_IDENT)
            push!(params, Symbol(param_tok.value))
        end
    end

    expect!(p, TK_RPAREN)
    expect!(p, TK_EQUAL)

    # Function body
    body = parse_expr(p)

    if length(params) != length(param_types)
        error("Parameter count mismatch for function $fname")
    end

    func_def = FunctionDef(params, param_types, return_type, body)
    return (fname, func_def)
end

function parse_program(input::String)::Program
    tokens = tokenize(input)
    p = Parser(tokens)

    functions = Dict{Symbol, FunctionDef}()

    while peek(p).type != TK_EOF
        fname, func_def = parse_function(p)
        functions[fname] = func_def
    end

    return Program(functions)
end

function parse_from_file(filename::String)::Program
    input = read(filename, String)
    return parse_program(input)
end
