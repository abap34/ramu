@enum TokenType begin
    # Keywords
    TK_LET
    TK_IN
    TK_IF
    TK_THEN
    TK_ELSE
    TK_MATCH
    TK_MATCH_PRIME
    TK_WITH
    TK_NIL
    TK_CONS
    TK_INL
    TK_INR
    TK_TRUE
    TK_FALSE
    TK_LIST

    # Symbols
    TK_LPAREN      # (
    TK_RPAREN      # )
    TK_COMMA       # ,
    TK_PIPE        # |
    TK_ARROW       # =>
    TK_COLON       # :
    TK_RARROW      # ->
    TK_TENSOR      # ⊗
    TK_PLUS        # +
    TK_EQUAL       # =
    TK_STAR        # *

    # Types
    TK_UNIT_TYPE   # 1
    TK_BOOL_TYPE   # B

    # Others
    TK_IDENT       # identifier
    TK_EOF
end

struct Token
    type::TokenType
    value::String
    line::Int
    col::Int
end

mutable struct Lexer
    input::String
    pos::Int
    line::Int
    col::Int
end

Lexer(input::String) = Lexer(input, 1, 1, 1)

function peek(lex::Lexer)::Char
    if lex.pos > ncodeunits(lex.input)
        return '\0'
    end
    if !isvalid(lex.input, lex.pos)
        return '\0'
    end
    return lex.input[lex.pos]
end

function advance!(lex::Lexer)::Char
    if lex.pos > ncodeunits(lex.input)
        return '\0'
    end
    c = lex.input[lex.pos]
    lex.pos = nextind(lex.input, lex.pos)
    if c == '\n'
        lex.line += 1
        lex.col = 1
    else
        lex.col += 1
    end
    return c
end

function skip_whitespace!(lex::Lexer)
    while peek(lex) in [' ', '\t', '\n', '\r']
        advance!(lex)
    end
end

function skip_comments_and_whitespace!(lex::Lexer)
    while true
        skip_whitespace!(lex)

        c = peek(lex)

        # Line comment: --
        if c == '-'
            old_pos = lex.pos
            old_line = lex.line
            old_col = lex.col
            advance!(lex)
            if peek(lex) == '-'
                advance!(lex)
                # Skip until end of line
                while peek(lex) != '\n' && peek(lex) != '\0'
                    advance!(lex)
                end
                continue
            else
                # Not a comment, restore position
                lex.pos = old_pos
                lex.line = old_line
                lex.col = old_col
                break
            end
        else
            break
        end
    end
end

function read_identifier(lex::Lexer)::String
    start = lex.pos
    while true
        c = peek(lex)
        if isalpha(c) || isdigit(c) || c == '_' || c == '\''
            advance!(lex)
        else
            break
        end
    end
    return lex.input[start:prevind(lex.input, lex.pos)]
end

isalpha(c::Char)::Bool = ('a' <= c <= 'z') || ('A' <= c <= 'Z')

function next_token!(lex::Lexer)::Token
    skip_comments_and_whitespace!(lex)

    line = lex.line
    col = lex.col
    c = peek(lex)

    # EOF
    if c == '\0'
        return Token(TK_EOF, "", line, col)
    end

    # Single character tokens
    if c == '('
        advance!(lex)
        return Token(TK_LPAREN, "(", line, col)
    elseif c == ')'
        advance!(lex)
        return Token(TK_RPAREN, ")", line, col)
    elseif c == ','
        advance!(lex)
        return Token(TK_COMMA, ",", line, col)
    elseif c == '|'
        advance!(lex)
        return Token(TK_PIPE, "|", line, col)
    elseif c == ':'
        advance!(lex)
        return Token(TK_COLON, ":", line, col)
    elseif c == '+'
        advance!(lex)
        return Token(TK_PLUS, "+", line, col)
    elseif c == '*'
        advance!(lex)
        return Token(TK_STAR, "*", line, col)
    elseif c == '⊗'
        advance!(lex)
        return Token(TK_TENSOR, "⊗", line, col)
    end

    # Multi-character operators
    if c == '='
        advance!(lex)
        if peek(lex) == '>'
            advance!(lex)
            return Token(TK_ARROW, "=>", line, col)
        else
            return Token(TK_EQUAL, "=", line, col)
        end
    end

    if c == '-'
        advance!(lex)
        if peek(lex) == '>'
            advance!(lex)
            return Token(TK_RARROW, "->", line, col)
        else
            error("Unexpected character '-' at line $line, col $col")
        end
    end

    # Numbers (for type 1)
    if isdigit(c)
        advance!(lex)
        if c == '1'
            return Token(TK_UNIT_TYPE, "1", line, col)
        else
            error("Unexpected digit '$c' at line $line, col $col")
        end
    end

    # Identifiers and keywords
    if isalpha(c) || c == '_'
        ident = read_identifier(lex)

        # Keywords
        if ident == "let"
            return Token(TK_LET, ident, line, col)
        elseif ident == "in"
            return Token(TK_IN, ident, line, col)
        elseif ident == "if"
            return Token(TK_IF, ident, line, col)
        elseif ident == "then"
            return Token(TK_THEN, ident, line, col)
        elseif ident == "else"
            return Token(TK_ELSE, ident, line, col)
        elseif ident == "match"
            return Token(TK_MATCH, ident, line, col)
        elseif ident == "match'"
            return Token(TK_MATCH_PRIME, ident, line, col)
        elseif ident == "with"
            return Token(TK_WITH, ident, line, col)
        elseif ident == "nil"
            return Token(TK_NIL, ident, line, col)
        elseif ident == "cons"
            return Token(TK_CONS, ident, line, col)
        elseif ident == "inl"
            return Token(TK_INL, ident, line, col)
        elseif ident == "inr"
            return Token(TK_INR, ident, line, col)
        elseif ident == "true"
            return Token(TK_TRUE, ident, line, col)
        elseif ident == "false"
            return Token(TK_FALSE, ident, line, col)
        elseif ident == "list"
            return Token(TK_LIST, ident, line, col)
        elseif ident == "B"
            return Token(TK_BOOL_TYPE, ident, line, col)
        else
            return Token(TK_IDENT, ident, line, col)
        end
    end

    error("Unexpected character '$c' at line $line, col $col")
end

function tokenize(input::String)::Vector{Token}
    lex = Lexer(input)
    tokens = Token[]

    while true
        tok = next_token!(lex)
        push!(tokens, tok)
        if tok.type == TK_EOF
            break
        end
    end

    return tokens
end
