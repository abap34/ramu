import Base

abstract type LFType end

struct TypeVar
    name::Symbol
end

const typevarnames = ["α", "β", "τ"]
const subscripts = Dict(
    0 => "",
    1 => "₁",
    2 => "₂",
    3 => "₃",
    4 => "₄",
    5 => "₅",
    6 => "₆",
    7 => "₇",
    8 => "₈",
    9 => "₉"
)

fresh_typevar = let
    counter = 0
    () -> begin
        name = Symbol(typevarnames[mod1(counter + 1, length(typevarnames))] *
                      join(subscripts[d] for d in digits(div(counter, length(typevarnames)))))
        counter += 1
        return TypeVar(name)
    end
end

const TypeLike = Union{LFType, TypeVar}

# A ::= 1 | B | List A | A ⊗ A | A + A
# F ::= (A1, A2, ..., An) -> A
struct UnitType <: LFType end
struct BoolType <: LFType end
struct ListType <: LFType
    elem_type::TypeLike
end

struct ProductType <: LFType
    fst_type::TypeLike
    snd_type::TypeLike
end

struct SumType <: LFType
    inl_type::TypeLike
    inr_type::TypeLike
end

struct FuncType <: LFType
    param_types::Vector{TypeLike}
    return_type::TypeLike
end

Base.show(io::IO, tv::TypeVar) = print(io, tv.name)
Base.show(io::IO, ::UnitType) = print(io, "1")
Base.show(io::IO, ::BoolType) = print(io, "B")
Base.show(io::IO, t::ListType) = print(io, "List ", t.elem_type)
Base.show(io::IO, t::ProductType) = print(io, "(", t.fst_type, " ⊗ ", t.snd_type, ")")
Base.show(io::IO, t::SumType) = print(io, "(", t.inl_type, " + ", t.inr_type, ")")
Base.show(io::IO, t::FuncType) = begin
    print(io, "(")
    for (i, pt) in enumerate(t.param_types)
        print(io, pt)
        if i < length(t.param_types)
            print(io, ", ")
        end
    end
    print(io, ") -> ", t.return_type)
end

Base.:(==)(t1::TypeVar, t2::TypeVar) = t1.name == t2.name
Base.:(==)(::UnitType, ::UnitType) = true
Base.:(==)(::BoolType, ::BoolType) = true
Base.:(==)(t1::ListType, t2::ListType) = t1.elem_type == t2.elem_type
Base.:(==)(t1::ProductType, t2::ProductType) = t1.fst_type == t2.fst_type && t1.snd_type == t2.snd_type
Base.:(==)(t1::SumType, t2::SumType) = t1.inl_type == t2.inl_type && t1.inr_type == t2.inr_type
Base.:(==)(t1::FuncType, t2::FuncType) = t1.param_types == t2.param_types && t1.return_type == t2.return_type
Base.:(==)(::T, ::S) where {T<:LFType, S<:LFType} = false

