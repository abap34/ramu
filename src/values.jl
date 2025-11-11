abstract type Val end

struct BoolVal <: Val
    value::Bool
end

struct UnitVal <: Val end

struct LocVal <: Val
    loc::Int
end

struct NilVal <: Val end

struct PairVal <: Val
    fst::Val
    snd::Val
end

struct InlVal <: Val
    value::Val
end

struct InrVal <: Val
    value::Val
end
