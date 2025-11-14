abstract type LFVal end

struct BoolVal <: LFVal
    value::Bool
end

struct UnitVal <: LFVal end

struct LocVal <: LFVal
    loc::Int
end

struct NilVal <: LFVal end

struct PairVal <: LFVal
    fst::LFVal
    snd::LFVal
end

struct InlVal <: LFVal
    value::LFVal
end

struct InrVal <: LFVal
    value::LFVal
end
