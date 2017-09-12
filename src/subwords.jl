using DataStructures

mutable struct NGram{S<:AbstractString, T}
    n::Int
    text::S
    inds::Dict{T, Set{Int}}
    count::Float64
    function NGram{S, T}(text) where {S<:AbstractString, T}
        new(length(text), text, Dict{T, Set{Int}}(), 0)
    end
end

entropy(ngram::NGram) = ngram.count * (ngram.n - 1)

struct Utterance{S<:AbstractString}
    text::S
    count::Int
    ngrams::Set{NGram{S, Utterance{S}}}
    function Utterance{S}(text::S, count::Int) where {S<:AbstractString}
        new(text, count, Set{NGram{S, Utterance{S}}}())
    end
end

NGram(text::S) where {S} = NGram{S, Utterance{S}}(text)
Utterance(text::S, count::Int) where {S} = Utterance{S}(text, count)

function overlaps(utterance, ngram1, ngram2)
    inds1, inds2 = ngram1.inds[utterance], ngram2.inds[utterance]
    ret = 0
    for i1 in inds1
        for i2 in inds2
            if !(i1 <= i2 <= i2 + ngram2.n <= i1 + ngram1.n)
                ret += 1
            end
        end
    end
    return ret / (length(inds1) * length(inds2))
end

function addinstance!(ngram::NGram, utterance::Utterance, i::Int)
    ngram.count += utterance.count
    inds = get!(Set{Int}, ngram.inds, utterance)
    push!(inds, i)
    push!(utterance.ngrams, ngram)
end

function buildngrams!(utterance::Utterance{S}, ngrams) where {S}
    utext = utterance.text
    i = 1
    while i < endof(utext)
        j = i
        while j < endof(utext)
            j = nextind(utext, j)
            text = utterance.text[i:j]
            ngram = get!(()->NGram(text), ngrams, text)
            addinstance!(ngram, utterance, i)
        end
        i = nextind(utext, i)
    end
end

function buildngrams(counts::Accumulator{S}) where {S}
    ngrams = Dict{S, NGram{S, Utterance{S}}}()
    for (text, count) in counts
        utterance = Utterance(text, count)
        buildngrams!(utterance, ngrams)
    end
    return values(ngrams)
end

function buildvocab(counts::Accumulator{S}, maxsize::Int) where {S}
    vocab = OrderedDict{S, Float64}(counter(string.(convert(Vector{Char}, join(keys(counts), "")))))
    #vocab = OrderedDict{S, Float64}()
    ngrams = Dict(ng => entropy(ng) for ng in buildngrams(counts))
    ngrams = PriorityQueue(ngrams, Base.Order.Reverse)
    while length(vocab) < maxsize
        best, bestentropy = dequeue_pair!(ngrams)
        for utterance in keys(best.inds)
            seen = Set([best])
            for ngram in utterance.ngrams
                if !(ngram in seen)
                    ngram.count -= utterance.count * overlaps(utterance, ngram, best)
                    if ngram in keys(ngrams)
                        #otherwise maybe update?
                        ngrams[ngram] = entropy(ngram)
                    end
                    push!(seen, ngram)
                end
            end
        end
        vocab[best.text] = bestentropy
    end
    return vocab
end
