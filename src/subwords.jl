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
            if (i2 <= i1 <= i1 + ngram1.n <= i2 + ngram2.n
                || i1 <= i2 < i1 + ngram1.n
                || i2 <= i1 < i2 + ngram2.n)
                ret += 1
            end
        end
    end
    ret = ret / (length(inds2))# * length(inds1))
    #display((ret, utterance.text, ngram1.text, ngram2.text))
    return ret
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

function increment_overlaps!(ngram::NGram, utterance::Utterance, ngrams::PriorityQueue)
    println("incrementing ", ngram.text, " in ", utterance.text)
    seen = Set([ngram])
    for ngram2 in utterance.ngrams
        if !(ngram2 in seen)
            olfrac = overlaps(utterance, ngram2, ngram)
            ngram2.count += utterance.count * olfrac
            if olfrac > 0 && ngram2 in keys(ngrams)
                ngrams[ngram2] = entropy(ngram2)
            end
            push!(seen, ngram2)
        end
    end
end

function decrement_overlaps!(ngram::NGram, ngrams::PriorityQueue)
    for utterance in keys(ngram.inds)
        seen = Set([ngram])
        for ngram2 in utterance.ngrams
            if !(ngram2 in seen)
                #display((ngram.text,ngram2.text=>ngram2.count, utterance.text=>utterance.count, overlaps(utterance, ngram2, ngram)))
                olfrac = overlaps(utterance, ngram2, ngram)
                ngram2.count -= utterance.count * olfrac
                if olfrac > 0 && ngram2 in keys(ngrams)
                    #otherwise maybe update?
                    ngrams[ngram2] = entropy(ngram2)
                elseif olfrac > 0
                    increment_overlaps!(ngram2, utterance, ngrams)
                end
                # if ngram2.text in vocab
                #     pop!(vocab, ngram2.text)
                # end
                # ngrams[ngram2] = entropy(ngram2)
                push!(seen, ngram2)
            end
        end
    end
end

function buildvocab(counts::Accumulator{S}, maxsize::Int) where {S}
    vocab = OrderedDict{S, Float64}(counter(string.(convert(Vector{Char}, join(keys(counts), "")))))
    #vocab = OrderedDict{S, Float64}()
    ngrams = Dict(ng => entropy(ng) for ng in buildngrams(counts))
    ngrams = PriorityQueue(ngrams, Base.Order.Reverse)
    while length(vocab) < maxsize
        #display(structure(ngrams))
        best, bestentropy = dequeue_pair!(ngrams)
        if bestentropy <= 0
            break
        end
        println("decrementing \"", best.text, "\" with entropy ", bestentropy)
        decrement_overlaps!(best, ngrams)
        vocab[best.text] = bestentropy
    end
    return vocab
end

function segment(utext::S, vocab::Associative{S, Float64}) where {S}
    vocab = keys(vocab)
    i, segments = 1, OrderedDict(1 => Vector{S}())
    while true
        j = i
        while j <= endof(utext)
            nextj = j < endof(utext) ? nextind(utext, j): j + 1
            subword = utext[i:j]
            #display(subword => subword in vocab)
            if subword in vocab
                curlen = if nextj in keys(segments)
                    length(segments[nextj])
                else
                    length(utext) + 1
                end
                if length(segments[i]) + 1 < curlen
                    segments[nextj] = segments[i][:]
                    push!(segments[nextj], subword)
                end
            end
            j = nextj
        end
        inds = collect(keys(segments))
        sort!(inds)
        firstind = findfirst(inds .== i)
        if firstind < length(inds)
            i = inds[firstind + 1]
        else
            break
        end
    end
    #display(segments)
    return segments[endof(utext) + 1]
end

# def __call__(self, utext):
#     #print(utterance)
#     i, segments = 0, {0: []}
#     while True:
#         for j in range(i + 1, len(utterance) + 1):
#             if utterance[i:j] in self.vocab:
#                 #print(i, j, segments)
#                 curlen = len(segments[j]) if j in segments else len(utterance) + 1
#                 if len(segments[i]) + 1 < curlen:
#                     segments[j] = segments[i] + [utterance[i:j]]
#         #print(i, segments)
#         inds = sorted(segments.keys())
#         if inds.index(i) < len(inds) - 1:
#             i = inds[inds.index(i) + 1]
#         else:
#             break
#     return segments[len(utterance)]
