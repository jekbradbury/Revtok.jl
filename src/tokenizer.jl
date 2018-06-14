using Base.UTF8proc: category_abbrev

const HALF = ' '
const CAP = '\ue302'
const SPACE_PRIORITY = Dict('L' => 7, 'M' => 7, 'N' => 5, 'S' => 3,
                            'P' => 1, 'Z' => -1, 'C' => -3)

function space_priority(char)
    return SPACE_PRIORITY[category_abbrev(char)[1]]
end

"Simple reversible tokenizer"
function tokenize(s::AbstractString, decap::Bool=false, split_punctuation=True)
    toks = [Vector{Char}()]
    current_cat = 0 # or -2?
    for c in s
        cat = space_priority(c)
        if c == ' '
            push!(toks[end], HALF)
            push!(toks, [HALF])
            current_cat = 0
            continue
        elseif current_cat == 0
            push!(toks[end], c)
        elseif cat == current_cat && (cat > 2 || !split_punctuation)
            push!(toks[end], c) # HALF + c
        elseif cat <= 0 && current_cat <= 0
            push!(toks, [c])
        elseif cat < current_cat
            push!(toks[end], HALF)
            push!(toks, [c])
        else
            push!(toks, [HALF, c])
        end
        current_cat = cat
    end
    if toks[1] == Vector{Char}()
        toks = toks[2:end]
    end
    if current_cat > 0
        push!(toks[end], HALF)
    end
    toks = map(arr -> convert(String, arr), toks)
    if decap
        return decapitalize.(toks)
    end
    return toks
end

function decapitalize(tok)
    pre, tok = tok[1] == HALF ? (HALF, tok[2:end]) : ("", tok)
    if length(tok) < 1 || tok[1] == lowercase(tok[1])
        return string(pre, tok)
    end
    if (tok[1] == uppercase(tok[1]) &&
        (length(tok) == 1 || tok[2] != uppercase(tok[2])))
        return string(CAP, pre, lcfirst(tok))
    end
    return string(pre, tok)
end

function detokenize(l)
    text = replace(join(l, ""), "$CAP$HALF", "$HALF$CAP")
    text = replace(text, Regex("$HALF+"), s->" " ^ (length(s) ÷ 2))
    return replace(text, Regex("\\Q$CAP\\E.", "s"), s->uppercase(s[end]))
end
