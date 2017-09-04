using Revtok
using Base.Test

# write your own tests here
@test begin
    text = replace(normalize_string(readstring(
        download("http://www.gutenberg.org/cache/epub/1661/pg1661.txt"))[4:end],
        newline2lf=true), r"\n+", s->length(s) == 1 ? " " : "\n" ^ (length(s) - 1));
    detokenize(tokenize(text)) == text
end
