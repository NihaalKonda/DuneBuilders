opam update
opam install graphics
eval $(opam env)

brew install xquartz
open -a XQuartz
export DISPLAY=:0