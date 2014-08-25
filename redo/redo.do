
echo ">> $REDO_TARGET" 1>&2
redo-ifchange redo.hs
ghc -o $3 -v0 redo.hs