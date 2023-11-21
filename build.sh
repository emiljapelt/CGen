dune build
if [ -e ./main.exe ] ; then rm -f ./main.exe; fi
mv ./_build/default/src/main.exe ./main.exe