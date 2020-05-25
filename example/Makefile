.PHONY : all deploy clean

all : app/simple.min.js app/stlc.min.js

app/%.min.js : app/%.js
	uglifyjs $< --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=$@

app/simple.js : src/Simple.elm
	elm make $< --output=$@

app/stlc.js : src/STLC.elm
	elm make $< --output=$@

deploy : app/main.min.js
	(cd .. ; git subtree push --squash --prefix example/app origin gh-pages)

clean :
	-rm app/simple.js app/simple.min.js
	-rm app/stlc.js app/stlc.min.js 
