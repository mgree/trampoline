.PHONY : all deploy clean

all : app/main.min.js

app/%.min.js : app/%.js
	uglifyjs $< --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=$@

app/main.js : src/Main.elm
	elm make $< --output=$@

deploy : app/app.min.js
	(cd .. ; git subtree push --squash --prefix example/app origin gh-pages)

clean :
	-rm app/main.js app/main.min.js
