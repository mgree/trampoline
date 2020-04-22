.PHONY : all deploy clean

all : app/app.min.js

app/%.min.js : app/%.js
	uglifyjs $< --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=$@

app/app.js : src/App.elm
	elm make $< --output=$@

deploy : app/app.min.js
	(cd .. ; git subtree push --prefix example/app origin gh-pages)

clean :
	-rm app/app.js app/app.min.js
