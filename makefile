default: static/main.js
	stack build

static/main.js:
	elm make --warn --output static/main.js src/Main.elm

serve: default
	stack exec hydra
