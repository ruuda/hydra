default: static/main.js static/sjcl.js static/index.html
	stack build

static/main.js: src/Main.elm src/Native/Sjcl.js
	elm make --warn --output static/main.js src/Main.elm

static/sjcl.js:
	curl -o static/sjcl.js https://bitwiseshiftleft.github.io/sjcl/sjcl.js

serve: default
	stack exec hydra
