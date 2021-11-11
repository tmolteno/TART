
prereq: gridlessLib
	yarn

dev:	prereq
	yarn serve

build:	prereq
	yarn build && python3 -m http.server --directory dist/ 1235

gridlessLib:
	cd ../rust/ && wasm-pack build --release --out-dir ../tart-vuer/pkg
	cp pkg/* node_modules/wasm-tart-imaging


