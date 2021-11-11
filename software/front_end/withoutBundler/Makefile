build_wasm:
	cd ../rust && wasm-pack build --release --target web
run:
	python3 -m http.server --directory ../ 1234 &
	firefox http://localhost:1234/withoutBundler
