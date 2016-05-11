implicit:
	/home/tim/.cabal/bin/extopenscad -r 2 quad_helix.icad
	meshlab quad_helix.stl

optimize:
	python tart_antenna.py

simulate:
	nec2++ -i inverted_v.nec -o inverted_v.out
	xnecview inverted_v.out

