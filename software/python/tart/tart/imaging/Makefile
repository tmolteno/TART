# Requires the object_position server to be running

all:	vis transient fits

vis:
	python calculate_visibilities.py --data-dir test_data/

transient:
	python detect_transients.py --show --vis test_data/measured_visibility.vis
	
fits:
	rm -f uvfits/test.uvfits
	cd uvfits; python data2uvfits.py --visibilities ../test_data/measured_visibility.vis --output test.uvfits

image:
	cd standard_pipeline; make difmap

simulate:
	time python simulate_transit.py --data-dir ./test_data  --orientation-angle -22.0 --sample-duration 1e-4
	
optimize:
	time python simulate_transit.py --data-dir ./test_data  --spacing 0.505 --orientation-angle -22.0 --optimize 1

data:
	python simulate_transit.py --spacing 0.505 --orientation-angle -22.0 --data-dir /freenas/telescope/data/2013/4/8/
	
remote:
	python forward_map.py --rpc-server=astro.elec.ac.nz --data-file ./test_data/12:55:03.532245_data.pkl

# Run all the testnarnesses
test:
	python ../test/testbench.py

# Remote server 54.225.82.53
