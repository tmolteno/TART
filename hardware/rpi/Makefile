
online_imaging:
	python acquire_continuous.py  --config 6_ant_acq_dir/6_antenna_board_config.json --blocksize 22 --chunk 2 --mode qt

dsp_acquire:
	python tartdsp.py --monitor --verbose --blocksize 23 --speed 32 --acquire

dsp_counter:
	python tartdsp.py --monitor --verbose --blocksize 23 --speed 32 --counter

dsp_shift:
	python tartdsp.py --monitor --verbose --blocksize 23 --speed 32 --shift
