fast:
	python acquire_continuous.py  --config 6_ant_setup/6_antenna_board_config.json --blocksize 20 --chunk 1 --mode qt --calib 6_ant_setup/

6:
	python acquire_continuous.py  --config 6_ant_setup/6_antenna_board_config.json --blocksize 22 --chunk 2 --mode qt --calib 6_ant_setup/

online_imaging:
	python acquire_continuous.py  --config 6_ant_setup/6_antenna_board_config.json --blocksize 22 --chunk 2 --mode qt --calib 6_ant_setup/

dsp_acquire:
	python low_level_dsp.py --monitor --verbose --blocksize 23 --speed 32 --acquire

dsp_counter:
	python low_level_dsp.py --monitor --verbose --blocksize 23 --speed 32 --counter

dsp_shift:
	python low_level_dsp.py --monitor --verbose --blocksize 23 --speed 32 --shift

acquire:
	python acquire_continuous.py  --config 6_ant_setup/6_antenna_board_config.json --blocksize 23 --chunk 180 --mode save_single --calib 6_ant_setup/
snapshot:
	python acquire_snapshot.py --speed 32 --bramexp 22 --data-dir test --config 24_ant_setup/telescope_config.json --save 
