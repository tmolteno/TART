rm -rf app
mkdir -p app
cp test.py app/
cp -a ../../../hardware/rpi/24_ant_setup app/
docker build -t tart_web_api .
