rm -rf app
mkdir -p app
cp -a ../../hardware/rpi/24_ant_setup app/
docker build -t tart_web_api .
