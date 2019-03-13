(cd containers/telescope_web_api && sh pre_build.sh);
docker-compose -f testbench-compose.yml up --build
