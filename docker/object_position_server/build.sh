rm -rf app
mkdir -p app
cp ~/github/projects/TART2/tools/object_position_server/python/*.py ./app/
# cp Dockerfile ./app/
docker build -t objectpositionserver .
