rm -rf app
mkdir -p app
cp test.py app/
touch app/tart_web_api_database.db
docker build -t tart_web_api .
