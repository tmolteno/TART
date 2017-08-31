# rpi version of pip and flask are to old.
```
sudo easy_install --upgrade pip
sudo pip install Flask
sudo pip install Flask-JWT
sudo pip install Flask-jwt-extended
sudo pip install -U flask-cors
sudo pip install flask-script
```




#To get a token make a request to the auth resource:
```
POST /auth HTTP/1.1
Host: localhost:5000
Content-Type: application/json

{
    "username": "admin",
    "password": "password"
}
```
#The response should look similar to:
```
HTTP/1.1 200 OK
Content-Type: application/json

{
    "access_token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpZGVudGl0eSI6MSwiaWF0IjoxNDQ0OTE3NjQwLCJuYmYiOjE0NDQ5MTc2NDAsImV4cCI6MTQ0NDkxNzk0MH0.KPmI6WSjRjlpzecPvs3q_T3cJQvAgJvaQAPtk1abC_E"
}
```
#This token can then be used to make requests against protected endpoints:
```
GET /protected HTTP/1.1
Authorization: JWT eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpZGVudGl0eSI6MSwiaWF0IjoxNDQ0OTE3NjQwLCJuYmYiOjE0NDQ5MTc2NDAsImV4cCI6MTQ0NDkxNzk0MH0.KPmI6WSjRjlpzecPvs3q_T3cJQvAgJvaQAPtk1abC_E
```





#Install latest nodejs version on RPI
```
curl -sL https://deb.nodesource.com/setup_8.x | sudo -E bash -
```
```
sudo apt install nodejs
```



