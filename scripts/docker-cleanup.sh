# Clean up docker, removing stopped containers and deleting images that aren't referenced
docker rm $(docker ps -a -q)
docker rmi $(docker images -f "dangling=true" -q)
docker volume prune
