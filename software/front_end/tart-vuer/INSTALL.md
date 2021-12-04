## Install yarn
```
curl --compressed -o- -L https://yarnpkg.com/install.sh | bash
```
export CI_PROJECT_NAME=viewer && make build

rsync --recursive --verbose dist tart@tart.elec.ac.nz:caddy/
