# Tart Web-App

## Setup

```npm install``` to install node packages

## Development server
Run `npm run ng serve` for a dev server. Navigate to `http://localhost:4200/`. The app will automatically reload if you change any of the source files.
<br/><br/>
Note: when running in development mode, all API requests will be made to `http://tart2-raspberry/api/v1`.

## Build

Run `npm run ng build` to build the project. The build artifacts will be stored in the `dist/` directory. Use the `--prod` flag and `--env=prod` for a production build.
<br/><br/>
Note: when running a development build, all API requests will be made to `http://tart2-raspberry/api/v1`. In a production build, the API requests will be made to `http://<hostname>/api/v1`

## Deploy build to TARTs
Upload build in `./dist/` to `/var/www/html/` on target hosts.
`make upload`

## Running unit tests

Run `npm run ng test` to execute the unit tests via [Karma](https://karma-runner.github.io).

## Running end-to-end tests

Run `ng e2e` to execute the end-to-end tests via [Protractor](http://www.protractortest.org/).
Before running the tests make sure you are serving the app via `ng serve`.
