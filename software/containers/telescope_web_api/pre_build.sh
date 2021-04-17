# Copy python modules into the local code directory
mkdir -p python_code/
cp -a ../../python_modules/tart_web_api python_code/
cp -a ../../python_modules/tart python_code/
cp -a ../../python_modules/tart_hardware_interface python_code/
rm -rf `find python_code -name '*.pyc'`
rm -rf `find python_code -name 'dist'`
rm -rf `find python_code -name 'build'`
rm -rf `find python_code -name '*egg-info'`
