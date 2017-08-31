.SUFFIXES: .cpp .o .dat .kml .bin

OBJS = ../util.o ../second_of_week.o ../gps_time.o ../julian_date.o ../ephemerides.o ../vector3.o ../ephemeris.o satpos.o
CXXFLAGS = -O2 -Wall -I ..


satpos:	${OBJS} ${HDRS}
	g++ -o satpos ${OBJS}

# Test this. You will need to go to the brdc directory and execute the script called 'uncompress.sh'
# to uncompress the broadcase ephemerides files.
test:
	@./satpos 1 08_10_05_279_04_57_04.000 ../../brdc/cddis.gsfc.nasa.gov/gps/data/daily/2008/brdc/
	@./satpos 1 08_10_05_279_04_57_04.002 ../../brdc/cddis.gsfc.nasa.gov/gps/data/daily/2008/brdc/
	@./satpos 2 08_10_05_279_04_57_04.000 ../../brdc/cddis.gsfc.nasa.gov/gps/data/daily/2008/brdc/
	@./satpos 3 08_10_05_279_04_57_04.000 ../../brdc/cddis.gsfc.nasa.gov/gps/data/daily/2008/brdc/
