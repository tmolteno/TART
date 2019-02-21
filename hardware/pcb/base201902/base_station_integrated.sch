EESchema Schematic File Version 4
LIBS:base_station_integrated-cache
EELAYER 26 0
EELAYER END
$Descr A4 11693 8268
encoding utf-8
Sheet 1 7
Title ""
Date "13 feb 2015"
Rev ""
Comp ""
Comment1 ""
Comment2 ""
Comment3 ""
Comment4 ""
$EndDescr
Text GLabel 1200 1350 0    60   Input ~ 0
+5V_PI
Text Label 4650 3250 2    60   ~ 0
MISO
Text Label 4650 3350 2    60   ~ 0
MOSI
Text Label 4650 3450 2    60   ~ 0
SCLK
Text GLabel 9150 1250 0    60   Input ~ 0
GND
Text GLabel 10150 1750 2    60   Input ~ 0
GND
Text GLabel 10150 1450 2    60   Input ~ 0
GND
Text GLabel 10550 1050 2    60   Input ~ 0
GND
Text GLabel 10150 950  2    60   Input ~ 0
+5V_PI
Text GLabel 10550 850  2    60   Input ~ 0
+5V_PI
Text Label 8950 1750 0    60   ~ 0
MOSI
Text Label 8950 1850 0    60   ~ 0
MISO
Text Label 8950 1950 0    60   ~ 0
SCLK
NoConn ~ 10050 1150
NoConn ~ 10050 1250
NoConn ~ 10050 1350
NoConn ~ 10050 1550
NoConn ~ 10050 1650
NoConn ~ 10050 1850
NoConn ~ 10050 2050
NoConn ~ 10050 2150
NoConn ~ 10050 2350
NoConn ~ 10050 2550
NoConn ~ 10050 2650
NoConn ~ 10050 2750
NoConn ~ 9250 2650
NoConn ~ 9250 2550
NoConn ~ 9250 2450
NoConn ~ 9250 2350
NoConn ~ 9250 2250
NoConn ~ 9250 2150
NoConn ~ 9250 1650
NoConn ~ 9250 1550
NoConn ~ 9250 1450
NoConn ~ 9250 1350
NoConn ~ 9250 1150
NoConn ~ 9250 1050
NoConn ~ 9250 950 
NoConn ~ 9250 850 
$Comp
L base_station_integrated-rescue:CONN_20X2 P1
U 1 1 5420D065
P 9650 1800
F 0 "P1" H 9650 2850 60  0000 C CNN
F 1 "CONN_20X2" V 9650 1800 50  0000 C CNN
F 2 "RASP_PI_B+_TART" H 9650 700 60  0000 C CNN
F 3 "" H 9650 1800 60  0001 C CNN
F 4 "S9197-ND" H 0   0   50  0001 C CNN "DIGIKEY"
	1    9650 1800
	1    0    0    -1  
$EndComp
Text GLabel 1200 1150 0    60   Input ~ 0
+5V
$Sheet
S 1300 850  1600 950 
U 5407950D
F0 "power_supply" 60
F1 "base_station_power_supply.sch" 60
F2 "GND" I L 1300 1700 60 
F3 "+3V3" I L 1300 1550 60 
F4 "+24V" I L 1300 950 60 
F5 "+5V" I L 1300 1150 60 
F6 "+5V_PI" I L 1300 1350 60 
$EndSheet
Text Label 800  4450 2    60   ~ 0
CLK
Text Label 6250 2650 2    60   ~ 0
CLK_OUT_3
Text Label 6250 2450 2    60   ~ 0
DT_1_3
Text Label 3250 4700 0    60   ~ 0
DT_1_3
Text Label 6250 2350 2    60   ~ 0
DT_2_3
Text Label 3250 4600 0    60   ~ 0
DT_2_3
Text Label 6250 2550 2    60   ~ 0
DT_3_3
Text Label 3250 4500 0    60   ~ 0
DT_3_3
Text Label 6250 2850 2    60   ~ 0
DT_4_3
Text Label 3250 4400 0    60   ~ 0
DT_4_3
Text Label 6250 2950 2    60   ~ 0
DT_5_3
Text Label 3250 4300 0    60   ~ 0
DT_5_3
Text Label 6250 2750 2    60   ~ 0
DT_6_3
Text Label 3250 4200 0    60   ~ 0
DT_6_3
$Sheet
S 1300 4050 1450 800 
U 5409247B
F0 "transceiver_block_3" 60
F1 "transceiver_block.sch" 60
F2 "GND_CABLE" I L 1300 4750 60 
F3 "GND_A" I L 1300 4600 60 
F4 "+24V" I L 1300 4150 60 
F5 "+3V3" I L 1300 4300 60 
F6 "DT_2" I R 2750 4600 60 
F7 "DT_3" I R 2750 4500 60 
F8 "DT_4" I R 2750 4400 60 
F9 "CLK" I L 1300 4450 60 
F10 "DT_1" I R 2750 4700 60 
F11 "DT_5" I R 2750 4300 60 
F12 "DT_6" I R 2750 4200 60 
$EndSheet
$Sheet
S 1300 6450 1450 800 
U 540924B7
F0 "transceiver_block_1" 60
F1 "transceiver_block.sch" 60
F2 "GND_CABLE" I L 1300 7150 60 
F3 "GND_A" I L 1300 7000 60 
F4 "+24V" I L 1300 6550 60 
F5 "+3V3" I L 1300 6700 60 
F6 "DT_2" I R 2750 7000 60 
F7 "DT_3" I R 2750 6900 60 
F8 "DT_4" I R 2750 6800 60 
F9 "CLK" I L 1300 6850 60 
F10 "DT_1" I R 2750 7100 60 
F11 "DT_5" I R 2750 6700 60 
F12 "DT_6" I R 2750 6600 60 
$EndSheet
$Sheet
S 1300 5250 1450 800 
U 54092498
F0 "transceiver_block_2" 60
F1 "transceiver_block.sch" 60
F2 "GND_CABLE" I L 1300 5950 60 
F3 "GND_A" I L 1300 5800 60 
F4 "+24V" I L 1300 5350 60 
F5 "+3V3" I L 1300 5500 60 
F6 "DT_2" I R 2750 5800 60 
F7 "DT_3" I R 2750 5700 60 
F8 "DT_4" I R 2750 5600 60 
F9 "CLK" I L 1300 5650 60 
F10 "DT_1" I R 2750 5900 60 
F11 "DT_5" I R 2750 5500 60 
F12 "DT_6" I R 2750 5400 60 
$EndSheet
$Sheet
S 1300 2850 1450 800 
U 540670D5
F0 "transceiver_block_4" 60
F1 "transceiver_block.sch" 60
F2 "GND_CABLE" I L 1300 3550 60 
F3 "GND_A" I L 1300 3400 60 
F4 "+24V" I L 1300 2950 60 
F5 "+3V3" I L 1300 3100 60 
F6 "DT_2" I R 2750 3400 60 
F7 "DT_3" I R 2750 3300 60 
F8 "DT_4" I R 2750 3200 60 
F9 "CLK" I L 1300 3250 60 
F10 "DT_1" I R 2750 3500 60 
F11 "DT_5" I R 2750 3100 60 
F12 "DT_6" I R 2750 3000 60 
$EndSheet
Text Label 3250 6600 0    60   ~ 0
DT_6_1
Text Label 3250 6700 0    60   ~ 0
DT_5_1
Text Label 3250 6800 0    60   ~ 0
DT_4_1
Text Label 3250 6900 0    60   ~ 0
DT_3_1
Text Label 3250 7000 0    60   ~ 0
DT_2_1
Text Label 3250 7100 0    60   ~ 0
DT_1_1
Text GLabel 1200 6550 0    60   Input ~ 0
+24V
Text GLabel 1200 7000 0    60   Input ~ 0
GND
Text GLabel 1200 7150 0    60   Input ~ 0
GND_L
Text Label 800  6850 2    60   ~ 0
CLK
Text GLabel 1200 6700 0    60   Input ~ 0
+3V3
Text GLabel 1200 5500 0    60   Input ~ 0
+3V3
Text Label 800  5650 2    60   ~ 0
CLK
Text GLabel 1200 5800 0    60   Input ~ 0
GND
Text GLabel 1200 5350 0    60   Input ~ 0
+24V
Text Label 3250 5900 0    60   ~ 0
DT_1_2
Text Label 3250 5800 0    60   ~ 0
DT_2_2
Text Label 3250 5700 0    60   ~ 0
DT_3_2
Text Label 3250 5600 0    60   ~ 0
DT_4_2
Text Label 3250 5500 0    60   ~ 0
DT_5_2
Text Label 3250 5400 0    60   ~ 0
DT_6_2
Text GLabel 1200 4150 0    60   Input ~ 0
+24V
Text GLabel 1200 4600 0    60   Input ~ 0
GND
Text GLabel 1200 4750 0    60   Input ~ 0
GND_T
Text GLabel 1200 4300 0    60   Input ~ 0
+3V3
Text GLabel 1200 3100 0    60   Input ~ 0
+3V3
Text Label 800  3250 2    60   ~ 0
CLK
Text GLabel 1100 2250 0    60   Input ~ 0
+3V3
Text GLabel 1100 2400 0    60   Input ~ 0
GND
Text Label 2850 2400 0    60   ~ 0
CLK
Text Label 2850 2250 0    60   ~ 0
CLK_FPGA
$Sheet
S 1300 2150 1300 350 
U 5407DDC4
F0 "clock" 60
F1 "base_station_clock.sch" 60
F2 "+3V3" I L 1300 2250 60 
F3 "GND" I L 1300 2400 60 
F4 "CLK_FPGA" I R 2600 2250 60 
F5 "CLK_OUT" I R 2600 2400 60 
$EndSheet
Text GLabel 1200 3550 0    60   Input ~ 0
GND_T
Text GLabel 1200 3400 0    60   Input ~ 0
GND
Text GLabel 1200 2950 0    60   Input ~ 0
+24V
Text GLabel 1200 950  0    60   Input ~ 0
+24V
Text GLabel 1200 1550 0    60   Input ~ 0
+3V3
Text GLabel 1200 1700 0    60   Input ~ 0
GND_PWR
Text GLabel 4200 5400 0    60   Input ~ 0
GND
Text GLabel 4200 4300 0    60   Input ~ 0
+5V
NoConn ~ 7850 4400
NoConn ~ 7850 4500
NoConn ~ 7850 5200
NoConn ~ 7850 5300
NoConn ~ 4650 5300
NoConn ~ 4650 5200
NoConn ~ 4650 4500
NoConn ~ 4650 4400
Text Label 6250 3650 2    60   ~ 0
CLK_FPGA
NoConn ~ 8050 3150
NoConn ~ 8050 3050
Text Label 7850 3250 2    60   ~ 0
DT_5_1
Text Label 7850 3350 2    60   ~ 0
DT_4_1
Text Label 7850 3450 2    60   ~ 0
DT_6_1
Text Label 7850 3550 2    60   ~ 0
DT_3_1
Text Label 7850 3650 2    60   ~ 0
DT_1_1
Text Label 7850 3750 2    60   ~ 0
DT_2_1
Text Label 7850 3850 2    60   ~ 0
CLK_OUT_1
Text Label 7850 2950 2    60   ~ 0
DT_5_4
Text Label 7850 2850 2    60   ~ 0
DT_4_4
Text Label 7850 2750 2    60   ~ 0
DT_6_4
Text Label 7850 2650 2    60   ~ 0
CLK_OUT_4
Text Label 7850 2550 2    60   ~ 0
DT_3_4
Text Label 7850 2450 2    60   ~ 0
DT_1_4
Text Label 7850 2350 2    60   ~ 0
DT_2_4
Text Label 6250 3850 2    60   ~ 0
DT_2_2
Text Label 6250 3750 2    60   ~ 0
DT_1_2
Text Label 6250 3550 2    60   ~ 0
DT_3_2
Text Label 6250 3450 2    60   ~ 0
DT_6_2
Text Label 6250 3350 2    60   ~ 0
DT_4_2
Text Label 6250 3250 2    60   ~ 0
DT_5_2
Text Label 6250 3150 2    60   ~ 0
START
Text Label 6250 3050 2    60   ~ 0
CLK_OUT_2
Text Label 3250 3500 0    60   ~ 0
DT_1_4
Text Label 3250 3400 0    60   ~ 0
DT_2_4
Text Label 3250 3300 0    60   ~ 0
DT_3_4
Text Label 3250 3200 0    60   ~ 0
DT_4_4
Text Label 3250 3100 0    60   ~ 0
DT_5_4
Text Label 3250 3000 0    60   ~ 0
DT_6_4
Text Label 4650 3150 2    60   ~ 0
CS0
Text Label 10300 1950 2    60   ~ 0
CS0
Text GLabel 1200 5950 0    60   Input ~ 0
GND_L
Text GLabel 6000 1150 0    60   Input ~ 0
GND_PWR
Text GLabel 6900 1150 2    60   Input ~ 0
GND_L
Text GLabel 6000 900  0    60   Input ~ 0
GND_PWR
Text GLabel 6900 900  2    60   Input ~ 0
GND_T
Text GLabel 6000 1400 0    60   Input ~ 0
GND
Text GLabel 6800 1400 2    60   Input ~ 0
GND_PWR
Text GLabel 6000 1650 0    60   Input ~ 0
GND
Text GLabel 6800 1650 2    60   Input ~ 0
GND_PWR
Text GLabel 9150 2050 0    60   Input ~ 0
GND
Text GLabel 10150 2250 2    60   Input ~ 0
GND
Text GLabel 10150 2450 2    60   Input ~ 0
GND
Text GLabel 9150 2750 0    60   Input ~ 0
GND
NoConn ~ 6250 5300
NoConn ~ 6250 5200
NoConn ~ 6250 4500
NoConn ~ 6250 4400
NoConn ~ 4850 2350
NoConn ~ 4850 2450
NoConn ~ 4850 2550
NoConn ~ 4850 2650
NoConn ~ 4850 2750
NoConn ~ 4850 2850
NoConn ~ 4850 2950
NoConn ~ 4850 3050
NoConn ~ 4850 3550
NoConn ~ 4850 3650
NoConn ~ 4850 3750
NoConn ~ 4850 3850
NoConn ~ 8050 2650
NoConn ~ 8050 3850
NoConn ~ 6450 2650
NoConn ~ 6450 3050
NoConn ~ 6450 3150
Wire Wire Line
	1200 1350 1300 1350
Wire Wire Line
	4850 3250 4650 3250
Wire Wire Line
	4850 3450 4650 3450
Wire Wire Line
	10050 1750 10150 1750
Wire Wire Line
	10050 1050 10550 1050
Wire Wire Line
	10050 850  10550 850 
Wire Wire Line
	9250 1850 8950 1850
Wire Wire Line
	1200 1150 1300 1150
Wire Wire Line
	1300 4450 800  4450
Wire Wire Line
	2750 4600 3250 4600
Wire Wire Line
	2750 4400 3250 4400
Wire Wire Line
	2750 4200 3250 4200
Wire Wire Line
	1200 4600 1300 4600
Wire Wire Line
	1200 4150 1300 4150
Wire Wire Line
	6450 2850 6250 2850
Wire Wire Line
	6450 2450 6250 2450
Wire Wire Line
	1300 2250 1100 2250
Wire Wire Line
	2850 2400 2600 2400
Wire Wire Line
	1200 3550 1300 3550
Wire Wire Line
	1200 3400 1300 3400
Wire Wire Line
	1200 1700 1300 1700
Wire Wire Line
	1200 950  1300 950 
Wire Wire Line
	8050 3750 7850 3750
Wire Wire Line
	8050 3550 7850 3550
Wire Wire Line
	8050 3350 7850 3350
Wire Wire Line
	8050 2950 7850 2950
Wire Wire Line
	8050 2750 7850 2750
Wire Wire Line
	8050 2550 7850 2550
Wire Wire Line
	8050 2350 7850 2350
Wire Wire Line
	6450 3750 6250 3750
Wire Wire Line
	6450 3550 6250 3550
Wire Wire Line
	6450 3350 6250 3350
Wire Wire Line
	2750 3400 3250 3400
Wire Wire Line
	2750 3200 3250 3200
Wire Wire Line
	2750 3000 3250 3000
Wire Wire Line
	2750 3100 3250 3100
Wire Wire Line
	2750 3300 3250 3300
Wire Wire Line
	2750 3500 3250 3500
Wire Wire Line
	6450 3250 6250 3250
Wire Wire Line
	6450 3450 6250 3450
Wire Wire Line
	6450 3650 6250 3650
Wire Wire Line
	6450 3850 6250 3850
Wire Wire Line
	8050 2450 7850 2450
Wire Wire Line
	8050 2850 7850 2850
Wire Wire Line
	8050 3250 7850 3250
Wire Wire Line
	8050 3450 7850 3450
Wire Wire Line
	8050 3650 7850 3650
Wire Wire Line
	1200 1550 1300 1550
Wire Wire Line
	1200 2950 1300 2950
Wire Wire Line
	800  3250 1300 3250
Wire Wire Line
	2600 2250 2850 2250
Wire Wire Line
	1300 2400 1100 2400
Wire Wire Line
	1200 3100 1300 3100
Wire Wire Line
	1200 5950 1300 5950
Wire Wire Line
	1200 5800 1300 5800
Wire Wire Line
	2750 5800 3250 5800
Wire Wire Line
	2750 5600 3250 5600
Wire Wire Line
	2750 5400 3250 5400
Wire Wire Line
	2750 5500 3250 5500
Wire Wire Line
	2750 5700 3250 5700
Wire Wire Line
	2750 5900 3250 5900
Wire Wire Line
	1200 5350 1300 5350
Wire Wire Line
	800  5650 1300 5650
Wire Wire Line
	1200 5500 1300 5500
Wire Wire Line
	1200 6700 1300 6700
Wire Wire Line
	800  6850 1300 6850
Wire Wire Line
	1200 6550 1300 6550
Wire Wire Line
	2750 7100 3250 7100
Wire Wire Line
	2750 6900 3250 6900
Wire Wire Line
	2750 6700 3250 6700
Wire Wire Line
	2750 6600 3250 6600
Wire Wire Line
	2750 6800 3250 6800
Wire Wire Line
	2750 7000 3250 7000
Wire Wire Line
	1200 7000 1300 7000
Wire Wire Line
	6450 2350 6250 2350
Wire Wire Line
	6450 2550 6250 2550
Wire Wire Line
	6450 2750 6250 2750
Wire Wire Line
	6450 2950 6250 2950
Wire Wire Line
	1200 4300 1300 4300
Wire Wire Line
	1200 4750 1300 4750
Wire Wire Line
	2750 4300 3250 4300
Wire Wire Line
	2750 4500 3250 4500
Wire Wire Line
	2750 4700 3250 4700
Wire Wire Line
	9250 1750 8950 1750
Wire Wire Line
	9250 1950 8950 1950
Wire Wire Line
	10050 950  10150 950 
Wire Wire Line
	10050 1450 10150 1450
Wire Wire Line
	9250 1250 9150 1250
Wire Wire Line
	4850 3350 4650 3350
Wire Wire Line
	4850 3150 4650 3150
Wire Wire Line
	10050 1950 10300 1950
Wire Wire Line
	1200 7150 1300 7150
Wire Wire Line
	6900 1150 6000 1150
Wire Wire Line
	6900 900  6000 900 
Wire Wire Line
	6000 1400 6800 1400
Wire Wire Line
	6000 1650 6800 1650
Wire Wire Line
	9150 2050 9250 2050
Wire Wire Line
	10050 2250 10150 2250
Wire Wire Line
	10050 2450 10150 2450
Wire Wire Line
	9150 2750 9250 2750
Wire Wire Line
	6250 4600 6050 4600
Wire Wire Line
	4200 4300 4450 4300
Wire Wire Line
	4200 5400 4350 5400
Wire Wire Line
	6050 5400 6250 5400
Wire Wire Line
	4450 5100 4650 5100
Wire Wire Line
	7650 5400 7850 5400
Wire Wire Line
	7850 4600 7650 4600
$Comp
L TART:PAPILIO_TART U1
U 1 1 54238645
P 6050 3200
F 0 "U1" H 6050 3200 10  0001 C CNN
F 1 "PAPILIO_TART" H 6850 250 60  0001 C CNN
F 2 "PAPILIO_TART" H 8950 1300 60  0000 C CNN
F 3 "" H 6050 3200 60  0001 C CNN
F 4 "PAPILIO" H 6850 350 60  0000 C CNN "NAME"
	1    6050 3200
	1    0    0    -1  
$EndComp
Wire Wire Line
	7650 4050 7650 4600
Wire Wire Line
	4450 4050 6050 4050
Wire Wire Line
	6050 4050 6050 4600
Connection ~ 7650 4600
Connection ~ 6050 4600
Wire Wire Line
	4450 4050 4450 4300
Connection ~ 6050 4050
Connection ~ 4450 4300
Wire Wire Line
	4650 4600 4350 4600
Wire Wire Line
	4350 4600 4350 5400
Connection ~ 4350 5400
Wire Wire Line
	4350 6100 5900 6100
Wire Wire Line
	5900 6100 5900 5100
Wire Wire Line
	5900 4300 6250 4300
Wire Wire Line
	6250 5100 5900 5100
Connection ~ 5900 5100
Wire Wire Line
	7500 6100 7500 5100
Wire Wire Line
	7500 5100 7850 5100
Connection ~ 5900 6100
Wire Wire Line
	7500 4300 7850 4300
Connection ~ 7500 5100
Wire Wire Line
	7650 4600 7650 5400
Wire Wire Line
	6050 4600 6050 5400
Wire Wire Line
	6050 4050 7650 4050
Wire Wire Line
	4450 4300 4650 4300
Wire Wire Line
	4450 4300 4450 5100
Wire Wire Line
	4350 5400 4650 5400
Wire Wire Line
	4350 5400 4350 6100
Wire Wire Line
	5900 5100 5900 4300
Wire Wire Line
	5900 6100 7500 6100
Wire Wire Line
	7500 5100 7500 4300
$EndSCHEMATC
