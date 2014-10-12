EESchema Schematic File Version 2
LIBS:power
LIBS:device
LIBS:transistors
LIBS:conn
LIBS:linear
LIBS:regul
LIBS:74xx
LIBS:cmos4000
LIBS:adc-dac
LIBS:memory
LIBS:xilinx
LIBS:special
LIBS:microcontrollers
LIBS:dsp
LIBS:microchip
LIBS:analog_switches
LIBS:motorola
LIBS:texas
LIBS:intel
LIBS:audio
LIBS:interface
LIBS:digital-audio
LIBS:philips
LIBS:display
LIBS:cypress
LIBS:siliconi
LIBS:opto
LIBS:atmel
LIBS:contrib
LIBS:valves
LIBS:TART
LIBS:spelsberg_box_template_TR
LIBS:base_station_integrated-cache
EELAYER 27 0
EELAYER END
$Descr A4 11693 8268
encoding utf-8
Sheet 1 7
Title ""
Date "12 oct 2014"
Rev ""
Comp ""
Comment1 ""
Comment2 ""
Comment3 ""
Comment4 ""
$EndDescr
$Comp
L PAPILIO_TART U1
U 1 1 54238645
P 7850 3800
F 0 "U1" H 7850 3800 10  0001 C CNN
F 1 "PAPILIO_TART" H 8650 850 60  0001 C CNN
F 2 "" H 7850 3800 60  0001 C CNN
F 3 "" H 7850 3800 60  0001 C CNN
F 4 "PAPILIO" H 8650 950 60  0000 C CNN "NAME"
	1    7850 3800
	1    0    0    -1  
$EndComp
Text GLabel 2900 1350 0    60   Input ~ 0
+5V_PI
Text Label 7350 2700 0    60   ~ 0
MOSI
Text Label 7350 2600 0    60   ~ 0
MISO
Text Label 7350 2500 0    60   ~ 0
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
NoConn ~ 10050 850 
NoConn ~ 10050 1050
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
L CONN_20X2 P1
U 1 1 5420D065
P 9650 1800
F 0 "P1" H 9650 2850 60  0000 C CNN
F 1 "CONN_20X2" V 9650 1800 50  0000 C CNN
F 2 "" H 9650 1800 60  0001 C CNN
F 3 "" H 9650 1800 60  0001 C CNN
	1    9650 1800
	1    0    0    -1  
$EndComp
Text GLabel 2900 1150 0    60   Input ~ 0
+5V
$Sheet
S 3000 850  1600 950 
U 5407950D
F0 "power_supply" 60
F1 "base_station_power_supply.sch" 60
F2 "GND" I L 3000 1700 60 
F3 "+3V3" I L 3000 1550 60 
F4 "+24V" I L 3000 950 60 
F5 "+5V" I L 3000 1150 60 
F6 "+5V_PI" I L 3000 1350 60 
$EndSheet
Text Label 2500 4450 2    60   ~ 0
CLK
Text Label 7350 3400 2    60   ~ 0
CLK_OUT_3
Text Label 7350 3200 2    60   ~ 0
DT_1_3
Text Label 4950 4700 0    60   ~ 0
DT_1_3
Text Label 7350 3100 2    60   ~ 0
DT_2_3
Text Label 4950 4600 0    60   ~ 0
DT_2_3
Text Label 7350 3300 2    60   ~ 0
DT_3_3
Text Label 4950 4500 0    60   ~ 0
DT_3_3
Text Label 7350 3600 2    60   ~ 0
DT_4_3
Text Label 4950 4400 0    60   ~ 0
DT_4_3
Text Label 7350 3700 2    60   ~ 0
DT_5_3
Text Label 4950 4300 0    60   ~ 0
DT_5_3
Text Label 7350 3500 2    60   ~ 0
DT_6_3
Text Label 4950 4200 0    60   ~ 0
DT_6_3
$Sheet
S 3000 4050 1450 800 
U 5409247B
F0 "transceiver_block_3" 60
F1 "transceiver_block.sch" 60
F2 "GND_CABLE" I L 3000 4750 60 
F3 "GND_A" I L 3000 4600 60 
F4 "+24V" I L 3000 4150 60 
F5 "+3V3" I L 3000 4300 60 
F6 "DT_2" I R 4450 4600 60 
F7 "DT_3" I R 4450 4500 60 
F8 "DT_4" I R 4450 4400 60 
F9 "CLK" I L 3000 4450 60 
F10 "DT_1" I R 4450 4700 60 
F11 "DT_5" I R 4450 4300 60 
F12 "DT_6" I R 4450 4200 60 
$EndSheet
$Sheet
S 3000 6450 1450 800 
U 540924B7
F0 "transceiver_block_1" 60
F1 "transceiver_block.sch" 60
F2 "GND_CABLE" I L 3000 7150 60 
F3 "GND_A" I L 3000 7000 60 
F4 "+24V" I L 3000 6550 60 
F5 "+3V3" I L 3000 6700 60 
F6 "DT_2" I R 4450 7000 60 
F7 "DT_3" I R 4450 6900 60 
F8 "DT_4" I R 4450 6800 60 
F9 "CLK" I L 3000 6850 60 
F10 "DT_1" I R 4450 7100 60 
F11 "DT_5" I R 4450 6700 60 
F12 "DT_6" I R 4450 6600 60 
$EndSheet
$Sheet
S 3000 5250 1450 800 
U 54092498
F0 "transceiver_block_2" 60
F1 "transceiver_block.sch" 60
F2 "GND_CABLE" I L 3000 5950 60 
F3 "GND_A" I L 3000 5800 60 
F4 "+24V" I L 3000 5350 60 
F5 "+3V3" I L 3000 5500 60 
F6 "DT_2" I R 4450 5800 60 
F7 "DT_3" I R 4450 5700 60 
F8 "DT_4" I R 4450 5600 60 
F9 "CLK" I L 3000 5650 60 
F10 "DT_1" I R 4450 5900 60 
F11 "DT_5" I R 4450 5500 60 
F12 "DT_6" I R 4450 5400 60 
$EndSheet
$Sheet
S 3000 2850 1450 800 
U 540670D5
F0 "transceiver_block_4" 60
F1 "transceiver_block.sch" 60
F2 "GND_CABLE" I L 3000 3550 60 
F3 "GND_A" I L 3000 3400 60 
F4 "+24V" I L 3000 2950 60 
F5 "+3V3" I L 3000 3100 60 
F6 "DT_2" I R 4450 3400 60 
F7 "DT_3" I R 4450 3300 60 
F8 "DT_4" I R 4450 3200 60 
F9 "CLK" I L 3000 3250 60 
F10 "DT_1" I R 4450 3500 60 
F11 "DT_5" I R 4450 3100 60 
F12 "DT_6" I R 4450 3000 60 
$EndSheet
Text Label 4950 6600 0    60   ~ 0
DT_6_1
Text Label 4950 6700 0    60   ~ 0
DT_5_1
Text Label 4950 6800 0    60   ~ 0
DT_4_1
Text Label 4950 6900 0    60   ~ 0
DT_3_1
Text Label 4950 7000 0    60   ~ 0
DT_2_1
Text Label 4950 7100 0    60   ~ 0
DT_1_1
Text GLabel 2900 6550 0    60   Input ~ 0
+24V
Text GLabel 2900 7000 0    60   Input ~ 0
GND
Text GLabel 2900 7150 0    60   Input ~ 0
GND_L
Text Label 2500 6850 2    60   ~ 0
CLK
Text GLabel 2900 6700 0    60   Input ~ 0
+3V3
Text GLabel 2900 5500 0    60   Input ~ 0
+3V3
Text Label 2500 5650 2    60   ~ 0
CLK
Text GLabel 2900 5800 0    60   Input ~ 0
GND
Text GLabel 2900 5350 0    60   Input ~ 0
+24V
Text Label 4950 5900 0    60   ~ 0
DT_1_2
Text Label 4950 5800 0    60   ~ 0
DT_2_2
Text Label 4950 5700 0    60   ~ 0
DT_3_2
Text Label 4950 5600 0    60   ~ 0
DT_4_2
Text Label 4950 5500 0    60   ~ 0
DT_5_2
Text Label 4950 5400 0    60   ~ 0
DT_6_2
Text GLabel 2900 4150 0    60   Input ~ 0
+24V
Text GLabel 2900 4600 0    60   Input ~ 0
GND
Text GLabel 2900 4750 0    60   Input ~ 0
GND_T
Text GLabel 2900 4300 0    60   Input ~ 0
+3V3
Text GLabel 2900 3100 0    60   Input ~ 0
+3V3
Text Label 2500 3250 2    60   ~ 0
CLK
Text GLabel 2800 2250 0    60   Input ~ 0
+3V3
Text GLabel 2800 2400 0    60   Input ~ 0
GND
Text Label 4550 2400 0    60   ~ 0
CLK
Text Label 4550 2250 0    60   ~ 0
CLK_FPGA
$Sheet
S 3000 2150 1300 350 
U 5407DDC4
F0 "clock" 60
F1 "base_station_clock.sch" 60
F2 "+3V3" I L 3000 2250 60 
F3 "GND" I L 3000 2400 60 
F4 "CLK_FPGA" I R 4300 2250 60 
F5 "CLK_OUT" I R 4300 2400 60 
$EndSheet
Text GLabel 2900 3550 0    60   Input ~ 0
GND_T
Text GLabel 2900 3400 0    60   Input ~ 0
GND
Text GLabel 2900 2950 0    60   Input ~ 0
+24V
Text GLabel 2900 950  0    60   Input ~ 0
+24V
Text GLabel 2900 1550 0    60   Input ~ 0
+3V3
Text GLabel 2900 1700 0    60   Input ~ 0
GND_PWR
Text GLabel 8950 5700 0    60   Input ~ 0
GND
Text GLabel 8950 4900 0    60   Input ~ 0
GND
Text GLabel 7150 5700 0    60   Input ~ 0
GND
Text GLabel 7150 5200 0    60   Input ~ 0
+5V
Text GLabel 7150 4900 0    60   Input ~ 0
GND
NoConn ~ 9150 5000
NoConn ~ 9150 5100
NoConn ~ 9150 5200
NoConn ~ 9150 5800
NoConn ~ 9150 5900
NoConn ~ 9150 6000
NoConn ~ 7350 6000
NoConn ~ 7350 5900
NoConn ~ 7350 5800
NoConn ~ 7350 5100
NoConn ~ 7350 5000
Text Label 7350 4400 2    60   ~ 0
CLK_FPGA
NoConn ~ 9350 3900
NoConn ~ 9350 3800
Text Label 9150 4000 2    60   ~ 0
DT_5_1
Text Label 9150 4100 2    60   ~ 0
DT_4_1
Text Label 9150 4200 2    60   ~ 0
DT_6_1
Text Label 9150 4300 2    60   ~ 0
DT_3_1
Text Label 9150 4400 2    60   ~ 0
DT_1_1
Text Label 9150 4500 2    60   ~ 0
DT_2_1
Text Label 9150 4600 2    60   ~ 0
CLK_OUT_1
Text Label 9150 3700 2    60   ~ 0
DT_5_4
Text Label 9150 3600 2    60   ~ 0
DT_4_4
Text Label 9150 3500 2    60   ~ 0
DT_6_4
Text Label 9150 3400 2    60   ~ 0
CLK_OUT_4
Text Label 9150 3300 2    60   ~ 0
DT_3_4
Text Label 9150 3200 2    60   ~ 0
DT_1_4
Text Label 9150 3100 2    60   ~ 0
DT_2_4
Text Label 7350 4600 2    60   ~ 0
DT_2_2
Text Label 7350 4500 2    60   ~ 0
DT_1_2
Text Label 7350 4300 2    60   ~ 0
DT_3_2
Text Label 7350 4200 2    60   ~ 0
DT_6_2
Text Label 7350 4100 2    60   ~ 0
DT_4_2
Text Label 7350 4000 2    60   ~ 0
DT_5_2
Text Label 7350 3900 2    60   ~ 0
START
Text Label 7350 3800 2    60   ~ 0
CLK_OUT_2
Text Label 4950 3500 0    60   ~ 0
DT_1_4
Text Label 4950 3400 0    60   ~ 0
DT_2_4
Text Label 4950 3300 0    60   ~ 0
DT_3_4
Text Label 4950 3200 0    60   ~ 0
DT_4_4
Text Label 4950 3100 0    60   ~ 0
DT_5_4
Text Label 4950 3000 0    60   ~ 0
DT_6_4
Text Label 7350 2800 0    60   ~ 0
CS0
Text Label 10300 1950 2    60   ~ 0
CS0
Text GLabel 2900 5950 0    60   Input ~ 0
GND_L
Text GLabel 6800 1150 0    60   Input ~ 0
GND_PWR
Text GLabel 7700 1150 2    60   Input ~ 0
GND_L
Text GLabel 6800 900  0    60   Input ~ 0
GND_PWR
Text GLabel 7700 900  2    60   Input ~ 0
GND_T
Text GLabel 6800 1400 0    60   Input ~ 0
GND
Text GLabel 7600 1400 2    60   Input ~ 0
GND_PWR
Wire Wire Line
	2900 1350 3000 1350
Wire Wire Line
	7550 2700 7350 2700
Wire Wire Line
	7550 2500 7350 2500
Wire Wire Line
	10050 1750 10150 1750
Wire Wire Line
	10050 1050 10550 1050
Wire Wire Line
	10050 850  10550 850 
Wire Wire Line
	9250 1850 8950 1850
Wire Wire Line
	2900 1150 3000 1150
Wire Wire Line
	3000 4450 2500 4450
Wire Wire Line
	4450 4600 4950 4600
Wire Wire Line
	4450 4400 4950 4400
Wire Wire Line
	4450 4200 4950 4200
Wire Wire Line
	2900 4600 3000 4600
Wire Wire Line
	2900 4150 3000 4150
Wire Wire Line
	7550 3600 7350 3600
Wire Wire Line
	7550 3400 7350 3400
Wire Wire Line
	7550 3200 7350 3200
Wire Wire Line
	3000 2250 2800 2250
Wire Wire Line
	4550 2400 4300 2400
Wire Wire Line
	2900 3550 3000 3550
Wire Wire Line
	2900 3400 3000 3400
Wire Wire Line
	2900 1700 3000 1700
Wire Wire Line
	2900 950  3000 950 
Wire Wire Line
	7350 5700 7150 5700
Wire Wire Line
	7350 4900 7150 4900
Wire Wire Line
	9150 5700 8950 5700
Wire Wire Line
	9150 4900 8950 4900
Wire Wire Line
	9350 4500 9150 4500
Wire Wire Line
	9350 4300 9150 4300
Wire Wire Line
	9350 4100 9150 4100
Wire Wire Line
	9350 3700 9150 3700
Wire Wire Line
	9350 3500 9150 3500
Wire Wire Line
	9350 3300 9150 3300
Wire Wire Line
	9350 3100 9150 3100
Wire Wire Line
	7550 4500 7350 4500
Wire Wire Line
	7550 4300 7350 4300
Wire Wire Line
	7550 4100 7350 4100
Wire Wire Line
	7550 3900 7350 3900
Wire Wire Line
	4450 3400 4950 3400
Wire Wire Line
	4450 3200 4950 3200
Wire Wire Line
	4450 3000 4950 3000
Wire Wire Line
	4450 3100 4950 3100
Wire Wire Line
	4450 3300 4950 3300
Wire Wire Line
	4450 3500 4950 3500
Wire Wire Line
	7550 3800 7350 3800
Wire Wire Line
	7550 4000 7350 4000
Wire Wire Line
	7550 4200 7350 4200
Wire Wire Line
	7550 4400 7350 4400
Wire Wire Line
	7550 4600 7350 4600
Wire Wire Line
	9350 3200 9150 3200
Wire Wire Line
	9350 3400 9150 3400
Wire Wire Line
	9350 3600 9150 3600
Wire Wire Line
	9350 4000 9150 4000
Wire Wire Line
	9350 4200 9150 4200
Wire Wire Line
	9350 4400 9150 4400
Wire Wire Line
	9350 4600 9150 4600
Wire Wire Line
	7350 5200 7150 5200
Wire Wire Line
	2900 1550 3000 1550
Wire Wire Line
	2900 2950 3000 2950
Wire Wire Line
	2500 3250 3000 3250
Wire Wire Line
	4300 2250 4550 2250
Wire Wire Line
	3000 2400 2800 2400
Wire Wire Line
	2900 3100 3000 3100
Wire Wire Line
	2900 5950 3000 5950
Wire Wire Line
	2900 5800 3000 5800
Wire Wire Line
	4450 5800 4950 5800
Wire Wire Line
	4450 5600 4950 5600
Wire Wire Line
	4450 5400 4950 5400
Wire Wire Line
	4450 5500 4950 5500
Wire Wire Line
	4450 5700 4950 5700
Wire Wire Line
	4450 5900 4950 5900
Wire Wire Line
	2900 5350 3000 5350
Wire Wire Line
	2500 5650 3000 5650
Wire Wire Line
	2900 5500 3000 5500
Wire Wire Line
	2900 6700 3000 6700
Wire Wire Line
	2500 6850 3000 6850
Wire Wire Line
	2900 6550 3000 6550
Wire Wire Line
	4450 7100 4950 7100
Wire Wire Line
	4450 6900 4950 6900
Wire Wire Line
	4450 6700 4950 6700
Wire Wire Line
	4450 6600 4950 6600
Wire Wire Line
	4450 6800 4950 6800
Wire Wire Line
	4450 7000 4950 7000
Wire Wire Line
	2900 7000 3000 7000
Wire Wire Line
	7550 3100 7350 3100
Wire Wire Line
	7550 3300 7350 3300
Wire Wire Line
	7550 3500 7350 3500
Wire Wire Line
	7550 3700 7350 3700
Wire Wire Line
	2900 4300 3000 4300
Wire Wire Line
	2900 4750 3000 4750
Wire Wire Line
	4450 4300 4950 4300
Wire Wire Line
	4450 4500 4950 4500
Wire Wire Line
	4450 4700 4950 4700
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
	7550 2600 7350 2600
Wire Wire Line
	7550 2800 7350 2800
Wire Wire Line
	10050 1950 10300 1950
Wire Wire Line
	2900 7150 3000 7150
Wire Wire Line
	7700 1150 6800 1150
Wire Wire Line
	7700 900  6800 900 
Text GLabel 6800 1650 0    60   Input ~ 0
GND
Text GLabel 7600 1650 2    60   Input ~ 0
GND_PWR
Wire Wire Line
	6800 1400 7600 1400
Wire Wire Line
	6800 1650 7600 1650
Text GLabel 9150 2050 0    60   Input ~ 0
GND
Text GLabel 10150 2250 2    60   Input ~ 0
GND
Text GLabel 10150 2450 2    60   Input ~ 0
GND
Text GLabel 9150 2750 0    60   Input ~ 0
GND
Wire Wire Line
	9150 2050 9250 2050
Wire Wire Line
	10050 2250 10150 2250
Wire Wire Line
	10050 2450 10150 2450
Wire Wire Line
	9150 2750 9250 2750
$EndSCHEMATC
