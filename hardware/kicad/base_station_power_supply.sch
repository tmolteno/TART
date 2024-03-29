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
Sheet 2 7
Title ""
Date "8 dec 2014"
Rev ""
Comp ""
Comment1 ""
Comment2 ""
Comment3 ""
Comment4 ""
$EndDescr
$Comp
L FUSE PF2
U 1 1 5421F748
P 1850 5500
F 0 "PF2" H 1950 5550 40  0000 C CNN
F 1 "FUSE" H 1750 5450 40  0000 C CNN
F 2 "" H 1850 5500 60  0001 C CNN
F 3 "" H 1850 5500 60  0001 C CNN
	1    1850 5500
	1    0    0    -1  
$EndComp
Text HLabel 4150 6300 3    60   Input ~ 0
GND
Text HLabel 4150 5350 1    60   Input ~ 0
+5V_PI
$Comp
L CP1 PC6
U 1 1 5421F74A
P 3200 5800
F 0 "PC6" H 3250 5900 50  0000 L CNN
F 1 "10u" H 3250 5700 50  0000 L CNN
F 2 "E5-10,5_TART" H 3200 5800 60  0001 C CNN
F 3 "" H 3200 5800 60  0001 C CNN
	1    3200 5800
	1    0    0    -1  
$EndComp
$Comp
L C PC7
U 1 1 5421F749
P 3650 5800
F 0 "PC7" H 3700 5900 50  0000 L CNN
F 1 "0.1u" H 3700 5700 50  0000 L CNN
F 2 "" H 3650 5800 60  0001 C CNN
F 3 "" H 3650 5800 60  0001 C CNN
	1    3650 5800
	1    0    0    -1  
$EndComp
Text HLabel 1350 4850 1    60   Input ~ 0
+24V
$Comp
L PWR_FLAG #FLG01
U 1 1 5421F747
P 2100 5250
F 0 "#FLG01" H 2100 5345 30  0001 C CNN
F 1 "PWR_FLAG" H 2100 5430 30  0000 C CNN
F 2 "" H 2100 5250 60  0001 C CNN
F 3 "" H 2100 5250 60  0001 C CNN
	1    2100 5250
	1    0    0    -1  
$EndComp
$Comp
L OK1-78SR_TART REG5V2
U 1 1 5421F746
P 2700 5550
F 0 "REG5V2" H 2450 5300 60  0000 C CNN
F 1 "OK1-78SR_TART" H 2700 5860 60  0001 C CNN
F 2 "" H 2700 5550 60  0001 C CNN
F 3 "" H 2700 5550 60  0001 C CNN
F 4 "OKI-78SR" H 2700 5754 60  0000 C CNN "NAME"
	1    2700 5550
	1    0    0    -1  
$EndComp
$Comp
L SPX3940_TART REG3V3
U 1 1 5419032C
P 9100 2400
F 0 "REG3V3" H 9400 2150 60  0000 C CNN
F 1 "SPX3940_TART" H 9100 2710 60  0001 C CNN
F 2 "" H 9100 2400 60  0001 C CNN
F 3 "" H 9100 2400 60  0001 C CNN
F 4 "SPX3940" H 9100 2604 60  0000 C CNN "NAME"
	1    9100 2400
	1    0    0    -1  
$EndComp
$Comp
L OK1-78SR_TART REG5V1
U 1 1 54190317
P 7600 2400
F 0 "REG5V1" H 7750 2184 60  0000 C CNN
F 1 "OK1-78SR_TART" H 7600 2710 60  0001 C CNN
F 2 "" H 7600 2400 60  0001 C CNN
F 3 "" H 7600 2400 60  0001 C CNN
F 4 "OKI-78SR" H 7600 2604 60  0000 C CNN "NAME"
	1    7600 2400
	1    0    0    -1  
$EndComp
$Comp
L TVS_DIODE_BI_TART D1
U 1 1 5418D706
P 2000 2450
F 0 "D1" H 2000 2575 40  0000 C CNN
F 1 "TVS_DIODE_BI_TART" H 1980 2645 40  0001 C CNN
F 2 "" H 2000 2450 60  0001 C CNN
F 3 "" H 2000 2450 60  0001 C CNN
F 4 "TVS_DIODE_BI" H 2010 2315 40  0000 C CNN "NAME"
	1    2000 2450
	0    -1   -1   0   
$EndComp
$Comp
L TVS_DIODE_UNI_TART PD2
U 1 1 5418D6C0
P 6250 2900
F 0 "PD2" H 6250 3000 40  0000 C CNN
F 1 "TVS_DIODE_UNI_TART" H 6240 3060 40  0001 C CNN
F 2 "" H 6250 2900 60  0001 C CNN
F 3 "" H 6250 2900 60  0001 C CNN
F 4 "TVS_DIODE_UNI" H 6250 2790 40  0000 C CNN "NAME"
	1    6250 2900
	0    -1   -1   0   
$EndComp
$Comp
L BRIDGE_TART PIC1
U 1 1 5418D689
P 3450 2450
F 0 "PIC1" H 3450 2500 70  0000 C CNN
F 1 "BRIDGE_TART" H 4000 3400 60  0001 C CNN
F 2 "" H 3450 2450 60  0001 C CNN
F 3 "" H 3450 2450 60  0001 C CNN
F 4 "MICRO COMMERCIAL" H 4150 3300 60  0001 C CNN "MANUFAC"
F 5 "MB16S-TP" H 3950 3200 60  0001 C CNN "MAN_PART"
F 6 "MB16S-TPMSCT-ND" H 4150 3100 60  0001 C CNN "DIGIKEY"
F 7 "BRIDGE" H 3450 2400 70  0000 C CNN "NAME"
	1    3450 2450
	1    0    0    -1  
$EndComp
Text HLabel 8100 1100 1    60   Input ~ 0
+5V
$Comp
L PWR_FLAG #FLG02
U 1 1 540E3627
P 10450 3600
F 0 "#FLG02" H 10450 3695 30  0001 C CNN
F 1 "PWR_FLAG" H 10450 3780 30  0000 C CNN
F 2 "" H 10450 3600 60  0001 C CNN
F 3 "" H 10450 3600 60  0001 C CNN
	1    10450 3600
	-1   0    0    1   
$EndComp
$Comp
L PWR_FLAG #FLG03
U 1 1 540E3477
P 7000 2100
F 0 "#FLG03" H 7000 2195 30  0001 C CNN
F 1 "PWR_FLAG" H 7000 2280 30  0000 C CNN
F 2 "" H 7000 2100 60  0001 C CNN
F 3 "" H 7000 2100 60  0001 C CNN
	1    7000 2100
	1    0    0    -1  
$EndComp
Text HLabel 6250 3600 3    60   Input ~ 0
GND
Text HLabel 9600 1100 1    60   Input ~ 0
+3V3
Text HLabel 6250 1100 1    60   Input ~ 0
+24V
$Comp
L C PC4
U 1 1 5407CB7A
P 5950 2900
F 0 "PC4" H 6000 3000 50  0000 L CNN
F 1 "0.1u" H 6000 2800 50  0000 L CNN
F 2 "" H 5950 2900 60  0001 C CNN
F 3 "" H 5950 2900 60  0001 C CNN
	1    5950 2900
	1    0    0    -1  
$EndComp
$Comp
L CP1 PC1
U 1 1 5407CB6C
P 5600 2900
F 0 "PC1" H 5650 3000 50  0000 L CNN
F 1 "100u" H 5650 2800 50  0000 L CNN
F 2 "E5-10,5_TART" H 5600 2900 60  0001 C CNN
F 3 "" H 5600 2900 60  0001 C CNN
	1    5600 2900
	1    0    0    -1  
$EndComp
$Comp
L FUSE PF1
U 1 1 5407A432
P 6750 2350
F 0 "PF1" H 6850 2400 40  0000 C CNN
F 1 "FUSE" H 6650 2300 40  0000 C CNN
F 2 "" H 6750 2350 60  0001 C CNN
F 3 "" H 6750 2350 60  0001 C CNN
	1    6750 2350
	1    0    0    -1  
$EndComp
$Comp
L LED PWR1
U 1 1 5407A3D9
P 10450 2900
F 0 "PWR1" H 10450 3000 50  0000 C CNN
F 1 "LED" H 10450 2800 50  0000 C CNN
F 2 "" H 10450 2900 60  0001 C CNN
F 3 "" H 10450 2900 60  0001 C CNN
	1    10450 2900
	0    1    1    0   
$EndComp
$Comp
L R PR1
U 1 1 5407A3AD
P 10100 2350
F 0 "PR1" V 10180 2350 50  0000 C CNN
F 1 "1k" V 10100 2350 50  0000 C CNN
F 2 "" H 10100 2350 60  0001 C CNN
F 3 "" H 10100 2350 60  0001 C CNN
	1    10100 2350
	0    -1   -1   0   
$EndComp
$Comp
L CP1 PC2
U 1 1 5407A35C
P 9600 2900
F 0 "PC2" H 9650 3000 50  0000 L CNN
F 1 "10u" H 9650 2800 50  0000 L CNN
F 2 "E2-5_TART" H 9600 2900 60  0001 C CNN
F 3 "" H 9600 2900 60  0001 C CNN
	1    9600 2900
	1    0    0    -1  
$EndComp
$Comp
L C PC5
U 1 1 5407A2F7
P 8550 2900
F 0 "PC5" H 8600 3000 50  0000 L CNN
F 1 "0.1u" H 8600 2800 50  0000 L CNN
F 2 "" H 8550 2900 60  0001 C CNN
F 3 "" H 8550 2900 60  0001 C CNN
	1    8550 2900
	1    0    0    -1  
$EndComp
$Comp
L CP1 PC3
U 1 1 5407A2C8
P 8100 2900
F 0 "PC3" H 8150 3000 50  0000 L CNN
F 1 "10u" H 8150 2800 50  0000 L CNN
F 2 "E5-10,5_TART" H 8100 2900 60  0001 C CNN
F 3 "" H 8100 2900 60  0001 C CNN
	1    8100 2900
	1    0    0    -1  
$EndComp
$Comp
L CONN_2 PCON1
U 1 1 542A164F
P 1100 2450
F 0 "PCON1" V 1050 2450 40  0000 C CNN
F 1 "~" V 1150 2450 40  0000 C CNN
F 2 "" H 1100 2450 60  0000 C CNN
F 3 "" H 1100 2450 60  0000 C CNN
	1    1100 2450
	-1   0    0    -1  
$EndComp
NoConn ~ 5300 2550
$Comp
L SWITCH SW1
U 1 1 5464220F
P 4800 2450
F 0 "SW1" H 4600 2600 50  0000 C CNN
F 1 "SWITCH" H 4650 2300 50  0000 C CNN
F 2 "CS12ANW03_TART" H 4900 2700 60  0001 C CNN
F 3 "~" H 4800 2450 60  0000 C CNN
	1    4800 2450
	1    0    0    -1  
$EndComp
Connection ~ 5600 3450
Wire Wire Line
	5600 3100 5600 3450
Connection ~ 5950 3450
Wire Wire Line
	5950 3450 5950 3100
Wire Wire Line
	6250 3100 6250 3600
Connection ~ 7600 3450
Wire Wire Line
	7600 3450 7600 2650
Connection ~ 8100 3450
Wire Wire Line
	8100 3450 8100 3100
Connection ~ 8550 3450
Wire Wire Line
	8550 3450 8550 3100
Connection ~ 9600 3450
Connection ~ 10450 3450
Wire Wire Line
	2600 3450 10450 3450
Wire Wire Line
	2700 6150 4150 6150
Wire Wire Line
	3100 5500 4150 5500
Wire Wire Line
	4150 5500 4150 5350
Wire Wire Line
	1350 5500 1350 4850
Wire Wire Line
	10450 3050 10450 3600
Wire Wire Line
	9600 1100 9600 2700
Connection ~ 6250 2350
Connection ~ 5950 2350
Wire Wire Line
	5950 2350 5950 2700
Connection ~ 2000 3300
Wire Wire Line
	1650 3300 3450 3300
Wire Wire Line
	1650 2550 1650 3300
Wire Wire Line
	2000 2700 2000 3300
Wire Wire Line
	1450 2550 1650 2550
Connection ~ 9600 2350
Wire Wire Line
	9500 2350 9850 2350
Connection ~ 8550 2350
Wire Wire Line
	8550 2350 8550 2700
Wire Wire Line
	8000 2350 8700 2350
Connection ~ 8100 2350
Wire Wire Line
	10450 2350 10350 2350
Wire Wire Line
	10450 2700 10450 2350
Wire Wire Line
	7000 2350 7200 2350
Wire Wire Line
	1650 2350 1450 2350
Wire Wire Line
	2000 2200 2000 1600
Wire Wire Line
	1650 1600 1650 2350
Wire Wire Line
	1650 1600 3450 1600
Connection ~ 2000 1600
Wire Wire Line
	2600 3450 2600 2450
Wire Wire Line
	5600 2350 5600 2700
Connection ~ 5600 2350
Wire Wire Line
	5300 2350 6500 2350
Wire Wire Line
	7000 2100 7000 2350
Wire Wire Line
	8100 1100 8100 2700
Wire Wire Line
	2100 5500 2100 5250
Wire Wire Line
	2300 5500 2100 5500
Connection ~ 3650 6150
Wire Wire Line
	3650 6150 3650 6000
Connection ~ 3200 5500
Wire Wire Line
	3650 5500 3650 5600
Connection ~ 3650 5500
Wire Wire Line
	3200 6000 3200 6150
Wire Wire Line
	3200 5600 3200 5500
Wire Wire Line
	1600 5500 1350 5500
Wire Wire Line
	2700 5800 2700 6150
Connection ~ 3200 6150
Wire Wire Line
	4150 6150 4150 6300
Wire Wire Line
	6250 1100 6250 2700
Connection ~ 6250 3450
Wire Wire Line
	9600 3100 9600 3450
Wire Wire Line
	9000 2650 9000 3450
Connection ~ 9000 3450
Wire Wire Line
	9200 2650 9200 3450
Connection ~ 9200 3450
$EndSCHEMATC
