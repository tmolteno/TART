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
LIBS:clk_module
LIBS:texas_lvds
LIBS:laird
LIBS:radio_hub-cache
LIBS:radio_hub-cache
EELAYER 27 0
EELAYER END
$Descr A4 11693 8268
encoding utf-8
Sheet 2 2
Title "noname.sch"
Date "6 nov 2014"
Rev ""
Comp ""
Comment1 ""
Comment2 ""
Comment3 ""
Comment4 ""
$EndDescr
$Comp
L TPS78227DDCR_TART REG?
U 1 1 545AE4C2
P 5900 3400
F 0 "REG?" H 6050 3184 60  0000 C CNN
F 1 "TPS78227DDCR_TART" H 5900 3710 60  0001 C CNN
F 2 "~" H 5900 3400 60  0000 C CNN
F 3 "~" H 5900 3400 60  0000 C CNN
F 4 "TPS78227DDCR" H 5900 3604 60  0000 C CNN "NAME"
	1    5900 3400
	1    0    0    -1  
$EndComp
$Comp
L C C?
U 1 1 545AE4C3
P 6400 3750
F 0 "C?" H 6400 3850 40  0000 L CNN
F 1 "C" H 6406 3665 40  0000 L CNN
F 2 "~" H 6438 3600 30  0000 C CNN
F 3 "~" H 6400 3750 60  0000 C CNN
	1    6400 3750
	1    0    0    -1  
$EndComp
$Comp
L C C?
U 1 1 545AE4C4
P 6650 3750
F 0 "C?" H 6650 3850 40  0000 L CNN
F 1 "C" H 6656 3665 40  0000 L CNN
F 2 "~" H 6688 3600 30  0000 C CNN
F 3 "~" H 6650 3750 60  0000 C CNN
	1    6650 3750
	1    0    0    -1  
$EndComp
$Comp
L C C?
U 1 1 545AE4C5
P 5400 3750
F 0 "C?" H 5400 3850 40  0000 L CNN
F 1 "C" H 5406 3665 40  0000 L CNN
F 2 "~" H 5438 3600 30  0000 C CNN
F 3 "~" H 5400 3750 60  0000 C CNN
	1    5400 3750
	1    0    0    -1  
$EndComp
$Comp
L TVS_DIODE_UNI_TART D?
U 1 1 545AE4C6
P 5000 3750
F 0 "D?" H 5000 3850 40  0000 C CNN
F 1 "TVS_DIODE_UNI_TART" H 4990 3910 40  0001 C CNN
F 2 "~" H 5000 3750 60  0000 C CNN
F 3 "~" H 5000 3750 60  0000 C CNN
F 4 "TVS_DIODE_UNI" H 5000 3640 40  0000 C CNN "NAME"
	1    5000 3750
	0    -1   -1   0   
$EndComp
$Comp
L R R?
U 1 1 545AE4C7
P 4450 3350
F 0 "R?" V 4530 3350 40  0000 C CNN
F 1 "R" V 4457 3351 40  0000 C CNN
F 2 "~" V 4380 3350 30  0000 C CNN
F 3 "~" H 4450 3350 30  0000 C CNN
	1    4450 3350
	0    -1   -1   0   
$EndComp
$Comp
L R R?
U 1 1 545AE4C8
P 4450 4100
F 0 "R?" V 4530 4100 40  0000 C CNN
F 1 "R" V 4457 4101 40  0000 C CNN
F 2 "~" V 4380 4100 30  0000 C CNN
F 3 "~" H 4450 4100 30  0000 C CNN
	1    4450 4100
	0    -1   -1   0   
$EndComp
$Comp
L CP1 C?
U 1 1 545AE4C9
P 4050 3750
F 0 "C?" H 4100 3850 50  0000 L CNN
F 1 "CP1" H 4100 3650 50  0000 L CNN
F 2 "~" H 4050 3750 60  0000 C CNN
F 3 "~" H 4050 3750 60  0000 C CNN
	1    4050 3750
	1    0    0    -1  
$EndComp
Text HLabel 6650 3250 1    60   Input ~ 0
2V8_A
Text HLabel 4050 3250 1    60   Input ~ 0
+3V3
$Comp
L TPS78227DDCR_TART REG?
U 1 1 545AE4CA
P 5950 4900
F 0 "REG?" H 6100 4684 60  0000 C CNN
F 1 "TPS78227DDCR_TART" H 5950 5210 60  0001 C CNN
F 2 "~" H 5950 4900 60  0000 C CNN
F 3 "~" H 5950 4900 60  0000 C CNN
F 4 "TPS78227DDCR" H 5950 5104 60  0000 C CNN "NAME"
	1    5950 4900
	1    0    0    -1  
$EndComp
$Comp
L C C?
U 1 1 545AE4CB
P 6450 5250
F 0 "C?" H 6450 5350 40  0000 L CNN
F 1 "C" H 6456 5165 40  0000 L CNN
F 2 "~" H 6488 5100 30  0000 C CNN
F 3 "~" H 6450 5250 60  0000 C CNN
	1    6450 5250
	1    0    0    -1  
$EndComp
$Comp
L C C?
U 1 1 545AE4CC
P 6700 5250
F 0 "C?" H 6700 5350 40  0000 L CNN
F 1 "C" H 6706 5165 40  0000 L CNN
F 2 "~" H 6738 5100 30  0000 C CNN
F 3 "~" H 6700 5250 60  0000 C CNN
	1    6700 5250
	1    0    0    -1  
$EndComp
Text HLabel 6700 4750 1    60   Input ~ 0
2V8_A
$Comp
L GND #PWR?
U 1 1 545AE4CD
P 5050 3650
F 0 "#PWR?" H 5050 3650 30  0001 C CNN
F 1 "GND" H 5050 3580 30  0001 C CNN
F 2 "" H 5050 3650 60  0000 C CNN
F 3 "" H 5050 3650 60  0000 C CNN
	1    5050 3650
	1    0    0    -1  
$EndComp
$Comp
L GND #PWR?
U 1 1 545AE4CE
P 4050 4200
F 0 "#PWR?" H 4050 4200 30  0001 C CNN
F 1 "GND" H 4050 4130 30  0001 C CNN
F 2 "" H 4050 4200 60  0000 C CNN
F 3 "" H 4050 4200 60  0000 C CNN
	1    4050 4200
	1    0    0    -1  
$EndComp
$Comp
L GND #PWR?
U 1 1 545AE4CF
P 5950 5800
F 0 "#PWR?" H 5950 5800 30  0001 C CNN
F 1 "GND" H 5950 5730 30  0001 C CNN
F 2 "" H 5950 5800 60  0000 C CNN
F 3 "" H 5950 5800 60  0000 C CNN
	1    5950 5800
	1    0    0    -1  
$EndComp
Wire Wire Line
	4200 3350 4050 3350
Wire Wire Line
	4050 3250 4050 3550
Wire Wire Line
	4050 3950 4050 4100
Wire Wire Line
	4050 4100 4200 4100
Wire Wire Line
	4700 3350 5500 3350
Wire Wire Line
	4700 4100 6650 4100
Wire Wire Line
	6650 4100 6650 3950
Wire Wire Line
	6400 3950 6400 4100
Connection ~ 6400 4100
Wire Wire Line
	5900 3650 5900 4100
Connection ~ 5900 4100
Wire Wire Line
	5400 3950 5400 4100
Connection ~ 5400 4100
Wire Wire Line
	5000 3950 5000 4100
Connection ~ 5000 4100
Wire Wire Line
	5000 3550 5000 3350
Connection ~ 5000 3350
Wire Wire Line
	5400 3350 5400 3550
Connection ~ 5400 3350
Wire Wire Line
	5500 3450 5400 3450
Connection ~ 5400 3450
Wire Wire Line
	6300 3350 6650 3350
Wire Wire Line
	6650 3250 6650 3550
Wire Wire Line
	6300 3450 6400 3450
Wire Wire Line
	6400 3450 6400 3550
Connection ~ 6650 3350
Connection ~ 4050 3350
Wire Wire Line
	5950 5600 6700 5600
Wire Wire Line
	6700 5600 6700 5450
Wire Wire Line
	6450 5450 6450 5600
Connection ~ 6450 5600
Wire Wire Line
	5950 5150 5950 5800
Connection ~ 5950 5600
Wire Wire Line
	6350 4850 6700 4850
Wire Wire Line
	6700 4750 6700 5050
Wire Wire Line
	6350 4950 6450 4950
Wire Wire Line
	6450 4950 6450 5050
Connection ~ 6700 4850
Wire Wire Line
	5200 3350 5200 4950
Connection ~ 5200 3350
Wire Wire Line
	5200 4850 5550 4850
Wire Wire Line
	5200 4950 5550 4950
Connection ~ 5200 4850
$Comp
L C C?
U 1 1 545AE4F4
P 7800 1250
F 0 "C?" H 7800 1350 40  0000 L CNN
F 1 "C" H 7806 1165 40  0000 L CNN
F 2 "~" H 7838 1100 30  0000 C CNN
F 3 "~" H 7800 1250 60  0000 C CNN
	1    7800 1250
	1    0    0    -1  
$EndComp
$Comp
L CP1 C?
U 1 1 545AE4F5
P 7450 1250
F 0 "C?" H 7500 1350 50  0000 L CNN
F 1 "CP1" H 7500 1150 50  0000 L CNN
F 2 "~" H 7450 1250 60  0000 C CNN
F 3 "~" H 7450 1250 60  0000 C CNN
	1    7450 1250
	1    0    0    -1  
$EndComp
$Comp
L TVS_DIODE_UNI_TART D?
U 1 1 545AE4F6
P 8100 1250
F 0 "D?" H 8100 1350 40  0000 C CNN
F 1 "TVS_DIODE_UNI_TART" H 8090 1410 40  0001 C CNN
F 2 "~" H 8100 1250 60  0000 C CNN
F 3 "~" H 8100 1250 60  0000 C CNN
F 4 "TVS_DIODE_UNI" H 8100 1140 40  0000 C CNN "NAME"
	1    8100 1250
	0    -1   -1   0   
$EndComp
$Comp
L OK1-78SR_TART REG?
U 1 1 545AE4F7
P 8750 950
F 0 "REG?" H 8900 734 60  0000 C CNN
F 1 "OK1-78SR_TART" H 8750 1260 60  0001 C CNN
F 2 "~" H 8750 950 60  0000 C CNN
F 3 "~" H 8750 950 60  0000 C CNN
F 4 "OKI-78SR" H 8750 1154 60  0000 C CNN "NAME"
	1    8750 950 
	1    0    0    -1  
$EndComp
$Comp
L CP1 C?
U 1 1 545AE4F8
P 9350 1250
F 0 "C?" H 9400 1350 50  0000 L CNN
F 1 "CP1" H 9400 1150 50  0000 L CNN
F 2 "~" H 9350 1250 60  0000 C CNN
F 3 "~" H 9350 1250 60  0000 C CNN
	1    9350 1250
	1    0    0    -1  
$EndComp
$Comp
L C C?
U 1 1 545AE4F9
P 9700 1250
F 0 "C?" H 9700 1350 40  0000 L CNN
F 1 "C" H 9706 1165 40  0000 L CNN
F 2 "~" H 9738 1100 30  0000 C CNN
F 3 "~" H 9700 1250 60  0000 C CNN
	1    9700 1250
	1    0    0    -1  
$EndComp
$Comp
L SPX3940_TART REG?
U 1 1 545AE4FA
P 10200 950
F 0 "REG?" H 10350 734 60  0000 C CNN
F 1 "SPX3940_TART" H 10200 1260 60  0001 C CNN
F 2 "~" H 10200 950 60  0000 C CNN
F 3 "~" H 10200 950 60  0000 C CNN
F 4 "SPX3940" H 10200 1154 60  0000 C CNN "NAME"
	1    10200 950 
	1    0    0    -1  
$EndComp
$Comp
L CP1 C?
U 1 1 545AE4FB
P 10750 1250
F 0 "C?" H 10800 1350 50  0000 L CNN
F 1 "CP1" H 10800 1150 50  0000 L CNN
F 2 "~" H 10750 1250 60  0000 C CNN
F 3 "~" H 10750 1250 60  0000 C CNN
	1    10750 1250
	1    0    0    -1  
$EndComp
$Comp
L R R?
U 1 1 545AE4FC
P 10200 1600
F 0 "R?" V 10280 1600 40  0000 C CNN
F 1 "R" V 10207 1601 40  0000 C CNN
F 2 "~" V 10130 1600 30  0000 C CNN
F 3 "~" H 10200 1600 30  0000 C CNN
	1    10200 1600
	1    0    0    -1  
$EndComp
$Comp
L GND #PWR?
U 1 1 545AE4FD
P 10200 1950
F 0 "#PWR?" H 10200 1950 30  0001 C CNN
F 1 "GND" H 10200 1880 30  0001 C CNN
F 2 "" H 10200 1950 60  0000 C CNN
F 3 "" H 10200 1950 60  0000 C CNN
	1    10200 1950
	1    0    0    -1  
$EndComp
$Comp
L GND #PWR?
U 1 1 545AE4FE
P 10750 1950
F 0 "#PWR?" H 10750 1950 30  0001 C CNN
F 1 "GND" H 10750 1880 30  0001 C CNN
F 2 "" H 10750 1950 60  0000 C CNN
F 3 "" H 10750 1950 60  0000 C CNN
	1    10750 1950
	1    0    0    -1  
$EndComp
$Comp
L GND #PWR?
U 1 1 545AE4FF
P 9700 1950
F 0 "#PWR?" H 9700 1950 30  0001 C CNN
F 1 "GND" H 9700 1880 30  0001 C CNN
F 2 "" H 9700 1950 60  0000 C CNN
F 3 "" H 9700 1950 60  0000 C CNN
	1    9700 1950
	1    0    0    -1  
$EndComp
Text GLabel 5800 1150 0    60   Input ~ 0
+24V_CABLE
Text GLabel 5800 1350 0    60   Input ~ 0
GND_CABLE
$Comp
L ACM3225-601-2P-T001_TART U?
U 1 1 545AE500
P 6600 1250
F 0 "U?" H 6600 1250 60  0001 C CNN
F 1 "ACM3225-601-2P-T001_TART" H 6600 1250 60  0001 C CNN
F 2 "" H 6600 1250 60  0000 C CNN
F 3 "" H 6600 1250 60  0000 C CNN
	1    6600 1250
	1    0    0    -1  
$EndComp
Wire Wire Line
	7100 1150 7100 900 
Wire Wire Line
	7100 900  7450 900 
Wire Wire Line
	7450 900  7800 900 
Wire Wire Line
	7800 900  8100 900 
Wire Wire Line
	8100 900  8350 900 
Wire Wire Line
	7100 1350 7100 1600
Wire Wire Line
	7100 1600 7450 1600
Wire Wire Line
	7450 1600 7800 1600
Wire Wire Line
	7800 1600 8100 1600
Wire Wire Line
	8100 1600 8750 1600
Wire Wire Line
	8750 1600 9350 1600
Wire Wire Line
	9350 1600 9700 1600
Wire Wire Line
	9700 1450 9700 1600
Wire Wire Line
	9700 1600 9700 1950
Wire Wire Line
	9350 1450 9350 1600
Connection ~ 9350 1600
Wire Wire Line
	8750 1200 8750 1600
Connection ~ 8750 1600
Wire Wire Line
	8100 1450 8100 1600
Connection ~ 8100 1600
Wire Wire Line
	7800 1450 7800 1600
Connection ~ 7800 1600
Wire Wire Line
	7450 1450 7450 1600
Connection ~ 7450 1600
Wire Wire Line
	7450 1050 7450 900 
Connection ~ 7450 900 
Wire Wire Line
	7800 1050 7800 900 
Connection ~ 7800 900 
Wire Wire Line
	8100 1050 8100 900 
Connection ~ 8100 900 
Wire Wire Line
	9150 900  9350 900 
Wire Wire Line
	9350 900  9700 900 
Wire Wire Line
	9700 900  9800 900 
Wire Wire Line
	9700 900  9700 1050
Wire Wire Line
	9350 900  9350 1050
Connection ~ 9350 900 
Wire Wire Line
	10600 900  10750 900 
Wire Wire Line
	10750 900  10750 1050
Wire Wire Line
	10750 1450 10750 1950
Wire Wire Line
	10200 1200 10200 1350
Wire Wire Line
	10200 1850 10200 1950
Connection ~ 9700 900 
Connection ~ 9700 1600
Wire Wire Line
	6100 1150 5800 1150
Wire Wire Line
	6100 1350 5800 1350
$EndSCHEMATC