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
LIBS:radio_hub-cache
EELAYER 27 0
EELAYER END
$Descr A4 11693 8268
encoding utf-8
Sheet 2 3
Title "noname.sch"
Date "3 dec 2014"
Rev ""
Comp ""
Comment1 ""
Comment2 ""
Comment3 ""
Comment4 ""
$EndDescr
$Comp
L TPS78227DDCR_TART ICA7
U 1 1 545AE4C2
P 9500 1400
F 0 "ICA7" H 9650 1184 60  0000 C CNN
F 1 "TPS78227DDCR_TART" H 9500 1710 60  0001 C CNN
F 2 "~" H 9500 1400 60  0000 C CNN
F 3 "~" H 9500 1400 60  0000 C CNN
F 4 "TPS78227DDCR" H 9500 1604 60  0000 C CNN "NAME"
	1    9500 1400
	1    0    0    -1  
$EndComp
$Comp
L C CA9
U 1 1 545AE4C3
P 10000 1750
F 0 "CA9" H 10000 1850 40  0000 L CNN
F 1 "10nF" H 10006 1665 40  0000 L CNN
F 2 "~" H 10038 1600 30  0000 C CNN
F 3 "~" H 10000 1750 60  0000 C CNN
	1    10000 1750
	1    0    0    -1  
$EndComp
$Comp
L C CA8
U 1 1 545AE4C5
P 9000 1750
F 0 "CA8" H 9000 1850 40  0000 L CNN
F 1 "1uF" H 9006 1665 40  0000 L CNN
F 2 "~" H 9038 1600 30  0000 C CNN
F 3 "~" H 9000 1750 60  0000 C CNN
	1    9000 1750
	1    0    0    -1  
$EndComp
$Comp
L TVS_DIODE_UNI_TART DA1
U 1 1 545AE4C6
P 8600 1750
F 0 "DA1" H 8600 1850 40  0000 C CNN
F 1 "TVS_DIODE_UNI_TART" H 8590 1910 40  0001 C CNN
F 2 "~" H 8600 1750 60  0000 C CNN
F 3 "~" H 8600 1750 60  0000 C CNN
F 4 "TVS_DIODE_UNI" H 8600 1640 40  0000 C CNN "NAME"
	1    8600 1750
	0    -1   -1   0   
$EndComp
$Comp
L R RA7
U 1 1 545AE4C7
P 8050 1350
F 0 "RA7" V 8130 1350 40  0000 C CNN
F 1 "6R8" V 8057 1351 40  0000 C CNN
F 2 "~" V 7980 1350 30  0000 C CNN
F 3 "~" H 8050 1350 30  0000 C CNN
	1    8050 1350
	0    -1   -1   0   
$EndComp
$Comp
L R RA8
U 1 1 545AE4C8
P 8050 2100
F 0 "RA8" V 8130 2100 40  0000 C CNN
F 1 "6R8" V 8057 2101 40  0000 C CNN
F 2 "~" V 7980 2100 30  0000 C CNN
F 3 "~" H 8050 2100 30  0000 C CNN
	1    8050 2100
	0    -1   -1   0   
$EndComp
$Comp
L CP1 CA7
U 1 1 545AE4C9
P 7650 1750
F 0 "CA7" H 7700 1850 50  0000 L CNN
F 1 "220uF" H 7700 1650 50  0000 L CNN
F 2 "~" H 7650 1750 60  0000 C CNN
F 3 "~" H 7650 1750 60  0000 C CNN
	1    7650 1750
	1    0    0    -1  
$EndComp
$Comp
L TPS78227DDCR_TART ICA8
U 1 1 545AE4CA
P 9500 2900
F 0 "ICA8" H 9650 2684 60  0000 C CNN
F 1 "TPS78227DDCR_TART" H 9500 3210 60  0001 C CNN
F 2 "~" H 9500 2900 60  0000 C CNN
F 3 "~" H 9500 2900 60  0000 C CNN
F 4 "TPS78227DDCR" H 9500 3104 60  0000 C CNN "NAME"
	1    9500 2900
	1    0    0    -1  
$EndComp
$Comp
L C CA11
U 1 1 545AE4CB
P 10000 3250
F 0 "CA11" H 10000 3350 40  0000 L CNN
F 1 "10nF" H 10006 3165 40  0000 L CNN
F 2 "~" H 10038 3100 30  0000 C CNN
F 3 "~" H 10000 3250 60  0000 C CNN
	1    10000 3250
	1    0    0    -1  
$EndComp
$Comp
L C CA12
U 1 1 545AE4CC
P 10250 3250
F 0 "CA12" H 10250 3350 40  0000 L CNN
F 1 "1uF" H 10256 3165 40  0000 L CNN
F 2 "~" H 10288 3100 30  0000 C CNN
F 3 "~" H 10250 3250 60  0000 C CNN
	1    10250 3250
	1    0    0    -1  
$EndComp
$Comp
L C C12
U 1 1 545AE4F4
P 4000 1700
F 0 "C12" H 4000 1800 40  0000 L CNN
F 1 "0.1uF" H 4006 1615 40  0000 L CNN
F 2 "~" H 4038 1550 30  0000 C CNN
F 3 "~" H 4000 1700 60  0000 C CNN
	1    4000 1700
	1    0    0    -1  
$EndComp
$Comp
L CP1 C13
U 1 1 545AE4F5
P 3650 1700
F 0 "C13" H 3700 1800 50  0000 L CNN
F 1 "100uF" H 3700 1600 50  0000 L CNN
F 2 "~" H 3650 1700 60  0000 C CNN
F 3 "~" H 3650 1700 60  0000 C CNN
	1    3650 1700
	1    0    0    -1  
$EndComp
$Comp
L TVS_DIODE_UNI_TART D1
U 1 1 545AE4F6
P 4300 1700
F 0 "D1" H 4300 1800 40  0000 C CNN
F 1 "TVS_DIODE_UNI_TART" H 4290 1860 40  0001 C CNN
F 2 "~" H 4300 1700 60  0000 C CNN
F 3 "~" H 4300 1700 60  0000 C CNN
F 4 "TVS_DIODE_UNI" H 4300 1590 40  0000 C CNN "NAME"
	1    4300 1700
	0    -1   -1   0   
$EndComp
$Comp
L OK1-78SR_TART IC4
U 1 1 545AE4F7
P 4950 1400
F 0 "IC4" H 5100 1184 60  0000 C CNN
F 1 "OK1-78SR_TART" H 4950 1710 60  0001 C CNN
F 2 "~" H 4950 1400 60  0000 C CNN
F 3 "~" H 4950 1400 60  0000 C CNN
F 4 "OKI-78SR" H 4950 1604 60  0000 C CNN "NAME"
	1    4950 1400
	1    0    0    -1  
$EndComp
$Comp
L CP1 C10
U 1 1 545AE4F8
P 5550 1700
F 0 "C10" H 5600 1800 50  0000 L CNN
F 1 "100uF" H 5600 1600 50  0000 L CNN
F 2 "~" H 5550 1700 60  0000 C CNN
F 3 "~" H 5550 1700 60  0000 C CNN
	1    5550 1700
	1    0    0    -1  
$EndComp
$Comp
L C C11
U 1 1 545AE4F9
P 5900 1700
F 0 "C11" H 5900 1800 40  0000 L CNN
F 1 "0.1uF" H 5906 1615 40  0000 L CNN
F 2 "~" H 5938 1550 30  0000 C CNN
F 3 "~" H 5900 1700 60  0000 C CNN
	1    5900 1700
	1    0    0    -1  
$EndComp
$Comp
L SPX3940_TART IC3
U 1 1 545AE4FA
P 6400 1400
F 0 "IC3" H 6650 1150 60  0000 C CNN
F 1 "SPX3940_TART" H 6400 1710 60  0001 C CNN
F 2 "~" H 6400 1400 60  0000 C CNN
F 3 "~" H 6400 1400 60  0000 C CNN
F 4 "SPX3940" H 6400 1604 60  0000 C CNN "NAME"
	1    6400 1400
	1    0    0    -1  
$EndComp
$Comp
L CP1 C9
U 1 1 545AE4FB
P 6950 1700
F 0 "C9" H 7000 1800 50  0000 L CNN
F 1 "10uF" H 7000 1600 50  0000 L CNN
F 2 "~" H 6950 1700 60  0000 C CNN
F 3 "~" H 6950 1700 60  0000 C CNN
	1    6950 1700
	1    0    0    -1  
$EndComp
$Comp
L ACM3225-601-2P-T001_TART L3
U 1 1 545AE500
P 2800 1700
F 0 "L3" H 2800 2000 60  0000 C CNN
F 1 "ACM3225-601-2P-T001_TART" H 2800 1700 60  0001 C CNN
F 2 "" H 2800 1700 60  0000 C CNN
F 3 "" H 2800 1700 60  0000 C CNN
	1    2800 1700
	1    0    0    -1  
$EndComp
$Comp
L GNDA #PWR017
U 1 1 545C443B
P 9500 2200
F 0 "#PWR017" H 9500 2200 40  0001 C CNN
F 1 "GNDA" H 9500 2130 40  0000 C CNN
F 2 "" H 9500 2200 60  0000 C CNN
F 3 "" H 9500 2200 60  0000 C CNN
	1    9500 2200
	1    0    0    -1  
$EndComp
$Comp
L GNDA #PWR018
U 1 1 545C444A
P 9500 3800
F 0 "#PWR018" H 9500 3800 40  0001 C CNN
F 1 "GNDA" H 9500 3730 40  0000 C CNN
F 2 "" H 9500 3800 60  0000 C CNN
F 3 "" H 9500 3800 60  0000 C CNN
	1    9500 3800
	1    0    0    -1  
$EndComp
Text GLabel 6950 1000 1    60   Input ~ 0
+3V3
Text GLabel 10250 1000 1    60   Input ~ 0
2V8_A
Text GLabel 10250 2750 1    60   Input ~ 0
2V8_B
Text GLabel 2000 1600 0    60   Input ~ 0
+24V_CABLE
Text GLabel 2000 1800 0    60   Input ~ 0
GND_CABLE
$Comp
L R RA9
U 1 1 545FE20A
P 2500 3400
F 0 "RA9" V 2580 3400 40  0000 C CNN
F 1 "0" V 2507 3401 40  0000 C CNN
F 2 "~" V 2430 3400 30  0000 C CNN
F 3 "~" H 2500 3400 30  0000 C CNN
	1    2500 3400
	0    -1   -1   0   
$EndComp
$Comp
L R RA10
U 1 1 545FE217
P 3400 3400
F 0 "RA10" V 3480 3400 40  0000 C CNN
F 1 "0" V 3407 3401 40  0000 C CNN
F 2 "~" V 3330 3400 30  0000 C CNN
F 3 "~" H 3400 3400 30  0000 C CNN
	1    3400 3400
	0    -1   -1   0   
$EndComp
$Comp
L R RA11
U 1 1 545FE21D
P 3400 4150
F 0 "RA11" V 3480 4150 40  0000 C CNN
F 1 "0" V 3407 4151 40  0000 C CNN
F 2 "~" V 3330 4150 30  0000 C CNN
F 3 "~" H 3400 4150 30  0000 C CNN
	1    3400 4150
	0    -1   -1   0   
$EndComp
$Comp
L GND #PWR019
U 1 1 545FE492
P 2150 3600
F 0 "#PWR019" H 2150 3600 30  0001 C CNN
F 1 "GND" H 2150 3530 30  0001 C CNN
F 2 "" H 2150 3600 60  0000 C CNN
F 3 "" H 2150 3600 60  0000 C CNN
	1    2150 3600
	1    0    0    -1  
$EndComp
$Comp
L GNDA #PWR020
U 1 1 545FE4A1
P 2850 3600
F 0 "#PWR020" H 2850 3600 40  0001 C CNN
F 1 "GNDA" H 2850 3530 40  0000 C CNN
F 2 "" H 2850 3600 60  0000 C CNN
F 3 "" H 2850 3600 60  0000 C CNN
	1    2850 3600
	1    0    0    -1  
$EndComp
$Comp
L GNDA #PWR021
U 1 1 545FE4C2
P 3050 3600
F 0 "#PWR021" H 3050 3600 40  0001 C CNN
F 1 "GNDA" H 3050 3530 40  0000 C CNN
F 2 "" H 3050 3600 60  0000 C CNN
F 3 "" H 3050 3600 60  0000 C CNN
	1    3050 3600
	1    0    0    -1  
$EndComp
$Comp
L GNDA #PWR022
U 1 1 545FE4C8
P 3750 3600
F 0 "#PWR022" H 3750 3600 40  0001 C CNN
F 1 "GNDA" H 3750 3530 40  0000 C CNN
F 2 "" H 3750 3600 60  0000 C CNN
F 3 "" H 3750 3600 60  0000 C CNN
	1    3750 3600
	1    0    0    -1  
$EndComp
$Comp
L GNDA #PWR023
U 1 1 545FE4CE
P 3050 4350
F 0 "#PWR023" H 3050 4350 40  0001 C CNN
F 1 "GNDA" H 3050 4280 40  0000 C CNN
F 2 "" H 3050 4350 60  0000 C CNN
F 3 "" H 3050 4350 60  0000 C CNN
	1    3050 4350
	1    0    0    -1  
$EndComp
$Comp
L GNDA #PWR024
U 1 1 545FE4D4
P 3750 4350
F 0 "#PWR024" H 3750 4350 40  0001 C CNN
F 1 "GNDA" H 3750 4280 40  0000 C CNN
F 2 "" H 3750 4350 60  0000 C CNN
F 3 "" H 3750 4350 60  0000 C CNN
	1    3750 4350
	1    0    0    -1  
$EndComp
Text GLabel 6500 2650 3    60   Input ~ 0
GNDINT
$Comp
L R R14
U 1 1 545FE665
P 6300 2600
F 0 "R14" V 6380 2600 40  0000 C CNN
F 1 "0" V 6307 2601 40  0000 C CNN
F 2 "~" V 6230 2600 30  0000 C CNN
F 3 "~" H 6300 2600 30  0000 C CNN
	1    6300 2600
	-1   0    0    1   
$EndComp
$Comp
L R RA12
U 1 1 545FE6B7
P 1600 4150
F 0 "RA12" V 1680 4150 40  0000 C CNN
F 1 "0" V 1607 4151 40  0000 C CNN
F 2 "~" V 1530 4150 30  0000 C CNN
F 3 "~" H 1600 4150 30  0000 C CNN
	1    1600 4150
	0    -1   -1   0   
$EndComp
Text GLabel 1250 4350 3    60   Input ~ 0
GNDINT
$Comp
L GND #PWR025
U 1 1 545FE6C2
P 1950 4350
F 0 "#PWR025" H 1950 4350 30  0001 C CNN
F 1 "GND" H 1950 4280 30  0001 C CNN
F 2 "" H 1950 4350 60  0000 C CNN
F 3 "" H 1950 4350 60  0000 C CNN
	1    1950 4350
	1    0    0    -1  
$EndComp
$Comp
L C CA10
U 1 1 546017E6
P 10250 1750
F 0 "CA10" H 10250 1850 40  0000 L CNN
F 1 "1uF" H 10256 1665 40  0000 L CNN
F 2 "~" H 10288 1600 30  0000 C CNN
F 3 "~" H 10250 1750 60  0000 C CNN
	1    10250 1750
	1    0    0    -1  
$EndComp
$Comp
L PWR_FLAG #FLG026
U 1 1 54612EAA
P 2300 1600
F 0 "#FLG026" H 2300 1695 30  0001 C CNN
F 1 "PWR_FLAG" H 2300 1780 30  0000 C CNN
F 2 "" H 2300 1600 60  0000 C CNN
F 3 "" H 2300 1600 60  0000 C CNN
	1    2300 1600
	1    0    0    -1  
$EndComp
$Comp
L PWR_FLAG #FLG027
U 1 1 54612EB0
P 2300 1800
F 0 "#FLG027" H 2300 1895 30  0001 C CNN
F 1 "PWR_FLAG" H 2300 1980 30  0000 C CNN
F 2 "" H 2300 1800 60  0000 C CNN
F 3 "" H 2300 1800 60  0000 C CNN
	1    2300 1800
	-1   0    0    1   
$EndComp
$Comp
L PWR_FLAG #FLG028
U 1 1 546296C9
P 8600 2100
F 0 "#FLG028" H 8600 2195 30  0001 C CNN
F 1 "PWR_FLAG" H 8600 2280 30  0000 C CNN
F 2 "" H 8600 2100 60  0000 C CNN
F 3 "" H 8600 2100 60  0000 C CNN
	1    8600 2100
	-1   0    0    1   
$EndComp
$Comp
L PWR_FLAG #FLG029
U 1 1 546296EA
P 1950 4150
F 0 "#FLG029" H 1950 4245 30  0001 C CNN
F 1 "PWR_FLAG" H 1950 4330 30  0000 C CNN
F 2 "" H 1950 4150 60  0000 C CNN
F 3 "" H 1950 4150 60  0000 C CNN
	1    1950 4150
	1    0    0    -1  
$EndComp
$Comp
L DGND #PWR030
U 1 1 547536F5
P 6300 2950
F 0 "#PWR030" H 6300 2950 40  0001 C CNN
F 1 "DGND" H 6300 2850 40  0000 C CNN
F 2 "" H 6300 2950 60  0000 C CNN
F 3 "" H 6300 2950 60  0000 C CNN
	1    6300 2950
	1    0    0    -1  
$EndComp
Wire Wire Line
	6800 1350 7800 1350
Wire Wire Line
	7650 2100 7800 2100
Wire Wire Line
	8300 1350 9100 1350
Wire Wire Line
	8300 2100 10250 2100
Wire Wire Line
	10250 2100 10250 1950
Wire Wire Line
	10000 1950 10000 2100
Connection ~ 10000 2100
Wire Wire Line
	9500 1650 9500 2200
Connection ~ 9500 2100
Wire Wire Line
	9000 1950 9000 2100
Connection ~ 9000 2100
Wire Wire Line
	8600 1950 8600 2100
Connection ~ 8600 2100
Wire Wire Line
	8600 1550 8600 1350
Connection ~ 8600 1350
Wire Wire Line
	9000 1350 9000 1550
Connection ~ 9000 1350
Wire Wire Line
	9100 1450 9000 1450
Connection ~ 9000 1450
Wire Wire Line
	9900 1350 10250 1350
Wire Wire Line
	10250 1000 10250 1550
Wire Wire Line
	9900 1450 10000 1450
Wire Wire Line
	10000 1450 10000 1550
Connection ~ 10250 1350
Connection ~ 7650 1350
Wire Wire Line
	9500 3600 10250 3600
Wire Wire Line
	10250 3600 10250 3450
Wire Wire Line
	10000 3450 10000 3600
Connection ~ 10000 3600
Wire Wire Line
	9500 3150 9500 3800
Connection ~ 9500 3600
Wire Wire Line
	9900 2850 10250 2850
Wire Wire Line
	10250 2750 10250 3050
Wire Wire Line
	9900 2950 10000 2950
Wire Wire Line
	10000 2950 10000 3050
Connection ~ 10250 2850
Wire Wire Line
	8800 1350 8800 2950
Connection ~ 8800 1350
Wire Wire Line
	8800 2850 9100 2850
Connection ~ 8800 2850
Wire Wire Line
	3300 1600 3300 1350
Wire Wire Line
	3300 1350 4550 1350
Wire Wire Line
	3300 1800 3300 2050
Wire Wire Line
	3300 2050 6500 2050
Wire Wire Line
	5900 2050 5900 1900
Wire Wire Line
	5550 1900 5550 2050
Connection ~ 5550 2050
Wire Wire Line
	4950 1650 4950 2050
Connection ~ 4950 2050
Wire Wire Line
	4300 1900 4300 2050
Connection ~ 4300 2050
Wire Wire Line
	4000 1900 4000 2050
Connection ~ 4000 2050
Wire Wire Line
	3650 1900 3650 2050
Connection ~ 3650 2050
Wire Wire Line
	3650 1500 3650 1350
Connection ~ 3650 1350
Wire Wire Line
	4000 1500 4000 1350
Connection ~ 4000 1350
Wire Wire Line
	4300 1500 4300 1350
Connection ~ 4300 1350
Wire Wire Line
	5350 1350 6000 1350
Wire Wire Line
	5900 1350 5900 1500
Wire Wire Line
	5550 1350 5550 1500
Connection ~ 5550 1350
Connection ~ 5900 1350
Connection ~ 5900 2050
Wire Wire Line
	2300 1600 2000 1600
Wire Wire Line
	2300 1800 2000 1800
Connection ~ 7650 2100
Wire Wire Line
	7650 1550 7650 1350
Wire Wire Line
	8800 2950 9100 2950
Wire Wire Line
	2250 3400 2150 3400
Wire Wire Line
	2150 3400 2150 3600
Wire Wire Line
	2750 3400 2850 3400
Wire Wire Line
	2850 3400 2850 3600
Wire Wire Line
	3150 3400 3050 3400
Wire Wire Line
	3050 3400 3050 3600
Wire Wire Line
	3650 3400 3750 3400
Wire Wire Line
	3750 3400 3750 3600
Wire Wire Line
	3150 4150 3050 4150
Wire Wire Line
	3050 4150 3050 4350
Wire Wire Line
	3650 4150 3750 4150
Wire Wire Line
	3750 4150 3750 4350
Wire Wire Line
	1350 4150 1250 4150
Wire Wire Line
	1250 4150 1250 4350
Wire Wire Line
	1850 4150 1950 4150
Wire Wire Line
	1950 4150 1950 4350
Wire Wire Line
	6500 1650 6500 2650
Connection ~ 6500 2050
Wire Wire Line
	6950 1000 6950 1500
Connection ~ 6950 1350
Wire Wire Line
	6300 1650 6300 2350
Wire Wire Line
	6300 2850 6300 2950
Wire Wire Line
	7650 1950 7650 2100
Wire Wire Line
	6950 1900 6950 2200
Wire Wire Line
	6950 2200 6300 2200
Connection ~ 6300 2200
$EndSCHEMATC
