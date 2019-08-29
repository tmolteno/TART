EESchema Schematic File Version 4
EELAYER 30 0
EELAYER END
$Descr A4 11693 8268
encoding utf-8
Sheet 1 1
Title ""
Date ""
Rev ""
Comp ""
Comment1 ""
Comment2 ""
Comment3 ""
Comment4 ""
$EndDescr
$Comp
L Device:L L2
U 1 1 5D67C00B
P 6900 3500
F 0 "L2" H 6953 3546 50  0000 L CNN
F 1 "100nH" H 6953 3455 50  0000 L CNN
F 2 "Inductor_SMD:L_0402_1005Metric" H 6900 3500 50  0001 C CNN
F 3 "Murata LQG15HS" H 6900 3500 50  0001 C CNN
	1    6900 3500
	1    0    0    -1  
$EndComp
$Comp
L power:GNDA #PWR03
U 1 1 5D67EEDE
P 5950 4650
F 0 "#PWR03" H 5950 4400 50  0001 C CNN
F 1 "GNDA" H 5955 4477 50  0000 C CNN
F 2 "" H 5950 4650 50  0001 C CNN
F 3 "" H 5950 4650 50  0001 C CNN
	1    5950 4650
	1    0    0    -1  
$EndComp
$Comp
L power:VCC #PWR04
U 1 1 5D67FC09
P 6500 3200
F 0 "#PWR04" H 6500 3050 50  0001 C CNN
F 1 "VCC" H 6517 3373 50  0000 C CNN
F 2 "" H 6500 3200 50  0001 C CNN
F 3 "" H 6500 3200 50  0001 C CNN
	1    6500 3200
	1    0    0    -1  
$EndComp
$Comp
L Device:L L1
U 1 1 5D680C02
P 4750 3850
F 0 "L1" V 4940 3850 50  0000 C CNN
F 1 "5.6nH " V 4849 3850 50  0000 C CNN
F 2 "Inductor_SMD:L_0402_1005Metric" H 4750 3850 50  0001 C CNN
F 3 "Murata LQW15A" H 4750 3850 50  0001 C CNN
	1    4750 3850
	0    -1   -1   0   
$EndComp
$Comp
L Device:C C1
U 1 1 5D69CCB0
P 5500 3200
F 0 "C1" V 5752 3200 50  0000 C CNN
F 1 "1nF" V 5661 3200 50  0000 C CNN
F 2 "Capacitor_SMD:C_0402_1005Metric" H 5538 3050 50  0001 C CNN
F 3 "~" H 5500 3200 50  0001 C CNN
	1    5500 3200
	0    -1   -1   0   
$EndComp
$Comp
L power:GND #PWR01
U 1 1 5D69F118
P 5050 3400
F 0 "#PWR01" H 5050 3150 50  0001 C CNN
F 1 "GND" H 5055 3227 50  0000 C CNN
F 2 "" H 5050 3400 50  0001 C CNN
F 3 "" H 5050 3400 50  0001 C CNN
	1    5050 3400
	1    0    0    -1  
$EndComp
Wire Wire Line
	7600 3850 6900 3850
Wire Wire Line
	6900 3650 6900 3850
Wire Wire Line
	6900 3350 6900 3200
Wire Wire Line
	6900 3200 6500 3200
Connection ~ 6500 3200
Wire Wire Line
	5350 3200 5050 3200
Wire Wire Line
	5050 3200 5050 3400
$Comp
L dk_Coaxial-Connectors-RF:142-0701-801 J1
U 1 1 5D6A8AB5
P 4100 3850
F 0 "J1" H 4178 4075 50  0000 C CNN
F 1 "142-0701-801" H 4178 3984 50  0000 C CNN
F 2 "Connector_Coaxial:SMA_Amphenol_132289_EdgeMount" H 4300 4050 60  0001 L CNN
F 3 "http://www.belfuse.com/resources/Johnson/drawings/dr-1420701801.pdf" H 4300 4150 60  0001 L CNN
F 4 "J502-ND" H 4300 4250 60  0001 L CNN "Digi-Key_PN"
F 5 "142-0701-801" H 4300 4350 60  0001 L CNN "MPN"
F 6 "Connectors, Interconnects" H 4300 4450 60  0001 L CNN "Category"
F 7 "Coaxial Connectors (RF)" H 4300 4550 60  0001 L CNN "Family"
F 8 "http://www.belfuse.com/resources/Johnson/drawings/dr-1420701801.pdf" H 4300 4650 60  0001 L CNN "DK_Datasheet_Link"
F 9 "/product-detail/en/cinch-connectivity-solutions-johnson/142-0701-801/J502-ND/35280" H 4300 4750 60  0001 L CNN "DK_Detail_Page"
F 10 "CONN SMA JACK STR 50OHM EDGE MNT" H 4300 4850 60  0001 L CNN "Description"
F 11 "Cinch Connectivity Solutions Johnson" H 4300 4950 60  0001 L CNN "Manufacturer"
F 12 "Active" H 4300 5050 60  0001 L CNN "Status"
	1    4100 3850
	1    0    0    -1  
$EndComp
$Comp
L dk_Coaxial-Connectors-RF:142-0701-801 J2
U 1 1 5D6AD89F
P 7800 3850
F 0 "J2" H 7712 3818 50  0000 R CNN
F 1 "142-0701-801" H 7712 3727 50  0000 R CNN
F 2 "Connector_Coaxial:SMA_Amphenol_132289_EdgeMount" H 8000 4050 60  0001 L CNN
F 3 "http://www.belfuse.com/resources/Johnson/drawings/dr-1420701801.pdf" H 8000 4150 60  0001 L CNN
F 4 "J502-ND" H 8000 4250 60  0001 L CNN "Digi-Key_PN"
F 5 "142-0701-801" H 8000 4350 60  0001 L CNN "MPN"
F 6 "Connectors, Interconnects" H 8000 4450 60  0001 L CNN "Category"
F 7 "Coaxial Connectors (RF)" H 8000 4550 60  0001 L CNN "Family"
F 8 "http://www.belfuse.com/resources/Johnson/drawings/dr-1420701801.pdf" H 8000 4650 60  0001 L CNN "DK_Datasheet_Link"
F 9 "/product-detail/en/cinch-connectivity-solutions-johnson/142-0701-801/J502-ND/35280" H 8000 4750 60  0001 L CNN "DK_Detail_Page"
F 10 "CONN SMA JACK STR 50OHM EDGE MNT" H 8000 4850 60  0001 L CNN "Description"
F 11 "Cinch Connectivity Solutions Johnson" H 8000 4950 60  0001 L CNN "Manufacturer"
F 12 "Active" H 8000 5050 60  0001 L CNN "Status"
	1    7800 3850
	-1   0    0    -1  
$EndComp
Text Notes 4100 5550 0    50   ~ 0
AN11420 NXP Application Note on Powering GPS\n
Wire Wire Line
	4600 3850 4300 3850
$Comp
L power:GNDA #PWR0101
U 1 1 5D6B5D30
P 7800 4250
F 0 "#PWR0101" H 7800 4000 50  0001 C CNN
F 1 "GNDA" H 7805 4077 50  0000 C CNN
F 2 "" H 7800 4250 50  0001 C CNN
F 3 "" H 7800 4250 50  0001 C CNN
	1    7800 4250
	1    0    0    -1  
$EndComp
$Comp
L power:GNDA #PWR0102
U 1 1 5D6B61DB
P 4100 4200
F 0 "#PWR0102" H 4100 3950 50  0001 C CNN
F 1 "GNDA" H 4105 4027 50  0000 C CNN
F 2 "" H 4100 4200 50  0001 C CNN
F 3 "" H 4100 4200 50  0001 C CNN
	1    4100 4200
	1    0    0    -1  
$EndComp
Wire Wire Line
	7800 4250 7800 4050
Wire Wire Line
	4100 4200 4100 4050
$Comp
L BGU7007_115:BGU7007,115 U1
U 1 1 5D6CB740
P 5850 3850
F 0 "U1" H 6394 3896 50  0000 L CNN
F 1 "BGU7007,115" H 6394 3805 50  0000 L CNN
F 2 "BGU7007_115:PSON50P100X145X50-6N" H 5850 3850 50  0001 L BNN
F 3 "NXP USA" H 5850 3850 50  0001 L BNN
F 4 "XFDFN-6 NXP Semiconductors" H 5850 3850 50  0001 L BNN "Field4"
F 5 "BGU7007 Series 2.85 V 51 MHz Bw Low Noise Amplifier MMIC for GPS - XSON-6" H 5850 3850 50  0001 L BNN "Field5"
F 6 "Unavailable" H 5850 3850 50  0001 L BNN "Field6"
F 7 "BGU7007,115" H 5850 3850 50  0001 L BNN "Field7"
F 8 "None" H 5850 3850 50  0001 L BNN "Field8"
	1    5850 3850
	1    0    0    -1  
$EndComp
Wire Wire Line
	6900 3850 6350 3850
Connection ~ 6900 3850
Wire Wire Line
	5650 3200 5750 3200
Wire Wire Line
	5950 3450 5950 3200
Connection ~ 5950 3200
Wire Wire Line
	5950 3200 6500 3200
Wire Wire Line
	5750 3450 5750 3200
Connection ~ 5750 3200
Wire Wire Line
	5750 3200 5950 3200
Wire Wire Line
	5350 3850 4900 3850
Wire Wire Line
	5950 4250 5950 4450
Wire Wire Line
	5750 4250 5750 4450
Wire Wire Line
	5750 4450 5950 4450
Connection ~ 5950 4450
Wire Wire Line
	5950 4450 5950 4650
$EndSCHEMATC
