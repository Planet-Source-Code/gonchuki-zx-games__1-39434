Attribute VB_Name = "modZXG_BlueThunder"
'Option Explicit

Sub RUN()
10 Rem ******************************************************************
20 Rem * Gonchuki Systems Productions - VB Port from the "Run" magazine *
30 Rem ******************************************************************
40 Rem *                     BLUE THUNDER 1985                          *
50 Rem ******************************************************************
60 Rem some variables
70 Let REC = 0
80 Let REC1 = 0
90 Let SW1 = 0
100 Let pt = 0
110 Let PUN = 0
115 INKEYS " " 'ADDED BY ME
120 Rem INIT
125 Randomize 'ADDED BY ME
130 GoSub 790
140 GoSub 260
150 GoTo 700
160 Let X = 20 - FILA
170 For Q = 0 To X
PAUSE 15 'ADDED BY ME
180 PRINT_AT FILA, COL, , 5, , " " & UDG("ABC")
190 PRINT_AT FILA + Q, COL, , 6, , UDG("E")
200 BEEP 0.001, 50
210 PRINT_AT FILA + Q - 1, COL, , 6, , " "
230 If ATTR(FILA + Q + 1, COL) = 56 Then PUN = PUN + 1
240 Next Q
245 PRINT_AT 20, B, 4, , , " "
250 Return
260 Rem Drawing...
270 Let F1 = 20
280 For W = 0 To 31
290 PRINT_AT F1, W, 4, , , " "
300 Next W
310 PRINT_AT 21, 18, 2, , , " HI-SCORE:    "
320 PRINT_AT 21, 28, 2, , , REC
330 Rem
340 For Z = 4 To 28
350 For j = Int(Rnd * 7) + 12 To 19
'360 PRINT_AT j, Z, 0, , , " "
370 PRINT_AT j, Z, 7, 0, , UDG("D")
380 Next j
390 Next Z
400 Let CERO = 0
410 Rem Helicopter Movement
420 Rem
430 For a = 1 To 19 Step 2
440 Let SW = 1
450 For B = 0 To 28
460 Let FILA = a: Let COL = B
462 If B = 28 Then Let CERO = -1: GoTo 470
464 Let CERO = 0
470 PRINT_AT FILA, COL, , 5, , " " & UDG("ABC")
480 BEEP 0.001, 40
490 Rem COLLISION DETECTION
500 If ATTR(FILA, COL + CERO + 4) = 56 Then Let CERO = CERO - 2: Let FILA = FILA: If FILA = FILA Then GoTo 580
510 If (FILA = 19) And (COL = 28) Then Let SW1 = 1: PRINT_AT 10, 9, , 7, 1, " GREAT PILOT! ": Let pt = pt + PUN: PRINT_AT 5, 9, , 6, , "SCORE: " & pt: GoSub 990: GoTo 710
520 If SW = 5 Then INKEYS " ": GoTo 540
530 If INKEYS(" ") Then Let SW = SW + 1: GoSub 160
540 Let FILA = FILA: Let COL = COL
PAUSE 60: DoEvents
550 Next B
560 PRINT_AT FILA, 29, 1, , , "   "
570 Next a
580 For D = FILA To 20
590 Rem CRASH
600 Rem
610 BEEP 0.001, 50
PAUSE 50
620 PRINT_AT FILA, COL + 2, , 2, , UDG("ABD")
630 PRINT_AT FILA, COL + 1, , , , " "
640 Next D
650 Rem SCORE
660 Let pt = PUN + pt
670 Return
680 Rem GAME ENDS
690 Rem
700 PRINT_AT 5, 9, , , , " SCORE: " & pt & " "
710 'If SW1 Then GoTo 930
720 GoSub 990
730 PRINT_AT 21, 28, 2, , , REC
740 PRINT_AT 7, 6, 6, , , " OTHER GAME?  (Y/N) "
750 Let K$ = INKEY$
760 If K$ = "Y" Then GoTo 90
770 If K$ = "N" Then SHUTDOWN
780 GoTo 750
790 Rem User Graphics
'800 RESTORE 810
810 DATA 0, 2, 128, 192, 127, 65, 128, 0
820 DATA 0, 171, 1, 15, 255, 255, 63, 0
830 DATA 0, 171, 0, 240, 136, 132, 194, 254
840 DATA 255, 255, 137, 137, 255, 137, 137, 137
850 DATA 108, 40, 40, 56, 16, 16, 16, 16
860 For CH = &HC0 To &HC4
870 BUILDUDG CH
880 Next CH
900 PAPER 1: BORDER 1: INK 0
910 Cls
920 Return
930 For N = 1 To 100
940
950 Next N
960 Let SW1 = 0
970 GoTo 110
980 Rem CHECK FOR HI-SCORE
990 If pt > REC Then Let REC = pt: GoTo 1010
1000 Return
1010 PRINT_AT 0, 5, 7, , , "THAT'S A NEW RECORD!!!"
1020 Return
End Sub
