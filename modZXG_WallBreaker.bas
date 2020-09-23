Attribute VB_Name = "modZXG_WallBreaker"
Sub RUN()
1 Rem ******************************************************************
2 Rem * Gonchuki Systems Productions - VB Port from the "Run" magazine *
3 Rem ******************************************************************
4 Rem *                     WALL BREAKER 1985                          *
5 Rem ******************************************************************
'8 POKE 23658, 8
10 GoSub 840
15 GoSub 1000
20 Let T = 0: Let P = 1: BORDER 0: INK 0: PAPER 7: Cls
30 For M = 1 To 12 Step 2
40 For N = 0 To 30 Step 2
50 PRINT_AT M + 3, N, 2, 7, , UDG("AB"): PRINT_AT M + 4, N, 2, 7, , UDG("BA")
60 Next N: Next M
65 PRINT_AT 1, 0, , 7, 1, UDG("GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG")
70 Let U = 0: Let V = 0
80 Let a = 14: Let T = 0: Let W = 0
90 For R = 1 To LIVES
91 GoSub 93: GoTo 95
93 PRINT_AT 0, 1, , 1, , "LIVES: " & LIVES - R + 1 & "     BRICKS LEFT: " & 192 - Val(T) & "  ": Return
95 PRINT_AT 21, 0, , , , "                                "
96 For i = 3 To 1 Step -1
97 PRINT_AT 19, 15, , 3, , i: PAUSE 600
98 Next i
99 INKEYS "Z": INKEYS "X": PRINT_AT 19, 15, , , , " "
100 Randomize: Let M = 15: Let U = 0: Let V = 0
105 If Rnd < 0.5 Then N = 13: GoTo 110
107 N = 15
110 Let G = 530: Let P = 0: Let a = 13
120 PRINT_AT 21, a, , , , UDG("DEF")
130 GoSub 133: PRINT_AT U, V, , , , " "
131 GoTo 140
132 Rem 'GoSub' SELECTION
133 PAUSE 130 - (T \ 3)
134 If G = 200 Then GoSub 200: Return
135 If G = 300 Then GoSub 300: Return
136 If G = 350 Then GoSub 350: Return
137 If G = 400 Then GoSub 400: Return
138 If G = 500 Then GoSub 500: Return
139 If G = 530 Then GoSub 530: Return
140 PRINT_AT M, N, , 4, , UDG("C"): Let U = M: Let V = N
150 If M = 20 Then PRINT_AT M, N, , 4, , UDG("C")
'160 Let k$ = INKEY$
170 If INKEYS("Z") Then GoSub 620
180 If INKEYS("X") Then GoSub 660
190 DoEvents: GoTo 130
200 If M > 20 Then GoTo 680
210 If M < 20 Then GoTo 260
220 If T >= 192 Then GoTo 930
230 Let P = 0: Let W = 0: If N = a + 1 Or N = a + 2 Then Let G = 300: GoTo 300
240 If N = a Then G = 500: GoTo 510
250 If N = a - 1 Then Let G = 350: GoTo 350
260 If N > 30 Then Let G = 400: GoTo 400
270 Let M = M + 1: Let N = N + 1
280 Let C = ATTR(M, N): If C = 23 Then GoSub 710: If P = 0 Or W = 1 Then Let G = 300
290 Return
300 If N > 30 Then Let G = 350: GoTo 350
310 If M < 3 Then Let W = 1: Let G = 200: GoTo 200
320 Let M = M - 1: Let N = N + 1
330 Let C = ATTR(M, N): If C = 23 Then GoSub 710: Let P = 1: Let G = 200 + 150 * W
340 Return
350 If M < 3 Then Let W = 1: Let G = 400: GoTo 400
360 If N < 1 Then Let G = 300: GoTo 300
370 Let M = M - 1: Let N = N - 1
380 Let C = ATTR(M, N): If C = 23 Then GoSub 710: Let P = 1: Let G = 400 - 100 * W
390 Return
400 If M > 20 Then GoTo 680
410 If M < 20 Then GoTo 460
420 If T >= 192 Then GoTo 930
430 Let P = 0: Let W = 0: If N = a + 3 Then Let G = 300: GoTo 300
440 If N = a + 2 Then Let G = 500: GoTo 500
450 If N = a Or N = a + 1 Then Let G = 350: GoTo 350
460 If N < 1 Then Let G = 200: GoTo 200
470 Let M = M + 1: Let N = N - 1
480 Let C = ATTR(M, N): If C = 23 Then GoSub 710: If P = 0 Or W = 1 Then Let G = 350
490 Return
500 If M < 3 Then Let G = 530: GoTo 590
510 Let M = M - 1: Let C = ATTR(M, N): If C = 23 Then GoSub 710: Let G = 530
520 Return
530 If M > 20 Then GoTo 680
540 If M < 20 Then GoTo 590
550 If T >= 192 Then GoTo 930
560 Let P = 0: Let W = 0: If N = a + 2 Then Let G = 300: GoTo 300
'570 If N = A + 1 Then Let G = 300 + (50 * (Int(Rnd * 2))): GoTo 133
580 If N = a Then Let G = 350: GoTo 350
590 Let M = M + 1: Return
600 If a < 1 Then Return
610 Let a = a - 1: PRINT_AT 21, a, , , , UDG("DEF") & " ": Return
620 If a < 2 Then GoTo 600
630 Let a = a - 2: PRINT_AT 21, a, , , , UDG("DEF") & "  ": Return
640 If a > 28 Then Return
650 Let a = a + 1: PRINT_AT 21, a - 1, , , , " " & UDG("DEF"): Return
660 If a > 27 Then GoTo 640
670 Let a = a + 2: PRINT_AT 21, a - 2, , , , "  " & UDG("DEF"): Return
680 BEEP 0.5, 40: BEEP 1, 10
690 Next R
700 GoTo 780
710 Let T = T + 1: GoSub 93
720 Let B = Abs(M - N)
730 Let y = (Int(B / 2)) * 2
740 BEEP 0.01, 40
750 If B = y And N < 31 Then PRINT_AT M, N + 1, , , , " "
760 If B <> y And N > 0 Then PRINT_AT M, N - 1, , , , " "
770 Return
780 Rem GAME ENDS
785 GoSub 93
790 PRINT_AT 10, 6, , , , "     GAME  OVER     "
800 PRINT_AT 4, 0, 7, 1, 1, "DO YOU WANT TO PLAY AGAIN? (Y/N)"
810 Let R = INKEY$
820 If R = "Y" Then GoTo 10
830 If R <> "N" Then GoTo 810
835 SHUTDOWN 'GOTO 10000 in Basic
840 Rem MAX LIVES
850 Cls
860 PRINT_AT 19, 1, , , , "HOW MANY LIVES? (3-9)"
870 Let K$ = INKEY$
890 If CODE(K$) < 51 Or CODE(K$) > 57 Then GoTo 870
900 BEEP 0.1, 40
910 Let LIVES = Val(K$)
920 Return
930 Rem WALL BROKEN
940 INK 6: PAPER 1: BORDER 1
945 Cls
950 PRINT_AT 10, 3, , , , "YOU HAVE BROKEN THE WALL!!!"
960 PRINT_AT 4, 0, , 2, 1, "DO YOU WANT TO PLAY AGAIN? (Y/N)"
970 Let R = INKEY$
980 If R = "Y" Then GoTo 10
990 If R <> "N" Then GoTo 970
995 SHUTDOWN 'GOTO 10000 in Basic
1000 Rem UDGs
1010 DATA 255, 192, 128, 128, 128, 128, 192, 255
1020 DATA 255, 3, 1, 1, 1, 1, 3, 255
1030 DATA 60, 126, 255, 223, 255, 255, 126, 60
1040 DATA 127, 255, 255, 255, 127, 1, 3, 15
1050 DATA 255, 255, 255, 255, 255, 195, 129, 0
1060 DATA 254, 255, 255, 255, 254, 128, 192, 240
1065 DATA 0, 0, 0, 0, 255, 0, 255, 0
1070 For N = &HC0 To &HC6
1080 BUILDUDG N
1090 Next N
1100 Return

End Sub
