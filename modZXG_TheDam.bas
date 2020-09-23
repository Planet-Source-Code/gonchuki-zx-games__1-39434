Attribute VB_Name = "modZXG_TheDam"
Sub RUN()

INKEYS " "
1 Rem ******************************************************************
2 Rem * Gonchuki Systems Productions - VB Port from the "Run" magazine *
3 Rem ******************************************************************
7 Let CONT = 0
'8 POKE 23658, 8
9 BRIGHT 0: PAPER 1: BORDER 1: INK 0: Cls
10 DATA 255, 136, 136, 136, 255, 162, 162, 162
20 DATA 255, 136, 136, 136, 255, 34, 34, 34
30 DATA 255, 139, 139, 139, 255, 35, 35, 35
40 DATA 255, 139, 139, 139, 255, 35, 255, 255
50 DATA 255, 136, 136, 136, 255, 34, 255, 255
55 DATA 255, 136, 136, 136, 255, 34, 35, 35
60 DATA 255, 139, 139, 139, 255, 35, 35, 35
61 DATA 255, 90, 44, 44, 44, 44, 223, 223
62 DATA 6, 11, 23, 46, 92, 184, 248, 252
63 DATA 240, 192, 128, 0, 0, 128, 192, 224
64 DATA 254, 139, 136, 139, 249, 38, 35, 17
65 DATA 255, 136, 152, 184, 205, 38, 72, 207
66 DATA 191, 196, 68, 34, 248, 47, 36, 34
67 DATA 128, 64, 48, 24, 46, 16, 25, 1
68 DATA 1, 12, 22, 44, 220, 184, 136, 224
69 DATA 15, 1, 12, 30, 54, 44, 88, 224
70 DATA 120, 0, 184, 180, 58, 28, 6, 1
90 DATA 0, 16, 51, 127, 243, 127, 51, 16
100 For N = &HC0 To &HD1
110 BUILDUDG N
140 Next N
150 Let a$ = UDG("ABBBBBBC")
160 Let B$ = UDG("ABBBBFED")
170 Let C$ = UDG("ABBBBG")
180 Let D$ = UDG("ABBBBBBBG")
185 Let E$ = UDG("ABBBBBBBBBBBBBBBBBBBBBBBBBBBBBBG")
190 Let X = 17
200 PRINT_AT 2, 3, 6, 0, , a$
210 PRINT_AT 3, 0, 5, , 1, "   ": PRINT_AT 3, 3, 6, 0, , B$
220 For N = 1 To 15
230 PRINT_AT 3 + N, 0, 5, , 1, "   ": PRINT_AT 3 + N, 3, 6, 0, , C$
240 Next N
250 PRINT_AT 19, 0, 6, 0, , D$
260 PRINT_AT 20, 0, 6, 0, , D$
270 PRINT_AT 21, 0, 4, 2, , E$
280 PRINT_AT 20, 11, 1, , , UDG("HI")
281 PRINT_AT 0, 10, , , , "SCORE: " & CONT
285 Let SW1 = 0
290 Let SW2 = 0
300 Let a = Int(Rnd * 15) + 3
310 Let B = 31
320 Let B = B - 1
PAUSE 40: DoEvents
325 If B = 2 Then GoTo 5000
330 If ATTR(a, B) = 34 Then PRINT_AT a, B, , 2, 1, UDG("R"): PRINT_AT a, B + 1, , , , " ": Let XP = a: Let YP = B: GoSub 7000: Let CONT = CONT + 10: Let X = 18: Let SW1 = 0: GoTo 300
340 If ATTR(a, B) = 48 Then GoTo 2000
350 PRINT_AT a, B, , 2, 1, UDG("R"): PRINT_AT a, B + 1, , , , " "
360 If SW1 Then GoTo 400
380 GoTo 600
400 If X = 1 Then Let SW1 = 0: PRINT_AT X + 1, 11, , , , " ": Let X = 18: GoTo 320
410 GoTo 3000
600 If INKEYS(" ") Then Let SW1 = 1: GoTo 3000
620 GoTo 320
2000 PRINT_AT a, B + 1, 1, , , " "
2003 If ATTR(a - 1, B) = 48 Then PRINT_AT a - 1, B, 6, , , UDG("L")
2005 If ATTR(a + 1, B) = 48 Then PRINT_AT a + 1, B, 6, , , UDG("M")
2010 PRINT_AT a, B - 1, 6, , , UDG("K")  ':BEEP...
2020 PRINT_AT a, B, 1, 6, , UDG("J")  ':BEEP...
2040 GoTo 300
3000 If ATTR(X, 11) = 74 Then GoTo 3200
3010 PRINT_AT X, 11, , 4, 1, UDG("H")
3020 PRINT_AT X + 1, 11, , , , " "
3030 Let X = X - 1
3040 GoTo 320
3200 PRINT_AT X, 11, , 4, 1, UDG("H")
3210 PRINT_AT X + 1, 11, , , , " "
3220 Let XP = X: Let YP = 10
3230 GoSub 7000
3240 Let CONT = CONT + 10
3250 Let SW1 = 0
3260 Let X = 18
3270 GoTo 300
5000 Rem DAM BROKEN
5010 Rem GAME ENDS
5030 PRINT_AT a, 0, 5, , 1, "   "
5035 Let F = a
'5040 PAPER 5: BRIGHT 1
5050 PRINT_AT a, B + 1, 5, , 1, " "
5060 PRINT_AT a + 1, B, 5, , 1, UDG("R")
5065 PRINT_AT a + 1, B, 5, , 1, " "
5070 For N = a + 1 To 17
5080 PRINT_AT N + 1, B - 1, 5, , 1, UDG("R")
5090 PRINT_AT N, B - 1, 5, , 1, " "
5095 PAUSE 40: BEEP 0.06, 40 - N / 2
5100 Next N
5110 Rem FLOODING
5520 For N = 3 To 31
5530 PRINT_AT F, N, 5, , 1, " "
5540 PAUSE 30
5550 Next N
5560 For N = F + 1 To 21
5570 PRINT_AT N, 31, 5, , 1, " "
5575 PAUSE 30
5580 Next N
5590 For N = 21 To a - 2 Step -1
5600 PRINT_AT N, 0, 5, , 1, "                                "
5602 PAUSE 30
5604 Next N
5605 For N = 2 To a - 3
5607 PRINT_AT N, 0, 1, 6, , "                                "
5610 Next N
5620 PRINT_AT 20, 8, 5, 1, 1, "G A M E  O V E R"
5630 PRINT_AT a - 2, 0, 1, 4, , "DO YOU WANT TO PLAY AGAIN? (Y/N)"
5640 Let R = INKEY$
5650 If R = "Y" Then GoTo 7
5660 If R <> "N" Then GoTo 5640
5670 SHUTDOWN 'GOTO 10000 in Basic
6999 Rem EXPLOSIONS
7000 PRINT_AT XP, YP, , , , "  "
7005 For Q = 1 To 6
7007 Let T = Int(Rnd * 6) + 1
7010 PRINT_AT XP, YP, , T, , UDG("NO"): PRINT_AT XP + 1, YP, , T, , UDG("PQ")
7015 PAUSE 50: BEEP 0.01, 40
7020 Next Q
7030 PRINT_AT XP, YP, , , , "  "
7040 PRINT_AT XP + 1, YP, , , , "  "
7045 Let CONT = CONT + 210 - a
7050 PRINT_AT 0, 18, , , , CONT
7060 Return

9999 INKEY$
End Sub
