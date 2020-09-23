Attribute VB_Name = "modZXU_Help"
Sub RUN()
10 BORDER 7
20 PAPER 7
30 INK 0
40 PRINT_AT 0, 0, 1, 6, , "         ZX HELP SYSTEM         "
50 PRINT_AT 2, 2, , 2, 1, "Game keys:"
51 PRINT_AT 5, 2, , 0, 1, "NIM: any number in a valid"
52 PRINT_AT 6, 7, , 0, 1, "range for the column"
53 PRINT_AT 8, 2, , 0, 1, "BLUE THUNDER: hit space to"
54 PRINT_AT 9, 16, , 0, 1, "fire"
55 PRINT_AT 11, 2, , 0, 1, "THE DAM: hit space to fire"
56 PRINT_AT 13, 2, , 0, 1, "WALL BREAKER: Z to move left"
57 PRINT_AT 14, 16, , 0, 1, "X to move right"

60 For i = 1 To 22
70 PRINT_AT i, 0, 1, 1, , " "
80 PRINT_AT i, 31, 1, 1, , " "
90 Next i
95 PRINT_AT 20, 3, 7, 3, , "HIT F12 TO ZOOM THE WINDOW"
100 PRINT_AT 23, 0, 1, 5, , "         press any key to return"

980 If INKEY$() = "" Then DoEvents: GoTo 980
End Sub
