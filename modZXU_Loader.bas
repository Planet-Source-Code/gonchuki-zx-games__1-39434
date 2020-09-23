Attribute VB_Name = "modZXU_Loader"
'Option Explicit

Sub Main()
'the non-avoidable VB initialization...
ChDir App.Path

Set modZXBasic.viewForm = frmNim
Set modZXBasic.viewPic = frmNim.picView
Call modZXBasic.InitBasic
frmNim.Show

'and we start our pseudo-Basic routine...
NEWs
RUN
End Sub

Sub RUN()
10 Rem *************************************************
15 Rem * Gonchuki Systems Productions - ZX Loader Menu *
20 Rem *************************************************
25 Rem *               ZX LOADER 2002                  *
30 Rem *************************************************
40 BORDER 6
50 PAPER 4
60 PRINT_AT 0, 0, 7, 1, , "          ZX GAME MENU          "
65 PRINT_AT 2, 2, , 2, 1, "Select a game from the list"
70 For i = 1 To 22
80 PRINT_AT i, 0, 7, 1, , " "
85 PRINT_AT i, 31, 7, 1, , " "
90 Next i
100 PRINT_AT 23, 0, 7, 1, , "                                "
110 PRINT_AT 5, 2, , , , "1) NIM"
120 PRINT_AT 7, 2, , , , "2) BLUE THUNDER"
130 PRINT_AT 9, 2, , , , "3) THE DAM"
140 PRINT_AT 11, 2, , , , "4) WALL BREAKER"

180 PRINT_AT 19, 2, , 1, , "H) HELP"
190 PRINT_AT 21, 2, , 1, , "X) EXIT"
200 Let S$ = INKEY$
210 If S$ = "1" Then GoTo 400
220 If S$ = "2" Then GoTo 410
230 If S$ = "3" Then GoTo 420
240 If S$ = "4" Then GoTo 430

370 If S$ = "H" Then GoTo 500
380 If S$ = "X" Then SHUTDOWN
390 GoTo 200
400 modZXBasic.InitBasic: modZXG_Nim.RUN:         GoTo 40
410 modZXBasic.InitBasic: modZXG_BlueThunder.RUN: GoTo 40
420 modZXBasic.InitBasic: modZXG_TheDam.RUN:      GoTo 40
430 modZXBasic.InitBasic: modZXG_WallBreaker.RUN: GoTo 40
500 modZXBasic.InitBasic: modZXU_Help.RUN:        GoTo 40
End Sub
