Attribute VB_Name = "some"
Option Explicit

Public Function printf(mask As String, ParamArray tokens()) As String
    Dim i As Long
    For i = 0 To UBound(tokens)
        mask = Replace$(mask, "%" & i + 1, tokens(i))
    Next
    printf = mask
End Function

Private Sub testPrintF()
  Call assertEquals("Start Price 15. End Price 25. Stop price 35. Result = 20.", _
                    printf("Start Price %1. End Price %2. Stop price %3. Result = %4.", 15, 25, 35, 20))
End Sub

Private Sub testPrintFShouldFail()
  Call assertEquals("Start Price 15. End Price 25. Stop price 35. Result = 25.", _
                    printf("Start Price %1. End Price %2. Stop price %3. Result = %4.", 15, 25, 35, 20))
End Sub

Private Sub testShouldWork()
  Call assertEquals("hello", "hello")
End Sub

Private Sub testShouldFail()
  Call assertEquals("hello", "world")
End Sub
