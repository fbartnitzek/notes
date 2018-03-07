Attribute VB_Name = "InlineTest"
Option Explicit

Private Sub testInline()
  Call inlineTestWithEntries(10)
  Call inlineTestWithEntries(100)
  Call inlineTestWithEntries(1000)
  Call inlineTestWithEntries(10000)
  Call inlineTestWithEntries(100000)
  Call inlineTestWithEntries(1000000)
  
  ' Output:
'InlineTest with 00000000010 Entries: direct: 000,000s, methodByVal: 000,004s, methodByRef: 000,000s
'InlineTest with 00000000100 Entries: direct: 000,000s, methodByVal: 000,000s, methodByRef: 000,000s
'InlineTest with 00000001000 Entries: direct: 000,000s, methodByVal: 000,004s, methodByRef: 000,000s
'InlineTest with 00000010000 Entries: direct: 000,012s, methodByVal: 000,012s, methodByRef: 000,012s
'InlineTest with 00000100000 Entries: direct: 000,066s, methodByVal: 000,086s, methodByRef: 000,090s
'InlineTest with 00001000000 Entries: direct: 000,664s, methodByVal: 000,957s, methodByRef: 000,902s


End Sub

Private Sub inlineTestWithEntries(ByVal lMax As Long)
  Dim sTmp As String, l As Long
  Dim tStartTime As Single, tDirect As Single, tMethodByVal As Single, tMethodByRef As Single
  
  tStartTime = Timer
  For l = 1 To lMax
    sTmp = "abc" & ";" & l
    Debug.Assert "abc;" & l = sTmp
  Next l
  tDirect = Timer
  
  For l = 1 To lMax
    sTmp = calcByVal("abc", l)
    Debug.Assert "abc;" & l = sTmp
  Next l
  tMethodByVal = Timer
  
  For l = 1 To lMax
    sTmp = calcByRef("abc", l)
    Debug.Assert "abc;" & l = sTmp
  Next l
  tMethodByRef = Timer
  
  Debug.Print "InlineTest with " & Format(lMax, "00000000000") & " Entries: " & _
              "direct: " & Format(tDirect - tStartTime, "000.000") & "s" & _
              ", methodByVal: " & Format(tMethodByVal - tDirect, "000.000") & "s" & _
              ", methodByRef: " & Format(tMethodByRef - tMethodByVal, "000.000") & "s"
End Sub

Private Function calcByVal(ByVal s1 As String, ByVal s2 As Long)
  calcByVal = s1 & ";" & s2
End Function

Private Function calcByRef(ByRef s1 As String, ByRef s2 As Long)
  calcByRef = s1 & ";" & s2
End Function
