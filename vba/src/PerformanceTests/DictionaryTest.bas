Attribute VB_Name = "DictionaryTest"
Option Explicit

Private Sub testDictionary()
  Call dictTestWithEntries(10)
  Call dictTestWithEntries(100)
  Call dictTestWithEntries(1000)
  Call dictTestWithEntries(10000)
  Call dictTestWithEntries(100000)
  Call dictTestWithEntries(200000)
  Call dictTestWithEntries(400000)
  Call dictTestWithEntries(800000)
  
  'output:
'00000000010 Entries: insert: 000,002s, read: 000,000s, readReverse: 000,000s, whole: 000,002s
'00000000100 Entries: insert: 000,002s, read: 000,000s, readReverse: 000,000s, whole: 000,002s
'00000001000 Entries: insert: 000,002s, read: 000,000s, readReverse: 000,002s, whole: 000,004s
'00000010000 Entries: insert: 000,020s, read: 000,006s, readReverse: 000,008s, whole: 000,033s
'00000100000 Entries: insert: 000,639s, read: 000,621s, readReverse: 000,709s, whole: 001,969s
'00000200000 Entries: insert: 002,145s, read: 002,514s, readReverse: 002,727s, whole: 007,385s
'00000400000 Entries: insert: 009,703s, read: 009,363s, readReverse: 011,174s, whole: 030,240s
'00000800000 Entries: insert: 039,586s, read: 038,922s, readReverse: 049,234s, whole: 127,742s

End Sub

Private Sub dictTestWithEntries(ByVal lMax As Long)
  Const cKey = "key"
  Const cValue = "value"
  Dim dDict As New Scripting.Dictionary, l As Long, sValue As String
  Dim tStartTime As Single, tAllInserted As Single, tAllRead As Single, tAllReadReverse As Single
  
  'init
  tStartTime = Timer
  For l = 1 To lMax
    dDict.Add cKey & l, cValue & l
  Next l
  tAllInserted = Timer
  
  ' get all
  For l = 1 To lMax
    sValue = dDict(cKey & l)
  Next l
  tAllRead = Timer
  
  ' get all reversed
  For l = lMax To 0 Step -1
    sValue = dDict(cKey & l)
  Next l
  tAllReadReverse = Timer
  
  Debug.Print "DictionaryTest with " & Format(lMax, "00000000000") & " Entries: " & _
              "insert: " & Format(tAllInserted - tStartTime, "000.000") & "s" & _
              ", read: " & Format(tAllRead - tAllInserted, "000.000") & "s" & _
              ", readReverse: " & Format(tAllReadReverse - tAllRead, "000.000") & "s" & _
              ", whole: " & Format(tAllReadReverse - tStartTime, "000.000") & "s"
End Sub

