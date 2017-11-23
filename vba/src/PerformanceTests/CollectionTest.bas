Attribute VB_Name = "CollectionTest"
Option Explicit

Private Sub testCollection()
  Call collTestWithEntries(10)
  Call collTestWithEntries(100)
  Call collTestWithEntries(1000)
  Call collTestWithEntries(10000)
  Call collTestWithEntries(100000)
'  Call collTestWithEntries(200000)
'  Call collTestWithEntries(400000)
'  Call collTestWithEntries(800000)
  
  'output:
'CollectionTest with 00000000010 Entries: insert: 000,000s, read: 000,000s, readReverse: 000,000s, whole: 000,000s
'CollectionTest with 00000000100 Entries: insert: 000,000s, read: 000,000s, readReverse: 000,000s, whole: 000,000s
'CollectionTest with 00000001000 Entries: insert: 000,000s, read: 000,004s, readReverse: 000,004s, whole: 000,008s
'CollectionTest with 00000010000 Entries: insert: 000,008s, read: 000,273s, readReverse: 000,250s, whole: 000,531s
'CollectionTest with 00000100000 Entries: insert: 000,047s, read: 039,184s, readReverse: 039,348s, whole: 078,578s

End Sub

Private Sub collTestWithEntries(ByVal lMax As Long)
  Const cKey = "key"
  Const cValue = "value"
  Dim cColl As New Collection, l As Long, sValue As String
  Dim tStartTime As Single, tAllInserted As Single, tAllRead As Single, tAllReadReverse As Single
  
  'init
  tStartTime = Timer
  For l = 1 To lMax
    cColl.Add cKey & l
  Next l
  tAllInserted = Timer
  
  ' get all
  For l = 1 To lMax
    sValue = cColl(l)
  Next l
  tAllRead = Timer
  
  ' get all reversed
  For l = lMax To 1 Step -1
    sValue = cColl(l)
  Next l
  tAllReadReverse = Timer
  
  Debug.Print "CollectionTest with " & Format(lMax, "00000000000") & " Entries: " & _
              "insert: " & Format(tAllInserted - tStartTime, "000.000") & "s" & _
              ", read: " & Format(tAllRead - tAllInserted, "000.000") & "s" & _
              ", readReverse: " & Format(tAllReadReverse - tAllRead, "000.000") & "s" & _
              ", whole: " & Format(tAllReadReverse - tStartTime, "000.000") & "s"
End Sub



