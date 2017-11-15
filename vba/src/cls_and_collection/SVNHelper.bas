Attribute VB_Name = "SVNHelper"
Option Explicit

Sub ExportCode()
'add a reference to Microsoft Visual Basic for Applications Extensibility 5.3 (or later)
' excel options: http://stackoverflow.com/questions/5300770/how-to-check-from-net-code-whether-trust-access-to-the-vba-project-object-mode

    If Not CanAccessVBOM Then Exit Sub ' Exit if access to VB object model is not allowed
    If (ThisWorkbook.VBProject.VBE.ActiveWindow Is Nothing) Then
        Exit Sub ' Exit if VBA window is not open
    End If
    Dim comp As VBComponent
    Dim codeFolder As String

    codeFolder = CombinePaths(GetWorkbookPath, "src\" & getWorkbookName())
    On Error Resume Next
    MkDir codeFolder
    On Error GoTo 0
    Dim FileName As String

    For Each comp In ThisWorkbook.VBProject.VBComponents
        Select Case comp.Type
            Case vbext_ct_ClassModule
                FileName = CombinePaths(codeFolder, comp.name & ".cls")
                DeleteFile FileName
                comp.Export FileName
            Case vbext_ct_StdModule
                FileName = CombinePaths(codeFolder, comp.name & ".bas")
                DeleteFile FileName
                comp.Export FileName
            Case vbext_ct_MSForm
                FileName = CombinePaths(codeFolder, comp.name & ".frm")
                DeleteFile FileName
                comp.Export FileName
            Case vbext_ct_Document
                FileName = CombinePaths(codeFolder, comp.name & ".cls")
                DeleteFile FileName
                comp.Export FileName
        End Select
    Next

End Sub
Function CanAccessVBOM() As Boolean
    ' Check resgistry to see if we can access the VB object model
    Dim wsh As Object
    Dim str1 As String
    Dim AccessVBOM As Long

    Set wsh = CreateObject("WScript.Shell")
    str1 = "HKEY_CURRENT_USER\Software\Microsoft\Office\" & _
        Application.Version & "\Excel\Security\AccessVBOM"
    On Error Resume Next
    AccessVBOM = wsh.RegRead(str1)
    Set wsh = Nothing
    CanAccessVBOM = (AccessVBOM = 1)
End Function


Sub DeleteFile(FileName As String)
    On Error Resume Next
    Kill FileName
End Sub

Function GetWorkbookPath() As String
    Dim fullName As String
    Dim wrkbookName As String
    Dim pos As Long

    wrkbookName = ThisWorkbook.name
    fullName = ThisWorkbook.fullName

    pos = InStr(1, fullName, wrkbookName, vbTextCompare)

    GetWorkbookPath = Left$(fullName, pos - 1)
End Function

Private Sub testGetWorkbookName()
    Debug.Print (getWorkbookName())
End Sub

Function getWorkbookName() As String
    Dim sName As String
    sName = ThisWorkbook.name
    getWorkbookName = Left(sName, InStrRev(sName, ".") - 1)
End Function

Function CombinePaths(ByVal Path1 As String, ByVal Path2 As String) As String
    If Not EndsWith(Path1, "\") Then
        Path1 = Path1 & "\"
    End If
    CombinePaths = Path1 & Path2
End Function

Function EndsWith(ByVal InString As String, ByVal TestString As String) As Boolean
    EndsWith = (Right$(InString, Len(TestString)) = TestString)
End Function


