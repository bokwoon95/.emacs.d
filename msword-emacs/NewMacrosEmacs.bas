Attribute VB_Name = "NewMacros"
Sub BackwardChar()
Attribute BackwardChar.VB_ProcData.VB_Invoke_Func = "Normal.NewMacros.BackwardChar"
'
' BackwardChar Macro
'
'
    Selection.MoveLeft Unit:=wdCharacter, Count:=1
End Sub
Sub BackwardWord()
Attribute BackwardWord.VB_ProcData.VB_Invoke_Func = "Normal.NewMacros.BackwardWord"
'
' BackwardWord Macro
'
'
    Selection.MoveLeft Unit:=wdWord, Count:=1
End Sub
Sub Down()
Attribute Down.VB_ProcData.VB_Invoke_Func = "Normal.NewMacros.Down"
'
' Down Macro
'
'
    Selection.MoveDown Unit:=wdLine, Count:=1
End Sub
Sub DownFive()
Attribute DownFive.VB_ProcData.VB_Invoke_Func = "Normal.NewMacros.DownFive"
'
' DownFive Macro
'
'
    Selection.MoveDown Unit:=wdLine, Count:=5
End Sub
Sub EndofLine()
Attribute EndofLine.VB_ProcData.VB_Invoke_Func = "Normal.NewMacros.EndofLine"
'
' EndofLine Macro
'
'
    Selection.EndKey Unit:=wdLine
End Sub
Sub ForwardChar()
Attribute ForwardChar.VB_ProcData.VB_Invoke_Func = "Normal.NewMacros.ForwardChar"
'
' ForwardChar Macro
'
'
    Selection.MoveRight Unit:=wdCharacter, Count:=1
End Sub
Sub ForwardWord()
Attribute ForwardWord.VB_ProcData.VB_Invoke_Func = "Normal.NewMacros.ForwardWord"
'
' ForwardWord Macro
'
'
    Selection.MoveRight Unit:=wdWord, Count:=1
End Sub
Sub StartofLine()
Attribute StartofLine.VB_ProcData.VB_Invoke_Func = "Normal.NewMacros.StartofLine"
'
' StartofLine Macro
'
'
    Selection.HomeKey Unit:=wdLine
End Sub
Sub Up()
Attribute Up.VB_ProcData.VB_Invoke_Func = "Normal.NewMacros.Up"
'
' Up Macro
'
'
    Selection.MoveUp Unit:=wdLine, Count:=1
End Sub
Sub UpFive()
Attribute UpFive.VB_ProcData.VB_Invoke_Func = "Normal.NewMacros.UpFive"
'
' UpFive Macro
'
'
    Selection.MoveUp Unit:=wdLine, Count:=5
End Sub
Sub StartofDocument()
Attribute StartofDocument.VB_ProcData.VB_Invoke_Func = "Normal.NewMacros.StartofDocument"
'
' StartofDocument Macro
'
'
    Selection.HomeKey Unit:=wdStory
End Sub
Sub EndofDocument()
Attribute EndofDocument.VB_ProcData.VB_Invoke_Func = "Normal.NewMacros.EndofDocument"
'
' EndofDocument Macro
'
'
    Selection.EndKey Unit:=wdStory
End Sub
Sub SelectBackwardChar()
Attribute SelectBackwardChar.VB_ProcData.VB_Invoke_Func = "Normal.NewMacros.SelectBackwardChar"
'
' SelectBackwardChar Macro
'
'
    Selection.MoveLeft Unit:=wdCharacter, Count:=1, Extend:=wdExtend
End Sub
Sub SelectBackwardWord()
Attribute SelectBackwardWord.VB_ProcData.VB_Invoke_Func = "Normal.NewMacros.SelectBackwardWord"
'
' SelectBackwardWord Macro
'
'
    Selection.MoveLeft Unit:=wdWord, Count:=1, Extend:=wdExtend
End Sub
Sub SelectDown()
Attribute SelectDown.VB_ProcData.VB_Invoke_Func = "Normal.NewMacros.SelectDown"
'
' SelectDown Macro
'
'
    Selection.MoveDown Unit:=wdLine, Count:=1, Extend:=wdExtend
End Sub
Sub SelectDownFive()
Attribute SelectDownFive.VB_ProcData.VB_Invoke_Func = "Normal.NewMacros.SelectDownFive"
'
' SelectDownFive Macro
'
'
    Selection.MoveDown Unit:=wdLine, Count:=5, Extend:=wdExtend
End Sub
Sub SelectTilEndofLine()
Attribute SelectTilEndofLine.VB_ProcData.VB_Invoke_Func = "Normal.NewMacros.SelectTilEndofLine"
'
' SelectTilEndofLine Macro
'
'
    Selection.EndKey Unit:=wdLine, Extend:=wdExtend
End Sub
Sub SelectForwardChar()
Attribute SelectForwardChar.VB_ProcData.VB_Invoke_Func = "Normal.NewMacros.SelectForwardChar"
'
' SelectForwardChar Macro
'
'
    Selection.MoveRight Unit:=wdCharacter, Count:=1, Extend:=wdExtend
End Sub
Sub SelectForwardWord()
Attribute SelectForwardWord.VB_ProcData.VB_Invoke_Func = "Normal.NewMacros.SelectForwardWord"
'
' SelectForwardWord Macro
'
'
    Selection.MoveRight Unit:=wdWord, Count:=1, Extend:=wdExtend
End Sub
Sub SelectTilStartofLine()
Attribute SelectTilStartofLine.VB_ProcData.VB_Invoke_Func = "Normal.NewMacros.SelectTilStartofLine"
'
' SelectTilStartofLine Macro
'
'
    Selection.HomeKey Unit:=wdLine, Extend:=wdExtend
End Sub
Sub SelectUp()
Attribute SelectUp.VB_ProcData.VB_Invoke_Func = "Normal.NewMacros.SelectUp"
'
' SelectUp Macro
'
'
    Selection.MoveUp Unit:=wdLine, Count:=1, Extend:=wdExtend
End Sub
Sub SelectUpFive()
Attribute SelectUpFive.VB_ProcData.VB_Invoke_Func = "Normal.NewMacros.SelectUpFive"
'
' SelectUpFive Macro
'
'
    Selection.MoveUp Unit:=wdLine, Count:=5, Extend:=wdExtend
End Sub
Sub SelectTilStartofDocument()
Attribute SelectTilStartofDocument.VB_ProcData.VB_Invoke_Func = "Normal.NewMacros.SelectTilStartofDocument"
'
' SelectTilStartofDocument Macro
'
'
    Selection.HomeKey Unit:=wdStory, Extend:=wdExtend
End Sub
Sub SelectTilEndofDocument()
Attribute SelectTilEndofDocument.VB_ProcData.VB_Invoke_Func = "Normal.NewMacros.SelectTilEndofDocument"
'
' SelectTilEndofDocument Macro
'
'
    Selection.EndKey Unit:=wdStory, Extend:=wdExtend
End Sub
Sub DeleteForwardChar()
Attribute DeleteForwardChar.VB_ProcData.VB_Invoke_Func = "Normal.NewMacros.DeleteForwardChar"
'
' DeleteForwardChar Macro
'
'
    Selection.MoveRight Unit:=wdCharacter, Count:=1
    Selection.TypeBackspace
End Sub
Sub KillTilEndofLine()
Attribute KillTilEndofLine.VB_ProcData.VB_Invoke_Func = "Normal.NewMacros.KillTilEndofLine"
'
' KillTilEndofLine Macro
'
'
    Selection.EndKey Unit:=wdLine, Extend:=wdExtend
    Selection.MoveLeft Unit:=wdCharacter, Count:=1, Extend:=wdExtend
    Selection.Delete Unit:=wdCharacter, Count:=1
End Sub
Sub KillTilStartofLine()
Attribute KillTilStartofLine.VB_ProcData.VB_Invoke_Func = "Normal.NewMacros.KillTilStartofLine"
'
' KillTilStartofLine Macro
'
'
    Selection.HomeKey Unit:=wdLine, Extend:=wdExtend
    Selection.TypeBackspace
End Sub
Sub DeleteForwardWord()
Attribute DeleteForwardWord.VB_ProcData.VB_Invoke_Func = "Normal.NewMacros.DeleteForwardWord"
'
' DeleteForwardWord Macro
'
'
    Selection.MoveRight Unit:=wdWord, Count:=1, Extend:=wdExtend
    Selection.TypeBackspace
End Sub
Sub NewLine()
Attribute NewLine.VB_ProcData.VB_Invoke_Func = "Normal.NewMacros.NewLine"
'
' NewLine Macro
'
'
    Selection.TypeParagraph
End Sub
Sub ScrollDown()
Attribute ScrollDown.VB_ProcData.VB_Invoke_Func = "Normal.NewMacros.ScrollDown"
'
' ScrollDown Macro
'
'
    ActiveWindow.ActivePane.SmallScroll Down:=5
End Sub
Sub ScrollUp()
Attribute ScrollUp.VB_ProcData.VB_Invoke_Func = "Normal.NewMacros.ScrollUp"
'
' ScrollUp Macro
'
'
    ActiveWindow.ActivePane.SmallScroll Up:=5
End Sub
