Public Class Form1
    Private Sub OpenFilesToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles OpenFilesToolStripMenuItem.Click
        Dim x1 As New Stat_Setting
        Dim t1 As New Controls.Set_Console_Properties(x1)
        If t1.ShowDialog <> DialogResult.OK Then Exit Sub

        Dim ofd1 As New OpenFileDialog
        ofd1.Multiselect = True
        ofd1.Filter = "Sam/Bam (*.sam,*bam)|*.sam;*.bam|Sam (*.sam)|*.sam|Bam (*.bam)|*.bam"
        If ofd1.ShowDialog = DialogResult.OK Then
            Dim Files As New List(Of System.IO.FileInfo)
            For Each f In ofd1.FileNames
                Files.Add(New System.IO.FileInfo(f))
            Next
            Dim x As New Szunyi.SAM_BAM.Stat
            Dim r = x.DoIt(x1, Files)

            Dim sfd1 As New SaveFileDialog
            If sfd1.ShowDialog = DialogResult.OK Then
                Try
                    Using sg As New System.IO.StreamWriter(sfd1.FileName)
                        sg.Write(r)
                    End Using
                Catch ex As Exception
                    MsgBox(ex.ToString)
                End Try
            End If
        End If
    End Sub
End Class
