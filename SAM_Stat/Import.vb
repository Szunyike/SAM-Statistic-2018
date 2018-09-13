Imports System.IO
Imports Bio.IO.SAM
Imports SAM_Stat


Public Class Import

#Region "Read IDs"
    Public Shared Function Get_Sorted_Distinct_Read_IDs(files As List(Of FileInfo)) As List(Of String)
        Dim Read_IDs = Get_Read_IDs(files)
        Read_IDs.Distinct
        Read_IDs.Sort()
        Return Read_IDs

    End Function
    Public Shared Function Get_Sorted_Distinct_Read_IDs(file As FileInfo) As List(Of String)
        Dim Read_IDs = Get_Read_IDs(file)
        Read_IDs = Read_IDs.Distinct.ToList
        Read_IDs.Sort()
        Return Read_IDs

    End Function
    Public Shared Function Get_Read_IDs(file As FileInfo) As List(Of String)
        Dim out As New List(Of String)
        For Each sam In Import.Parse(file)
            out.Add(sam.QName)
        Next
        Return out
    End Function
    Public Shared Function Get_Read_IDs(files As List(Of FileInfo)) As List(Of String)
        Dim out As New List(Of String)
        For Each sam In Import.Parse(files)
            out.Add(sam.QName)
        Next
        Return out
    End Function
    Public Shared Function Get_Sorted_Read_IDs(file As FileInfo) As List(Of String)
        Dim Read_IDs = Get_Read_IDs(file)
        Read_IDs.Sort()
        Return Read_IDs
    End Function
#End Region
#Region "Parse"
    Public Shared Function ParseAll(Files As List(Of FileInfo)) As List(Of Bio.IO.SAM.SAMAlignedSequence)
        Dim allsam As New List(Of Bio.IO.SAM.SAMAlignedSequence)
        For Each File In Files
            allsam.AddRange(ParseAll(File))
        Next
        Return allsam
    End Function
    Public Shared Function ParseAll(fIle As FileInfo) As List(Of Bio.IO.SAM.SAMAlignedSequence)
        Dim allsam As New List(Of Bio.IO.SAM.SAMAlignedSequence)
        For Each Sam In Parse(fIle)

            allsam.Add(Sam)

        Next
        Return allsam
    End Function
    Public Shared Iterator Function Parse(File As FileInfo) As IEnumerable(Of Bio.IO.SAM.SAMAlignedSequence)
        If File.Extension = ".bam" Then
            Using sr As New FileStream(File.FullName, FileMode.Open)

                Dim sa As New Bio.IO.BAM.BAMParser()
                For Each SAM As Bio.IO.SAM.SAMAlignedSequence In sa.Parse(sr)
                    Yield (SAM)
                Next
            End Using
        ElseIf File.Extension = ".sam" Then
            Dim sa As New Bio.IO.SAM.SAMParser()
            Using sr As New StreamReader(File.FullName)

                Do
                    Dim Line = sr.ReadLine
                    If IsNothing(Line) = False Then
                        If Line.StartsWith("@") <> True Then
                            Yield Bio.IO.SAM.SAMParser.ParseSequence(Line)
                        End If
                    End If
                Loop Until sr.EndOfStream = True
            End Using
        End If


    End Function


    Public Shared Iterator Function Parse(Files As List(Of FileInfo)) As IEnumerable(Of Bio.IO.SAM.SAMAlignedSequence)
        For Each File In Files
            For Each sam In Parse(File)
                Yield sam
            Next
        Next
    End Function

    Public Shared Function ParseAll_IntoStat(FIle As FileInfo) As List(Of Szunyi.SAM_BAM.Simple_Stat)
        Dim current As New List(Of Szunyi.SAM_BAM.Simple_Stat)
        Dim Header = Szunyi.SAM_BAM.Headers.Get_Header(FIle)
        Dim Index As Integer = 0
        For Each SAM In Import.Parse(FIle)

            If SAM.Flag <> SAMFlags.UnmappedQuery Then
                Index += 1
                If IsNothing(SAM.QuerySequence) = False Then
                    current.Add(New Szunyi.SAM_BAM.Simple_Stat(SAM, Index))
                End If
            End If
        Next
        Return current

    End Function
    Public Shared Iterator Function ParseAll_IntoStat_Bests(FIle As FileInfo) As IEnumerable(Of Szunyi.SAM_BAM.Simple_Stat)
        Dim current As New List(Of Szunyi.SAM_BAM.Simple_Stat)
        Dim Header = Szunyi.SAM_BAM.Headers.Get_Header(FIle)
        Dim Index As Integer = 0
        For Each SAM In Import.Parse(FIle)

            If SAM.Flag <> SAMFlags.UnmappedQuery Then
                Index += 1
                If IsNothing(SAM.QuerySequence) = False Then
                    current.Add(New Szunyi.SAM_BAM.Simple_Stat(SAM, Index))
                End If
            End If
        Next

        Dim r = From x In current Group By x.Read_ID Into Group

        For Each gr In r
            Dim Best = From k In gr.Group Order By k.Match.Sum Descending
            Yield Best.First
        Next

    End Function


#End Region
End Class
