Imports System.IO
Imports Bio.IO.SAM

Namespace Szunyi.SAM_BAM

    Public Class Headers
        ''' <summary>
        ''' Return Read Group Record Fields  
        ''' </summary>
        ''' <param name="files"></param>
        ''' <returns></returns>
        Public Shared Function Get_Read_Groups(files As List(Of FileInfo)) As IEnumerable(Of SAMRecordField)
            Dim Headers = Get_Header(files)
            Dim out As New List(Of SAMRecordField)
            For Each Header As SAMAlignmentHeader In Headers
                Dim RGs = From x In Header.RecordFields Where x.Typecode = "RG"

                out.AddRange(RGs)

            Next
            Return out
        End Function

        ''' <summary>
        ''' Return the length of Sequence of the specified ID
        ''' </summary>
        ''' <param name="File"></param>
        ''' <param name="RName"></param>
        ''' <returns></returns>
        Public Shared Function Get_Organism_Length(File As FileInfo, RName As String) As Long
            Dim S = Get_ReferenceSequences(File)
            Dim r = (From x In S Where x.Name = RName Select x.Length).Sum
            Return r

        End Function

        ''' <summary>
        ''' Return the total length of Sequences in SAM/BAM File
        ''' </summary>
        ''' <param name="File"></param>
        ''' <returns></returns>
        Public Shared Function Get_Organism_Length(File As FileInfo) As Long
            Dim S = Get_ReferenceSequences(File)
            Dim r = (From x In S Select x.Length).Sum
            Return r

        End Function

        ''' <summary>
        ''' 
        ''' </summary>
        ''' <param name="File"></param>
        ''' <returns></returns>
        Public Shared Function Get_Comments(File As FileInfo) As String
            If IsNothing(File) = True Then Return String.Empty
            Dim Header = Get_Header(File)
            Dim str As New System.Text.StringBuilder
            For Each CO In Header.Comments
                str.Append("#").Append(CO).AppendLine()
            Next
            If str.Length > 0 Then
                str.Length -= 2
                Return str.ToString
            End If
            Return String.Empty
        End Function
#Region "Header"
        ''' <summary>
        ''' Return Headers From SAM OR Bam File
        ''' </summary>
        ''' <param name="Files"></param>
        ''' <returns></returns>
        Public Shared Function Get_Header(files As List(Of FileInfo)) As IEnumerable(Of SAMAlignmentHeader)
            Dim out As New List(Of SAMAlignmentHeader)
            For Each File In files
                out.Add(Get_Header(File))
            Next
            Return out
        End Function

        ''' <summary>
        ''' Return Header From SAM OR Bam File
        ''' </summary>
        ''' <param name="File"></param>
        ''' <returns></returns>
        Public Shared Function Get_Header(File As FileInfo) As SAMAlignmentHeader
            Try
                Using sr As New FileStream(File.FullName, FileMode.Open)
                    If File.Extension = ".Bam" Or File.Extension = ".bam" Then
                        Dim sa As New Bio.IO.BAM.BAMParser()
                        Return sa.GetHeader(sr)
                    ElseIf File.Extension = ".Sam" Or File.Extension = ".sam" Then
                        Return Bio.IO.SAM.SAMParser.ParseSAMHeader(sr)
                    End If
                End Using
            Catch ex As Exception

            End Try

            Return Nothing
        End Function
        ''' <summary>
        ''' Return Header From SAM OR Bam File
        ''' </summary>
        ''' <returns></returns>
        Public Shared Function Get_Header_s(h As SAMAlignmentHeader) As String

            Dim str As New System.Text.StringBuilder
            '  str.Append(File.Name).AppendLine()
            For Each I In h.RecordFields
                str.Append("@").Append(I.Typecode)
                For Each i1 In I.Tags
                    str.Append(vbTab).Append(i1.Tag).Append(":").Append(i1.Value)
                Next
                str.AppendLine()
            Next
            For Each I In h.ReferenceSequences
                str.Append(I.Name).Append(vbTab).Append(I.Length).AppendLine()

            Next
            Return str.ToString
        End Function
#End Region

        ''' <summary>
        ''' 
        ''' </summary>
        ''' <param name="reference_Sequences"></param>
        ''' <returns></returns>
        Public Shared Function Get_Header(reference_Sequences As List(Of ReferenceSequenceInfo)) As String
            Dim str As New System.Text.StringBuilder
            For Each refSeq In reference_Sequences
                str.Append("@SQ").Append(vbTab).Append("SN:").Append(refSeq.Name).Append("LN:").Append(refSeq.Length).AppendLine()
            Next
            If str.Length > 0 Then str.Length -= 2
            Return str.ToString
        End Function

        ''' <summary>
        ''' return all unique Reference Sequence info
        ''' </summary>
        ''' <param name="File"></param>
        ''' <returns></returns>
        Public Shared Function Get_ReferenceSequences(File As FileInfo) As IEnumerable(Of Bio.IO.SAM.ReferenceSequenceInfo)
            Dim Headers = Get_Header(File)

            Dim RefSeqs As New List(Of Bio.IO.SAM.ReferenceSequenceInfo)

            RefSeqs.AddRange(Headers.ReferenceSequences)

            Dim uRefSeqs = From c In RefSeqs Select New With {Key c.Name, c.Length} Distinct.ToList
            Dim out As New List(Of Bio.IO.SAM.ReferenceSequenceInfo)
            For Each RefSeq In uRefSeqs
                out.Add(New Bio.IO.SAM.ReferenceSequenceInfo(RefSeq.Name, RefSeq.Length))
            Next
            Return out
        End Function

        ''' <summary>
        ''' return all unique Reference Sequence info
        ''' </summary>
        ''' <param name="Files"></param>
        ''' <returns></returns>
        Public Shared Function Get_ReferenceSequences(Files As List(Of FileInfo)) As IEnumerable(Of Bio.IO.SAM.ReferenceSequenceInfo)
            Dim Headers = Get_Header(Files)

            Dim RefSeqs As New List(Of Bio.IO.SAM.ReferenceSequenceInfo)
            For Each Header In Headers
                RefSeqs.AddRange(Header.ReferenceSequences)
            Next
            Dim uRefSeqs = From c In RefSeqs Select New With {Key c.Name, c.Length} Distinct.ToList
            Dim out As New List(Of Bio.IO.SAM.ReferenceSequenceInfo)
            For Each RefSeq In uRefSeqs
                out.Add(New Bio.IO.SAM.ReferenceSequenceInfo(RefSeq.Name, RefSeq.Length))
            Next
            Return out
        End Function
        Friend Shared Function Get_Reference_SeqIDS(bam_Files As List(Of FileInfo)) As IEnumerable(Of String)
            Dim IDs = Get_ReferenceSequences(bam_Files)
            If IDs.Count > 0 Then
                Dim res = (From x In IDs Select x.Name).ToList
                Return res.Distinct.ToList
            Else
                Return New List(Of String)
            End If
        End Function

        Friend Shared Function Get_Read_Groups_IDs(bam_Files As List(Of FileInfo)) As Object


            Dim IDs As New List(Of String)
            For Each r In Get_Read_Groups(bam_Files)
                Dim ks = From x In r.Tags Where x.Tag = "ID"
                For Each k In ks
                    IDs.Add(k.Value)
                Next

            Next
            Return IDs.Distinct.ToList
        End Function

        Friend Shared Function Get_Read_Group_ID(SAM As SAMAlignedSequence) As String
            Throw New NotImplementedException()
        End Function
    End Class
End Namespace
