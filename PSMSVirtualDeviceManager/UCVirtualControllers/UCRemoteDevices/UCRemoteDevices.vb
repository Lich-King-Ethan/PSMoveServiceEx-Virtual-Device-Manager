' =========================
' UCRemoteDevices.vb
' PART 1 / 3
' =========================

Imports System.Net
Imports System.Net.NetworkInformation
Imports System.Net.Sockets
Imports System.Numerics
Imports System.Text

Public Class UCRemoteDevices
    Public g_mUCVirtualControllers As UCVirtualControllers

    Const DEFAULT_SOCKET_PORT As Integer = 6969

    Private Shared g_mThreadLock As New Object
    Private g_bInit As Boolean = False

    Public g_mClassStrackerSocket As ClassTrackerSocket

    Private g_iSocketPort As Integer = 0
    Private g_sLocalIP As String = ""

    Private g_mLocalAddressThread As Threading.Thread = Nothing
' =========================
' PART 2 / 3
' =========================

    Class ClassRemoteDevicesListViewItem
        Inherits ListViewItem
        Implements IDisposable

        Public g_UCRemoteDeviceItem As UCRemoteDeviceItem
        Public g_UCRemoteDevices As UCRemoteDevices

        Public Sub New(sTrackerName As String, _UCRemoteDevices As UCRemoteDevices)
            MyBase.New(New String() {""})
            g_UCRemoteDevices = _UCRemoteDevices
            g_UCRemoteDeviceItem = New UCRemoteDeviceItem(sTrackerName, _UCRemoteDevices)
            g_UCRemoteDeviceItem.Init()
            UpdateItem()
        End Sub

        Public Sub UpdateItem()
            Const LISTVIEW_SUBITEM_NAME As Integer = 0
            If g_UCRemoteDeviceItem Is Nothing OrElse g_UCRemoteDeviceItem.IsDisposed Then Return
            If g_UCRemoteDeviceItem.g_mClassIO Is Nothing Then Return

            Dim sTrackerName As String = g_UCRemoteDeviceItem.m_TrackerName
            If Not String.IsNullOrEmpty(g_UCRemoteDeviceItem.m_Nickname) Then
                sTrackerName &= $" ({g_UCRemoteDeviceItem.m_Nickname})"
            End If

            Me.SubItems(LISTVIEW_SUBITEM_NAME).Text = sTrackerName
            Me.BackColor = If(g_UCRemoteDeviceItem.m_HasStatusError,
                              Color.FromArgb(255, 192, 192),
                              Color.White)
        End Sub

        ReadOnly Property m_TrackerName As String
            Get
                If g_UCRemoteDeviceItem Is Nothing OrElse g_UCRemoteDeviceItem.IsDisposed Then Return Nothing
                Return g_UCRemoteDeviceItem.m_TrackerName
            End Get
        End Property
' =========================
' PART 3 / 3
' =========================

    Class ClassTrackerSocket
        Implements IDisposable

        Public g_mUCRemoteDevices As UCRemoteDevices
        Private g_mSocket As Socket = Nothing
        Private g_mBuffer As Byte() = New Byte(512) {}
        Private g_mTrackers As New Dictionary(Of String, ClassTracker)
        Private g_bAllowNewDevices As Boolean = False

        Private Sub SetupNewTracker(mBinReader As IO.BinaryReader, mEndPoint As IPEndPoint)
            Dim sMacAddress As String = ""
            Dim sTrackerKey As String

            ' --- ORIGINAL HANDSHAKE READ (UNCHANGED) ---
            mBinReader.ReadInt64()
            mBinReader.ReadInt32()
            mBinReader.ReadInt32()
            mBinReader.ReadInt32()
            mBinReader.ReadInt32()
            mBinReader.ReadInt32()
            mBinReader.ReadInt32()

            Dim iFirmwareBuild As Integer = BR_ReadInt32(mBinReader)
            If iFirmwareBuild < 1 Then Return

            Dim iLen As Integer = (mBinReader.ReadByte And &HFF)
            Dim sb As New StringBuilder
            While iLen > 0
                Dim c As Char = Chr(mBinReader.ReadByte())
                If Asc(c) = 0 Then Exit While
                sb.Append(c)
                iLen -= 1
            End While

            Dim iMac(5) As Byte
            If (mBinReader.BaseStream.Length - mBinReader.BaseStream.Position) >= 6 Then
                iMac = mBinReader.ReadBytes(6)
                If iMac.Any(Function(b) b <> 0) Then
                    sMacAddress = $"{iMac(0):X2}:{iMac(1):X2}:{iMac(2):X2}:{iMac(3):X2}:{iMac(4):X2}:{iMac(5):X2}"
                End If
            End If

            ' === THIS IS THE FIX ===
            sTrackerKey = If(Not String.IsNullOrEmpty(sMacAddress),
                             sMacAddress,
                             mEndPoint.Address.ToString())

            SyncLock g_mThreadLock
                If Not g_mTrackers.ContainsKey(sTrackerKey) Then
                    Dim name = If(String.IsNullOrEmpty(sMacAddress),
                                  $"UDP: {mEndPoint.Address}",
                                  $"MAC: {sMacAddress}")
                    g_mTrackers(sTrackerKey) = New ClassTracker(name, ClassTracker.ENUM_PROTOCOL_TYPE.SLIMEVR, mEndPoint)
                    RaiseEvent OnTrackerConnected(g_mTrackers(sTrackerKey))
                End If
            End SyncLock
        End Sub
    End Class
End Class
