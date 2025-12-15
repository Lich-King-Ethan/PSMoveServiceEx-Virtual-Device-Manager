Imports System.Net
Imports System.Net.Sockets
Imports System.Threading
Imports System.IO
Imports System.Text

Public Class UCRemoteDevices
    Public g_mUCVirtualControllers As UCVirtualControllers = Nothing

    Private g_mThreadReceive As Thread = Nothing
    Private g_mReceiveUdpClient As UdpClient = Nothing
    Private g_bIsRunning As Boolean = False

    Private ReadOnly g_mLockRemoteDevices As New Object
    ' Key is either:
    ' - MAC (preferred when provided), OR
    ' - legacy IP string (mEndPoint.Address.ToString())
    Private g_mRemoteDevices As New Dictionary(Of String, ClassTracker)

    Private g_bUiReady As Boolean = False

    ' Optional routing header: [Int32 "MAC1"][Int32 macLen][macBytes UTF8]
    ' Little-endian int for ASCII "MAC1" = 0x3143414D
    Private Const PACKET_MAGIC_ROUTEKEY As Integer = &H3143414D

    Private Enum ENUM_PACKET_RECEIVE_TYPE
        ENUM_PACKET_RECEIVE_TYPE_HANDSHAKE = 0
        ENUM_PACKET_RECEIVE_TYPE_DISCONNECT = 1
        ENUM_PACKET_RECEIVE_TYPE_ROTATION = 2
        ENUM_PACKET_RECEIVE_TYPE_ROTATION_WHEEL = 3
        ENUM_PACKET_RECEIVE_TYPE_ACCEL = 4
        ENUM_PACKET_RECEIVE_TYPE_POSITION = 5
    End Enum

    Private Enum ENUM_PROTOCOL_TYPE
        ENUM_PROTOCOL_TYPE_GENERIC_TRACKER = 0
        ENUM_PROTOCOL_TYPE_WHEEL_TRACKER = 1
    End Enum

    Public Sub New()
        InitializeComponent()
    End Sub

    Private Sub UCRemoteDevices_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        If (DesignMode) Then
            Return
        End If

        g_bUiReady = True
        UpdateRemoteDeviceList()
    End Sub

    Public Sub StartRemoteDevices()
        If (g_bIsRunning) Then
            Return
        End If

        g_bIsRunning = True

        Try
            g_mReceiveUdpClient = New UdpClient(6969)
            g_mReceiveUdpClient.Client.ReceiveBufferSize = 1024 * 1024
        Catch
            g_bIsRunning = False
            Throw
        End Try

        g_mThreadReceive = New Thread(AddressOf ThreadReceiveLoop)
        g_mThreadReceive.IsBackground = True
        g_mThreadReceive.Start()
    End Sub

    Public Sub StopRemoteDevices()
        g_bIsRunning = False

        Try
            If (g_mReceiveUdpClient IsNot Nothing) Then
                g_mReceiveUdpClient.Close()
                g_mReceiveUdpClient = Nothing
            End If
        Catch
        End Try

        Try
            If (g_mThreadReceive IsNot Nothing) Then
                If (g_mThreadReceive.IsAlive) Then
                    g_mThreadReceive.Join(500)
                End If
                g_mThreadReceive = Nothing
            End If
        Catch
        End Try

        SyncLock g_mLockRemoteDevices
            g_mRemoteDevices.Clear()
        End SyncLock

        If (g_bUiReady) Then
            UpdateRemoteDeviceList()
        End If
    End Sub

    Private Shared Function NormalizeMac(s As String) As String
        If (String.IsNullOrWhiteSpace(s)) Then Return ""

        Dim t As String = s.Trim().ToUpperInvariant()
        t = t.Replace("-", ":").Replace(" ", "")

        ' If it is 12 hex chars (no separators), format as XX:XX:XX:XX:XX:XX
        Dim hexOnly As String = t.Replace(":", "")
        If (hexOnly.Length = 12) Then
            Dim ok As Boolean = True
            For Each ch As Char In hexOnly
                If Not ((ch >= "0"c AndAlso ch <= "9"c) OrElse (ch >= "A"c AndAlso ch <= "F"c)) Then
                    ok = False
                    Exit For
                End If
            Next

            If (ok) Then
                Return String.Format("{0}:{1}:{2}:{3}:{4}:{5}",
                                     hexOnly.Substring(0, 2),
                                     hexOnly.Substring(2, 2),
                                     hexOnly.Substring(4, 2),
                                     hexOnly.Substring(6, 2),
                                     hexOnly.Substring(8, 2),
                                     hexOnly.Substring(10, 2))
            End If
        End If

        Return t
    End Function

    Private Sub ThreadReceiveLoop()
        While (g_bIsRunning)
            Try
                Dim mRemoteEndpoint As New IPEndPoint(IPAddress.Any, 0)

                Dim arrBytes As Byte() = g_mReceiveUdpClient.Receive(mRemoteEndpoint)
                If (arrBytes Is Nothing OrElse arrBytes.Length < 8) Then
                    Continue While
                End If

                Using ms As New MemoryStream(arrBytes)
                    Using br As New BinaryReader(ms)
                        Dim iPacketId As Integer = br.ReadInt32()
                        Dim iPacketIndex As Integer = br.ReadInt32()

                        ' Optional MAC routing header (Dendrite can include this; legacy WiFi packets won't)
                        Dim sRouteMac As String = ""
                        If ((br.BaseStream.Length - br.BaseStream.Position) >= 4) Then
                            Dim iMagic As Integer = br.ReadInt32()
                            If (iMagic = PACKET_MAGIC_ROUTEKEY) Then
                                Dim iMacLen As Integer = br.ReadInt32()
                                If (iMacLen > 0 AndAlso iMacLen <= 64 AndAlso (br.BaseStream.Length - br.BaseStream.Position) >= iMacLen) Then
                                    sRouteMac = NormalizeMac(Encoding.UTF8.GetString(br.ReadBytes(iMacLen)))
                                Else
                                    sRouteMac = ""
                                End If
                            Else
                                ' Not our header: rewind so legacy parsing stays correct
                                br.BaseStream.Position -= 4
                            End If
                        End If

                        ' Identity key:
                        ' - MAC if present (lets multiple trackers share one sender IP)
                        ' - else legacy IP-based behavior
                        Dim sTrackerKey As String = If(Not String.IsNullOrEmpty(sRouteMac), sRouteMac, mRemoteEndpoint.Address.ToString())

                        Dim mTracker As ClassTracker = Nothing
                        SyncLock g_mLockRemoteDevices
                            If (g_mRemoteDevices.ContainsKey(sTrackerKey)) Then
                                mTracker = g_mRemoteDevices(sTrackerKey)
                            End If
                        End SyncLock

                        Select Case CType(iPacketId, ENUM_PACKET_RECEIVE_TYPE)
                            Case ENUM_PACKET_RECEIVE_TYPE.ENUM_PACKET_RECEIVE_TYPE_HANDSHAKE
                                If (mTracker IsNot Nothing) Then
                                    mTracker.m_LastPingMs = Environment.TickCount
                                Else
                                    mTracker = SetupNewTracker(br, mRemoteEndpoint, sRouteMac)
                                    If (mTracker IsNot Nothing) Then
                                        SyncLock g_mLockRemoteDevices
                                            If (Not g_mRemoteDevices.ContainsKey(mTracker.m_Key)) Then
                                                g_mRemoteDevices.Add(mTracker.m_Key, mTracker)
                                            End If
                                        End SyncLock

                                        If (g_bUiReady) Then
                                            BeginInvoke(CType(Sub()
                                                                  UpdateRemoteDeviceList()
                                                              End Sub, MethodInvoker))
                                        End If
                                    End If
                                End If

                            Case ENUM_PACKET_RECEIVE_TYPE.ENUM_PACKET_RECEIVE_TYPE_DISCONNECT
                                If (mTracker IsNot Nothing) Then
                                    SyncLock g_mLockRemoteDevices
                                        If (g_mRemoteDevices.ContainsKey(mTracker.m_Key)) Then
                                            g_mRemoteDevices.Remove(mTracker.m_Key)
                                        End If
                                    End SyncLock

                                    If (g_bUiReady) Then
                                        BeginInvoke(CType(Sub()
                                                              UpdateRemoteDeviceList()
                                                          End Sub, MethodInvoker))
                                    End If
                                End If

                            Case ENUM_PACKET_RECEIVE_TYPE.ENUM_PACKET_RECEIVE_TYPE_ROTATION
                                If (mTracker IsNot Nothing) Then
                                    mTracker.UpdateRotationData(br)
                                End If

                            Case ENUM_PACKET_RECEIVE_TYPE.ENUM_PACKET_RECEIVE_TYPE_ROTATION_WHEEL
                                If (mTracker IsNot Nothing) Then
                                    mTracker.UpdateRotationWheelData(br)
                                End If

                            Case ENUM_PACKET_RECEIVE_TYPE.ENUM_PACKET_RECEIVE_TYPE_ACCEL
                                If (mTracker IsNot Nothing) Then
                                    mTracker.UpdateAccelData(br)
                                End If

                            Case ENUM_PACKET_RECEIVE_TYPE.ENUM_PACKET_RECEIVE_TYPE_POSITION
                                If (mTracker IsNot Nothing) Then
                                    mTracker.UpdatePositionData(br)
                                End If
                        End Select
                    End Using
                End Using

            Catch ex As SocketException
                If (Not g_bIsRunning) Then
                    Exit While
                End If
            Catch
                ' Keep receiver thread alive
            End Try

            CleanupTimedOutDevices()
        End While
    End Sub

    Private Sub CleanupTimedOutDevices()
        Dim arrRemoveKeys As New List(Of String)

        SyncLock g_mLockRemoteDevices
            For Each kvp In g_mRemoteDevices
                Dim mTracker As ClassTracker = kvp.Value
                If (mTracker Is Nothing) Then
                    arrRemoveKeys.Add(kvp.Key)
                Else
                    Dim iDelta As Integer = Environment.TickCount - mTracker.m_LastPingMs
                    If (iDelta > 10000) Then
                        arrRemoveKeys.Add(kvp.Key)
                    End If
                End If
            Next

            For Each sKey In arrRemoveKeys
                If (g_mRemoteDevices.ContainsKey(sKey)) Then
                    g_mRemoteDevices.Remove(sKey)
                End If
            Next
        End SyncLock

        If (arrRemoveKeys.Count > 0 AndAlso g_bUiReady) Then
            BeginInvoke(CType(Sub()
                                  UpdateRemoteDeviceList()
                              End Sub, MethodInvoker))
        End If
    End Sub

    ' NOTE:
    ' This remains compatible with legacy packets.
    ' Dendrite can supply sRouteMac via the optional "MAC1" header (preferred routing key).
    Private Function SetupNewTracker(br As BinaryReader, mEndPoint As IPEndPoint, Optional sRouteMac As String = "") As ClassTracker
        Try
            Dim iProtocolType As Integer = br.ReadInt32()

            Dim iNameSize As Integer = br.ReadInt32()
            If (iNameSize < 0 OrElse iNameSize > 1024) Then
                Return Nothing
            End If

            Dim sName As String = ""
            If (iNameSize > 0) Then
                Dim arrName As Byte() = br.ReadBytes(iNameSize)
                sName = Encoding.UTF8.GetString(arrName)
            End If

            Dim iMacSize As Integer = br.ReadInt32()
            If (iMacSize < 0 OrElse iMacSize > 64) Then
                Return Nothing
            End If

            Dim sMacAddress As String = ""
            If (iMacSize > 0) Then
                Dim arrMac As Byte() = br.ReadBytes(iMacSize)

                If (arrMac.Length >= 6) Then
                    Dim iMacAddress(5) As Integer
                    iMacAddress(0) = arrMac(0)
                    iMacAddress(1) = arrMac(1)
                    iMacAddress(2) = arrMac(2)
                    iMacAddress(3) = arrMac(3)
                    iMacAddress(4) = arrMac(4)
                    iMacAddress(5) = arrMac(5)

                    sMacAddress = String.Format("{0:X2}:{1:X2}:{2:X2}:{3:X2}:{4:X2}:{5:X2}",
                                                iMacAddress(0), iMacAddress(1), iMacAddress(2),
                                                iMacAddress(3), iMacAddress(4), iMacAddress(5))
                Else
                    sMacAddress = BitConverter.ToString(arrMac).Replace("-", ":")
                End If
            End If

            Dim sMacNorm As String = NormalizeMac(sMacAddress)
            Dim sRouteNorm As String = NormalizeMac(sRouteMac)

            ' Decide identity key:
            ' 1) Handshake MAC (best)
            ' 2) Route header MAC (Dendrite) (next best)
            ' 3) Legacy IP (fallback)
            Dim sKey As String
            If (Not String.IsNullOrEmpty(sMacNorm)) Then
                sKey = sMacNorm
            ElseIf (Not String.IsNullOrEmpty(sRouteNorm)) Then
                sKey = sRouteNorm
            Else
                sKey = mEndPoint.Address.ToString()
            End If

            Dim mTracker As New ClassTracker()
            mTracker.m_EndPoint = mEndPoint
            mTracker.m_ProtocolType = iProtocolType
            mTracker.m_LastPingMs = Environment.TickCount

            mTracker.m_MacAddress = If(Not String.IsNullOrEmpty(sMacNorm), sMacNorm, sKey)
            mTracker.m_Key = sKey

            ' IMPORTANT:
            ' UCRemoteDeviceItem uses m_Name to identify UI updates.
            ' For MAC-based trackers, we set name to MAC so they are uniquely addressable.
            ' For legacy WiFi trackers, name is still IP (same as before).
            mTracker.m_Name = sKey

            ' Optional: keep original display name if it exists (but do NOT change identity)
            If (Not String.IsNullOrWhiteSpace(sName)) Then
                mTracker.m_DisplayName = sName.Trim()
            Else
                mTracker.m_DisplayName = ""
            End If

            Return mTracker
        Catch
            Return Nothing
        End Try
    End Function

    Private Sub UpdateRemoteDeviceList()
        ' In your designer you have Panel_RemoteDevices (not a flow panel).
        ' This function is used by the original project to populate UI.
        ' We'll keep it safe: only clear controls if the container exists.
        If (Panel_RemoteDevices Is Nothing) Then
            Return
        End If

        Panel_RemoteDevices.Controls.Clear()

        Dim arrTrackers As New List(Of ClassTracker)
        SyncLock g_mLockRemoteDevices
            For Each kvp In g_mRemoteDevices
                If (kvp.Value IsNot Nothing) Then
                    arrTrackers.Add(kvp.Value)
                End If
            Next
        End SyncLock

        ' Sort: display name if present, else key (MAC/IP)
        arrTrackers.Sort(Function(a, b)
                             Dim sa As String = If(a.m_DisplayName, "")
                             If (String.IsNullOrEmpty(sa)) Then sa = If(a.m_Name, "")
                             Dim sb As String = If(b.m_DisplayName, "")
                             If (String.IsNullOrEmpty(sb)) Then sb = If(b.m_Name, "")
                             Return String.Compare(sa, sb, StringComparison.OrdinalIgnoreCase)
                         End Function)

        ' Simple vertical stack, no dependency on FlowLayoutPanel
        Dim y As Integer = 0
        For Each t As ClassTracker In arrTrackers
            Dim mItem As New UCRemoteDeviceItem()
            mItem.g_mTracker = t
            mItem.g_mUCRemoteDevices = Me
            mItem.Left = 0
            mItem.Top = y
            mItem.Width = Panel_RemoteDevices.ClientSize.Width
            mItem.Anchor = AnchorStyles.Left Or AnchorStyles.Right Or AnchorStyles.Top
            Panel_RemoteDevices.Controls.Add(mItem)
            y += mItem.Height + 6
        Next
    End Sub

    Public Class ClassTracker
        ' Identity/routing
        Public m_Key As String = ""         ' MAC or legacy IP
        Public m_MacAddress As String = ""  ' normalized MAC if known

        ' UI identity used by UCRemoteDeviceItem
        Public m_Name As String = ""        ' we set this to m_Key to keep UI consistent
        Public m_DisplayName As String = "" ' optional "friendly" name from handshake

        ' Transport
        Public m_EndPoint As IPEndPoint = Nothing
        Public m_ProtocolType As Integer = 0
        Public m_LastPingMs As Integer = 0

        ' Data
        Public m_QuatW As Single = 1.0F
        Public m_QuatX As Single = 0.0F
        Public m_QuatY As Single = 0.0F
        Public m_QuatZ As Single = 0.0F

        Public m_AccelX As Single = 0.0F
        Public m_AccelY As Single = 0.0F
        Public m_AccelZ As Single = 0.0F

        Public m_PosX As Single = 0.0F
        Public m_PosY As Single = 0.0F
        Public m_PosZ As Single = 0.0F

        Public m_WheelAngle As Single = 0.0F

        Public Sub UpdateRotationData(br As BinaryReader)
            Try
                m_QuatW = br.ReadSingle()
                m_QuatX = br.ReadSingle()
                m_QuatY = br.ReadSingle()
                m_QuatZ = br.ReadSingle()
            Catch
            End Try
        End Sub

        Public Sub UpdateRotationWheelData(br As BinaryReader)
            Try
                m_WheelAngle = br.ReadSingle()
            Catch
            End Try
        End Sub

        Public Sub UpdateAccelData(br As BinaryReader)
            Try
                m_AccelX = br.ReadSingle()
                m_AccelY = br.ReadSingle()
                m_AccelZ = br.ReadSingle()
            Catch
            End Try
        End Sub

        Public Sub UpdatePositionData(br As BinaryReader)
            Try
                m_PosX = br.ReadSingle()
                m_PosY = br.ReadSingle()
                m_PosZ = br.ReadSingle()
            Catch
            End Try
        End Sub
    End Class
End Class
