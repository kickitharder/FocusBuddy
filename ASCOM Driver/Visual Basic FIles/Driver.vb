'tabs=4
' --------------------------------------------------------------------------------
' TODO fill in this information for your driver, then remove this line!
'
' ASCOM Focuser driver for FocusBuddy
'
' Description:	Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam 
'				nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam 
'				erat, sed diam voluptua. At vero eos et accusam et justo duo 
'				dolores et ea rebum. Stet clita kasd gubergren, no sea takimata 
'				sanctus est Lorem ipsum dolor sit amet.
'
' Implements:	ASCOM Focuser interface version: 1.0
' Author:		(XXX) Your N. Here <your@email.here>
'
' Edit Log:
'
' Date			Who	Vers	Description
' -----------	---	-----	-------------------------------------------------------
' dd-mmm-yyyy	XXX	1.0.0	Initial edit, from Focuser template
' ---------------------------------------------------------------------------------
'
'
' Your driver's ID is ASCOM.FocusBuddy.Focuser
'
' The Guid attribute sets the CLSID for ASCOM.DeviceName.Focuser
' The ClassInterface/None attribute prevents an empty interface called
' _Focuser from being created and used as the [default] interface
'

' This definition is used to select code that's only applicable for one device type
#Const Device = "Focuser"

Imports System
Imports System.Collections
Imports System.Collections.Generic
Imports System.Globalization
Imports System.Runtime.InteropServices
Imports System.Text
Imports System.IO.Ports
Imports ASCOM
Imports ASCOM.Astrometry
Imports ASCOM.Astrometry.AstroUtils
Imports ASCOM.DeviceInterface
Imports ASCOM.Utilities

'<Guid("54b6b9eb-ccca-4e1e-8a33-9f28be664b50")>
<Guid("C5FBACC0-9E36-482B-9190-EE343A006019")>
<ClassInterface(ClassInterfaceType.None)>
Public Class Focuser
    Implements IFocuserV3
    Public Shared version = "V0.211029"

    ' The Guid attribute sets the CLSID for ASCOM.FocusBuddy.Focuser
    ' The ClassInterface/None attribute prevents an empty interface called
    ' _FocusBuddy from being created and used as the [default] interface

    ' TODO Replace the not implemented exceptions with code to implement the function or
    ' throw the appropriate ASCOM exception.
    '
    '
    ' Driver ID and descriptive string that shows in the Chooser
    '
    Friend Shared driverID As String = "ASCOM.FocusBuddy.Focuser"
    Private Shared driverDescription As String = "FocusBuddy"

    Friend Shared comPortProfileName As String = "COM Port" 'Constants used for Profile persistence
    Friend Shared traceStateProfileName As String = "Trace Level"
    Friend Shared absoluteModeProfileName As String = "Absolute Mode"
    Friend Shared powerDetectName As String = "Power Detect"
    Friend Shared resolutionName As String = "Resolution"

    Friend Shared comPortDefault As String = "COM12"
    Friend Shared traceStateDefault As String = "False"
    Friend Shared absoluteModeDefault As String = "False"
    Friend Shared powerDetectDefault As String = "True"
    Friend Shared resolutionDefault As String = "100"

    Friend Shared comPort As String ' Variables to hold the current device configuration
    Friend Shared traceState As Boolean
    Friend Shared absoluteMode As Boolean
    Friend Shared powerDetect As Boolean
    Friend Shared resolution As Integer

    Private connectedState As Boolean ' Private variable to hold the connected state
    Private utilities As Util ' Private variable to hold an ASCOM Utilities object
    Private astroUtilities As AstroUtils ' Private variable to hold an AstroUtils object to provide the Range method
    Private TL As TraceLogger ' Private variable to hold the trace logger object (creates a diagnostic log file with information that you specify)

    '
    ' Constructor - Must be public for COM registration!
    '
    Public Sub New()

        ReadProfile() ' Read device configuration from the ASCOM Profile store
        TL = New TraceLogger("", "FocusBuddy")
        TL.Enabled = traceState
        TL.LogMessage("Focuser", "Starting initialisation")

        connectedState = False ' Initialise connected to false
        utilities = New Util() ' Initialise util object
        astroUtilities = New AstroUtils 'Initialise new astro utilities object

        'TODO: Implement your additional construction here

        TL.LogMessage("Focuser", "Completed initialisation")
    End Sub

    '
    ' PUBLIC COM INTERFACE IFocuserV3 IMPLEMENTATION
    '
#Region "Common properties and methods"
    ''' <summary>
    ''' Displays the Setup Dialog form.
    ''' If the user clicks the OK button to dismiss the form, then
    ''' the new settings are saved, otherwise the old values are reloaded.
    ''' THIS IS THE ONLY PLACE WHERE SHOWING USER INTERFACE IS ALLOWED!
    ''' </summary>
    Public Sub SetupDialog() Implements IFocuserV3.SetupDialog
        ' consider only showing the setup dialog if not connected
        ' or call a different dialog if connected
        Using F As SetupDialogForm = New SetupDialogForm()
            Dim result As System.Windows.Forms.DialogResult = F.ShowDialog()
            If result = DialogResult.OK Then
                WriteProfile() ' Persist device configuration values to the ASCOM Profile store
            End If
        End Using
    End Sub

    Public ReadOnly Property SupportedActions() As ArrayList Implements IFocuserV3.SupportedActions
        Get
            TL.LogMessage("SupportedActions Get", "Returning empty arraylist")
            Return New ArrayList()
        End Get
    End Property

    Public Function Action(ByVal ActionName As String, ByVal ActionParameters As String) As String Implements IFocuserV3.Action
        Throw New ActionNotImplementedException("Action " & ActionName & " is not supported by this driver")
    End Function

    Public Sub CommandBlind(ByVal Command As String, Optional ByVal Raw As Boolean = False) Implements IFocuserV3.CommandBlind
        CheckConnected("CommandBlind")
        ' Call CommandString and return as soon as it finishes
        Me.CommandString(Command, Raw)
        ' or
        Throw New MethodNotImplementedException("CommandBlind")
    End Sub

    Public Function CommandBool(ByVal Command As String, Optional ByVal Raw As Boolean = False) As Boolean _
        Implements IFocuserV3.CommandBool
        CheckConnected("CommandBool")
        Dim ret As String = CommandString(Command, Raw)
        ' TODO decode the return string and return true or false
        ' or
        Throw New MethodNotImplementedException("CommandBool")
    End Function

    Public Function CommandString(ByVal Command As String, Optional ByVal Raw As Boolean = False) As String _
        Implements IFocuserV3.CommandString
        CheckConnected("CommandString")
        ' it's a good idea to put all the low level communication with the device here,
        ' then all communication calls this function
        ' you need something to ensure that only one command is in progress at a time
        Throw New MethodNotImplementedException("CommandString")
    End Function

    Public Property Connected() As Boolean Implements IFocuserV3.Connected
        Get
            TL.LogMessage("Connected Get", IsConnected.ToString())
            Return IsConnected
        End Get
        Set(value As Boolean)
            TL.LogMessage("Connected Set", value.ToString())
            ' If value = IsConnected Then
            ' Return
            ' End If

            If value Then
                ' TODO connect to the device
                connectedState = True
                TL.LogMessage("Connected Set", "Connecting to port " + comPort)
                Dim respStr As String = SerialCmd("~")
                If respStr = "Error" Then
                    connectedState = False
                    MsgBox("FocusBuddy not responding", vbCritical, "FocusBuddy")
                    'Throw New NotConnectedException("FocusBuddy is not responding")
                End If
                If powerDetect AndAlso respStr <> "1" Then
                    connectedState = False
                    MsgBox("No focuser power detected", vbCritical, "FocusBuddy")
                    'Throw New NotConnectedException("No focuser power detected")
                End If
                TL.LogMessage("Connected Set", "Disconnecting from port " + comPort)
                ' TODO disconnect from the device
            End If
        End Set
    End Property

    Public ReadOnly Property Description As String Implements IFocuserV3.Description
        Get
            ' this pattern seems to be needed to allow a public property to return a private field
            Dim d As String = driverDescription
            TL.LogMessage("Description Get", d)
            Return d
        End Get
    End Property

    Public ReadOnly Property DriverInfo As String Implements IFocuserV3.DriverInfo
        Get
            Dim m_version As Version = System.Reflection.Assembly.GetExecutingAssembly().GetName().Version
            ' TODO customise this driver description
            Dim s_driverInfo As String = version + "." + m_version.Major.ToString() + "." + m_version.Minor.ToString()
            TL.LogMessage("DriverInfo Get", s_driverInfo)
            Return s_driverInfo
        End Get
    End Property

    Public ReadOnly Property DriverVersion() As String Implements IFocuserV3.DriverVersion
        Get
            ' Get our own assembly and report its version number
            TL.LogMessage("DriverVersion Get", Reflection.Assembly.GetExecutingAssembly.GetName.Version.ToString(2))
            Return Reflection.Assembly.GetExecutingAssembly.GetName.Version.ToString(2)
        End Get
    End Property

    Public ReadOnly Property InterfaceVersion() As Short Implements IFocuserV3.InterfaceVersion
        Get
            TL.LogMessage("InterfaceVersion Get", "3")
            Return 3
        End Get
    End Property

    Public ReadOnly Property Name As String Implements IFocuserV3.Name
        Get
            Dim s_name As String = "FocusBuddy"
            TL.LogMessage("Name Get", s_name)
            Return s_name
        End Get
    End Property

    Public Sub Dispose() Implements IFocuserV3.Dispose
        ' Clean up the trace logger and util objects
        TL.Enabled = False
        TL.Dispose()
        TL = Nothing
        utilities.Dispose()
        utilities = Nothing
        astroUtilities.Dispose()
        astroUtilities = Nothing
    End Sub

#End Region

#Region "IFocuser Implementation"

    Private focuserPosition As Integer = 500000 ' Class level variable to hold the current focuser position
    Private Const focuserSteps As Integer = 1000000

    Public ReadOnly Property Absolute() As Boolean Implements IFocuserV3.Absolute
        Get
            TL.LogMessage("Absolute Get", True.ToString())
            Return absoluteMode
        End Get
    End Property

    Public Sub Halt() Implements IFocuserV3.Halt
        TL.LogMessage("Halt", "Not implemented")
        ' Throw New ASCOM.MethodNotImplementedException("Halt")
        SerialCmd("Q")
    End Sub

    Public ReadOnly Property IsMoving() As Boolean Implements IFocuserV3.IsMoving
        Get
            TL.LogMessage("IsMoving Get", False.ToString())
            Dim movingStr As String = SerialCmd("M")
            If movingStr = "Error" Then
                MsgBox("FocusBuddy not responding", vbCritical, "FocusBuddy")
                movingStr = "0"
            End If
            Return movingStr = "1" ' This focuser always moves instantaneously so no need for IsMoving ever to be True
        End Get
    End Property

    Public Property Link() As Boolean Implements IFocuserV3.Link
        Get
            TL.LogMessage("Link Get", Me.Connected.ToString())
            Return Me.Connected ' Direct function to the connected method, the Link method is just here for backwards compatibility
        End Get
        Set(value As Boolean)
            TL.LogMessage("Link Set", value.ToString())
            Me.Connected = value ' Direct function to the connected method, the Link method is just here for backwards compatibility
        End Set
    End Property

    Public ReadOnly Property MaxIncrement() As Integer Implements IFocuserV3.MaxIncrement
        Get
            TL.LogMessage("MaxIncrement Get", focuserSteps.ToString())
            Return focuserSteps ' Maximum change in one move
        End Get
    End Property

    Public ReadOnly Property MaxStep() As Integer Implements IFocuserV3.MaxStep
        Get
            TL.LogMessage("MaxStep Get", focuserSteps.ToString())
            Dim maxStr As String
            maxStr = SerialCmd("E9")    'Get upper limit
            If maxStr = "Error" Then
                'Throw New NotConnectedException("FocusBuddy is not responding")
                MsgBox("FocusBuddy not responding", vbCritical, "FocusBuddy")
                maxStr = "0"
            End If
            Return Int(Val(maxStr) / resolution)
            ' Return focuserSteps ' Maximum extent of the focuser, so position range is 0 to 10,000
        End Get
    End Property

    Public Sub Move(Position As Integer) Implements IFocuserV3.Move
        TL.LogMessage("Move", Position.ToString())
        If powerDetect AndAlso SerialCmd("~") = "0" Then
            MsgBox("No focuser power detected", vbCritical, "FocusBuddy")
            Exit Sub
        End If
        If Not absoluteMode Then
            Dim focPos As String = SerialCmd("P")
            If focPos = "Error" Then
                'Throw New NotConnectedException("FocusBuddy is not responding")
                MsgBox("FocusBuddy not responding", vbCritical, "FocusBuddy")
                Exit Sub
            End If
            Position += Val(Int(focPos / resolution))
        End If
        Position *= resolution ' Native resolution is milliseconds (1000ths of a second)
        If Position < 1 Then Position = 1
        If SerialCmd("g" + Trim(Str(Position))) = "Error" Then Throw New NotConnectedException("FocusBuddy is not responding")
    End Sub

    Public ReadOnly Property Position() As Integer Implements IFocuserV3.Position
        Get
            Dim posStr As String = SerialCmd("P")
            'If posStr = "Error" Then Throw New NotConnectedException("FocusBuddy is not responding")
            If posStr = "Error" Then MsgBox("FocusBuddy not responding", vbCritical, "FocusBuddy")
            focuserPosition = Int(Val(posStr) / resolution)
            Return focuserPosition ' Return the focuser position
        End Get
    End Property

    Public ReadOnly Property StepSize() As Double Implements IFocuserV3.StepSize
        Get
            TL.LogMessage("StepSize Get", "Not implemented")
            Throw New ASCOM.PropertyNotImplementedException("StepSize", False)
        End Get
    End Property

    Public Property TempComp() As Boolean Implements IFocuserV3.TempComp
        Get
            TL.LogMessage("TempComp Get", False.ToString())
            Return False
        End Get
        Set(value As Boolean)
            TL.LogMessage("TempComp Set", "Not implemented")
            Throw New ASCOM.PropertyNotImplementedException("TempComp", True)
        End Set
    End Property

    Public ReadOnly Property TempCompAvailable() As Boolean Implements IFocuserV3.TempCompAvailable
        Get
            TL.LogMessage("TempCompAvailable Get", False.ToString())
            Return False ' Temperature compensation is not available in this driver
        End Get
    End Property

    Public ReadOnly Property Temperature() As Double Implements IFocuserV3.Temperature
        Get
            TL.LogMessage("Temperature Get", "Not implemented")
            Throw New ASCOM.PropertyNotImplementedException("Temperature", False)
        End Get
    End Property

#End Region

#Region "Private properties and methods"
    ' here are some useful properties and methods that can be used as required
    ' to help with

#Region "ASCOM Registration"

    Private Shared Sub RegUnregASCOM(ByVal bRegister As Boolean)

        Using P As New Profile() With {.DeviceType = "Focuser"}
            If bRegister Then
                P.Register(driverID, driverDescription)
            Else
                P.Unregister(driverID)
            End If
        End Using

    End Sub

    <ComRegisterFunction()>
    Public Shared Sub RegisterASCOM(ByVal T As Type)

        RegUnregASCOM(True)

    End Sub

    <ComUnregisterFunction()>
    Public Shared Sub UnregisterASCOM(ByVal T As Type)

        RegUnregASCOM(False)

    End Sub

#End Region

    ''' <summary>
    ''' Returns true if there is a valid connection to the driver hardware
    ''' </summary>
    Private ReadOnly Property IsConnected As Boolean
        Get
            ' TODO check that the driver hardware connection exists and is connected to the hardware
            ' If connectedState Then connectedState = (SerialCmd("A") = "A")
            Return connectedState
        End Get
    End Property

    ''' <summary>
    ''' Use this function to throw an exception if we aren't connected to the hardware
    ''' </summary>
    ''' <param name="message"></param>
    Private Sub CheckConnected(ByVal message As String)
        If Not IsConnected Then
            Throw New NotConnectedException(message)
        End If
    End Sub

    ''' <summary>
    ''' Read the device configuration from the ASCOM Profile store
    ''' </summary>
    Friend Sub ReadProfile()
        Using driverProfile As New Profile()
            driverProfile.DeviceType = "Focuser"
            traceState = Convert.ToBoolean(driverProfile.GetValue(driverID, traceStateProfileName, String.Empty, traceStateDefault))
            comPort = driverProfile.GetValue(driverID, comPortProfileName, String.Empty, comPortDefault)
            absoluteMode = Convert.ToBoolean(driverProfile.GetValue(driverID, absoluteModeProfileName, String.Empty, absoluteModeDefault))
            powerDetect = Convert.ToBoolean(driverProfile.GetValue(driverID, powerDetectName, String.Empty, powerDetectDefault))
            resolution = Convert.ToInt32(driverProfile.GetValue(driverID, resolutionName, String.Empty, resolutionDefault))
        End Using
    End Sub

    ''' <summary>
    ''' Write the device configuration to the  ASCOM  Profile store
    ''' </summary>
    Friend Sub WriteProfile()
        Using driverProfile As New Profile()
            driverProfile.DeviceType = "Focuser"
            driverProfile.WriteValue(driverID, traceStateProfileName, traceState.ToString())
            driverProfile.WriteValue(driverID, comPortProfileName, comPort.ToString())
            driverProfile.WriteValue(driverID, traceStateProfileName, traceState.ToString())
            driverProfile.WriteValue(driverID, absoluteModeProfileName, absoluteMode.ToString)
            driverProfile.WriteValue(driverID, powerDetectName, powerDetect.ToString)
            driverProfile.WriteValue(driverID, resolutionName, resolution.ToString)
        End Using

    End Sub

    Private Function SerialCmd(cmdStr As String) As String
        Dim retStr As String = ""
        Dim objSerial As New SerialPort
        cmdStr += vbCrLf

        Try
            With objSerial
                .PortName = comPort
                .BaudRate = 9600
                .ReadTimeout = 1000
                .WriteTimeout = 1000
                .Open()
                .Write(cmdStr)
            End With
            retStr = objSerial.ReadTo(vbCrLf)
        Catch ex As Exception
            retStr = "Error"
        End Try

        Try
            objSerial.Close()
        Catch ex As Exception
        End Try

        Return retStr

    End Function


#End Region

End Class
