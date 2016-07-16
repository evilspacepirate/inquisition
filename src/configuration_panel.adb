-----------------------------------------------------------------
--                                                             --
-- CONFIGURAITON_PANEL                                         --
--                                                             --
-- Copyright (c) 2016, John Leimon                             --
--                                                             --
-- Permission to use, copy, modify, and/or distribute          --
-- this software for any purpose with or without fee           --
-- is hereby granted, provided that the above copyright        --
-- notice and this permission notice appear in all copies.     --
--                                                             --
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR             --
-- DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE       --
-- INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY         --
-- AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE         --
-- FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL         --
-- DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS       --
-- OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF            --
-- CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING      --
-- OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF      --
-- THIS SOFTWARE.                                              --
-----------------------------------------------------------------
with Gtk.Box;          use Gtk.Box;
with Gtk.Button;       use Gtk.Button;
with Gtk.Combo_Box;    use Gtk.Combo_Box;
with Gtk.GEntry;       use Gtk.GEntry;
with Gtk.Label;        use Gtk.Label;
with Gtk.Radio_Button; use Gtk.Radio_Button;
with Gtk.Separator;    use Gtk.Separator;
with Util;             use Util;

package body Configuration_Panel is

   -- Pixel spacing magic numbers --
   Horizontal_Spaceage       : constant := 14;
   Vertical_Spaceage         : constant :=  2;
   Device_Name_Spaceage      : constant := 13;
   Baud_Rate_Spaceage        : constant := 98;
   TCP_IPv4_Address_Spaceage : constant := 14;

   Operation_Box             : Gtk_HBox;
   Serial_Config_Box_Top     : Gtk_HBox;
   Serial_Config_Box_Bottom  : Gtk_HBox;
   USBHID_Config_Box         : Gtk_HBox;
   TCPv4_Config_Box          : Gtk_HBox;
   Data_Link_Config_Label    : Gtk_Label;
   Device_Name_Label         : Gtk_Label;
   Baud_Rate_Label           : Gtk_Label;
   Stop_Bits_Label           : Gtk_Label;
   Parity_Label              : Gtk_Label;
   VID_Label                 : Gtk_Label;
   PID_Label                 : Gtk_Label;
   TCP_IPv4_Address_Label    : Gtk_Label;
   TCP_Port_Label            : Gtk_Label;
   Top_Separator             : Gtk_Separator;
   Bottom_Separator          : Gtk_Separator;
   Serial_Radio_Button       : Gtk_Radio_Button;
   USBHID_Radio_Button       : Gtk_Radio_Button;
   TCPv4_Radio_Button        : Gtk_Radio_Button;
   VID_Text_Entry            : Gtk_Entry;
   PID_Text_Entry            : Gtk_Entry;
   Device_Name_Text_Entry    : Gtk_Entry;
   TCP_IPv4_Text_Entry       : Gtk_Entry;
   TCP_Port_Text_Entry       : Gtk_Entry;
   Baud_Rate_Combo_Box       : Gtk_Combo_Box;
   Stop_Bits_Combo_Box       : Gtk_Combo_Box;
   Parity_Combo_Box          : Gtk_Combo_Box;
   Connect_Button            : Gtk_Button;
   Disconnect_Button         : Gtk_Button;

   ------------
   -- CREATE --
   ------------

   procedure Create is
   begin
      Gtk_New_VBox(Box);

      Gtk_New_HBox(Operation_Box);
      Gtk_New_HBox(Serial_Config_Box_Top);
      Gtk_New_HBox(Serial_Config_Box_Bottom);
      Gtk_New_HBox(USBHID_Config_Box);
      Gtk_New_HBox(TCPv4_Config_Box);

      Gtk_New_HSeparator(Top_Separator);
      Gtk_New_HSeparator(Bottom_Separator);

      Gtk_New(Baud_Rate_Combo_Box);
      Gtk_New(Stop_Bits_Combo_Box);
      Gtk_New(Parity_Combo_Box);

      Gtk_New(Connect_Button, "Connect");
      Gtk_New(Disconnect_Button, "Disconnect");

      Gtk_New(Data_Link_Config_Label, "Data Link Configuration");
      Gtk_New(VID_Label,              "VID");
      Gtk_New(PID_Label,              "PID");
      Gtk_New(Device_Name_Label,      "Device Name");
      Gtk_New(Baud_Rate_Label,        "Baud Rate");
      Gtk_New(Stop_Bits_Label,        "Stop Bits");
      Gtk_New(Parity_Label,           "Parity");
      Gtk_New(TCP_IPv4_Address_Label, "IPv4");
      Gtk_New(TCP_Port_Label,         "Port");

      Gtk_New(Serial_Radio_Button, Null,                "Serial");
      Gtk_New(USBHID_Radio_Button, Serial_Radio_Button, "USBHID");
      Gtk_New(TCPv4_Radio_Button,  Serial_Radio_Button, "TCP   ");
      Gtk_New(VID_Text_Entry);
      Gtk_New(PID_Text_Entry);
      Gtk_New(Device_Name_Text_Entry);
      Gtk_New(TCP_IPv4_Text_Entry);
      Gtk_New(TCP_Port_Text_Entry);

      Pack_Start(Serial_Config_Box_Top,    Serial_Radio_Button,    False, False, Horizontal_Spaceage);
      Pack_Start(Serial_Config_Box_Top,    Device_Name_Label,      False, False, Device_Name_Spaceage);
      Pack_Start(Serial_Config_Box_Top,    Device_Name_Text_Entry, True,  True,  Horizontal_Spaceage);

      Pack_Start(Serial_Config_Box_Bottom, Baud_Rate_Label,        False, False, Baud_Rate_Spaceage);
      Pack_Start(Serial_Config_Box_Bottom, Baud_Rate_Combo_Box,    False, False);
      Pack_Start(Serial_Config_Box_Bottom, Stop_Bits_Label,        False, False, Horizontal_Spaceage);
      Pack_Start(Serial_Config_Box_Bottom, Stop_Bits_Combo_Box,    False, False);
      Pack_Start(Serial_Config_Box_Bottom, Parity_Label,           False, False, Horizontal_Spaceage);
      Pack_Start(Serial_Config_Box_Bottom, Parity_Combo_Box,       False, False);

      Pack_Start(USBHID_Config_Box,        USBHID_Radio_Button,    False, False, Horizontal_Spaceage);
      Pack_Start(USBHID_Config_Box,        VID_Label,              False, False);
      Pack_Start(USBHID_Config_Box,        VID_Text_Entry,         True,  True,  Horizontal_Spaceage);
      Pack_Start(USBHID_Config_Box,        PID_Label,              False, False);
      Pack_Start(USBHID_Config_Box,        PID_Text_Entry,         True,  True,  Horizontal_Spaceage);

      Pack_Start(TCPv4_Config_Box,         TCPv4_Radio_Button,     False, False, Horizontal_Spaceage);
      Pack_Start(TCPv4_Config_Box,         TCP_IPv4_Address_Label, False, False, TCP_IPv4_Address_Spaceage);
      Pack_Start(TCPv4_Config_Box,         TCP_IPv4_Text_Entry,    True , True);
      Pack_Start(TCPv4_Config_Box,         TCP_Port_Label,         False, False, Horizontal_Spaceage);
      Pack_Start(TCPv4_Config_Box,         TCP_Port_Text_Entry,    True,  True,  Horizontal_Spaceage);

      Add(Operation_Box, Connect_Button);
      Add(Operation_Box, Disconnect_Button);

      Pack_Start(Box, Data_Link_Config_Label,   False, False, Vertical_Spaceage);
      Pack_Start(Box, Top_Separator,            False, False, Vertical_Spaceage);
      Pack_Start(Box, Serial_Config_Box_Top,    False, False, Vertical_Spaceage);
      Pack_Start(Box, Serial_Config_Box_Bottom, False, False, Vertical_Spaceage);
      Pack_Start(Box, USBHID_Config_Box,        False, False, Vertical_Spaceage);
      Pack_Start(Box, TCPv4_Config_Box,         False, False, Vertical_Spaceage);
      Pack_Start(Box, Bottom_Separator,         False, False, Vertical_Spaceage);
      Pack_Start(Box, Operation_Box,            False, False, Vertical_Spaceage);

      -- Disable configuration panel widgets until they do something useful --
      Set_Sensitive(Serial_Radio_Button,    False);
      Set_Sensitive(USBHID_Radio_Button,    False);
      Set_Sensitive(TCPv4_Radio_Button,     False);
      Set_Sensitive(VID_Text_Entry,         False);
      Set_Sensitive(PID_Text_Entry,         False);
      Set_Sensitive(Device_Name_Text_Entry, False);
      Set_Sensitive(TCP_IPv4_Text_Entry,    False);
      Set_Sensitive(TCP_Port_Text_Entry,    False);
      Set_Sensitive(Baud_Rate_Combo_Box,    False);
      Set_Sensitive(Stop_Bits_Combo_Box,    False);
      Set_Sensitive(Parity_Combo_Box,       False);
      Set_Sensitive(Connect_Button,         False);
      Set_Sensitive(Disconnect_Button,      False);
      Set_Sensitive(Device_Name_Label,      False);
      Set_Sensitive(Baud_Rate_Label,        False);
      Set_Sensitive(Stop_Bits_Label,        False);
      Set_Sensitive(Parity_Label,           False);
      Set_Sensitive(VID_Label,              False);
      Set_Sensitive(PID_Label,              False);
      Set_Sensitive(TCP_IPv4_Address_Label, False);
      Set_Sensitive(TCP_Port_Label,         False);

      -- TODO: Make configuration panel do something useful --
   end Create;

end Configuration_Panel;
