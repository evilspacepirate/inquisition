-----------------------------------------------------------------
--                                                             --
-- NEXUS                                                       --
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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Configuration;         use Configuration;
with Configuration_Panel;
with Control_Panel;
with Device;
with Primatives;            use Primatives;
with Raw_Data_Panel;
with Status_Bar_Panel;
with System_Messages_Panel;

-- XXX DEBUG ONLY XXX --
with Ada.Text_IO; use Ada.Text_IO;
-- XXX DEBUG ONLY XXX --

package body Nexus is

   Datalink             : Datalink_Configuration;
   Protocol             : Protocol_Configuration;
   Adaptable_Parameters : Adaptable_Parameter_Record_Vectors.Vector;

   ----------------
   -- INITIALIZE --
   ----------------

   procedure Initialize is
      Configuration_File_Errors : UnStr.Unbounded_String;
      -- Search for an inquisition configuration file in the current working --
      -- directory and load configuration data from it. If there are more    --
      -- than one .iq files in the local directory or none, an empty string  --
      -- will be returned.                                                   --
      Config_File_Name          : String := Get_Configuration_File_Name;
   begin
      Get_Config_From_File(Config_File_Name,
                           Adaptable_Parameters,
                           Datalink,
                           Protocol,
                           Configuration_File_Errors);

      if Config_File_Name /= "" then
         System_Messages_Panel.Append_Message("Loading configuration from " & Config_File_Name & CRLF);
         System_Messages_Panel.Append_Error(UnStr.To_String(Configuration_File_Errors));
      else
         System_Messages_Panel.Append_Message("No configuration file found in current directory.");
      end if;

      Control_Panel.Set_Adaptable_Parameters(Adaptable_Parameters);
      Status_Bar_Panel.Set_Configuration_Text(Datalink_Configuration_To_String(Datalink));
      Status_Bar_Panel.Set_Protocol_Text(Protocol_Configuration_To_String(Protocol));

      if Datalink.Datalink = None or
         Protocol.Protocol = None
      then
         Null;
      else
         Configuration_Panel.Set_Connect_Button_Enabled(True);
      end if;

      Configuration_Panel.Assign_Event_Callbacks(Connect_Clicked    => Connect_Button_Click_Event'access,
                                                 Disconnect_Clicked => Disconnect_Button_Click_Event'access);
      Control_Panel.Assign_Event_Callbacks(Log_Data_Updated        => Log_Data_Update_Event'access,
                                           Requesting_Data_Updated => Requesting_Data_Update_Event'access,
                                           Set_Value_Clicked       => Set_Value_Click_Event'access);
   end;

   --------------
   -- SHUTDOWN --
   --------------

   procedure Shutdown is
   begin
      Device.Shutdown;
   end Shutdown;

   --------------------------------
   -- CONNECT_BUTTON_CLICK_EVENT --
   --------------------------------

   procedure Connect_Button_Click_Event is
   begin
      -- XXX DEBUG ONLY XXX --
      Put_Line("Connect!");
      -- XXX DEBUG ONLY XXX --
   end;

   -----------------------------------
   -- DISCONNECT_BUTTON_CLICK_EVENT --
   -----------------------------------

   procedure Disconnect_Button_Click_Event is
   begin
      -- XXX DEBUG ONLY XXX --
      Put_Line("Disconnect!");
      -- XXX DEBUG ONLY XXX --
   end;

   ---------------------------
   -- LOG_DATA_UPDATE_EVENT --
   ---------------------------

   procedure Log_Data_Update_Event (Parameter_Index : Natural;
                                    Logging_Data    : Boolean) is
   begin
      -- XXX DEBUG ONLY XXX --
      Put_Line("AP" & Natural'image(Parameter_Index) &
               " Logging Data: " & Boolean'image(Logging_Data) & "!");
      -- XXX DEBUG ONLY XXX --
   end Log_Data_Update_Event;

   ----------------------------------
   -- REQUESTING_DATA_UPDATE_EVENT --
   ----------------------------------

   procedure Requesting_Data_Update_Event (Parameter_Index : Natural;
                                           Requesting_Data : Boolean) is
   begin
      -- XXX DEBUG ONLY XXX --
      Put_Line("AP" & Natural'image(Parameter_Index) &
               " Requesting data: " & Boolean'image(Requesting_Data) & "!");
      -- XXX DEBUG ONLY XXX --
   end;

   ---------------------
   -- SET_VALUE_EVENT --
   ---------------------

   procedure Set_Value_Click_Event (Parameter_Index : Natural;
                                    New_Value       : Unsigned_32) is
   begin
      -- XXX DEBUG ONLY XXX --
      Put_Line("AP " & Natural'image(Parameter_Index) &
               " Set Value: " & Unsigned_32'image(New_Value) & "!");
      -- XXX DEBUG ONLY XXX --
   end Set_Value_Click_Event;

end Nexus;
