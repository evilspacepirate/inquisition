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
with NVP_Protocol;          use NVP_Protocol;
with Raw_Data_Panel;
with Status_Bar_Panel;
with System;                use System;
with System_Messages_Panel;

-- XXX DEBUG ONLY XXX --
with Ada.Text_IO; use Ada.Text_IO;
-- XXX DEBUG ONLY XXX --

package body Nexus is

   Datalink             : Datalink_Configuration;
   Protocol             : Protocol_Configuration;
   Adaptable_Parameters : Adaptable_Parameter_Record_Vectors.Vector;

   IO_Error             : Boolean := False;

   Values_Received      : Values_Buffer;

   task type Data_Requestor_Task is
      entry Set_Request_Period(Period : in Duration);
      entry Set_Requests(New_Requests : in Unsigned_16_Vectors.Vector);
      entry Set_Requesting_Data(Enabled : in Boolean);
   end Data_Requestor_Task;

   task type Data_Interpreter_Task is
      entry Start;
      entry Stop;
   end Data_Interpreter_Task;

   -------------------------
   -- DATA_REQUESTOR TASK --
   -------------------------

   task body Data_Requestor_Task is
      Request_Period  : Duration;
      Requests        : Requests_Buffer;
      Requesting_Data : Boolean := False;

      ------------------
      -- REQUEST_DATA --
      ------------------

      procedure Request_Data is
      begin
         case Protocol.Name is
            when NVP =>
               declare
                  Request_Value_Packet : Unsigned_8_Array := Create_Request_Value_Packet(Requests.Get_Requests);
               begin
                  Device.Send_Data(Request_Value_Packet);
               end;
            when NVP_With_Routing =>
               declare
                  Request_Value_Packet : Unsigned_8_Array := Create_Request_Value_Packet(Requests.Get_Requests,
                                                                                         Address_To_Unsigned_8(Protocol.Source),
                                                                                         Address_To_Unsigned_8(Protocol.Destination));
               begin
                  Device.Send_Data(Request_Value_Packet);
               end;
            when IQ =>
               -- TODO Not supported yet --
               abort Data_Requestor_Task;
            when None =>
               -- Invalid protocol --
               abort Data_Requestor_Task;
         end case;
      end Request_Data;

   begin
      loop
         if Requesting_Data then
            Request_Data;
         end if;
         select
            accept Set_Request_Period(Period : in Duration) do
               Request_Period := Period;
            end;
         or
            accept Set_Requests(New_Requests : in Unsigned_16_Vectors.Vector) do
               Requests.Set(New_Requests);
            end;
         or
            accept Set_Requesting_Data(Enabled : Boolean) do
               Requesting_Data := Enabled;
            end;
         or
            delay Request_Period;
         end select;
      end loop;
   exception
      when others =>
         if Device.Connected then
            -- Send signal to main thread to report an alert --
            IO_Error := True;
         end if;
         abort Data_Requestor_Task;
   end Data_Requestor_Task;

   ---------------------------
   -- DATA_INTERPRETER_TASK --
   ---------------------------

   task body Data_Interpreter_Task is
      type Task_State is (Started, Stopped);
      State : Task_State := Stopped;

   begin
      loop
         select
            accept Start do
               State := Started;
            end;
         or
            accept Stop do
               State := Stopped;
            end;
         end select;

         if State = Started then
            declare
               Incoming_Data : Unsigned_8_Array := Device.Get_Data;
            begin
               if Incoming_Data'Length /= 0 then
                  case Protocol.Name is
                     when NVP_With_Routing =>
                        for Index in Natural range Incoming_Data'First .. Incoming_Data'Last loop
                           declare
                              Message : Unsigned_8_Array := Interpret_Data_With_Routing(Incoming_Data(Index));
                           begin
                              if Message'Length /= 0 then
                                 -- Valid message found --
                                 -- TODO --
                                 Null;
                              end if;
                           end;
                        end loop;
                     when NVP =>
                        for Index in Natural range Incoming_Data'First .. Incoming_Data'Last loop
                           declare
                              Message : Unsigned_8_Array := Interpret_Data(Incoming_Data(Index));
                           begin
                              if Message'Length /= 0 then
                                 -- Valid message found --
                                 -- TODO --
                                 Null;
                              end if;
                           end;
                        end loop;
                     when IQ =>
                        -- TODO Not supported yet --
                        abort Data_Interpreter_Task;
                     when None =>
                        -- Invalid protocol --
                        abort Data_Interpreter_Task;
                  end case;
               end if;
            end;
         else
            -- Not started. Take a nap. --
            delay 0.1;
         end if;
      end loop;
   exception
      when others =>
         if Device.Connected then
            -- Send signal to main thread to report an alert --
            IO_Error := True;
         end if;
         abort Data_Interpreter_Task;
   end Data_Interpreter_Task;

   ----------------
   -- INITIALIZE --
   ----------------

   procedure Initialize is
      Configuration_File_Errors : UnStr.Unbounded_String;
      -- Search for an inquisition configuration file in the current working --
      -- directory and load configuration data from it. If there are more    --
      -- than one .iq files in the local directory or none, an empty string  --
      -- will be returned.                                                   --
      Config_File_Name : String := Get_Configuration_File_Name;
   begin
      Get_Config_From_File(Config_File_Name,
                           Adaptable_Parameters,
                           Datalink,
                           Protocol,
                           Configuration_File_Errors);

      if Config_File_Name /= "" then
         System_Messages_Panel.Append_Message("Loading configuration from " & Config_File_Name & CRLF);
         if Configuration_File_Errors /= "" then
            System_Messages_Panel.Append_Error(UnStr.To_String(Configuration_File_Errors));
         end if;
      else
         System_Messages_Panel.Append_Message("No configuration file found in current directory.");
      end if;

      Control_Panel.Set_Adaptable_Parameters(Adaptable_Parameters);
      Status_Bar_Panel.Set_Configuration_Text("Data Link :  " & Datalink_Configuration_To_String(Datalink));
      Status_Bar_Panel.Set_Protocol_Text(Protocol_Configuration_To_String(Protocol));
      Configuration_Panel.Set_Connect_Button_Enabled(True);
      Configuration_Panel.Set_Disconnect_Button_Enabled(False);

      if Datalink.Datalink = None or
         Protocol.Name = None
      then
         Null;
      else
         Configuration_Panel.Set_Connect_Button_Enabled(True);
      end if;

      Configuration_Panel.Assign_Event_Callbacks(Connect_Clicked    => Connect_Event'access,
                                                 Disconnect_Clicked => Disconnect_Event'access);
      Control_Panel.Assign_Event_Callbacks(Log_Data_Updated         => Log_Data_Update_Event'access,
                                           Requesting_Data_Updated  => Requesting_Data_Update_Event'access,
                                           Set_Value_Clicked        => Set_Value_Click_Event'access,
                                           Parameter_Double_Clicked => Double_Click_On_Parameter_Event'access,
                                           Request_Period_Updated   => Request_Period_Update_Event'access);
   end;

   --------------
   -- SHUTDOWN --
   --------------

   procedure Shutdown is
   begin
      Device.Shutdown;
   end Shutdown;

   -------------------
   -- CONNECT_EVENT --
   -------------------

   procedure Connect_Event is
   begin
      System_Messages_Panel.Append_Message("Connect: " & Datalink_Configuration_To_String(Datalink) & CRLF);

      if Protocol.Name = None then
         System_Messages_Panel.Append_Error("Connect Error: No protocol selected for data link" & CRLF);
         return;
      end if;

      if Device.Connected = False then
         begin
            Device.Connect(Datalink);
         exception
            when Device.Invalid_Datalink_Config =>
               System_Messages_Panel.Append_Error("Connect Error: No data link configured" & CRLF);
            when Device.Error_Opening_Device =>
               System_Messages_Panel.Append_Error("Connect Error: Could not open: " & Datalink_Configuration_To_String(Datalink) & CRLF);
         end;
      end if;

      if Device.Connected = True then
         Configuration_Panel.Set_Connect_Button_Enabled(False);
         Configuration_Panel.Set_Disconnect_Button_Enabled(True);
      end if;
   end;

   ----------------------
   -- DISCONNECT_EVENT --
   ----------------------

   procedure Disconnect_Event is
   begin
      if Device.Connected = True then
         Device.Disconnect;
         System_Messages_Panel.Append_Message("Disconnected: " & Datalink_Configuration_To_String(Datalink) & CRLF);
      end if;

      if Device.Connected = False then
         Configuration_Panel.Set_Connect_Button_Enabled(True);
         Configuration_Panel.Set_Disconnect_Button_Enabled(False);
      end if;
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

   -------------------------------------
   -- DOUBLE_CLICK_ON_PARAMETER_EVENT --
   -------------------------------------

   procedure Double_Click_On_Parameter_Event (Parameter_Index : Natural) is
   begin
      -- XXX DEBUG ONLY XXX --
      Put_Line("AP " & Natural'image(Parameter_Index) & " Double-click: ");
      -- XXX DEBUG ONLY XXX --
   end Double_Click_On_Parameter_Event;

   ---------------------------------
   -- REQUEST_PERIOD_UPDATE_EVENT --
   ---------------------------------

   procedure Request_Period_Update_Event (Parameter_Index : Natural;
                                          Period          : Duration) is
   begin
      -- XXX DEBUG ONLY XXX --
      Put_Line("AP " & Natural'image(Parameter_Index) &
               " New request period: " & Duration'Image(Period));
      -- XXX DEBUG ONLY XXX --
   end Request_Period_Update_Event;

   -------------
   -- Service --
   -------------

   function Service return Boolean is
   begin
      if IO_Error then
         System_Messages_Panel.Append_Error("IO Error" & CRLF);
         Disconnect_Event;
      end if;
      return True;
   end Service;

end Nexus;
