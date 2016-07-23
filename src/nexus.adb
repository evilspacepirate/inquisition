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
with Control_Panel;
with Device;
with Primatives;            use Primatives;
with Raw_Data_Panel;
with Status_Bar_Panel;
with System_Messages_Panel;

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
   end;

   --------------
   -- SHUTDOWN --
   --------------

   procedure Shutdown is
   begin
      Device.Shutdown;
   end Shutdown;

end Nexus;
