-----------------------------------------------------------------
--                                                             --
-- NEXUS Specification                                         --
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
with Interfaces;    use Interfaces;
with Control_Panel; use Control_Panel;

package Nexus is

   procedure Initialize(Panel : Control_Panel_Widget);
   procedure Shutdown;
   function Service return Boolean;
   -- Must be called from the main thread for GTK to behave --

   private

   procedure Connect_Event;
   procedure Disconnect_Event;

   procedure Log_Data_Update_Event           (Parameter_Index : Natural;
                                              Logging_Data    : Boolean);

   procedure Requesting_Data_Update_Event    (Parameter_Index : Natural;
                                              Requesting_Data : Boolean);

   procedure Set_Value_Click_Event           (Parameter_Index : Natural;
                                              New_Value       : Unsigned_32);

   procedure Double_Click_On_Parameter_Event (Parameter_Index : Natural);

   procedure Request_Period_Update_Event     (Parameter_Index : Natural;
                                              Period          : Duration);

end Nexus;
