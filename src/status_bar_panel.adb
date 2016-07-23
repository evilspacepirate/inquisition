-----------------------------------------------------------------
--                                                             --
-- STATUS_BAR_PANEL                                            --
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
with Gtk.Box;   use Gtk.Box;
with Gtk.Label; use Gtk.Label;

package body Status_Bar_Panel is

   Connection_Config_Label : Gtk_Label;
   Protocol_Label          : Gtk_Label;

   ------------
   -- CREATE --
   ------------

   procedure Create is
   begin
      Gtk_New_HBox(Box);
      Gtk_New(Connection_Config_Label);
      Gtk_New(Protocol_Label);
      Pack_Start(Box, Connection_Config_Label, False, False, Pad_Pixels);
      Pack_End(Box, Protocol_Label, False, False, Pad_Pixels);
   end Create;

   ----------------------------
   -- SET_CONFIGURATION_TEXT --
   ----------------------------

   procedure Set_Configuration_Text(Text : String) is
   begin
      Set_Label(Connection_Config_Label, Text);
   end Set_Configuration_Text;

   -----------------------
   -- SET_PROTOCOL_TEXT --
   -----------------------

   procedure Set_Protocol_Text(Text : String) is
   begin
      Set_Label(Protocol_Label, Text);
   end Set_Protocol_Text;

end Status_Bar_Panel;
