-----------------------------------------------------------------
--                                                             --
-- RAW_DATA_PANEL                                              --
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
with Glib;                use Glib;
with Glib.Properties;
with Gtk.Box;             use Gtk.Box;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Text_Buffer;     use Gtk.Text_Buffer;
with Gtk.Text_Iter;       use Gtk.Text_Iter;
with Gtk.Text_Tag;        use Gtk.Text_Tag;
with Gtk.Text_View;       use Gtk.Text_View;

package body Raw_Data_Panel is
   
   Data_Sent_Color           : constant String := "#254A64";
   Data_Received_Color       : constant String := "#484D53";
   
   Received_Data_Text_Tag    : Gtk_Text_Tag;
   Sent_Data_Text_Tag        : Gtk_Text_Tag;

   Control_Box               : Gtk_HBox;
   Raw_Data_Window           : Gtk_Scrolled_Window;
   Text_View                 : Gtk_Text_View;
   Buffer                    : Gtk_Text_Buffer;

   ------------
   -- CREATE --
   ------------

   procedure Create is
   begin
      Gtk_New_VBox(Box);
      Gtk_New_HBox(Control_Box);
      Gtk_New(Raw_Data_Window);
      Gtk_New(Buffer);
      Gtk_New(Text_View, Buffer);
      
      Received_Data_Text_Tag := Create_Tag(Buffer, "Data_Received");
      Sent_Data_Text_Tag     := Create_Tag(Buffer, "Data_Sent");

      Glib.Properties.Set_Property(Received_Data_Text_Tag, Foreground_Property, Data_Received_Color);
      Glib.Properties.Set_Property(Sent_Data_Text_Tag, Foreground_Property, Data_Sent_Color);
      
      Set_Policy(Raw_Data_Window, Policy_Automatic, Policy_Automatic);
      Set_Editable(Text_View, False);

      Add(Raw_Data_Window, Text_View);

      Pack_Start(Box, Control_Box,     False, False);
      Pack_End(Box,   Raw_Data_Window, True,  True);
      
   end Create;

end Raw_Data_Panel;
