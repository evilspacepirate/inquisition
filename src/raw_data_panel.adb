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
with Ada.Calendar;          use Ada.Calendar;
with Configuration;         use Configuration;
with Glib;                  use Glib;
with Glib.Properties;
with GNAT.Calendar.Time_IO;
with Gtk.Box;               use Gtk.Box;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Scrolled_Window;   use Gtk.Scrolled_Window;
with Gtk.Text_Buffer;       use Gtk.Text_Buffer;
with Gtk.Text_Iter;         use Gtk.Text_Iter;
with Gtk.Text_Tag;          use Gtk.Text_Tag;
with Gtk.Text_View;         use Gtk.Text_View;
with Palette;
with Primatives;            use Primatives;
with Util;                  use Util;

with ada.text_io; use ada.text_io;

package body Raw_Data_Panel is

   package Time_IO renames GNAT.Calendar.Time_IO;

   Received_Data_Text_Tag    : Gtk_Text_Tag;
   Sent_Data_Text_Tag        : Gtk_Text_Tag;
   Time_Text_Tag             : Gtk_Text_Tag;

   Control_Box               : Gtk_HBox;
   Window                    : Gtk_Scrolled_Window;
   Text_View                 : Gtk_Text_View;
   Buffer                    : Gtk_Text_Buffer;

   ------------
   -- CREATE --
   ------------

   procedure Create is
   begin
      Gtk_New_VBox(Box);
      Gtk_New_HBox(Control_Box);
      Gtk_New(Window);
      Gtk_New(Buffer);
      Gtk_New(Text_View, Buffer);

      Set_Policy(Window, Policy_Automatic, Policy_Automatic);
      Set_Editable(Text_View, False);

      Received_Data_Text_Tag := Create_Tag(Buffer, "Received");
      Glib.Properties.Set_Property(Received_Data_Text_Tag, Foreground_Property, Palette.Purple);
      Glib.Properties.Set_Property(Received_Data_Text_Tag, Family_Property, "Monospace");

      Sent_Data_Text_Tag := Create_Tag(Buffer, "Sent");
      Glib.Properties.Set_Property(Sent_Data_Text_Tag, Foreground_Property, Palette.Green);
      Glib.Properties.Set_Property(Sent_Data_Text_Tag, Family_Property, "Monospace");

      Time_Text_Tag := Create_Tag(Buffer, "Time");
      Glib.Properties.Set_Property(Time_Text_Tag, Foreground_Property, Palette.Blue);
      Glib.Properties.Set_Property(Time_Text_Tag, Family_Property, "Monospace");

      Add(Window, Text_View);
      Pack_Start(Box, Control_Box, False, False);
      Pack_End  (Box, Window,      True,  True);
   end Create;

   ---------------------------
   -- ADD_RECEIVED_MESSAGES --
   ---------------------------

   procedure Add_Received_Messages(Messages : Message_Record_Vectors.Vector) is
      use Message_Record_Vectors;
      Iter : Gtk_Text_Iter;
   begin

      if Natural(Messages.Length) = 0 then
         return;
      end if;

      Get_End_Iter(Buffer, Iter);

      for Index in Natural range 0 .. Natural(Messages.Length) - 1 loop
         Insert_With_Tags(Buffer, Iter, Time_IO.Image(Clock, "%H%M:%S %Y.%m.%d: "), Time_Text_Tag);
         Insert_With_Tags(Buffer, Iter, To_Hex(Element(Messages, Index).Message), Received_Data_Text_Tag);
         Insert(Buffer, Iter, CRLF);
      end loop;

      Scroll_To_Mark(Text_View     => Text_View,
                     Mark          => Create_Mark(Buffer, "", Iter),
                     Within_Margin => 0.0,
                     Use_Align     => True,
                     XAlign        => 0.0,
                     YAlign        => 1.0);
   end Add_Received_Messages;

   ------------------------------
   -- ADD_TRANSMITTED_MESSAGES --
   ------------------------------

   procedure Add_Transmitted_Messages(Messages : Message_Record_Vectors.Vector) is
      use Message_Record_Vectors;
      Iter : Gtk_Text_Iter;
      use ada.text_io;
   begin

      if Natural(Messages.Length) = 0 then
         return;
      end if;

      Get_End_Iter(Buffer, Iter);

      for Index in Natural range 0 .. Natural(Messages.Length) - 1 loop
         Insert_With_Tags(Buffer, Iter, Time_IO.Image(Clock, "%H%M:%S %Y.%m.%d: "), Time_Text_Tag);
         Insert_With_Tags(Buffer, Iter, To_Hex(Messages.Element(Index).Message), Sent_Data_Text_Tag);
         Insert(Buffer, Iter, CRLF);
      end loop;

      Scroll_To_Mark(Text_View     => Text_View,
                     Mark          => Create_Mark(Buffer, "", Iter),
                     Within_Margin => 0.0,
                     Use_Align     => True,
                     XAlign        => 0.0,
                     YAlign        => 1.0);
   end Add_Transmitted_Messages;

end Raw_Data_Panel;
