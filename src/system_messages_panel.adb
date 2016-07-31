-----------------------------------------------------------------
--                                                             --
-- SYSTEM_MESSAGES_PANEL                                       --
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

package body System_Messages_Panel is

   package Time_IO renames GNAT.Calendar.Time_IO;

   Error_Text_Tag : Gtk_Text_Tag;
   Time_Text_Tag  : Gtk_Text_Tag;

   Window         : Gtk_Scrolled_Window;
   Text_View      : Gtk_Text_View;
   Buffer         : Gtk_Text_Buffer;

   Blue           : constant String := "#2797CD";
   Red            : constant String := "#FF0000";

   procedure Create is
   begin
      Gtk_New_VBox(Box);
      Gtk_New(Window);
      Gtk_New(Buffer);
      Gtk_New(Text_View, Buffer);

      Set_Policy(Window, Policy_Automatic, Policy_Automatic);
      Set_Editable(Text_View, False);

      Error_Text_Tag := Create_Tag(Buffer, "Error");
      Glib.Properties.Set_Property(Error_Text_Tag, Foreground_Property, Red);

      Time_Text_Tag := Create_Tag(Buffer, "Time");
      Glib.Properties.Set_Property(Time_Text_Tag, Foreground_Property, Blue);

      Add(Window, Text_View);
      Pack_Start(Box, Window);
   end Create;

   procedure Append_Message (Text : String) is
      Iter : Gtk_Text_Iter;
   begin
      Get_End_Iter(Buffer, Iter);
      Scroll_To_Mark(Text_View     => Text_View,
                     Mark          => Create_Mark(Buffer, "", Iter),
                     Within_Margin => 0.0,
                     Use_Align     => True,
                     XAlign        => 0.0,
                     YAlign        => 1.0);

      Insert_With_Tags(Buffer, Iter, Time_IO.Image(Clock, "%H%M:%S %Y.%m.%d :  "), Time_Text_Tag);
      Insert(Buffer, Iter, Text);
   end Append_Message;

   procedure Append_Error (Text : String) is
      Iter : Gtk_Text_Iter;
   begin
      Get_End_Iter(Buffer, Iter);
      Scroll_To_Mark(Text_View     => Text_View,
                     Mark          => Create_Mark(Buffer, "", Iter),
                     Within_Margin => 0.0,
                     Use_Align     => True,
                     XAlign        => 0.0,
                     YAlign        => 1.0);

      Insert_With_Tags(Buffer, Iter, Time_IO.Image(Clock, "%H%M:%S %Y.%m.%d :  "), Time_Text_Tag);
      Insert_With_Tags(Buffer, Iter, Text, Error_Text_Tag);
   end Append_Error;

end System_Messages_Panel;
