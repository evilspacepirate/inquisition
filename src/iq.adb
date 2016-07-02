-----------------------------------------------------------------
--                                                             --
-- Inquisition                                                 --
--                                                             --
-- Visual debugging tool for exchanging data with embedded     --
-- systems.                                                    --
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
with Glib;          use Glib;
with Glib.Object;   use Glib.Object;
with Gtk;           use Gtk;
with Gtk.Main;      
with Gtk.Box;       use Gtk.Box;
with Gtk.Enums;     use Gtk.Enums;
with Gtk.Handlers;  use Gtk.Handlers;
with Gtk.Label;     use Gtk.Label;
with Gtk.Paned;     use Gtk.Paned;
with Gtk.Window;    use Gtk.Window;
with Control_Panel;

function Iq return Integer is

   package Basic_Callback is new Gtk.Handlers.Callback(GObject_Record);

   Status_Bar_Pad_Pixels   : constant := 7;

   Main_Window             : Gtk_Window;
   Main_Window_Box         : Gtk_VBox;
   Main_Window_Label       : Gtk_Label;
   Main_Panel_HPane        : Gtk_HPaned;
   Status_Bar_Box          : Gtk_HBox;
   Connection_Config_Label : Gtk_Label;
   Protocol_Label          : Gtk_Label;

   ---------------------
   -- ON_MAIN_DESTROY --
   ---------------------
   
   procedure On_Main_Destroy (Self : access GObject_Record'class) is
   begin
      Gtk.Main.Main_Quit;
   end On_Main_Destroy;

begin

   Gtk.Main.Init;

   Gtk_New(Main_Window);
   Gtk_New_VBox(Main_Window_Box);
   Gtk_New(Main_Window_Label);
   Gtk_New_HPaned(Main_Panel_HPane);
   Gtk_New_HBox(Status_Bar_Box);
   Gtk_New(Connection_Config_Label);
   Gtk_New(Protocol_Label);

   Set_Label(Connection_Config_Label, "Connection Configuration Placeholder");
   Set_Label(Protocol_Label, "Protocol Placeholder");
   Set_Label(Main_Window_Label, "Main Window Area");
   
   Main_Window.Set_Default_Size(400, 400);

   Pack_Start(Status_Bar_Box, Connection_Config_Label, False, False, Status_Bar_Pad_Pixels);
   Pack_End(Status_Bar_Box, Protocol_Label, False, False, Status_Bar_Pad_Pixels);

   Control_Panel.Create(Main_Window);
   Add(Main_Panel_HPane, Control_Panel.View);

   Add(Main_Window_Box, Main_Panel_HPane);
   Add(Main_Window_Box, Status_Bar_Box);

   Add(Main_Window, Main_Window_Box);

   Set_Child_Packing(Main_Window_Box, 
                     Status_Bar_Box,
                     Expand    => false,
                     Fill      => false,
                     Padding   => 0,
                     Pack_Type => Pack_Start);

   Basic_Callback.Connect(Main_Window, "destroy", On_Main_Destroy'access);

   Main_Window.Show_All;
   Gtk.Main.Main;

   -- Success! --
   return 0;
end Iq;
