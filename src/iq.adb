-----------------------------------------------------------------
--                                                             --
-- INQUISITION                                                 --
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
with Device;
with Ada.Text_IO;           use Ada.Text_IO;
with Configuration;         use Configuration;
with Control_Panel;         use Control_Panel;
with Configuration_Panel;
with Interfaces;            use Interfaces;
with Gdk.Event;             use Gdk.Event;
with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Glib.Properties;
with Glib.Values;           use Glib.Values;
with Gtk;                   use Gtk;
with Gtk.Arguments;         use Gtk.Arguments;
with Gtk.Box;               use Gtk.Box;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Handlers;          use Gtk.Handlers;
with Gtk.Main;
with Gtk.Paned;             use Gtk.Paned;
with Gtk.Window;            use Gtk.Window;
with Nexus;
with Primatives;            use Primatives;
with Raw_Data_Panel;
with Status_Bar_Panel;
with System_Messages_Panel;
with Util;                  use Util;

function Iq return Integer is

   package Basic_Callback is new Gtk.Handlers.Callback(GObject_Record);
   package Return_Callback is new Gtk.Handlers.Return_Callback(GObject_Record, Boolean);

   Main_Window               : Gtk_Window;
   Main_Window_Box           : Gtk_VBox;
   Main_Window_VPane         : Gtk_VPaned;
   Main_Panel_HPane          : Gtk_HPaned;
   Right_Side_Box            : Gtk_VBox;
   Control_Panel             : Control_Panel_Widget;

   Main_Window_Height        : GInt;
   System_Messages_Area_Size : GInt;
   System_Messages_Area_Set  : Boolean := False;

   Nexus_Callback_Period     : constant := 1; -- Milliseconds --
   Nexus_Timeout_Handler_ID  : GTK.Main.Timeout_Handler_ID;

   -- Search for an inquisition configuration file in the current working --
   -- directory and load configuration data from it. If there are more    --
   -- than one .iq files in the local directory or none, no configuration --
   -- files will be loaded.                                               --
   Config_File_Name          : String := Get_Configuration_File_Name;

   ---------------------
   -- ON_MAIN_DESTROY --
   ---------------------

   procedure On_Main_Destroy (Self : access GObject_Record'class) is
   begin
      Gtk.Main.Main_Quit;
   end On_Main_Destroy;

   -------------------------------------
   -- ON_MAIN_WINDOW_PANE_SIZE_CHANGE --
   -------------------------------------

   procedure On_Main_Window_Pane_Size_Change (Self : access GObject_Record'class) is
   begin
      -- This callback is triggered whenever the position of the main --
      -- window vertical pane is changed.                             --
      if System_Messages_Area_Set then
         System_Messages_Area_Size := Main_Window_Height - Get_Position(Main_Window_VPane);
      end if;
   end On_Main_Window_Pane_Size_Change;

   --------------------------------
   -- ON_MAIN_WINDOW_SIZE_CHANGE --
   --------------------------------

   function On_Main_Window_Size_Change (Self   : access GObject_Record'class;
                                        Params : GValues) return Boolean is
      Event           : constant Gdk_Event := To_Event(Params, 1);
      Slider_Position : constant GInt      := Get_Position(Main_Window_VPane);
   begin
      -- This callback is triggered whenever the size of the main --
      -- window has changed.                                      --

      Main_Window_Height := Get_Height(Event);

      -- Update position of the Main Window Pane Slider such that --
      -- the size of the System Messages area is constant.        --
      if System_Messages_Area_Set then
         Set_Position(Main_Window_VPane, Main_Window_Height - System_Messages_Area_Size);
      else
         System_Messages_Area_Size := Main_Window_Height - Slider_Position;
         System_Messages_Area_Set  := True;
      end if;

      return False;
   end On_Main_Window_Size_Change;

begin

   Gtk.Main.Init;

   Gtk_New(Main_Window);
   Gtk_New_VPaned(Main_Window_VPane);
   Gtk_New_HPaned(Main_Panel_HPane);
   Gtk_New_VBox(Main_Window_Box);
   Gtk_New_VBox(Right_Side_Box);
   Gtk_New(Control_Panel);

   Main_Window.Set_Default_Size(400, 400);

   Status_Bar_Panel.Create;
   Raw_Data_Panel.Create;
   Configuration_Panel.Create;
   System_Messages_Panel.Create;

   Add1(Main_Panel_HPane, Control_Panel);
   Add2(Main_Panel_HPane, Right_Side_Box);

   Pack_Start(Right_Side_Box, Configuration_Panel.Box, False, False);
   Pack_End  (Right_Side_Box, Raw_Data_Panel.Box,      True,  True);

   Add1(Main_Window_VPane, Main_Panel_HPane);
   Add2(Main_Window_VPane, System_Messages_Panel.Box);

   Add(Main_Window_Box, Main_Window_VPane);
   Add(Main_Window_Box, Status_Bar_Panel.Box);

   Add(Main_Window, Main_Window_Box);

   Set_Child_Packing(Main_Window_Box,
                     Status_Bar_Panel.Box,
                     Expand    => false,
                     Fill      => false,
                     Padding   => 0,
                     Pack_Type => Pack_Start);

   Basic_Callback.Connect(Main_Window, "destroy", On_Main_Destroy'access);
   Return_Callback.Connect(Main_Window, "configure_event", On_Main_Window_Size_Change'access);
   Basic_Callback.Connect(Main_Window, "size_allocate", On_Main_Window_Pane_Size_Change'access);

   Nexus.Initialize(Control_Panel);

   Main_Window.Show_All;

   Nexus_Timeout_Handler_ID := Gtk.Main.Timeout_Add(Nexus_Callback_Period, Nexus.Service'Access);
   Gtk.Main.Main;

   Nexus.Shutdown;

   -- Success! --
   return 0;
end Iq;
