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
with Ada.Text_IO;           use Ada.Text_IO;
with Configuration;         use Configuration;
with Control_Panel;
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
with Gtk.Label;             use Gtk.Label;
with Gtk.Main;
with Gtk.Paned;             use Gtk.Paned;
with Gtk.Window;            use Gtk.Window;
with Raw_Data_Panel;
with System_Messages_Panel;
with Util;                  use Util;

function Iq return Integer is

   use Adaptable_Parameter_Record_Vectors;

   package Basic_Callback is new Gtk.Handlers.Callback(GObject_Record);
   package Return_Callback is new Gtk.Handlers.Return_Callback(GObject_Record, Boolean);

   Status_Bar_Pad_Pixels     : constant := 7;

   Main_Window               : Gtk_Window;
   Main_Window_Box           : Gtk_VBox;
   Main_Window_VPane         : Gtk_VPaned;
   Main_Panel_HPane          : Gtk_HPaned;
   Status_Bar_Box            : Gtk_HBox;
   Right_Side_Box            : Gtk_VBox;
   Connection_Config_Label   : Gtk_Label;
   Protocol_Label            : Gtk_Label;

   Adaptable_Parameters      : Adaptable_Parameter_Record_Vectors.Vector;
   Config                    : Datalink_Configuration;
   Config_Errors             : UnStr.Unbounded_String;
   Config_Valid              : Boolean;

   Main_Window_Height        : GInt;
   System_Messages_Area_Size : GInt;
   System_Messages_Area_Set  : Boolean := False;

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

   Get_Config_From_File(Config_File_Name,
                        Adaptable_Parameters,
                        Config,
                        Config_Errors,
                        Config_Valid);

   Gtk.Main.Init;

   Gtk_New(Main_Window);
   Gtk_New_VPaned(Main_Window_VPane);
   Gtk_New_HPaned(Main_Panel_HPane);
   Gtk_New_VBox(Main_Window_Box);
   Gtk_New_VBox(Right_Side_Box);

   -- Create status bar --
   Gtk_New_HBox(Status_Bar_Box);
   Gtk_New(Connection_Config_Label);
   Gtk_New(Protocol_Label);
   Set_Label(Connection_Config_Label, Datalink_Configuration_To_String(Config));
   Set_Label(Protocol_Label, "Protocol Placeholder");
   Pack_Start(Status_Bar_Box, Connection_Config_Label, False, False, Status_Bar_Pad_Pixels);
   Pack_End(Status_Bar_Box, Protocol_Label, False, False, Status_Bar_Pad_Pixels);

   Main_Window.Set_Default_Size(400, 400);

   if Config_Valid then
      Control_Panel.Create(Main_Window, Adaptable_Parameters);
      Add1(Main_Panel_HPane, Control_Panel.View);
   end if;

   Add2(Main_Panel_HPane, Right_Side_Box);

   Raw_Data_Panel.Create;
   Configuration_Panel.Create;
   System_Messages_Panel.Create;

   Pack_Start(Right_Side_Box, Configuration_Panel.Box, False, False);
   Pack_End  (Right_Side_Box, Raw_Data_Panel.Box,      True,  True);

   Add1(Main_Window_VPane, Main_Panel_HPane);
   Add2(Main_Window_VPane, System_Messages_Panel.Box);

   Add(Main_Window_Box, Main_Window_VPane);
   Add(Main_Window_Box, Status_Bar_Box);

   Add(Main_Window, Main_Window_Box);

   Set_Child_Packing(Main_Window_Box,
                     Status_Bar_Box,
                     Expand    => false,
                     Fill      => false,
                     Padding   => 0,
                     Pack_Type => Pack_Start);

   Basic_Callback.Connect(Main_Window, "destroy", On_Main_Destroy'access);
   Return_Callback.Connect(Main_Window, "configure_event", On_Main_Window_Size_Change'access);
   Basic_Callback.Connect(Main_Window, "size_allocate", On_Main_Window_Pane_Size_Change'access);

   if Config_File_Name /= "" then
      System_Messages_Panel.Append_Message("Loading configuration from " & Config_File_Name & CRLF);
      System_Messages_Panel.Append_Error(UnStr.To_String(Config_Errors));
   else
      System_Messages_Panel.Append_Message("No configuration file found in current directory.");
   end if;

   Main_Window.Show_All;
   Gtk.Main.Main;

   -- Success! --
   return 0;
end Iq;
