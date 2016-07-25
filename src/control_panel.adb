-----------------------------------------------------------------
--                                                             --
-- CONTROL PANEL                                               --
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
with Ada.Text_IO;              use Ada.Text_IO;
with Configuration;            use Configuration;
with Control_Panel.Buttons;    use Control_Panel.Buttons;
with Gdk.PixBuf;               use Gdk.PixBuf;
with Glib;                     use Glib;
with Gtk.Handlers;             use Gtk.Handlers;
with Gtk.List_Store;           use Gtk.List_Store;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle; use Gtk.Cell_Renderer_Toggle;
with Gtk.Cell_Renderer_Pixbuf; use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Window;               use Gtk.Window;
with Interfaces;               use Interfaces;
with Primatives;               use Primatives;
with Util;                     use Util;

package body Control_Panel is

   package Object_Callback is new Gtk.Handlers.Callback (GObject_Record);
   package Return_Callbacks is new Gtk.Handlers.Return_Callback(Gobject_Record, Boolean);

   Store                              : Gtk_List_Store;
   Column                             : aliased Gtk_Tree_View_Column;
   Clicked_Row_Iter                   : Gtk_Tree_Iter;

   Name_ID                            : constant :=  0;
   Value_ID                           : constant :=  1;
   Units_ID                           : constant :=  2;
   Set_Button_ID                      : constant :=  3;
   Set_Value_ID                       : constant :=  4;
   Set_Value_Is_Editable_ID           : constant :=  5;
   Is_Requesting_Data_ID              : constant :=  6;
   Is_Requesting_Data_Is_Checkable_ID : constant :=  7;
   Data_Request_Period_ID             : constant :=  8;
   Data_Request_Period_Is_Editable_ID : constant :=  9;
   Is_Logged_ID                       : constant := 10;
   Is_Logged_Is_Checkable_ID          : constant := 11;

   Button_Width                       : constant := 75;
   Button_Height                      : constant := 25;

   Text_Renderer                      : Gtk_Cell_Renderer_Text;
   Toggle_Renderer                    : Gtk_Cell_Renderer_Toggle;
   Pix_Renderer                       : Gtk_Cell_Renderer_PixBuf;

   Button_Clicked_Pix                 : Gdk_PixBuf;
   Button_Unclicked_Pix               : Gdk_PixBuf;
   Button_Disabled_Pix                : Gdk_PixBuf;

   Window                             : Gtk_Window;

   On_Log_Data_Updated                : Parameter_Boolean_Update_Event_Callback;
   On_Requesting_Data_Updated         : Parameter_Boolean_Update_Event_Callback;
   On_Set_Value_Clicked               : Parameter_Unsigned_32_Update_Event_Callback;
   On_Parameter_Double_Clicked        : Parameter_Event_Callback;
   On_Request_Period_Updated          : Parameter_Duration_Update_Event_Callback;

   Adaptable_Parameters               : Adaptable_Parameter_Record_Vectors.Vector;

   Panel_Enabled                      : Boolean := False;

   ------------
   -- CREATE --
   ------------

   procedure Create (Main_Window : in out Gtk_Window) is
      Column_Number : GInt;
   begin

      -- Save a handle to the main window so we can determine the --
      -- X, Y location of mouse clicks later.                     --
      Window := Main_Window;

      -- Create Button Images --
      Button_Clicked_Pix   := Gdk_New_From_XPM_Data(Button_Clicked_XPM);
      Button_UnClicked_Pix := Gdk_New_From_XPM_Data(Button_Unclicked_XPM);
      Button_Disabled_Pix  := Gdk_New_From_XPM_Data(Button_Disabled_XPM);
      Button_Clicked_Pix   := Scale_Simple(Button_Clicked_Pix, Button_Width, Button_Height);
      Button_UnClicked_Pix := Scale_Simple(Button_Unclicked_Pix, Button_Width, Button_Height);
      Button_Disabled_Pix  := Scale_Simple(Button_Disabled_Pix, Button_Width, Button_Height);

      -- Create View --
      Gtk_New(View);

      -- Create Model --
      Gtk_New(Store, (Name_ID                            => GType_String,
                      Value_ID                           => GType_String,
                      Units_ID                           => GType_String,
                      Set_Button_ID                      => GType_Object,
                      Set_Value_ID                       => GType_String,
                      Set_Value_Is_Editable_ID           => GType_Boolean,
                      Is_Requesting_Data_ID              => GType_Boolean,
                      Is_Requesting_Data_Is_Checkable_ID => GType_Boolean,
                      Data_Request_Period_ID             => GType_String,
                      Data_Request_Period_Is_Editable_ID => GType_Boolean,
                      Is_Logged_ID                       => GType_Boolean,
                      Is_Logged_Is_Checkable_ID          => GType_Boolean));

      -- Attach model to view --
      
      Set_Model(View.all'access, store.all'access);

      -- Add column renderers to the TreeView --

      -- Data Element Name Column --
      
      Gtk_New(Column);
      Gtk_New(Text_Renderer);

      Set_Title(Column, "Name");
      Pack_Start(Column.all'access, Text_Renderer, True);
      Set_Sizing(Column, Tree_View_Column_Autosize);
      Add_Attribute(Column, Text_Renderer, "text", Name_ID);
      Column_Number := Append_Column(View.all'access, Column.all'access);

      Gtk_New(Column);
      Gtk_New(Text_Renderer);

      -- Data Element Value Column --
      
      Set_Title(Column, "Value");
      Pack_Start(Column.all'access, Text_Renderer, True);
      Set_Sizing(Column, Tree_View_Column_Autosize);
      Add_Attribute(Column, Text_Renderer, "text", Value_ID);
      Column_Number := Append_Column(View.all'access, Column.all'access);

      -- Data Element Units Column --
      
      Gtk_New(Column);
      Gtk_New(Text_Renderer);

      Set_Title(Column, "Units");
      Pack_Start(Column.all'access, Text_Renderer, True);
      Set_Sizing(Column, Tree_View_Column_Autosize);
      Add_Attribute(Column, Text_Renderer, "text", Units_ID);
      Column_Number := Append_Column(View.all'access, Column.all'access);

      -- Set Data Element Button Column --
      
      Gtk_New(Column);
      Gtk_New(Pix_Renderer);

      Set_Title(Column, "Set Data Element");
      Pack_Start(Column.all'access, Pix_Renderer, True);
      Add_Attribute(Column, Pix_Renderer, "pixbuf", Set_Button_ID);
      Column_Number := Append_Column(View.all'access, Column.all'access);

      -- Set Value ID Editbox Column --
      
      Gtk_New(Column);
      Gtk_New(Text_Renderer);

      Set_Title(Column, "Set Value");
      Pack_Start(Column.all'access, Text_Renderer, True);
      set_sizing(Column, Tree_View_Column_Autosize);
      Add_Attribute(Column, Text_Renderer, "text", Set_Value_ID);
      Add_Attribute(Column, Text_Renderer, "editable", Set_Value_Is_Editable_ID);
      Column_Number := Append_Column(View.all'access, Column.all'access);
      Object_Callback.Object_Connect(Text_Renderer, "edited", Set_Value_Edited'access, Slot_Object => Store);

      -- Is Requesting Data Checkbox --

      Gtk_New(Column);
      Gtk_New(Toggle_Renderer);

      Set_Title(Column, "Requesting Data");
      Pack_Start(Column.all'access, Toggle_Renderer, True);
      Add_Attribute(Column, Toggle_Renderer, "active", Is_Requesting_Data_ID);
      Add_Attribute(Column, Toggle_Renderer, "activatable", Is_Requesting_Data_Is_Checkable_ID);
      Column_Number := Append_Column(View.all'access, Column.all'access);
      Object_Callback.Object_Connect(Toggle_Renderer, "toggled", Is_Requesting_Checkbox_Toggled'access, Store);

      -- Data Request Period Editbox Column --

      Gtk_New(Column);
      Gtk_New(Text_Renderer);

      Set_Title(Column, "Request Period");
      Pack_Start(Column.all'access, Text_Renderer, True);
      set_sizing(Column, Tree_View_Column_Autosize);
      Add_Attribute(Column, Text_Renderer, "text", Data_Request_Period_ID);
      Add_Attribute(Column, Text_Renderer, "editable", Data_Request_Period_Is_Editable_ID);
      Column_Number := Append_Column(View.all'access, Column.all'access);
      Object_Callback.Object_Connect(Text_Renderer, "edited", Request_Period_Edited'access, Slot_Object => Store);

      -- Is Logged Checkbox --

      Gtk_New(Column);
      Gtk_New(Toggle_Renderer);

      Set_Title(Column, "Log Data");
      Pack_Start(Column.all'access, Toggle_Renderer, True);
      Add_Attribute(Column, Toggle_Renderer, "active", Is_Logged_ID);
      Add_Attribute(Column, Toggle_Renderer, "activatable", Is_Logged_Is_Checkable_ID);
      Column_Number := Append_Column(View.all'access, Column.all'access);
      Object_Callback.Object_Connect(Toggle_Renderer, "toggled", Logging_Checkbox_Toggled'access, Store);

      -- Connect double-click callback for the tree view --

      Object_Callback.Connect(View, "row-activated", Double_Click_On_Data_Element_Row'access);
      Return_Callbacks.Connect(View, "button-press-event", Set_Button_Pressed'access);
      Return_Callbacks.Connect(View, "button-release-event", Set_Button_Released'access);

   end Create;

   ------------------------------
   -- SET_ADAPTABLE_PARAMETERS --
   ------------------------------

   procedure Set_Adaptable_Parameters(Parameters : in Adaptable_Parameter_Record_Vectors.Vector) is
      use Adaptable_Parameter_Record_Vectors;
      Iter : Gtk_Tree_Iter := Null_Iter;
   begin
      Adaptable_Parameters := Parameters;
      for Index in Natural range 0 .. Natural(Length(Parameters)) - 1 loop

         Append(Store.all'access, Iter);
         Set(Store.all'access, Iter, Name_ID,  UnStr.To_String(Parameters.Element(Index).Friendly_Name));
         Set(Store.all'access, Iter, Units_ID, UnStr.To_String(Parameters.Element(Index).Units_Name));

         if Parameters.Element(Index).Is_Readable then
            -- Readable --
            Set(Store.all'access, Iter, Value_ID,                           "-");
            Set(Store.all'access, Iter, Set_Value_Is_Editable_ID,           False);
            Set(Store.all'access, Iter, Is_Requesting_Data_ID,              Parameters.Element(Index).Is_Sampling);
            Set(Store.all'access, Iter, Is_Requesting_Data_Is_Checkable_ID, False);
            Set(Store.all'access, Iter, Data_Request_Period_ID,             UnStr.To_String(Parameters.Element(Index).Sample_Period));
            Set(Store.all'access, Iter, Data_Request_Period_Is_Editable_ID, True);
            Set(Store.all'access, Iter, Is_Logged_ID,                       False);
            Set(Store.all'access, Iter, Is_Logged_Is_Checkable_ID,          False);
         else
            -- Not Readable --
            Set(Store.all'access, Iter, Is_Requesting_Data_ID,              False);
            Set(Store.all'access, Iter, Is_Requesting_Data_Is_Checkable_ID, False);
            Set(Store.all'access, Iter, Data_Request_Period_ID,             "");
            Set(Store.all'access, Iter, Data_Request_Period_Is_Editable_ID, False);
            Set(Store.all'access, Iter, Is_Logged_ID,                       False);
            Set(Store.all'access, Iter, Is_Logged_Is_Checkable_ID,          False);
         end if;

         if Parameters.Element(Index).Is_Writable then
            -- Writable --
            Set(Store.all'access, Iter, Set_Button_ID,            Button_Disabled_Pix);
            Set(Store.all'access, Iter, Set_Value_ID,             Unsigned_32'image(Parameters.Element(Index).Default_Set_Value));
            Set(Store.all'access, Iter, Set_Value_Is_Editable_ID, True);
         else
            -- Not Writable --
            Set(Store.all'access, Iter, Set_Value_ID,             "");
            Set(Store.all'access, Iter, Set_Value_Is_Editable_ID, False);
         end if;
      end loop;
   end Set_Adaptable_Parameters;

   ----------------------------
   -- ASSIGN_EVENT_CALLBACKS --
   ----------------------------

   procedure Assign_Event_Callbacks(Log_Data_Updated         : in not null Parameter_Boolean_Update_Event_Callback;
                                    Requesting_Data_Updated  : in not null Parameter_Boolean_Update_Event_Callback;
                                    Set_Value_Clicked        : in not null Parameter_Unsigned_32_Update_Event_Callback;
                                    Parameter_Double_Clicked : in not null Parameter_Event_Callback;
                                    Request_Period_Updated   : in not null Parameter_Duration_Update_Event_Callback) is
   begin
      On_Log_Data_Updated         := Log_Data_Updated;
      On_Requesting_Data_Updated  := Requesting_Data_Updated;
      On_Set_Value_Clicked        := Set_Value_Clicked;
      On_Parameter_Double_Clicked := Parameter_Double_Clicked;
      On_Request_Period_Updated   := Request_Period_Updated;
   end Assign_Event_Callbacks;

   ------------------------------
   -- LOGGING_CHECKBOX_TOGGLED --
   ------------------------------

   procedure Logging_Checkbox_Toggled(Object : access GObject_Record'class;
                                      Params : GValues) is
      Old_Value : Boolean;
      New_Value : Boolean;
      Path      : constant String        := Get_String(Nth(Params, 1));
      Iter      : constant Gtk_Tree_Iter := Get_Iter_From_String(Store, Path);
   begin
      if Panel_Enabled = False then
         return;
      end if;

      Old_Value := Get_Boolean(Store, Iter, Is_Logged_ID);
      New_Value := not Old_Value;
      Set(Store, Iter, Is_Logged_ID, New_Value);
      if On_Log_Data_Updated /= Null then
         if New_Value then
            On_Log_Data_Updated(Natural'Value(Path), True);
         else
            On_Log_Data_Updated(Natural'Value(Path), False);
         end if;
      end if;
   end Logging_Checkbox_Toggled;

   ------------------------------------
   -- IS_REQUESTING_CHECKBOX_TOGGLED --
   ------------------------------------

   procedure Is_Requesting_Checkbox_Toggled(Object : access GObject_Record'class;
                                            Params : GValues) is
      Old_Value : Boolean;
      New_Value : Boolean;
      Path      : constant String        := Get_String(Nth(Params, 1));
      Iter      : constant Gtk_Tree_Iter := Get_Iter_From_String(Store, Path);
   begin
      if Panel_Enabled = False then
         return;
      end if;

      Old_Value := Get_Boolean(Store, Iter, Is_Requesting_Data_ID);
      New_Value := not Old_Value;
      Set(Store, Iter, Is_Requesting_Data_ID, New_Value);
      if On_Requesting_Data_Updated /= Null then
         if New_Value then
            On_Requesting_Data_Updated(Natural'Value(Path), True);
         else
            On_Requesting_Data_Updated(Natural'Value(Path), False);
         end if;
      end if;
   end Is_Requesting_Checkbox_Toggled;

   --------------------------------------
   -- DOUBLE_CLICK_ON_DATA_ELEMENT_ROW --
   --------------------------------------

   procedure Double_Click_On_Data_Element_Row(Object : access GObject_Record'class;
                                              Params : GValues) is
      Column : Gtk_Tree_View_Column;
      Path   : Gtk_Tree_Path;
   begin
      if Panel_Enabled = False then
         return;
      end if;

      Get_Cursor(View.all'access, Path, Column);
      if On_Parameter_Double_Clicked /= Null then
         On_Parameter_Double_Clicked(Natural'Value(To_String(Path)));
      end if;
   end Double_Click_On_Data_Element_Row;

   ------------------------
   -- SET_BUTTON_PRESSED --
   ------------------------

   function Set_Button_Pressed(Object : access GObject_Record'class;
                               Params : GValues) return Boolean is
      X      : GInt;
      Y      : GInt;
      Cell_X : GInt;
      Cell_Y : GInt;
      Found  : Boolean;
      Path   : Gtk_Tree_Path;
      Column : Gtk_Tree_View_Column;
      Iter   : Gtk_Tree_Iter;
   begin
      if Panel_Enabled = False then
         return False;
      end if;

      Get_Pointer(Window, X, Y);

      Convert_Widget_To_Tree_Coords(View, X, Y, X, Y);

      Get_Path_At_Pos(View, X, Y, Path, Column, Cell_X, Cell_Y, Found);

      declare
         use Adaptable_Parameter_Record_Vectors;
         Parameter_Clicked : Adaptable_Parameter_Record := Element(Adaptable_Parameters, Natural'Value(To_String(Path)));
      begin
         if Parameter_Clicked.Is_Writable = False then
            -- Set button only works for writable adaptable parameters --
            return False;
         end if;

         if Found then
           if Get_Title(Column) = "Set Data Element" then
              Iter := Get_Iter_From_String(Store, To_String(Path));
              Set(Store, Iter, Set_Button_ID, Button_Clicked_Pix);
              Clicked_Row_Iter := Iter;
              if On_Set_Value_Clicked /= Null then
                 On_Set_Value_Clicked(Natural'Value(To_String(Path)), String_To_Unsigned_32(Get_String(Store, Iter, Set_Value_ID)));
              end if;
           end if;
         else
           Clicked_Row_Iter := Null_Iter;
         end if;
      end;
      return False;
   end Set_Button_Pressed;

   -------------------------
   -- SET_BUTTON_RELEASED --
   -------------------------

   function Set_Button_Released(Object : access GObject_Record'class;
                                Params : GValues) return Boolean is
   begin
      if Panel_Enabled = False then
         return False;
      end if;

      if Clicked_Row_Iter /= Null_Iter then
         Set(Store, Clicked_Row_Iter, Set_Button_ID, Button_Unclicked_Pix);
         Clicked_Row_Iter := Null_Iter;
      end if;

      return False;
   end Set_Button_Released;

   ----------------------
   -- SET_VALUE_EDITED --
   ----------------------

   procedure Set_Value_Edited(Object : access GObject_Record'class;
                              Params : GValues) is
      Path  : String        := Get_String(Nth(Params, 1));
      Value : GValue        := Nth(Params, 2);
      Iter  : Gtk_Tree_Iter := Get_Iter_From_String(Store, Path);
   begin

      -- Make sure this value is convertable to an unsigned_32 --
      declare
         New_Value : Unsigned_32 := String_To_Unsigned_32(Get_String(Value));
      begin
         -- Update value in model --
         Set_Value(Store, Iter, Set_Value_ID, Value);
      end;
   exception
      when Others =>
         Null;
   end Set_Value_Edited;
   
   ---------------------------
   -- REQUEST_PERIOD_EDITED --
   ---------------------------

   procedure Request_Period_Edited(Object : access GObject_Record'class;
                                   Params : GValues) is
      Path  : String        := Get_String(Nth(Params, 1));
      Value : GValue        := Nth(Params, 2);
      Iter  : Gtk_Tree_Iter := Get_Iter_From_String(Store, Path);
   begin

      declare
         New_Value : Duration := String_To_Duration(Get_String(Value));
      begin
         Set_Value(Store, Iter, Data_Request_Period_ID, Value);
         if On_Request_Period_Updated /= Null then
            On_Request_Period_Updated(Natural'Value(Path), New_Value);
         end if;
      end;
   exception
      when Others =>
         Null;
   end Request_Period_Edited;

   ---------------
   -- SET_VALUE --
   ---------------

   procedure Set_Value (Adaptable_Parameter_Index : Natural;
                        Value                     : Unsigned_32) is
   begin
      -- TODO --
      Null;
   end Set_Value;

   -------------
   -- DISABLE --
   -------------

   procedure Disable is
      use Adaptable_Parameter_Record_Vectors;
      Iter : Gtk_Tree_Iter := Null_Iter;
   begin
      for Index in Natural range 0 .. Natural(Length(Adaptable_Parameters)) - 1 loop
         Iter := Get_Iter_From_String(Store, Natural'image(Index));

         if Adaptable_Parameters.Element(Index).Is_Readable then
            -- Readable --
            Set(Store.all'access, Iter, Value_ID,                           "-");
            Set(Store.all'access, Iter, Set_Value_Is_Editable_ID,           True);
            Set(Store.all'access, Iter, Is_Requesting_Data_Is_Checkable_ID, False);
            Set(Store.all'access, Iter, Data_Request_Period_Is_Editable_ID, True);
            Set(Store.all'access, Iter, Is_Logged_Is_Checkable_ID,          False);
         else
            -- Not Readable --
            Set(Store.all'access, Iter, Is_Requesting_Data_ID,              False);
            Set(Store.all'access, Iter, Is_Requesting_Data_Is_Checkable_ID, False);
            Set(Store.all'access, Iter, Data_Request_Period_ID,             "");
            Set(Store.all'access, Iter, Data_Request_Period_Is_Editable_ID, False);
            Set(Store.all'access, Iter, Is_Logged_Is_Checkable_ID,          False);
         end if;

         if Adaptable_Parameters.Element(Index).Is_Writable then
            -- Writable --
            Set(Store.all'access, Iter, Set_Button_ID,            Button_Disabled_Pix);
            Set(Store.all'access, Iter, Set_Value_Is_Editable_ID, True);
         else
            -- Not Writable --
            Set(Store.all'access, Iter, Set_Value_ID,             "");
            Set(Store.all'access, Iter, Set_Value_Is_Editable_ID, False);
         end if;
      end loop;
      Panel_Enabled := False;
   end Disable;

   ------------
   -- ENABLE --
   ------------

   procedure Enable is
      use Adaptable_Parameter_Record_Vectors;
      Iter : Gtk_Tree_Iter := Null_Iter;
   begin
      for Index in Natural range 0 .. Natural(Length(Adaptable_Parameters)) - 1 loop
         Iter := Get_Iter_From_String(Store, Natural'image(Index));

         if Adaptable_Parameters.Element(Index).Is_Readable then
            -- Readable --
            Set(Store.all'access, Iter, Value_ID,                           "-");
            Set(Store.all'access, Iter, Set_Value_Is_Editable_ID,           True);
            Set(Store.all'access, Iter, Is_Requesting_Data_Is_Checkable_ID, True);
            Set(Store.all'access, Iter, Data_Request_Period_Is_Editable_ID, True);
            Set(Store.all'access, Iter, Is_Logged_Is_Checkable_ID,          True);
         else
            -- Not Readable --
            Set(Store.all'access, Iter, Is_Requesting_Data_ID,              False);
            Set(Store.all'access, Iter, Is_Requesting_Data_Is_Checkable_ID, True);
            Set(Store.all'access, Iter, Data_Request_Period_ID,             "");
            Set(Store.all'access, Iter, Data_Request_Period_Is_Editable_ID, True);
            Set(Store.all'access, Iter, Is_Logged_Is_Checkable_ID,          True);
         end if;

         if Adaptable_Parameters.Element(Index).Is_Writable then
            -- Writable --
            Set(Store.all'access, Iter, Set_Button_ID,            Button_Unclicked_Pix);
            Set(Store.all'access, Iter, Set_Value_Is_Editable_ID, True);
         else
            -- Not Writable --
            Set(Store.all'access, Iter, Set_Value_ID,             "");
            Set(Store.all'access, Iter, Set_Value_Is_Editable_ID, False);
         end if;
      end loop;
      Panel_Enabled := True;
   end Enable;

end Control_Panel;

