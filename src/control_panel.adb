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
with Gdk.PixBuf;               use Gdk.PixBuf;
with Glib;                     use Glib;
with Gtk.List_Store;           use Gtk.List_Store;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle; use Gtk.Cell_Renderer_Toggle;
with Gtk.Cell_Renderer_Pixbuf; use Gtk.Cell_Renderer_Pixbuf;
with Control_Panel.Buttons;    use Control_Panel.Buttons;

package body Control_Panel is
   
   Store                           : Gtk_List_Store;
   Iter                            : Gtk_Tree_Iter;
   Column                          : aliased Gtk_Tree_View_Column;
   
   Name_ID                         : constant :=  0;
   Value_ID                        : constant :=  1;
   Units_ID                        : constant :=  2;
   Set_Button_ID                   : constant :=  3;
   Set_Value_ID                    : constant :=  4;
   Set_Value_Is_Editable           : constant :=  5;
   Is_Requesting_Data              : constant :=  6;
   Is_Requesting_Data_Is_Checkable : constant :=  7;
   Data_Request_Period             : constant :=  8;
   Is_Logged                       : constant :=  9;
   Is_Logged_Is_Checkable          : constant := 10;

   Button_Width                    : constant := 75;
   Button_Height                   : constant := 25;

   Text_Renderer                   : Gtk_Cell_Renderer_Text;
   Toggle_Renderer                 : Gtk_Cell_Renderer_Toggle;
   Pix_Renderer                    : Gtk_Cell_Renderer_PixBuf;

   Button_Clicked_Pix              : Gdk_PixBuf;
   Button_Unclicked_Pix            : Gdk_PixBuf;

   ------------
   -- CREATE --
   ------------
   
   procedure Create is
      Column_Number : GInt;
   begin

      -- Create Button Images --
      Button_Clicked_Pix   := Gdk_New_From_XPM_Data(Button_Clicked_XPM);
      Button_UnClicked_Pix := Gdk_New_From_XPM_Data(Button_Unclicked_XPM);
      Button_Clicked_Pix   := Scale_Simple(Button_Clicked_Pix, Button_Width, Button_Height);
      Button_Clicked_Pix   := Scale_Simple(Button_Clicked_Pix, Button_Width, Button_Height);

      -- Create View --
      Gtk_New(View);

      -- Create Model --
      Gtk_New(Store, (Name_ID                         => GType_String,
                      Value_ID                        => GType_String,
                      Units_ID                        => GType_String,
                      Set_Button_ID                   => GType_Object,
                      Set_Value_ID                    => GType_String,
                      Set_Value_Is_Editable           => GType_Boolean,
                      Is_Requesting_Data              => GType_Boolean,
                      Is_Requesting_Data_Is_Checkable => GType_Boolean,
                      Data_Request_Period             => GType_String,
                      Is_Logged                       => GType_Boolean,
                      Is_Logged_Is_Checkable          => GType_Boolean));

      Iter := Null_Iter;

      -- TODO Build model using system configuration file TODO --
      
      -- Add a placeholder data element control in the model --

      Append(Store.all'access, Iter);
      Set(Store.all'access, Iter, Name_ID, "Bus Voltage");
      Set(Store.all'access, Iter, Value_ID, "12.0");
      Set(Store.all'access, Iter, Units_ID, "Volts");
      Set(Store.all'access, Iter, Set_Button_ID, Button_Unclicked_Pix);
      Set(Store.all'access, Iter, Set_Value_ID, "0");
      Set(Store.all'access, Iter, Set_Value_Is_Editable, true);
      Set(Store.all'access, Iter, Is_Requesting_Data, true);
      Set(Store.all'access, Iter, Is_Requesting_Data_Is_Checkable, true);
      Set(Store.all'access, Iter, Data_Request_Period, "100 ms");
      Set(Store.all'access, Iter, Is_Logged, false);
      Set(Store.all'access, Iter, Is_Logged_Is_Checkable, true);

      -- Attach model to view --
      
      Set_Model(View.all'access, store.all'access);

      Gtk_New(Column);
      Gtk_New(Text_Renderer);

      -- Add column renderers to the TreeView --

      Set_Title(Column, "Name");
      Pack_Start(Column.all'access, Text_Renderer, true);
      set_sizing(Column, Tree_View_Column_Autosize);
      Add_Attribute(Column, Text_Renderer, "text", Name_ID);
      Column_Number := Append_Column(View.all'access, Column.all'access);

   end Create;

end Control_Panel;
