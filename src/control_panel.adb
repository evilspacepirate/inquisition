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
with Ada.Text_IO;           use Ada.Text_IO;
with Cairo;                 use Cairo;
with Cairo.Image_Surface;   use Cairo.Image_Surface;
with Cairo.Font_Options;    use Cairo.Font_Options;
with Configuration;         use Configuration;
with Control_Panel.Buttons; use Control_Panel.Buttons;
with GDK;                   use GDK;
with GDK.Cairo;             use GDK.Cairo;
with GDK.Color;             use GDK.Color;
with GDK.Drawable;          use GDK.Drawable;
with GDK.Event;             use GDK.Event;
with GDK.GC;                use GDK.GC;
with GDK.Window;            use GDK.Window;
with GLib;                  use GLib;
with GLib.Object;           use GLib.Object;
with GTK;                   use GTK;
with GTK.Drawing_Area;      use GTK.Drawing_Area;
with GTK.Widget;            use GTK.Widget;
with GTK.Handlers;          use GTK.Handlers;
with GTKAda.Types;          use GTKAda.Types;
with Interfaces;            use Interfaces;
with Interfaces.C.Strings;
with Pango.Cairo;           use Pango.Cairo;
with Pango.Context;         use Pango.Context;
with Pango.Font;            use Pango.Font;
with Pango.Layout;          use Pango.Layout;
with Primatives;            use Primatives;
with Util;                  use Util;

package body Control_Panel is

   package Internal_Callback is new Handlers.Callback (Control_Panel_Widget_Record);
   package Return_Boolean_Callback is new Handlers.Return_Callback (Control_Panel_Widget_Record, Boolean);
   package Size_Callback is new Handlers.Callback (Control_Panel_Widget_Record);
   package Requisition_Marshaller is new Size_Callback.Marshallers.Generic_Marshaller (Gtk_Requisition_Access, Gtk.Widget.Get_Requisition);
   package Allocation_Callback is new Handlers.Callback (Control_Panel_Widget_Record);
   package Allocation_Marshaller is new Allocation_Callback.Marshallers.Generic_Marshaller (Gtk_Allocation_Access, Gtk.Widget.Get_Allocation);

   Signals      : Chars_Ptr_Array := Null_Array;
   Class_Record : GObject_Class   := Uninitialized_Class;

   Button_Width                 : constant :=     75;
   Button_Height                : constant :=     25;
   Widget_Vertical_Start        : constant :=   41.0;
   Widget_Horizontal_Start      : constant :=    5.0;
   Widget_Vertical_Pitch        : constant :=   25.0;
   Border_Width                 : constant := 8000.0;
   Border_Top_Line_Y            : constant :=    1.0;
   Border_Bottom_Line_Y         : constant :=   23.0;
   Border_Light_Gradient_Base   : constant := 16#F7#;
   Border_Dark_Gradient_Base    : constant := 16#D7#;
   Border_Text_Vertical_Start   : constant :=   16.0;
   Background_Color             : constant := 16#F7#;
   Title_Horizontal_Pad         : constant :=    7.0;

   Name_Column_Text             : constant String := "Name";
   Value_Column_Text            : constant String := "Value";
   Units_Column_Text            : constant String := "Units";
   Set_Data_Element_Column_Text : constant String := "Set Data Element";
   Set_Value_Column_Text        : constant String := "Set Value";
   Requesting_Data_Column_Text  : constant String := "Requesting Data";
   Request_Period_Column_Text   : constant String := "Request Period";
   Log_Data_Column_Text         : constant String := "Log Data";

   Panel_Enabled                : Boolean := False;

   UID_To_AP_Index_Map          : Name_Index_Maps.Map;
   Size_Requested               : Boolean := False;

   -------------
   -- GTK_NEW --
   -------------

   procedure GTK_New (Widget : out Control_Panel_Widget) is
   begin
      Widget := new Control_Panel_Widget_Record;
      Control_Panel.Initialize(Widget);
   end GTK_New;

   ------------------
   -- TEXT_EXTENTS --
   ------------------

   procedure Text_Extents (Font    : Cairo_Scaled_Font;
                           UTF8    : Interfaces.C.Strings.Chars_Ptr;
                           Extents : access Cairo_Text_Extents);
   -- XXX Use custom Text_Extents signature until GTK Ada fixes their signature XXX --
   -- XXX We need Extents to be an access type.                                 XXX --
   pragma Import(C, Text_Extents, "cairo_scaled_font_text_extents");

   ---------------------------
   -- GET_NAME_COLUMN_WIDTH --
   ---------------------------

   function Get_Name_Column_Width (Widget  : access Control_Panel_Widget_Record'Class;
                                   Context : Cairo_Context) return GDouble is
      Extents : aliased Cairo_Text_Extents;
      Font    : Cairo_Scaled_Font := Get_Scaled_Font(Context);
      Width   : GDouble           := 0.0;
   begin
      Text_Extents(Font, Interfaces.C.Strings.New_String(Name_Column_Text), Extents'access);
      Width := Extents.Width;
      for Index in Natural range 0 .. Natural(Widget.Adaptable_Parameters.Length) - 1 loop
         Text_Extents(Font,
                      Interfaces.C.Strings.New_String(UnStr.To_String(Widget.Adaptable_Parameters.Element(Index).Friendly_Name)),
                      Extents'access);
         if Extents.Width > Width then
            Width := Extents.Width;
         end if;
      end loop;
      return Width;
   end Get_Name_Column_Width;

   ----------------------------
   -- GET_VALUE_COLUMN_WIDTH --
   ----------------------------

   function Get_Value_Column_Width (Widget  : access Control_Panel_Widget_Record'Class;
                                    Context : Cairo_Context) return GDouble is
     Extents : aliased Cairo_Text_Extents;
     Font    : Cairo_Scaled_Font := Get_Scaled_Font(Context);
     Width   : GDouble           := 0.0;
   begin
      Text_Extents(Font, Interfaces.C.Strings.New_String(Value_Column_Text), Extents'access);
      Width := Extents.Width;
      for Index in Natural range 0 .. Natural(Widget.Adaptable_Parameters.Length) - 1 loop
         Text_Extents(Font,
                      Interfaces.C.Strings.New_String(UnStr.To_String(Widget.Values.Element(Index))),
                      Extents'access);
         if Extents.Width > Width then
            Width := Extents.Width;
         end if;
      end loop;
      return Width;
   end Get_Value_Column_Width;

   ----------------------------
   -- GET_UNITS_COLUMN_WIDTH --
   ----------------------------

   function Get_Units_Column_Width (Widget  : access Control_Panel_Widget_Record'Class;
                                    Context : Cairo_Context) return GDouble is
     Extents : aliased Cairo_Text_Extents;
     Font    : Cairo_Scaled_Font := Get_Scaled_Font(Context);
     Width   : GDouble           := 0.0;
   begin
      Text_Extents(Font, Interfaces.C.Strings.New_String(Units_Column_Text), Extents'access);
      Width := Extents.Width;
      for Index in Natural range 0 .. Natural(Widget.Adaptable_Parameters.Length) - 1 loop
         Text_Extents(Font,
                      Interfaces.C.Strings.New_String(UnStr.To_String(Widget.Adaptable_Parameters.Element(Index).Units_Name)),
                      Extents'access);
         if Extents.Width > Width then
            Width := Extents.Width;
         end if;
      end loop;
      return Width;
   end Get_Units_Column_Width;

   ---------------------------------------
   -- GET_SET_DATA_ELEMENT_COLUMN_WIDTH --
   ---------------------------------------

   function Get_Set_Data_Element_Column_Width (Widget  : access Control_Panel_Widget_Record'Class;
                                               Context : Cairo_Context) return GDouble is
     Extents : aliased Cairo_Text_Extents;
     Font    : Cairo_Scaled_Font := Get_Scaled_Font(Context);
     Width   : GDouble           := 0.0;
   begin
      Text_Extents(Font, Interfaces.C.Strings.New_String(Set_Data_Element_Column_Text), Extents'access);
      Width := Extents.Width;
      for Index in Natural range 0 .. Natural(Widget.Adaptable_Parameters.Length) - 1 loop
         Text_Extents(Font,
                      Interfaces.C.Strings.New_String(UnStr.To_String(Widget.Set_Values.Element(Index))),
                      Extents'access);
         if Extents.Width > Width then
            Width := Extents.Width;
         end if;
      end loop;
      return Width;
   end Get_Set_Data_Element_Column_Width;

   --------------------------------------
   -- GET_REQUESTING_DATA_COLUMN_WIDTH --
   --------------------------------------

   function Get_Requesting_Data_Column_Width (Widget  : access Control_Panel_Widget_Record'Class;
                                    Context : Cairo_Context) return GDouble is
     Extents : aliased Cairo_Text_Extents;
     Font    : Cairo_Scaled_Font := Get_Scaled_Font(Context);
     Width   : GDouble           := 0.0;
   begin
      Text_Extents(Font, Interfaces.C.Strings.New_String(Requesting_Data_Column_Text), Extents'access);
      Width := Extents.Width;
      return Width;
   end Get_Requesting_Data_Column_Width;

   -------------------------------------
   -- GET_REQUEST_PERIOD_COLUMN_WIDTH --
   -------------------------------------

   function Get_Request_Period_Column_Width (Widget  : access Control_Panel_Widget_Record'Class;
                                             Context : Cairo_Context) return GDouble is
     Extents : aliased Cairo_Text_Extents;
     Font    : Cairo_Scaled_Font := Get_Scaled_Font(Context);
     Width   : GDouble           := 0.0;
   begin
      Text_Extents(Font, Interfaces.C.Strings.New_String(Request_Period_Column_Text), Extents'access);
      Width := Extents.Width;
      for Index in Natural range 0 .. Natural(Widget.Adaptable_Parameters.Length) - 1 loop
         Text_Extents(Font,
                      Interfaces.C.Strings.New_String(UnStr.To_String(Widget.Adaptable_Parameters.Element(Index).Sample_Period)),
                      Extents'access);
         if Extents.Width > Width then
            Width := Extents.Width;
         end if;
      end loop;
      return Width;
   end Get_Request_Period_Column_Width;

   -------------------------------
   -- GET_LOG_DATA_COLUMN_WIDTH --
   -------------------------------

   function Get_Log_Data_Column_Width (Widget  : access Control_Panel_Widget_Record'Class;
                                       Context : Cairo_Context) return GDouble is
     Extents : aliased Cairo_Text_Extents;
     Font    : Cairo_Scaled_Font := Get_Scaled_Font(Context);
     Width   : GDouble           := 0.0;
   begin
      Text_Extents(Font,
                   Interfaces.C.Strings.New_String(Log_Data_Column_Text),
                   Extents'access);
      Width := Extents.Width;
      return Width;
   end Get_Log_Data_Column_Width;

   ------------------
   -- SIZE_REQUEST --
   ------------------

   procedure Size_Request (Widget      : access Control_Panel_Widget_Record'Class;
                           Requisition : in Gtk_Requisition_Access) is
   begin
      Requisition.Width  := 1;
      Requisition.Height := 300;
      Gtk.Handlers.Emit_Stop_By_Name (Widget, "size_request");
   end Size_Request;

   ------------------------------
   -- SET_ADAPTABLE_PARAMETERS --
   ------------------------------

   procedure Set_Adaptable_Parameters(Widget     : access Control_Panel_Widget_Record;
                                      Parameters : in Adaptable_Parameter_Record_Vectors.Vector) is
      use Adaptable_Parameter_Record_Vectors;
   begin
      Widget.Adaptable_Parameters := Parameters;
      Widget.Values.Clear;
      Widget.Set_Values.Clear;

      for Index in Natural range 0 .. Natural(Parameters.Length) - 1 loop
         Widget.Values.Append(UnStr.To_Unbounded_String("-"));
         case Widget.Adaptable_Parameters.Element(Index).Display_As is
            when IEEE754 =>
               Widget.Set_Values.Append(UnStr.To_Unbounded_String("[IEEE754 Not Supported]"));
               -- TODO --
            when Signed =>
               Widget.Set_Values.Append(UnStr.To_Unbounded_String("[Signed Not Supported]"));
               -- TODO --
            when Unsigned =>
               Widget.Set_Values.Append(UnStr.To_Unbounded_String(Unsigned_32'Image(Widget.Adaptable_Parameters.Element(Index).Default_Set_Value)));
            when Hex =>
               Widget.Set_Values.Append(UnStr.To_Unbounded_String(To_Hex(Widget.Adaptable_Parameters.Element(Index).Default_Set_Value)));
         end case;
      end loop;
      -- TODO Redraw widget --
   end Set_Adaptable_Parameters;

   --------------
   -- ON_CLICK --
   --------------

   function On_Click (Widget : access Control_Panel_Widget_Record'Class;
                      Event  : in Gdk_Event) return Boolean
   is
      X      : GInt;
      Y      : GInt;
      Width  : GInt;
      Height : GInt;
   begin
      Gdk.Drawable.Get_Size (Get_Window (Widget), Width, Height);
      X := GInt(Get_X (Event));
      Y := GInt(Get_Y (Event));
      return True;
   end On_Click;

   -------------------
   -- SIZE_ALLOCATE --
   -------------------

   procedure Size_Allocate (Widget     : access Control_Panel_Widget_Record'Class;
                            Allocation : in     Gtk_Allocation_Access)
   is
   begin
      if Realized_Is_Set (Widget) then
         Gdk.Window.Move_Resize (Get_Window (Widget),
                                 Allocation.X, Allocation.Y,
                                 Gint (Allocation.Width),
                                 Gint (Allocation.Height));
      end if;
      Gtk.Handlers.Emit_Stop_By_Name (Widget, "size_allocate");
   end Size_Allocate;

   ----------------
   -- DRAW_TITLE --
   ----------------

   procedure Draw_Title (Widget  : access Control_Panel_Widget_Record'Class;
                         Context : Cairo_Context) is
   begin
      Set_Source_RGB(Context, 0.1, 0.1, 0.1);
      Select_Font_Face(Context, "Helvetica", Cairo_Font_Slant_Normal, Cairo_Font_Weight_Normal);
      Set_Font_Size(Context, 13.0);
      Set_Line_Width(Context, 0.5);

      for Index in Natural range 2 .. 22 loop
         declare
            Step : constant Float := (Float(Border_Light_Gradient_Base) - Float(Border_Dark_Gradient_Base)) / 20.0;
         begin
            Set_Source_RGB(Context,
                           GDouble((Float(Border_Light_Gradient_Base) - Step * Float(Index)) / Float(16#FF#)),
                           GDouble((Float(Border_Light_Gradient_Base) - Step * Float(Index)) / Float(16#FF#)),
                           GDouble((Float(Border_Light_Gradient_Base) - Step * Float(Index)) / Float(16#FF#)));
            Move_To(Context, 0.0, GDouble(Border_Top_Line_Y) + GDouble(Index));
            Line_To(Context, Border_Width, GDouble(Border_Top_Line_Y) + GDouble(Index));
            Stroke(Context);
         end;
      end loop;

      declare
         X : GDouble := Widget_Horizontal_Start;
      begin
         Set_Source_RGB(Context, 0.0, 0.0, 0.0);

         Move_To(Context, X, Border_Text_Vertical_Start);
         Show_Text(Context, Name_Column_Text);
         X := X + Get_Name_Column_Width(Widget, Context) + Title_Horizontal_Pad;
         Move_To(Context, X,  0.0);
         Line_To(Context, X, 23.0);
         X := X + Title_Horizontal_Pad;
         Stroke(Context);

         Move_To(Context, X, Border_Text_Vertical_Start);
         Show_Text(Context, Value_Column_Text);
         X := X + Get_Value_Column_Width(Widget, Context) + Title_Horizontal_Pad;
         Move_To(Context, X,  0.0);
         Line_To(Context, X, 23.0);
         X := X + Title_Horizontal_Pad;
         Stroke(Context);

         Move_To(Context, X, Border_Text_Vertical_Start);
         Show_Text(Context, Units_Column_Text);
         X := X + Get_Units_Column_Width(Widget, Context) + Title_Horizontal_Pad;
         Move_To(Context, X,  0.0);
         Line_To(Context, X, 23.0);
         X := X + Title_Horizontal_Pad;
         Stroke(Context);

         Move_To(Context, X, Border_Text_Vertical_Start);
         Show_Text(Context, Set_Data_Element_Column_Text);
         X := X + Get_Set_Data_Element_Column_Width(Widget, Context) + Title_Horizontal_Pad;
         Move_To(Context, X,  0.0);
         Line_To(Context, X, 23.0);
         X := X + Title_Horizontal_Pad;
         Stroke(Context);

         Move_To(Context, X, Border_Text_Vertical_Start);
         Show_Text(Context, Requesting_Data_Column_Text);
         X := X + Get_Requesting_Data_Column_Width(Widget, Context) + Title_Horizontal_Pad;
         Move_To(Context, X,  0.0);
         Line_To(Context, X, 23.0);
         X := X + Title_Horizontal_Pad;
         Stroke(Context);

         Move_To(Context, X, Border_Text_Vertical_Start);
         Show_Text(Context, Request_Period_Column_Text);
         X := X + Get_Request_Period_Column_Width(Widget, Context) + Title_Horizontal_Pad;
         Move_To(Context, X,  0.0);
         Line_To(Context, X, 23.0);
         X := X + Title_Horizontal_Pad;
         Stroke(Context);

         Move_To(Context, X, Border_Text_Vertical_Start);
         Show_Text(Context, Log_Data_Column_Text);
         X := X + Get_Log_Data_Column_Width(Widget, Context) + Title_Horizontal_Pad;
         Move_To(Context, X,  0.0);
         Line_To(Context, X, 23.0);
         X := X + Title_Horizontal_Pad;
         Stroke(Context);
      end;
   end Draw_Title;

   ----------
   -- DRAW --
   ----------

   function Draw (Widget : access Control_Panel_Widget_Record'Class) return Boolean is
      Window   : aliased Gdk.Window.Gdk_Window := Get_Window(Widget);
      Drawable : GDK.Drawable.GDK_Drawable;
      Context  : Cairo_Context;
      X        : GInt;
      Y        : GInt;
      Width    : GInt;
      Height   : GInt;
      Depth    : GInt;
   begin
      Get_Geometry(Window, X, Y, Width, Height, Depth);
      Drawable := GDK.Drawable.GDK_Drawable(Window);
      Context  := Create(Drawable);

      Set_Source_RGB(Context,
                     GDouble(Background_Color) / GDouble(16#FF#),
                     GDouble(Background_Color) / GDouble(16#FF#),
                     GDouble(Background_Color) / GDouble(16#FF#));
      Paint(Context);

      Draw_Title(Widget, Context);

      if not Size_Requested then
         Size_Requested := True;
         Set_Size_Request(Widget,
                          GInt(Title_Horizontal_Pad * 14.0 +
                               Get_Name_Column_Width(Widget, Context) +
                               Get_Value_Column_Width(Widget, Context) +
                               Get_Units_Column_Width(Widget, Context) +
                               Get_Set_Data_Element_Column_Width(Widget, Context) +
                               Get_Requesting_Data_Column_Width(Widget, Context) +
                               Get_Request_Period_Column_Width(Widget, Context) +
                               Get_Log_Data_Column_Width(Widget, Context)),
                          300);

      end if;

      Set_Source_RGB(Context, 0.0, 0.0, 0.0);

      for Index in Natural range 0 .. Natural(Widget.Adaptable_Parameters.Length) - 1 loop
         Move_To(Context, Widget_Horizontal_Start, Widget_Vertical_Start + GDouble(Index) * Widget_Vertical_Pitch);
         Show_Text(Context, UnStr.To_String(Widget.Adaptable_Parameters.Element(Index).Friendly_Name));
         Move_To(Context,
                 Widget_Horizontal_Start + Get_Name_Column_Width(Widget, Context) + Title_Horizontal_Pad * 2.0,
                 Widget_Vertical_Start + GDouble(Index) * Widget_Vertical_Pitch);
         Show_Text(Context, UnStr.To_String(Widget.Values.Element(Index)));
      end loop;

      Move_To(Context, Widget_Horizontal_Start, Widget_Vertical_Start + GDouble(Widget.Adaptable_Parameters.Length) * Widget_Vertical_Pitch);
      Show_Text(Context, GInt'Image(Width) & " x" & GInt'Image(Height));

      return True;
   end Draw;

   ----------------
   -- INITIALIZE --
   ----------------

   procedure Initialize (Widget : access Control_Panel_Widget_Record'Class) is
   begin
      Gtk.Drawing_Area.Initialize(Widget);
      Glib.Object.Initialize_Class_Record(Widget, Signals, Class_Record, "ControlPanelWidget");
      Set_Events(Widget, Exposure_Mask or Button_Release_Mask or Button_Press_Mask);

      Return_Boolean_Callback.Connect(Widget, "expose_event", Return_Boolean_Callback.To_Marshaller(Draw'Access), True);

      Size_Callback.Connect(Widget, "size_request", Requisition_Marshaller.To_Marshaller(Size_Request'Access));
      Return_Boolean_Callback.Connect(Widget, "button_release_event", Return_Boolean_Callback.To_Marshaller(On_Click'Access));
      Allocation_Callback.Connect(Widget, "size_allocate", Allocation_Marshaller.To_Marshaller(Size_Allocate'Access));
   end;

   ----------------------
   -- UPDATE_UID_VALUE --
   ----------------------

   procedure Update_UID_Value (UID   : Unsigned_16;
                               Value : Unsigned_32) is
      Index_Vector : Natural_Vectors.Vector;
   begin
      -- TODO --
      Null;
   end Update_UID_Value;

   -------------
   -- DISABLE --
   -------------

   procedure Disable is
   begin
      Panel_Enabled := False;
      -- TODO --
   end Disable;

   ------------
   -- ENABLE --
   ------------

   procedure Enable is
   begin
      Panel_Enabled := True;
      -- TODO --
   end Enable;

end Control_Panel;

