-----------------------------------------------------------------
--                                                             --
-- CONTROL PANEL Specification                                 --
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
with Ada.Containers.Indefinite_Vectors; use Ada.Containers;
with Cairo;                             use Cairo;
with Configuration;                     use Configuration;
with GLib;                              use GLib;
with GLib.Object;                       use GLib.Object;
with GLib.Values;                       use GLib.Values;
with Gtk.Drawing_Area;
with Gtk.Window;                        use Gtk.Window;
with Interfaces;                        use Interfaces;
with Primatives;                        use Primatives;

package Control_Panel is

   type Control_Panel_Widget_Record is new Gtk.Drawing_Area.Gtk_Drawing_Area_Record with private;
   type Control_Panel_Widget is access all Control_Panel_Widget_Record'Class;
   type Button_State is (Pressed, Not_Pressed);
   type Checkbox_State is (Checked, Not_Checked);

   package Button_State_Vectors is new Indefinite_Vectors (Natural, Button_State);

   procedure Gtk_New (Widget : out Control_Panel_Widget);
   procedure Initialize (Widget : access Control_Panel_Widget_Record'Class);
   procedure Set_Adaptable_Parameters (Widget     : access Control_Panel_Widget_Record;
                                       Parameters : in Adaptable_Parameter_Record_Vectors.Vector);

   procedure Update_UID_Value (UID   : Unsigned_16;
                               Value : Unsigned_32);

   procedure Enable  (Widget : access Control_Panel_Widget_Record);
   procedure Disable (Widget : access Control_Panel_Widget_Record);

   private

   type Control_Panel_Widget_Record is new Gtk.Drawing_Area.Gtk_Drawing_Area_Record with
   record
      Adaptable_Parameters          : Adaptable_Parameter_Record_Vectors.Vector;
      Values                        : String_Vectors.Vector;
      Set_Values                    : String_Vectors.Vector;
      Friendly_Name_Column_Width    : GDouble;
      Value_Column_Width            : GDouble;
      Units_Column_Width            : GDouble;
      Set_Data_Element_Column_Width : GDouble;
      Request_Column_Width          : GDouble;
      Request_Period_Column_Width   : GDouble;
      Log_Column_Width              : GDouble;
      Set_Button_State              : Button_State_Vectors.Vector;
      Enabled                       : Boolean;
   end record;

end Control_Panel;
