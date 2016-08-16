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
with Configuration;         use Configuration;
with GLib;                  use GLib;
with GLib.Object;           use GLib.Object;
with GLib.Values;           use GLib.Values;
with Gtk.Drawing_Area;
with Gtk.Window;            use Gtk.Window;
with Interfaces;            use Interfaces;
with Primatives;            use Primatives;

package Control_Panel is

   type Control_Panel_Widget_Record is new Gtk.Drawing_Area.Gtk_Drawing_Area_Record with private;
   type Control_Panel_Widget is access all Control_Panel_Widget_Record'Class;

   procedure Gtk_New (Widget : out Control_Panel_Widget);
   procedure Initialize (Widget : access Control_Panel_Widget_Record'Class);
   procedure Set_Adaptable_Parameters(Widget     : access Control_Panel_Widget_Record;
                                      Parameters : in Adaptable_Parameter_Record_Vectors.Vector);

   procedure Update_UID_Value (UID   : Unsigned_16;
                               Value : Unsigned_32);

   private

   type Control_Panel_Widget_Record is new Gtk.Drawing_Area.Gtk_Drawing_Area_Record with
   record
      Adaptable_Parameters : Adaptable_Parameter_Record_Vectors.Vector;
      Values               : String_Vectors.Vector;
      Set_Values           : String_Vectors.Vector;
   end record;

end Control_Panel;
