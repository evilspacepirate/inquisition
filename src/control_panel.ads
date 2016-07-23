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
with Configuration; use Configuration;
with Glib.Object;   use Glib.Object;
with Glib.Values;   use Glib.Values;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Window;    use Gtk.Window;

package Control_Panel is

   View : Gtk_Tree_View;

   procedure Create(Main_Window : in out Gtk_Window);

   procedure Set_Adaptable_Parameters(Parameters : in Adaptable_Parameter_Record_Vectors.Vector);

   private

   procedure Logging_Checkbox_Toggled(Object : access GObject_Record'class;
                                      Params : GValues); 

   procedure Is_Requesting_Checkbox_Toggled(Object : access GObject_Record'class;
                                            Params : GValues);

   procedure Double_Click_On_Data_Element_Row(Object : access GObject_Record'class;
                                              Params : GValues);

   function Set_Button_Pressed(Object : access GObject_Record'class;
                               Params : GValues) return Boolean;

   function Set_Button_Released(Object : access GObject_Record'class;
                                Params : GValues) return Boolean;

   procedure Set_Value_Edited(Object : access GObject_Record'class;
                              Params : GValues);

   procedure Request_Period_Edited(Object : access GObject_Record'class;
                                   Params : GValues);

end Control_Panel;
