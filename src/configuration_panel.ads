-----------------------------------------------------------------
--                                                             --
-- CONFIGURAITON_PANEL Specification                           --
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
with Gtk.Box;     use Gtk.Box;
with Glib.Object; use Glib.Object;

package Configuration_Panel is

   Box : Gtk_VBox;

   type Event_Callback is access procedure;

   procedure Create;
   -- Builds up the configuration panel VBox widget --

   procedure Assign_Event_Callbacks(Connect_Clicked    : in not null Event_Callback;
                                    Disconnect_Clicked : in not null Event_Callback);

   procedure Set_Connect_Button_Enabled(Enabled : Boolean);
   procedure Set_Disconnect_Button_Enabled(Enabled : Boolean);

   private

   procedure Connect_Clicked (Self : access GObject_Record'class);
   procedure Disconnect_Clicked (Self : access GObject_Record'class);

end Configuration_Panel;
