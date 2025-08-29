
with Gtk.Box;          use Gtk.Box;
with Gtk.Main;      --   use Gtk.Main;
with Gtk.Window;       use Gtk.Window;
with Gtk.Frame;        use Gtk.Frame;
with Gtk.Widget;       use Gtk.Widget;
with Gtk.Progress_Bar; use Gtk.Progress_Bar;  --  Timer interrupt
--  with OpenAL.Types;     use OpenAL.Types;
with OpenAL.Context;
with Glib.Main;        use Glib.Main; --  Timer Interrupt  Time_Cb.Timeout_Add
--  with Gdk.Event;        use Gdk.Event;
with Audio;
with Gui;
with Tintr;
with Common;

procedure Gui_Doppler_Plane_1 is
--
   package Time_Cb  is new Glib.Main.Generic_Sources (Gtk_Progress_Bar);
--
   Win   : Gtk_Window;
   FrameM : Gtk_Frame;
   BoxM   : Gtk_Vbox;
--
begin
--  Initialize GtkAda.
   Gtk.Main.Init;

--  Create a window with a size of 400x400
   Gtk_New (Win);
   Win.Set_Default_Size (1100, 700);

--  Create a box to organize vertically the contents of the window
   Gtk_New_Vbox (BoxM);
   Win.Add (BoxM);

   Gtk_New (FrameM);
   BoxM.Add (FrameM);

   Gui.Run (FrameM);

--  Start Timer Interrupt
   Tintr.Gid_dummy := Time_Cb.Timeout_Add
           (Tintr.TINTR_PITCH, Tintr.Timer_Intr'Access, Tintr.Datatype_dummy);

--  End, close OpenAL device
   OpenAL.Context.Close_Device (Audio.CX_Devicet);  --  CLOSE "OpenAL Soft"

--  Stop the Gtk process when closing the window
   Win.On_Delete_Event (Common.Delete_Event_Cb'Unrestricted_Access);

--  Show the window and present it
   Win.Show_All;
   Win.Present;

--  Start the Gtk+ main loop
   Gtk.Main.Main;

end Gui_Doppler_Plane_1;
