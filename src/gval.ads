--  GLOBAL VALIABLES  (.ADS only)
with Gtk.Box;       use Gtk.Box;
with Gtk.Label;     use Gtk.Label;
with Gtk.Button;    use Gtk.Button;
with Gtk.Window;      use Gtk.Window;
with Gtk.Fixed;      use Gtk.Fixed;     --  To layout Button

with Ada.Real_Time;  use Ada.Real_Time;

with Gtk.Progress_Bar;    use Gtk.Progress_Bar;
--  with Glib;           use Glib;
--  with Glib.Object;    use Glib.Object;
--  with Gtk.Frame;      use Gtk.Frame;
--  with Ada.Text_IO;    use Ada.Text_IO;
--  with Gtk.Fixed;      use Gtk.Fixed;     --  To layout Button
--  with Gtk.Spin_Button; use Gtk.Spin_Button;
--  with Gtk.Toggle_Button; use Gtk.Toggle_Button;
with Gtk.Progress_Bar;    use Gtk.Progress_Bar;
--  with Ada.Real_Time;  use Ada.Real_Time;
with Glib.Main;    use Glib.Main;     --Timer Interrupt  Time_Cb.Timeout_Add
with OpenAL.Source;
with OpenAL.Context; use OpenAL.Context;
with OpenAL.Types;


package Gval is
   Win   : Gtk_Window;     --  Need with Gtk.Window
   Fix   : Gtk_Fixed;
   Box1      : Gtk_Box;
   Label1    : Gtk_Label;
   Label2    : Gtk_Label;
   Label3    : Gtk_Label;
   Button1   : Gtk_Button;
   Button2   : Gtk_Button;
--   Counter   : Integer := 0;


   CX_Devicet : OpenAL.Context.Device_t;  
   TimeCnt : Integer := 0;   --  Playback Time/Location/Speed Counter
   Direction : Integer := 0;  --  0=Positive(to right), 1=Negative (to Left)
   SetPosF, SetVelF : OpenAL.Types.Float_t;   --  Vector Calculation
   SourcePositionF, SourceVelocityF : Float;
   Sound_Source : OpenAL.Source.Source_t;
   Sound_Source_Array : OpenAL.Source.Source_Array_t (1..1);  
      
   Datatype_dummy : Gtk_Progress_Bar;    --  Timer interrupt
   Gid_dummy   : G_Source_Id;     --  Timer interrupt   
--   TIntr_Flag : Integer := 0;   --  0:before start 
   AudioFlag : Integer := 0;
   AudioLoopFlag : Integer := 0;
   TimerIntrDuration : Time_Span;  --  need with Ada.Real_Time
   TimerIntrCounter : integer := 0;
   TINTR_PITCH : Constant := 50;   --1  00=50ms, 200=100ms
   EndFlag : Integer;   --  Playback loop End Flag
   
end Gval; 

