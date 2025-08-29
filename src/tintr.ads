--
with Gtk.Progress_Bar;   use Gtk.Progress_Bar;  --  Timer Interrupt
with Ada.Real_Time;      use Ada.Real_Time;     --  Timer Interrupt
with Glib.Main;          use Glib.Main;  --  T-Interrupt  Time_Cb.Timeout_Add
--
package Tintr is

   function Timer_Intr (Pbar : Gtk_Progress_Bar) return Boolean;

   TimerIntrDuration : Time_Span;  --  need with Ada.Real_Time
   TimerIntrCounter : Integer := 0;
   TINTR_PITCH : constant := 100;   --  100=100ms, 200=200ms
   TIMECNT_DRAW_PRESET : constant := 4;  --  In Draw, countdown TimeCnt preset
   TimeCnt : Integer := TIMECNT_DRAW_PRESET;   --  Playback Time/Location/Speed
   TimerIntrCounter_Before : Integer := 0;   --  In Draw, check TimeCnt
   TimeCnt_Draw   : Integer := 0;   --  how many Draw is accepted in a TimeCnt

   Direction : Integer := 0;  --  0=Positive(to right), 1=Negative (to Left)
   Datatype_dummy : Gtk_Progress_Bar;    --  Timer interrupt
   EndFlag : Integer;   --  Playback loop End Flag
   Gid_dummy   : G_Source_Id;     --  Timer interrupt
   --  0=stop,1=Single Start,2=Single busy, 3=Step Start,
   --  4=Step busy,5=Loop start,6=Loop busy
   Start_Flag : Integer := 0;
   Pause_Flag : Integer := 0;   --  default=0, Pause=1

end Tintr;
