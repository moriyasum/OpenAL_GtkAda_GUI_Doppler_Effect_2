
with Gtk.Box;         use Gtk.Box;
with Gtk.Label;       use Gtk.Label;
with Gtk.Widget;      use Gtk.Widget;
--  with Gtk.Main;        use Gtk.Main;
--  with Gtk.Window;      use Gtk.Window;
with Gtk.Frame;       use Gtk.Frame;
with Gtk.Button;          use Gtk.Button;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Handlers;        use Gtk.Handlers;
--  with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
--  with Gtk.Adjustment;      use Gtk.Adjustment;
with Gtk.Drawing_Area;    use Gtk.Drawing_Area;
with Gtk.Spin_Button;     use Gtk.Spin_Button;
with Gtkada.Canvas;       use Gtkada.Canvas;
--  with Gtkada.Canvas_View;  use Gtkada.Canvas_View;
--  with Gtkada.Canvas_View.Views; use Gtkada.Canvas_View.Views;
--  with Gtkada.Style;        use Gtkada.Style;

with Ada.Numerics.Discrete_Random;
--  with Ada.Numerics.Elementary_Functions;    --  For Sin
--  use  Ada.Numerics.Elementary_Functions;     --  For Sin
with Ada.Numerics;      use Ada.Numerics;
--  with Ada.Float_Text_IO; use Ada.Float_Text_IO;

with Gdk;               use Gdk;
--  with Gdk.Cairo;         use Gdk.Cairo;
with Gdk.RGBA;          use Gdk.RGBA;
with Gdk.Pixbuf;        use Gdk.Pixbuf;
with Glib;              use Glib;
--  with Glib.Error;        use Glib.Error;
with Glib.Object;       use Glib.Object;
with Cairo;             use Cairo;
--  with Cairo.Pattern;     use Cairo.Pattern;
--  with Cairo.Png;         use Cairo.Png;
--  with Cairo.Region;      use Cairo.Region;
--  with Cairo.Surface;     use Cairo.Surface;
--  with OpenAL.Context;    use OpenAL.Context;
--  with Ada.Text_IO;           use Ada.Text_IO;
--

package Gui is

-----------------------------
--  Misc. types and variables
-----------------------------
--   package Items_Random is new Ada.Numerics.Discrete_Random (Positive);
--   use Items_Random;

   Max_Size : constant := 400;
   subtype Coordinate_Type is Gint range Default_Grid_Size + 1 .. Max_Size;
   package Coordinate_Random is new
      Ada.Numerics.Discrete_Random (Coordinate_Type);
--   use Coordinate_Random;

   subtype Zoom_Type is Gint range 1 .. 2;
   package Zoom_Random is new Ada.Numerics.Discrete_Random (Zoom_Type);
--   use Zoom_Random;

   type FixedPoint0018 is delta 0.001 digits 8;
   FixPnt008 : FixedPoint0018;
   type FixedPoint0016 is delta 0.1 digits 6;
   FixPnt006 : FixedPoint0016;

----------------------------------------------------------------
--  Redefine our own item type, since we want to provide our own
--  graphics.
----------------------------------------------------------------
   type Display_Item_Record is new Gtkada.Canvas.Canvas_Item_Record with record
      Canvas : Interactive_Canvas;
      Color  : Gdk.RGBA.Gdk_RGBA;
      W, H   : Gint;
      Num    : Positive;
   end record;
   type Display_Item is access all Display_Item_Record'Class;
--
--
----------------------------------------------------
--  Our own canvas, with optional background image --
----------------------------------------------------
   type Image_Canvas_Record is new Interactive_Canvas_Record with record
      Background : Cairo_Pattern := Null_Pattern;
      Draw_Grid  : Boolean := True;
   end record;
   type Image_Canvas is access all Image_Canvas_Record'Class;

   type Image_Drawing_Record is new Gtk.Box.Gtk_Box_Record with record
      Area : Gtk.Drawing_Area.Gtk_Drawing_Area;
      PixG  : Gdk.Pixbuf.Gdk_Pixbuf;
   end record;

   type Image_Drawing is access all Image_Drawing_Record'Class;
   --  A special type of drawing area that can be associated with
   --  an image.

   package Canvas_Cb is new Gtk.Handlers.Callback
     (Interactive_Canvas_Record);
   package Canvas_User_Cb is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Image_Canvas);
--
--
------------------------
-- Callbacks packages --
------------------------
   package Expose_Cb is new Gtk.Handlers.Return_Callback
     (Image_Drawing_Record, Boolean);

   package Destroy_Cb is new Gtk.Handlers.Callback (Image_Drawing_Record);
--
--
-----------------------
--  PROTYPE
-----------------------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class);

   procedure Button_Single_Cb
     (Canvas : access Interactive_Canvas_Record'Class);
   procedure Button_Loop_Cb
     (Canvas : access Interactive_Canvas_Record'Class);
   procedure Button_Step_Cb
     (Canvas : access Interactive_Canvas_Record'Class);
   procedure Button_Clear_Cb
     (Canvas : access Interactive_Canvas_Record'Class);

   procedure Set_Button_Style
     (Button : Gtk_Button; Color : String); --  Added 8/25/2025

   overriding
   procedure Draw       --  Draw #1 *********
     (Item   : access Display_Item_Record;
      Cr     : Cairo_Context);

   procedure Initial_Plane_Item_Setup
     (Canvas : access Interactive_Canvas_Record'Class);

   procedure Initialize_Listener_Image
     (Draw   : out Image_Drawing;
      Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf;
      Title  : String);

   function On_Draw_Listener_Image
      (Draw : access Image_Drawing_Record'Class;
       Cr   : Cairo_Context) return Boolean;

   procedure Calculate_Display_Sound
     (Counter : Integer);

   procedure Display_Plane
     (Counter : Integer);

   procedure On_Listner_Loc (Spin : access Glib.Object.GObject_Record'Class);

--
-----------------------------
--  GLOBAL VARIABLES
-----------------------------
   Button_Single       : Gtk_Button;
   Button_Loop         : Gtk_Button;
   Button_Step         : Gtk_Button;
   Button_Clear        : Gtk_Button;
   ButtonColor         : Gdk_RGBA;
   Dummy_Boolean       : Boolean;

   Label_Dummy1  : Gtk_Label;   --  Left pane top for clearance
   Label_TCnt    : Gtk_Label;   --  Left pane,  TimeCnt
   Label_S2SndPos_X : Gtk_Label;   --  Left pane,  New position X
   Label_S2SndPos_Y : Gtk_Label;   --  Left pane,  New position Y
   Label_SndVel_X :   Gtk_Label;   --  Left pane,  Sound Veloctiy X
   Label_SndVel_Y :   Gtk_Label;   --  Left pane,  Sound Veloctiy Y
   Label_Speed_Angle : Gtk_Label;     --  Left pane,  Sound Angle
   Label_Start_Flag :  Gtk_Label;     --  Left pane,  Status

   OFFSET_X      : Gtk_Spin_Button;
   OFFSET_Y      : Gtk_Spin_Button;
   PIXFACTOR_X   : Gtk_Spin_Button;
   PIXFACTOR_Y   : Gtk_Spin_Button;
   SND_POS_FACTOR : Gtk_Spin_Button;   --  Sound Distance Factor
   SND_SPD_FACTOR : Gtk_Spin_Button;   --  Sound SPeed Factor
   TIME_CYCLE_X  : Gtk_Spin_Button;
   TIME_CYCLE_Y  : Gtk_Spin_Button;
   LPOS_X        : Gtk_Spin_Button;
   LPOS_Y        : Gtk_Spin_Button;
   Buf_LPOS_X    : Float;
   Buf_LPOS_Y    : Float;

   S1Pos_X  : Float := 99999.9;  --  @previous Timer Interrupt Position
   S1Pos_Y  : Float := 99999.9;  --  @previous Timer Interrupt Position
   S2Pos_X  : Float := 99999.9;  --  Source X Coordinate NEW, Normalized value
   S2Pos_Y  : Float := 99999.9;  --  Source Y Coordinate NEW, Normalized value
   S2PixPos_X  : Float;  --  Source Absolute Position, = S2Pos_X * PIXFACTOR_X
   S2PixPos_Y  : Float;  --  Source Absolute Position, = S2Pos_Y * PIXFACTOR_Y
   S2SndPos_X  : Float;   --  (S2Pos_X - Buf_LPOS_X) * SND_POS_FACTOR
   S2SndPos_Y  : Float;   --  (S2Pos_Y - Buf_LPOS_Y) * SND_POS_FACTOR
   Distance_SL : Float;   --  Distance Source-Listener * Sound_Factor
   Vel_Snd_X : Float;     --  SND_SPD_FACTOR * (X2-X1)/Intr_cycle
   Vel_Snd_Y : Float;     --  SND_SPD_FACTOR * (Y2-Y1)/Intr_cycle
   Speed_Angle : Float := 0.0;  --  Plane Angle [rad]
   Speed_NOM : Float;          --  Plane Speed Nominal Scalar
   Speed_Pix  : Float;         --  Plane Speed Pixel Scalar
   Time_X, Time_Y  : Float;    --  Figure Tracing Timer counter
   Draw_Listener_Counter : Integer := 0; --  Cnter Draw_Listener called by Sys
   Draw_Plane_Counter : Integer := 0;   --  Counter Draw_Plane called by System
   Step_Counter : Integer := 0; --  Set STEP mode, Counter Interrupts, downCnt
   STEP_NUMBER  : constant := 3;  --  For "Step". It presets Step_Counter
   PLANE_BODY_COLOR : constant Gdk.RGBA.Gdk_RGBA := (0.0, 0.0, 0.8, 1.0);

end Gui;
