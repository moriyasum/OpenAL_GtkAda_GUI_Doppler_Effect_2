
with Audio;               use Audio;
with Tintr;               use Tintr;
with Gtk.Css_Provider;    use Gtk.Css_Provider;
with Gtk.Style_Context;   use Gtk.Style_Context;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with OpenAL.Context;    --  use OpenAL.Context;
with Glib.Error;        use Glib.Error;
with Gdk.Cairo;         use Gdk.Cairo;
with Ada.Float_Text_IO; --  use Ada.Float_Text_IO;
with Ada.Text_IO;           use Ada.Text_IO;

with Gtk.Adjustment;      use Gtk.Adjustment;
with Ada.Numerics.Elementary_Functions;    --  For Sin
use  Ada.Numerics.Elementary_Functions;     --  For Sin
--
--
package body Gui is

-------------------------------
--  Valiables
------------------------------
   Box          : Gtk_Box;
   HBox1, HBox2 : Gtk_Box;
   Scrolled     : Gtk_Scrolled_Window;
   CanvasG      : Image_Canvas;
   DrawPic      : Image_Drawing;   --  Picture

   VBoxL : Gtk_Box;   --  Left pane
   VBoxL1, VBoxL2 : Gtk_Box;  --  Left pane upper and lower boxes

   PixG     : Gdk_Pixbuf;
   Item0    : Display_Item;   --  DUMMY, but it is needed to work properly
   Item1    : Display_Item;   --  Plane Item. Plane is drawn on this Item.
--   Items_List : array (1 .. 500) of Gtkada.Canvas.Canvas_Item;

   Draw1_Before : Integer := 0;   --  for debug

-----------------------------------------------------------
--  Button_START SINGLE Callback
------------------------------------------------------------
   procedure Button_Single_Cb
     (Canvas : access Interactive_Canvas_Record'Class) is
      pragma Unreferenced (Canvas);
   begin

      Draw_Listener_Counter := 0;   --  Counter, photo Draw called for debug
      if Start_Flag = 0 then   --  Normal Start turns ON
--  If SINGLE button is pressed from OFF to ON then start up.
         ALSound_Initialize;
         S1Pos_X := 0.0;        --  Save for next Direction Calc
         S1Pos_Y := 0.0;        --  Save for next Direction Calc
         Start_Flag := 1;
         Pause_Flag := 0;
         Set_Label (Button_Single, "Single");
         Set_Button_Style (Button_Single, "Blue");
         --  Button Character Color is switched to Blue

      elsif Start_Flag = 1 or else Start_Flag = 2 then --  working/Pausing
         if Pause_Flag = 0 then
--  Switch SINGLE to PAUSE
            Pause_Flag := 1;    --  PAUSE
            Set_Label (Button_Single, "Single");
            Set_Button_Style (Button_Single, "Red");
            --  Button Character Color is switched to Red

         else   --  Now PAUSE, then switch to continue Single again
--  Switch PAUSE to SINGLE (Re-starting)
            Pause_Flag := 0;
            Set_Label (Button_Single, "Single");
            Set_Button_Style (Button_Single, "Blue");
            --  Button Character Color is switched to Blue,
         end if;

      else   --  In the other modes and pressed Single button
--  Switch to SINGLE mode and go again
         Pause_Flag := 0;
         Start_Flag := 2;   --  SINGLE and CONTINUE flag
   --  If button is pressed from ON to OFF then stop
         Set_Label (Button_Single, "Single");
            --  Button Character Color is switched to Red
            Set_Button_Style (Button_Single, "Blue");

         Set_Label (Button_Loop, "Loop");  --  Turn Loop button to Black
         Set_Button_Style (Button_Loop, "Black");
            --  Button Character Color is switched to Red, Modify is obsolete
         Set_Label (Button_Step, "Step");
         Set_Button_Style (Button_Step, "Black");
            --  Button Character Color is switched to Red, Modify is obsolete
--      OpenAL.Context.Close_Device (Audio.CX_Devicet);  -- CLOSE "OpenAL Soft"
      end if;
--      Show_All (CanvasG);

   end Button_Single_Cb;
--
--
-----------------------------------------------------------
--  Button_START LOOP Callback
------------------------------------------------------------
   procedure Button_Loop_Cb
     (Canvas : access Interactive_Canvas_Record'Class) is
      pragma Unreferenced (Canvas);
   begin
      Draw_Listener_Counter := 0;  --  Counter, photo Draw called for debug
      if Start_Flag = 0 then   --  Normal Start turns ON
--  If button is pressed from OFF to ON then start up.
         ALSound_Initialize;
         S1Pos_X := 0.0;   --  Previous locaction X = 0
         S1Pos_Y := 0.0;   --  Previous locaction Y = 0
         Start_Flag := 5;
         Pause_Flag := 0;
         Set_Label (Button_Loop, "Loop");
--  Button Character Color is switched to Red
         Set_Button_Style (Button_Loop, "Blue");

      elsif Start_Flag = 5 or else Start_Flag = 6 then --  LOOPing/Pausing LOOP
         if Pause_Flag = 0 then
      --  Not PAUSE, then Switch LOOP to PAUSE
            Pause_Flag := 1;    --  PAUSE
            Set_Label (Button_Loop, "Loop");
            Set_Button_Style (Button_Loop, "Red");
            --  Button Character Color is switched to Red, Modify is obsolete

         else   --  Now PAUSE, then switch to continue Single again
--  PAUSE, then Switch PAUSE to LOOP
            Pause_Flag := 0;
            Set_Label (Button_Loop, "Loop");
            Set_Button_Style (Button_Loop, "Blue");
            --  Button Character Color is switched to Red, Modify is obsolete
         end if;

      else   --  In the other modes and pressed LOOP button
--  Switch to LOOP mode and go again
         Pause_Flag := 0;
         Start_Flag := 6;   --  LOOP and CONTINUE flag
--  If button is pressed from ON to OFF then stop
         Set_Label (Button_Single, "Single");
         Set_Button_Style (Button_Single, "Black");
         --  Button Character Color is switched to Red, Modify is obsolete
         Set_Label (Button_Loop, "Loop");
         Set_Button_Style (Button_Loop, "Blue");
         --  Button Character Color is switched to Red, Modify is obsolete
         Set_Label (Button_Step, "Step");
         Set_Button_Style (Button_Step, "Black");
         --  Button Character Color is switched to Red, Modify is obsolete
--      OpenAL.Context.Close_Device (Audio.CX_Devicet);  -- CLOSE "OpenAL Soft"
      end if;
--    Show (Gval.Box);
--    Show_All (CanvasG);
   end Button_Loop_Cb;
--
--
------------------------------------------------------------
--  Button_STEP Callback
--  NOTE STEP cannot stop
------------------------------------------------------------
   procedure Button_Step_Cb
     (Canvas : access Interactive_Canvas_Record'Class) is
      pragma Unreferenced (Canvas);
   begin
      Draw_Listener_Counter := 0;  --  Counter photo Draw called for debug
      if Start_Flag = 0 then   --  Normal Start turns ON
--  If button is pressed from OFF to ON then start STEP.
         ALSound_Initialize;
         S1Pos_X := 0.0;   --  Previous locaction X = 0
         S1Pos_Y := 0.0;   --  Previous locaction Y = 0
         Step_Counter := STEP_NUMBER;   --  Preset down-counter
         Start_Flag := 3;      --  Flag=3: Start STEP
         Pause_Flag := 0;      --  Reset WAIT counter
         Set_Label (Button_Step, "Step");
         Set_Button_Style (Button_Step, "Blue");
            --  Button Character Color is switched to Red, Modify is obsolete
      else
--  Pressed STEP button at Working any mode or HALT, then Re-start STEP
         Start_Flag := 3;      --  Flag=3: Start STEP
         Pause_Flag := 0;      --  Reset WAIT counter
         Step_Counter := STEP_NUMBER;   --  Preset Down-counter
         Set_Label (Button_Loop, "Loop");
         Set_Button_Style (Button_Loop, "Black");
         --  Button Character Color is switched to Red, Modify is obsolete
         Set_Label (Button_Single, "Single");
         Set_Button_Style (Button_Single, "Black");
         --  Button Character Color is switched to Red, Modify is obsolete
         Set_Label (Button_Step, "Step");
         Set_Button_Style (Button_Step, "Blue");
         --  Button Character Color is switched to Red, Modify is obsolete
      end if;

   end Button_Step_Cb;
--
--
--------------------------------------------------------
--  Button CLEAR Callback
--------------------------------------------------------
   procedure Button_Clear_Cb
     (Canvas : access Interactive_Canvas_Record'Class) is
      pragma Unreferenced (Canvas);

   begin
      Start_Flag := 0;
      Pause_Flag := 0;
      Step_Counter := 0;
      TimeCnt := 0;
      S1Pos_X := 0.0;   --  Previous locaction X = 0
      S1Pos_Y := 0.0;   --  Previous locaction Y = 0

      TimerIntrCounter_Before := 0;  --  Used in Tintr
      Set_Label (Gui.Button_Single, "Single");
      Set_Button_Style (Button_Single, "Black");
      --  Button Character Color is switched to Red, Modify is obsolete
      Set_Label (Gui.Button_Loop, "Loop");
      Set_Button_Style (Button_Loop, "Black");
      --  Button Character Color is switched to Red, Modify is obsolete
      OpenAL.Context.Close_Device (Audio.CX_Devicet);  --  CLOSE and stop sound
      Set_Label (Button_Step, "Step");
      Set_Button_Style (Button_Step, "Black");
      --  Button Character Color is switched to Red, Modify is obsolete

      Display_Plane (10);

      Put_Line ("TimeCnt=" & Integer'Image (TimeCnt) &
                  " S2PixPos_X= " & Float'Image (S2PixPos_X) &
                  "   S2PixPos_Y" & Float'Image (S2PixPos_Y));

   end Button_Clear_Cb;
--
--
--------------------------------------------------------
--  Change Button's Character Color
--  How to use:
--    Set_Label (Button_Single, "Single");
--    Set_Button_Style (Button_Single, "blue");
--  This procedure serves as a replacement for the deprecated 'Override_Color'.
--  with Gtk.Css_Provider;
--  with Gtk.Style_Context;
--  with Gtk.Style_Provider;
--  with Glib; use Glib;
--------------------------------------------------------
   procedure Set_Button_Style (Button : Gtk_Button; Color : String) is
      Provider : Gtk_Css_Provider;
      Context  : Gtk_Style_Context;
      Error    : aliased GError;
      Success  : Boolean;
      CSS_Data : constant String :=
        "button { color: " & Color & "; }";
   begin
   --  Setup CSS Provider
      Gtk_New (Provider);

   --  Load CSS Data
      Success := Load_From_Data (Provider, CSS_Data, Error'Access);

   --  Get Button's Style Context if success
      if Success then
         Context := Get_Style_Context (Gtk_Widget (Button));
         Add_Provider (Context, +Provider, 600);

      end if;
   end Set_Button_Style;
--
--
------------------------------------------------------------
--  Draw_#1   To_Double_Buffer -- Regular Rectangle
--  Plane Drawing
------------------------------------------------------------
   overriding
   procedure Draw
     (Item   : access Display_Item_Record;
      Cr     : Cairo_Context) is

      Iw, Ih, Iwc, Ihc : Gdouble;
      Ang :     Float;
      Icolor  : Gdk.RGBA.Gdk_RGBA;

   begin
      if TimerIntrCounter = TimerIntrCounter_Before then
         if TimeCnt_Draw = 0 then
            goto END_OF_DRAW;
         end if;   --  SKIP if counter is full (protect from too much display)
         TimeCnt_Draw := TimeCnt_Draw - 1;
      else
         TimeCnt_Draw := TIMECNT_DRAW_PRESET;   --  Initialize down-counter
      end if;

      Ang := Speed_Angle;
      --  Radian, Angle from Origin (positive X axis=0 deg,CW=Positive,Radian)

      Iw := Gdouble (Item.W);   --  Image Width
      Ih := Gdouble (Item.H);   --  Image Height
      Iwc := 0.5 * Iw;         --  Image Rotation Center=a half Width
      Ihc := 0.5 * Ih;         --  Image Rotation Center=a half Height
      Icolor := Item.Color;    --  Save body color, Gdk.RGBA.Gdk_RGBA

--  Draw exhaust flame Lower Half
      Arc (Cr,
           Iwc + 0.325 * Iw * Gdouble (Cos (3.5358 + Ang)),
           Ihc + 0.325 * Iw * Gdouble (Sin (3.5358 + Ang)),
           0.25 * Iw,
           Gdouble (0.5235 + Ang),
           Gdouble (2.618 + Ang));
      Set_Source_Rgb (Cr, 1.0, 0.5, 0.5);   --  Red
      Cairo.Fill (Cr);      --  Fill color
--  Draw exhaust flame Upper Half
      Arc (Cr,
           Iwc + 0.325 * Iw * Gdouble (Cos (2.7462 + Ang)),
           Ihc + 0.325 * Iw * Gdouble (Sin (2.7462 + Ang)),
           0.25 * Iw,
           Gdouble (3.663 + Ang),
           Gdouble (5.757 + Ang));
      Set_Source_Rgb (Cr, 1.0, 0.5, 0.5);
      Cairo.Fill (Cr);      --  Fill color

--  Draw Jet nozzle Circle
      Item.Color := (1.0, 0.0, 0.0, 1.0);   --  Red color
      Gdk.Cairo.Set_Source_RGBA (Cr, Item.Color);
      Arc (Cr,
           Iwc + 0.2 * Iw * Gdouble (Cos (Pi + Ang)),
           Ihc + 0.2 * Ih * Gdouble (Sin (Pi + Ang)),
           0.1 * Iw,
           0.0,
           2.0 * Pi);
      Cairo.Fill (Cr);       --  Fill without outline

      pragma Style_Checks (Off);  --  *********************************
--  Plane body
      Move_To (Cr, Iwc+0.45*Iw*GDouble(cos(Ang)), Ihc+0.45*Ih*GDouble(sin(Ang)));   --  #1 Move to the Top
      Line_To (Cr, Iwc+0.4060*Iw*GDouble(cos(0.1732+Ang)), Ihc+0.4060*Ih*GDouble(sin(0.1732+Ang)));   --  #2 Draw Lower Half
      Line_To (Cr, Iwc+0.3569*Iw*GDouble(cos(0.1974+Ang)), Ihc+0.3569*Ih*GDouble(sin(0.1974+Ang)));   --  #3
      Line_To (Cr, Iwc+0.3640*Iw*GDouble(cos(0.2783+Ang)), Ihc+0.3640*Ih*GDouble(sin(0.2783+Ang)));   --  #4
      Line_To (Cr, Iwc+0.3162*Iw*GDouble(cos(0.3218+Ang)), Ihc+0.3162*Ih*GDouble(sin(0.3218+Ang)));   --  #5
      Line_To (Cr, Iwc+0.3162*Iw*GDouble(cos(1.2490+Ang)), Ihc+0.3162*Ih*GDouble(sin(1.2490+Ang)));   --  #6
      Line_To (Cr, Iwc+0.3000*Iw*GDouble(cos(1.5700+Ang)), Ihc+0.3000*Ih*GDouble(sin(1.5700+Ang)));   --  #7  90'
      Line_To (Cr, Iwc+0.1414*Iw*GDouble(cos(0.7850+Ang)), Ihc+0.1414*Ih*GDouble(sin(0.7850+Ang)));   --  #8
      Line_To (Cr, Iwc+0.1000*Iw*GDouble(cos(1.5700+Ang)), Ihc+0.1000*Ih*GDouble(sin(1.5700+Ang)));   --  #9  90'
      Line_To (Cr, Iwc+0.2236*Iw*GDouble(cos(2.0330+Ang)), Ihc+0.2236*Ih*GDouble(sin(2.0330+Ang)));   --  #10
      Line_To (Cr, Iwc+0.2500*Iw*GDouble(cos(2.2120+Ang)), Ihc+0.2500*Ih*GDouble(sin(2.2120+Ang)));   --  #11
      Line_To (Cr, Iwc+0.1581*Iw*GDouble(cos(2.8180+Ang)), Ihc+0.1581*Ih*GDouble(sin(2.8180+Ang)));   --  #12
      Line_To (Cr, Iwc+0.1118*Iw*GDouble(cos(2.6760+Ang)), Ihc+0.1118*Ih*GDouble(sin(2.6760+Ang)));   --  #13
      Line_To (Cr, Iwc+0.1000*Iw*GDouble(cos(3.1410+Ang)), Ihc+0.1000*Ih*GDouble(sin(3.1410+Ang)));   --  #14  Tail center

      Move_To(Cr, Iwc, Ihc);   --  Move to the Center, the Center is always the same position
      Move_To (Cr, Iwc+0.45*Iw*GDouble(cos(Ang)), Ihc+0.45*Ih*GDouble(sin(Ang)));   --#1 Move to the Top
      Line_To (Cr, Iwc+0.406*Iw*GDouble(cos(2.0*Pi-0.1732+Ang)), Ihc+0.406*Ih*GDouble(sin(2.0*Pi-0.1732+Ang)));   --  #2  Draw upper half
      Line_To (Cr, Iwc+0.3569*Iw*GDouble(cos(2.0*Pi-0.1974+Ang)), Ihc+0.3569*Ih*GDouble(sin(2.0*Pi-0.1974+Ang)));   --  #3
      Line_To (Cr, Iwc+0.3640*Iw*GDouble(cos(2.0*Pi-0.2783+Ang)), Ihc+0.3640*Ih*GDouble(sin(2.0*Pi-0.2783+Ang)));   --  #4
      Line_To (Cr, Iwc+0.3162*Iw*GDouble(cos(2.0*Pi-0.3218+Ang)), Ihc+0.3162*Ih*GDouble(sin(2.0*Pi-0.3218+Ang)));   --  #5
      Line_To (Cr, Iwc+0.3162*Iw*GDouble(cos(2.0*Pi-1.249+Ang)), Ihc+0.3162*Ih*GDouble(sin(2.0*Pi-1.249+Ang)));   --  #6
      Line_To (Cr, Iwc+0.3000*Iw*GDouble(cos(2.0*Pi-1.5700+Ang)), Ihc+0.3000*Ih*GDouble(sin(2.0*Pi-1.5700+Ang)));   --  #7  270'
      Line_To (Cr, Iwc+0.1414*Iw*GDouble(cos(2.0*Pi-0.785 +Ang)), Ihc+0.1414*Ih*GDouble(sin(2.0*Pi-0.7850+Ang)));   --  #8
      Line_To (Cr, Iwc+0.1000*Iw*GDouble(cos(2.0*Pi-1.570+Ang)), Ihc+0.1000*Ih*GDouble(sin(2.0*Pi-1.5700+Ang)));   --  #9  270'
      Line_To (Cr, Iwc+0.2236*Iw*GDouble(cos(2.0*Pi-2.033+Ang)), Ihc+0.2236*Ih*GDouble(sin(2.0*Pi-2.033+Ang)));   --  #10
      Line_To (Cr, Iwc+0.2500*Iw*GDouble(cos(2.0*Pi-2.212+Ang)), Ihc+0.2500*Ih*GDouble(sin(2.0*Pi-2.212+Ang)));   --  #11
      Line_To (Cr, Iwc+0.1581*Iw*GDouble(cos(2.0*Pi-2.818+Ang)), Ihc+0.1581*Ih*GDouble(sin(2.0*Pi-2.818+Ang)));   --  #12
      Line_To (Cr, Iwc+0.1118*Iw*GDouble(cos(2.0*Pi-2.676+Ang)), Ihc+0.1118*Ih*GDouble(sin(2.0*Pi-2.676+Ang)));   --  #13
      Line_To (Cr, Iwc+0.1000*Iw*GDouble(cos(2.0*Pi-3.141+Ang)), Ihc+0.1000*Ih*GDouble(sin(2.0*Pi-3.141+Ang)));   --  #14

      pragma Style_Checks (On);  --  *********************************

      Close_Path (Cr);   --  Close Polygon to fill color

      Item.Color := Icolor;                       --  Return to the saved color
      Gdk.Cairo.Set_Source_RGBA (Cr, Item.Color); --  Paint the plane body
--    Gdk.Cairo.Set_Source_RGBA (Cr, Icolor);     --  NOTE: This does not work

      Cairo.Fill (Cr);    --  Fill color
      --  Just to countup calling this Draw how many times
      Draw_Plane_Counter := Draw_Plane_Counter + 1;

<< END_OF_DRAW >>   --  GOTO Jump Flag
      TimerIntrCounter_Before := TimeCnt;

   end Draw;
--
--
------------------------------------------------------------------
--  On_Draw_Listener_Image
--  This On_Draw_Listener_Image is coming from libart_demo.adb
--  Display Picture image in LEFT VBoxL or RIGHT Canvas
------------------------------------------------------------------
   function On_Draw_Listener_Image
      (Draw : access Image_Drawing_Record'Class;
       Cr   : Cairo_Context) return Boolean is
   begin
      Put_Line ("Width=" & Gint'Image (Get_Width (Draw.PixG)) &
                        "   Height=" & Gint'Image (Get_Height (Draw.PixG)));

      Set_Source_Pixbuf (Cr, Draw.PixG, 0.0, 0.0);

      Cairo.Paint (Cr);      --  Need to display picture

      Cairo.Stroke (Cr);

      Draw_Listener_Counter := Draw_Listener_Counter + 1;
      return False;

   end On_Draw_Listener_Image;
--
--
-----------------------------------------------------------
--  Initialize_Listener_Image -- to display Picture image
--   Called from "Run"
-----------------------------------------------------------
   procedure Initialize_Listener_Image
     (Draw   : out Image_Drawing;   --  DrawPic
      Pixbuf : Gdk_Pixbuf;          --  PixG
      Title  : String)  is          --  "Initial Image"
      Label : Gtk_Label;
   begin
      Draw := new Image_Drawing_Record;
      Initialize_Vbox (Draw, Homogeneous => False, Spacing => 0);

      Gtk_New (Label, Title);
      Pack_Start (Draw, Label, Expand => False, Fill => False);
      Draw.PixG := Pixbuf;    --  Picture
      Set_Size_Request
        (Draw,
         Get_Width (Draw.PixG),
         Get_Height (Draw.PixG) + Get_Allocated_Height (Label));

      Gtk_New (Draw.Area);
      Pack_Start (Draw, Draw.Area);

      Expose_Cb.Object_Connect
         (Draw.Area, Signal_Draw,
         Expose_Cb.To_Marshaller (On_Draw_Listener_Image'Access),
         Slot_Object => Draw);

   end Initialize_Listener_Image;
--
--
---------------------------------------------------------------
--  Initialize Display Plane Items
---------------------------------------------------------------
   procedure Initial_Plane_Item_Setup
     (Canvas : access Interactive_Canvas_Record'Class) is

   begin
--  Place a Dummy Item ***************************
--  Workaround for the first location (0,0) BUG and another BUG
      Item0 := new Display_Item_Record;
      Item0.Canvas := Interactive_Canvas (Canvas);
      Item0.Color := (1.0, 1.0, 1.0, 0.0);       --  RGBA Color=White
      Item0.W := 0;                              --  Width=0
      Item0.H := 0;                              --  Height=0
      Set_Screen_Size (Item0, Item0.W, Item0.H);
      Put (Canvas, Item0, 0, 0);               --  Dummy PUT, Location X=0,Y=0
--  ***********************************************

      Item1 := new Display_Item_Record;   --  Draw #1
      Item1.Canvas := Interactive_Canvas (Canvas);
      Item1.Color := PLANE_BODY_COLOR;    --  Color: Gdk.RGBA.Gdk_RGBA
      Item1.W := 140;   --  Figure W size
      Item1.H := 140;   --  Figure H size
      Item1.Num := 1;
      --  Item drawing area, wider than figure to rotate
      Set_Screen_Size (Item1, 150, 150);

   end Initial_Plane_Item_Setup;
--
--
--------------------------------------------------------------
--  Calculate all Parameters of Source, Move and Display
--------------------------------------------------------------
   procedure Display_Plane
     (Counter : Integer) is
   begin
      Calculate_Display_Sound (Counter);   --  Calculate all

      Move_To (CanvasG, Item1, Glib.Gint (S2PixPos_X), Glib.Gint (S2PixPos_Y));
      --  Important. Don't use Put for an Item multiple times.

   --  Force canvas refresh - this is the key fix,  Added 8/27/2025
      Refresh_Canvas (CanvasG);

      Show_All (CanvasG);
   end Display_Plane;

--------------------------------------------------------------
--  Calculate all Plane Location and Audio Parameters
--------------------------------------------------------------
   procedure Calculate_Display_Sound
     (Counter : Integer) is             --  TimeCnt
   pragma Unreferenced (Counter);
   pragma Style_Checks (Off);  --  *******************************
   Just_Begin_Flag : Boolean;

   Begin
      if (Pause_Flag /= 0) then Return; end if;

      if (S1Pos_X=0.0 and S1Pos_Y=0.0) then
         --  If this is the first call, then set flag to skip some calculatios
        Just_Begin_Flag := True;
      else
        Just_Begin_Flag := False;
      end if;

   --  Calculate Current P2 Location
      S2Pos_X := float (Get_Value (OFFSET_X)) + Sin (2.0 * Pi / float (Get_Value (TIME_CYCLE_X)) * float (TINTR_PITCH) / 1000.0 * Time_X);   --  Normalized  1+Sin(wt)
      S2PixPos_X := float (Get_Value (PIXFACTOR_X)) * S2Pos_X;              --  For Display Absolute
      S2SndPos_X := float (Get_Value (SND_POS_FACTOR)) * (S2Pos_X - Buf_LPOS_X);   --  For Sound  Relative S-L

      S2Pos_Y := float (Get_Value (OFFSET_Y)) + Cos (2.0 * Pi / float (Get_Value (TIME_CYCLE_Y)) * float (TINTR_PITCH) / 1000.0 * Time_Y);   --  Normalized  2+Cos(wt)
      S2PixPos_Y := float (Get_Value (PIXFACTOR_Y)) * S2Pos_Y;              --  For Display Absolute
--   S2SndPos_Y := float(Get_Value(SND_POS_FACTOR)) * (Buf_LPOS_Y - S2Pos_Y);   --  For Sound Relative S-L
      S2SndPos_Y := float (Get_Value (SND_POS_FACTOR)) * (S2Pos_Y - Buf_LPOS_Y);   --  For Sound Relative S-L

      if (Just_Begin_Flag = False) then
 --  Not the first time, then calculate speed and direction
         Vel_Snd_X := float (Get_Value (SND_SPD_FACTOR)) * (S2Pos_X - S1Pos_X) * 1000.0 / float (TINTR_PITCH);   --  For Sound_SPD_Factor * (X2-X1)/Intr_cycle
         Vel_Snd_Y := float (Get_Value (SND_SPD_FACTOR)) * (S2Pos_Y - S1Pos_Y) * 1000.0 / float (TINTR_PITCH);   --  For Sound_SPD_Factor * (Y2-Y1)/Intr_cycle

         if (Abs (S2Pos_X - S1Pos_X) <= 0.0001) then   --  To avoid 0 division
            if (S2Pos_Y > S1Pos_Y) then
               Speed_Angle := 0.5 * Pi;   --  Pi/2rad=90deg, Protect Zero division
            else
               Speed_Angle := 1.5 * Pi;   --  4.712rad=270 deg, Protect Zero division
            end if;
         else   --  Not 0, then normal calculatio
            Speed_Angle := Abs (Arctan ((S2Pos_Y-S1Pos_Y) / (S2Pos_X - S1Pos_X)));  --  Right Lower(no correction)
            if ((S2Pos_Y - S1Pos_Y) < 0.0 and (S2Pos_X - S1Pos_X )> 0.0) then   --  Right upper
               Speed_Angle := 2.0 * Pi - Speed_Angle;
            elsif ((S2Pos_Y - S1Pos_Y) >= 0.0 and (S2Pos_X - S1Pos_X) < 0.0) then   --  Left Lower
               Speed_Angle := Pi - Speed_Angle;
            elsif ((S2Pos_Y - S1Pos_Y) < 0.0 and (S2Pos_X - S1Pos_X) < 0.0) then   --  Left Upper
               Speed_Angle := Pi + Speed_Angle;
            end if;
         end if;

      else   --  First time, INITIAL, just started
         Speed_Angle := 0.0;
         Vel_Snd_X := 0.0;
         Vel_Snd_Y := 0.0;
      end if;

      Distance_SL := SQRT (S2SndPos_X ** 2 + S2SndPos_Y ** 2);  --  Use Sound,because Relative

      S1Pos_X := S2Pos_X;        --  Save for next Direction Calc
      S1Pos_Y := S2Pos_Y;        --  Save for next Direction Calc

   --  Display Labels
      Set_Label (Gui.Label_TCnt, "TimeCnt=" & Integer'Image (TimeCnt));

      FixPnt006 := FixedPoint0016 (S2SndPos_X);
      Set_Label (Gui.Label_S2SndPOS_X, "SndPos_X=" & FixedPoint0016'Image (FixPnt006));

      FixPnt006 := FixedPoint0016 (S2SndPos_Y);
      Set_Label (Gui.Label_S2SndPos_Y, "SndPos_Y=" & FixedPoint0016'Image (FixPnt006));

      FixPnt006 := FixedPoint0016 (Vel_Snd_X);
      Set_Label (Gui.Label_SndVel_X, "SndVel_X=" & FixedPoint0016'Image (FixPnt006));

      FixPnt006 := FixedPoint0016 (Vel_Snd_Y);
      Set_Label (Gui.Label_SndVel_Y, "SndVel_Y=" & FixedPoint0016'Image (FixPnt006));

      FixPnt008 := FixedPoint0018 (Speed_Angle);
      Set_Label (Gui.Label_Speed_Angle, "Angle=" & FixedPoint0018'Image (FixPnt008));

      Set_Label (Gui.Label_Start_Flag, "Start_Flag=" & Integer'Image (Start_Flag));

--  Display on Text Screen
     Put ("TimeCnt=" & Integer'Image (TimeCnt));
     Put ("   Pos_X=");
     Ada.Float_Text_IO.Put (S2Pos_X, 4, 2, 0);   --  Item, Fore, Aft, Exp
     Put ("   Pos_Y=");
     Ada.Float_Text_IO.Put (S2Pos_Y, 4, 2, 0);   --  Item, Fore, Aft, Exp
     Put ("   L=");
     Ada.Float_Text_IO.Put (Distance_SL, 4, 2, 0);   --  Item, Fore, Aft, Exp
     Put_Line ("   Draw1=" & Integer'Image (Draw_Plane_Counter) & "  Diff=" & Integer'Image (Draw_Plane_Counter - Draw1_Before)
        &  "   Pause_F=" & Integer'Image (Pause_Flag));

     Draw1_Before := Draw_Plane_Counter;
  pragma Style_Checks (On);  --  *******************************
   end Calculate_Display_Sound;
--
--
-------------------------------------------------------------------
--  Set Listner location, set Spins LPOS_X & LPOS_Y to Buffers
-------------------------------------------------------------------
   procedure On_Listner_Loc (Spin : access GObject_Record'Class) is
      pragma Unreferenced (Spin);
--      S : constant Gtk_Spin_Button := Gtk_Spin_Button (Spin);
      X, Y       : Glib.Gint;
   begin
      Buf_LPOS_X := Float (Get_Value (LPOS_X));
      X := Glib.Gint (Buf_LPOS_X * Float (Get_Value (PIXFACTOR_X)));
      Buf_LPOS_Y := Float (Get_Value (LPOS_Y));
      Y := Glib.Gint (Buf_LPOS_Y * Float (Get_Value (PIXFACTOR_Y)));
      --  To move the picture NOT "Cairo.Move_To" , use this Move command
      Move (CanvasG, DrawPic, X, Y);
   end On_Listner_Loc;
--
--
-------------------------------------------------------
-- Run --
-------------------------------------------------------
   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is

      Label      : Gtk_Label;
      Error      : Glib.Error.GError;
      X, Y       : Glib.Gint;
      TempBox    : Gtk_Box;           --  Spin setting
      TempLabel  : Gtk_Label;         --  Spin label setting
      TempAdj    : Gtk_Adjustment;    --  Spin setting

   begin
      Gtk_New_Vbox (Box, Homogeneous => False);   --  All screen
      Add (Frame, Box);                           --  Box.Add
   --  Top Buttons Box, if True:Wide buttons, Faluse:Narrow
      Gtk_New_Hbox (HBox1, Homogeneous => False);
      Pack_Start (Box, HBox1, Expand => False, Fill => False);
   --  L pane +R Canvas. False:narrow, True: 50%/50%
      Gtk_New_Hbox (HBox2, Homogeneous => False);
      Pack_Start (Box, HBox2, Expand => True, Fill => True);
   --  LEFT Pane Box, If True then VBoxL1/VBoxL2=50%/50%
      Gtk_New_Vbox (VBoxL, Homogeneous => False);
   --  30 is VBoxL1/VBoxL2 portion of VBox2 Width
      Pack_Start (HBox2, VBoxL, False, True, 30);

   --  LEFT Pane Upper status area Box
      Gtk_New_Vbox (VBoxL1, Homogeneous => False);
      Pack_Start (VBoxL, VBoxL1, False, True, 0);
   --  LEFT Pane Lower Spin area Box
      Gtk_New_Vbox (VBoxL2, Homogeneous => False);
      Pack_Start (VBoxL, VBoxL2, False, True, 0);

      Gtk_New (Scrolled);     --  RIGHT Canvas Box
   --  Main-RIGHT Canvas is all remained area (wide)
      Pack_Start (HBox2, Scrolled, True, True);

      CanvasG := new Image_Canvas_Record;
      Gtkada.Canvas.Initialize (CanvasG);
      Add (Scrolled, CanvasG);
--  "Single" Button
      Gtk_New (Button_Single, "Single");
      Pack_Start (HBox1, Button_Single, Expand => False, Fill => True);
      Canvas_Cb.Object_Connect
         (Button_Single, "clicked",
          Canvas_Cb.To_Marshaller (Button_Single_Cb'Access), CanvasG);
--  "Loop" Button
      Gtk_New (Button_Loop, "Loop");
      Pack_Start (HBox1, Button_Loop, Expand => False, Fill => True);
      Canvas_Cb.Object_Connect
         (Button_Loop, "clicked",
          Canvas_Cb.To_Marshaller (Button_Loop_Cb'Access), CanvasG);
--  "Step" Button
      Gtk_New (Button_Step, "Step");
      Pack_Start (HBox1, Button_Step, Expand => False, Fill => True);
      Canvas_Cb.Object_Connect
         (Button_Step, "clicked",
          Canvas_Cb.To_Marshaller (Button_Step_Cb'Access), CanvasG);
--  "Clear" Button
      Gtk_New (Button_Clear, "Clear");
      Pack_Start (HBox1, Button_Clear, Expand => False, Fill => True);
      Canvas_Cb.Object_Connect
         (Button_Clear, "clicked",
          Canvas_Cb.To_Marshaller (Button_Clear_Cb'Access), CanvasG);

--  Left Pane Labels (Upper VBoxL1)
      Gtk_New (Label_Dummy1, "   ");   --  Dummy for clearance
      Pack_Start (VBoxL1, Label_Dummy1, Expand => False, Fill => True);
      Gtk_New (Label_TCnt, "TimeCnt=        ");

      Pack_Start (VBoxL1, Label_TCnt, Expand => False, Fill => True);
      Gtk_New (Label_S2SndPos_X, "SndPos_X=        ");
      Pack_Start (VBoxL1, Label_S2SndPos_X, Expand => False, Fill => True);

      Gtk_New (Label_S2SndPos_Y, "SndPos_Y=        ");

      Pack_Start (VBoxL1, Label_S2SndPos_Y, Expand => False, Fill => True);
      Gtk_New (Label_SndVel_X, "SndVel_X=        ");
      Pack_Start (VBoxL1, Label_SndVel_X, Expand => False, Fill => True);

      Gtk_New (Label_SndVel_Y, "SndVel_Y=        ");
      Pack_Start (VBoxL1, Label_SndVel_Y, Expand => False, Fill => True);

      Gtk_New (Label_Speed_Angle, "Angle=        ");
      Pack_Start (VBoxL1, Label_Speed_Angle, Expand => False, Fill => True);

      Gtk_New (Label_Start_Flag, "Start_Flag=        ");
      Pack_Start (VBoxL1, Label_Start_Flag, Expand => False, Fill => True);

      Gtk_New (Label_Dummy1, "   ");   --  Dummy for clearance
      Pack_Start (VBoxL1, Label_Dummy1, Expand => False, Fill => True);

   --  "OFFSET_X" Spin   LEFT Pane upper status area Box
      Gtk_New_Vbox (TempBox, Homogeneous => False);
      Gtk_New (TempLabel, "OFFSET_X");
      Pack_Start (TempBox, TempLabel, Expand => False, Fill => False);
   --  Initial, Min, Max, Step, Page (Gtk.Adjustment)
      Gtk_New (TempAdj, 1.0, 0.2, 3.0, 0.1, 0.2);
   --  Spin,Adj, Step, Digits after DP (Gtk.Spin_Button)
      Gtk_New (OFFSET_X, TempAdj, 0.01, 2);
      Pack_Start (TempBox, OFFSET_X, Expand => False, Fill => False);
      Pack_Start (VBoxL2, TempBox, Expand => False, Fill => False);
--  "OFFSET_Y" Spin   LEFT Pane upper status area Box
      Gtk_New_Vbox (TempBox, Homogeneous => False);
      Gtk_New (TempLabel, "OFFSET_Y");
      Pack_Start (TempBox, TempLabel, Expand => False, Fill => False);
   --  Initial, Min, Max, Step, Page (Gtk.Adjustment)
      Gtk_New (TempAdj, 1.0, 0.2, 5.0, 0.1, 0.2);
      Gtk_New (OFFSET_Y, TempAdj, 0.01, 2);
      Pack_Start (TempBox, OFFSET_Y, Expand => False, Fill => False);
      Pack_Start (VBoxL2, TempBox, Expand => False, Fill => False);
--  "PIXFACTOR-X" Spin     LEFT Pane upper status area Box
      Gtk_New_Vbox (TempBox, Homogeneous => False);
      Gtk_New (TempLabel, "PIXFACTOR_X");
      Pack_Start (TempBox, TempLabel, Expand => False, Fill => False);
      Gtk_New (TempAdj, 350.0, 200.0, 500.0, 10.0, 20.0);
      Gtk_New (PIXFACTOR_X, TempAdj, 1.0, 0);
      Pack_Start (TempBox, PIXFACTOR_X, Expand => False, Fill => False);
      Pack_Start (VBoxL2, TempBox, Expand => False, Fill => False);
--  "PIXFACTOR-Y" Spin     LEFT Pane upper status area Box
      Gtk_New_Vbox (TempBox, Homogeneous => False);
      Gtk_New (TempLabel, "PIXFACTOR_Y");
      Pack_Start (TempBox, TempLabel, Expand => False, Fill => False);
      Gtk_New (TempAdj, 180.0, 50.0, 300.0, 10.0, 20.0);
      Gtk_New (PIXFACTOR_Y, TempAdj, 1.0, 0);
      Pack_Start (TempBox, PIXFACTOR_Y, Expand => False, Fill => False);
      Pack_Start (VBoxL2, TempBox, Expand => False, Fill => False);
--  SON_POS_FACTOR" Spin     LEFT Pane upper status area Box
      Gtk_New_Vbox (TempBox, Homogeneous => False);
      Gtk_New (TempLabel, "SND_POS_FACTOR");
      Pack_Start (TempBox, TempLabel, Expand => False, Fill => False);
      Gtk_New (TempAdj, 30.0, 1.0, 500.0, 1.0, 10.0);
      Gtk_New (SND_POS_FACTOR, TempAdj, 1.0, 0);
      Pack_Start (TempBox, SND_POS_FACTOR, Expand => False, Fill => False);
      Pack_Start (VBoxL2, TempBox, Expand => False, Fill => False);
--  SND_SPD_FACTOR" Spin     LEFT Pane upper status area Box
      Gtk_New_Vbox (TempBox, Homogeneous => False);
      Gtk_New (TempLabel, "SND_SPD_FACTOR");
      Pack_Start (TempBox, TempLabel, Expand => False, Fill => False);
      Gtk_New (TempAdj, 200.0, 1.0, 500.0, 1.0, 10.0);
      Gtk_New (SND_SPD_FACTOR, TempAdj, 1.0, 0);
      Pack_Start (TempBox, SND_SPD_FACTOR, Expand => False, Fill => False);
      Pack_Start (VBoxL2, TempBox, Expand => False, Fill => False);
--  "TIME_CYCLE_X" Spin     LEFT Pane upper status area Box
      Gtk_New_Vbox (TempBox, Homogeneous => False);
      Gtk_New (TempLabel, "TIME_CYCLE_X");
      Pack_Start (TempBox, TempLabel, Expand => False, Fill => False);
      Gtk_New (TempAdj, 10.0, 3.0, 30.0, 1.0, 2.0);
      Gtk_New (TIME_CYCLE_X, TempAdj, 1.0, 0);
      Pack_Start (TempBox, TIME_CYCLE_X, Expand => False, Fill => False);
      Pack_Start (VBoxL2, TempBox, Expand => False, Fill => False);
--  "TIME_CYCLE_Y" Spin   LEFT Pane upper status area Box
      Gtk_New_Vbox (TempBox, Homogeneous => False);
      Gtk_New (TempLabel, "TIME_CYCLE_Y");
      Pack_Start (TempBox, TempLabel, Expand => False, Fill => False);
      Gtk_New (TempAdj, 10.0, 3.0, 30.0, 1.0, 2.0);
      Gtk_New (TIME_CYCLE_Y, TempAdj, 1.0, 0);
      Pack_Start (TempBox, TIME_CYCLE_Y, Expand => False, Fill => False);
      Pack_Start (VBoxL2, TempBox, Expand => False, Fill => False);
--  "LPOS_X" Spin   LEFT Pane upper status area Box
      Gtk_New_Vbox (TempBox, Homogeneous => False);
      Gtk_New (TempLabel, "LPOS_X");
      Pack_Start (TempBox, TempLabel, Expand => False, Fill => False);
      Gtk_New (TempAdj, 1.0, 0.2, 3.0, 0.01, 0.1);
      Gtk_New (LPOS_X, TempAdj, 0.01, 2);
      Pack_Start (TempBox, LPOS_X, Expand => False, Fill => False);
      Pack_Start (VBoxL2, TempBox, Expand => False, Fill => False);
      TempAdj.On_Value_Changed (On_Listner_Loc'Access, LPOS_X);
--  "LPOS_Y" Spin   LEFT Pane upper status area Box
      Gtk_New_Vbox (TempBox, Homogeneous => False);
      Gtk_New (TempLabel, "LPOS_Y");
      Pack_Start (TempBox, TempLabel, Expand => False, Fill => False);
   --  Initial, Min, Max, Step, Page (Gtk.Adjustment)
      Gtk_New (TempAdj, 2.8, 0.5, 5.0, 0.01, 0.1);
   --  Spin,Adj, Step, Digits after DP (Gtk.Spin_Button)
      Gtk_New (LPOS_Y, TempAdj, 0.01, 2);
      Pack_Start (TempBox, LPOS_Y, Expand => False, Fill => False);
      Pack_Start (VBoxL2, TempBox, Expand => False, Fill => False);
      TempAdj.On_Value_Changed (On_Listner_Loc'Access, LPOS_Y);

   --  Setup Canvas background color and location
   --  Locate dragged Object on the nearest grid or not.
      Align_On_Grid (CanvasG, False);
      Configure (CanvasG,  --  Size=0 is no grid on the Canvas, 10=10dot pitch
             Grid_Size => 0,
   --  Canvas Back color: RGBA,  Gdk.RGBA.Gdk_RGBA := Gdk.RGBA.White_RGBA);
      Background => (0.01, 0.01, 0.01, 0.03));

--  Display Listner Picture image in RIGHT CANVAS ************
      Gdk_New_From_File (PixG, "./BackShot140140.png", Error);
      if PixG = Null_Pixbuf then    --  If Error:
         Gtk_New (Label, "Pixmaps #1 not found. Please run testgtk from the"
                & " testgtk/ directory itself.");
         Add (VBoxL, Label);
         Show_All (VBoxL);
         return;
      end if;
   --  Set Picture. New Original Procedure Initialize_Listener_Image
      Initialize_Listener_Image (DrawPic, PixG, "Listener");

--  Display Listner Image
      Buf_LPOS_X := Float (Get_Value (LPOS_X));
      X := Glib.Gint (Buf_LPOS_X * Float (Get_Value (PIXFACTOR_X)));
      Buf_LPOS_Y := Float (Get_Value (LPOS_Y));
      Y := Glib.Gint (Buf_LPOS_Y * Float (Get_Value (PIXFACTOR_Y)));
      Put (CanvasG, DrawPic, X, Y);      --  Display Listener Photo image

--  Display Source Image
      Initial_Plane_Item_Setup (CanvasG);    --  Setup Item1
   --  Gtkada.Canvas.Put(), Display Plane Graphics at the initial position
      Put (CanvasG, Item1, 2, 200);

--   Refresh_Canvas (CanvasG);
--   Show_Item (CanvasG, Item1);
--   Show_All (FrameM);
   end Run;

end Gui;
