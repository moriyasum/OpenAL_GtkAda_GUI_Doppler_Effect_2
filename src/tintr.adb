--
--
with Gui;          use Gui;
with Audio;        use Audio;
with Ada.Text_IO;        use Ada.Text_IO;
with Gtk.Button;         use Gtk.Button;
with Gtk.Spin_Button;    use Gtk.Spin_Button;
with Gdk.RGBA;           use Gdk.RGBA;
with OpenAL.Context;
with OpenAL.Source;

package body Tintr is
--------------------------------------------------------
--------------------------------------------------------
--  TIMER INTERRUPT
--------------------------------------------------------
--------------------------------------------------------
   function Timer_Intr (Pbar : Gtk_Progress_Bar) return Boolean is
      pragma Unreferenced (Pbar);
   begin
      if Start_Flag = 0 then    --  Flag=0  Nothing to do
         TimerIntrCounter := TimerIntrCounter + 1;
         TimeCnt := 0;
         Time_X  := 0.0;
         Time_Y  := 0.0;
         return True;
      end if;

--------------------
--  Display Graphics and Audio Output
--------------------
      Display_Plane (TimeCnt);       --  Calculate and Move Plane
--  Set Audio Source & Listner Position XYZ and Source Velocity XYZ
      ALSound_Process;
      if Pause_Flag = 1 then
      --  Pause, then do nothing (keep audio and position)
         TimerIntrCounter := TimerIntrCounter + 1;
         return True;
      end if;

      if Start_Flag = 1 or else Start_Flag = 3 or else Start_Flag = 5 then
   --  Button was pressed, Begin Audio
   --  Manual doc is wrong
         OpenAL.Source.Set_Looping (Audio.Sound_Source, Looping => True);
         OpenAL.Source.Play (Audio.Sound_Source_Array (1));

         Put_Line ("Play (Sound_Source)");

         if Start_Flag = 1 or else Start_Flag = 5 then
            TimeCnt := 0;    --  Initialize
         end if;
         Start_Flag := Start_Flag + 1;   --  Flag 1=>2, 3=>4, 5=>6
      end if;

--  STEP mode and Counter is busy
      if Start_Flag = 4 then   --  STEP mode
         if Step_Counter /= 0 then
            Step_Counter := Step_Counter - 1;
            if Step_Counter = 0 then
               Pause_Flag := 1;
               Set_Label (Button_Step, "Step");   --  Turn to stop STEP
               Gui.Set_Button_Style (Button_Step, "Red");

            end if;
         end if;
      end if; --  End Start_Flag=1,3,5

      TimerIntrCounter := TimerIntrCounter + 1;
      TimeCnt := TimeCnt + 1;
      Time_X := Time_X + 1.0;
      if Time_X * Float (TINTR_PITCH) / 1000.0 >=
        Float (Get_Value (TIME_CYCLE_X))
      then
--  Timer(X) is full, Plane is the start position, then one loop was finished.
         Time_X := 0.0;
         if Start_Flag = 2 then --  Single loop end, then stop the process
   --  Single mode and End cycle. Finish Single process
            Start_Flag := 0;
            Pause_Flag := 0;
            Set_Label (Gui.Button_Single, "Single");
            Parse (ButtonColor, "Black", Dummy_Boolean);
            Gui.Set_Button_Style (Button_Single, "Black");

            Set_Label (Gui.Button_Loop, "Loop");
            Parse (ButtonColor, "Black", Dummy_Boolean);
            Gui.Set_Button_Style (Button_Loop, "Black");

            Set_Label (Button_Step, "Step");
            Parse (ButtonColor, "Black", Dummy_Boolean);
            Gui.Set_Button_Style (Button_Step, "Black");

            --  CLOSE "OpenAL Soft" and stop sound
            OpenAL.Context.Close_Device (Audio.CX_Devicet);
            return True;
         end if;
--  On the way processing
      end if;
      Time_Y := Time_Y + 1.0;
      if Time_Y * Float (TINTR_PITCH) / 1000.0 >=
        Float (Get_Value (TIME_CYCLE_Y))
      then
         Time_Y := 0.0;
      end if;

      return True;
   end Timer_Intr;

end Tintr;
