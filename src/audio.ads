
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Handlers;    use Gtk.Handlers;
with Interfaces;      use Interfaces;
--  with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with OpenAL.Buffer;   use OpenAL.Buffer;
with OpenAL.Types;     use OpenAL.Types;
with OpenAL.Context;
with OpenAL.Source;

package Audio is

   package Handlers is new Gtk.Handlers.Callback --  Need with Gtk.Handlers
     (Widget_Type => Gtk_Widget_Record);    --  Need with Gtk.Widget

   function Test_Error (Display_Message : String) return Boolean;

   WAVE_FILE_NAME : constant String := "testsound.wav";
   function LoadWaveFile (
      WaveFile : String;        --  WAV File name string
      Format :  out Unsigned_16;   --  PCM=01,00
      Data :    out OpenAL.Buffer.Sample_Array_16_t;
      Length :  out Unsigned_32;   --  Byte, Only Data portion=(05..08)+8-44
      Freq :    out OpenAL.Types.Size_t;
      Channel : out Unsigned_16;   --  1ch=01,00, 2ch=02,00
      Sample :  out Unsigned_16    --  16bit=10,00
   ) return Boolean;

   procedure ALSound_Initialize;

   procedure ALSound_Process;
---------------------------------------
--  CONSTANT
---------------------------------------
   LISTENER_GAIN : constant := 10.0;

---------------------------------------
--   GLOBAL VARIABLES
---------------------------------------
   CX_Devicet : OpenAL.Context.Device_t;
   Sound_Source : OpenAL.Source.Source_t;
   Sound_Source_Array : OpenAL.Source.Source_Array_t (1 .. 1);

end Audio;
