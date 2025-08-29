------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 1998-2018, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify it --
--  under terms of the  GNU General Public License  as published by the Free --
--  Software  Foundation;  either version 3,  or (at your  option) any later --
--  version. This library is distributed in the hope that it will be useful, --
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are granted --
--  additional permissions described in the GCC Runtime Library Exception,   --
--  version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
--  You should have received a copy of the GNU General Public License and    --
--  a copy of the GCC Runtime Library Exception along with this program;     --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
--  <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  with Gtk.Main;         use Gtk.Main;
with Gtk;              use Gtk;
with Gtk.Adjustment;   use Gtk.Adjustment;
with Gtk.Button;       use Gtk.Button;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Dialog;       use Gtk.Dialog;
with Gtk.Label;        use Gtk.Label;
with Gtk.Handlers;     use Gtk.Handlers;
with Gtk.Widget;       use Gtk.Widget;
with Gtk.Window;       use Gtk.Window;
--  with Glib;             use Glib;
--  with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Gdk.Event;        use Gdk.Event;

package Common is

   package Widget_Handler is new Handlers.Callback (Gtk_Widget_Record);
   package User_Widget_Handler is new Handlers.User_Callback
     (Gtk_Widget_Record, Gtk_Widget);
   package Label_Handler  is new Handlers.Callback (Gtk_Label_Record);
   package Adj_Handler    is new Handlers.Callback (Gtk_Adjustment_Record);
   package Check_Handler  is new Handlers.Callback (Gtk_Check_Button_Record);
   package Button_Handler is new Handlers.Callback (Gtk_Button_Record);

   type Gtk_Window_Access is access all Gtk_Window;
   package Destroy_Handler is new Handlers.User_Callback
     (Gtk_Window_Record, Gtk_Window_Access);

   function Delete_Event_Cb
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event)
      return Boolean;

   procedure Destroy_Window
      (Win : access Gtk.Window.Gtk_Window_Record'Class;
       Ptr : Gtk_Window_Access);

   type Gtk_Dialog_Access is access all Gtk_Dialog;
   package Destroy_Dialog_Handler is new Handlers.User_Callback
     (Gtk_Dialog_Record, Gtk_Dialog_Access);
   procedure Destroy_Dialog (Win : access Gtk_Dialog_Record'Class;
                             Ptr : Gtk_Dialog_Access);

end Common;
