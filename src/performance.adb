with Ada.Text_IO;
with Ada.Calendar;

with GNAT.OS_Lib;

with Spawn.Pool;

procedure Performance
is
   use type Ada.Calendar.Time;

   Loops   : constant        := 1000;
   Cmd     : constant String := "/bin/bash -c /bin/true";
   Runtime : Duration        := Duration (0);
   Start   : Ada.Calendar.Time;
   Args    : constant GNAT.OS_Lib.Argument_List_Access
     := GNAT.OS_Lib.Argument_String_To_List (Arg_String => Cmd);
   Status  : Integer;
begin
   Spawn.Pool.Init;

   Ada.Text_IO.Put_Line ("* Calling '" & Cmd & "'" & Loops'Img & " times");

   for I in  1 .. Loops loop
      Start := Ada.Calendar.Clock;
      Spawn.Pool.Execute (Command => Cmd);
      Runtime := Runtime + (Ada.Calendar.Clock - Start);
   end loop;
   Ada.Text_IO.Put_Line ("* IPC 0MQ   :" & Duration'Image (Runtime / Loops));

   Runtime := Duration (0);
   for I in  1 .. Loops loop
      Start := Ada.Calendar.Clock;
      Status := GNAT.OS_Lib.Spawn
        (Program_Name => Args (Args'First).all,
         Args         => Args (Args'First + 1 .. Args'Last));
      Runtime := Runtime + (Ada.Calendar.Clock - Start);
   end loop;
   Ada.Text_IO.Put_Line ("* GNAT Spawn:"
                         & Duration'Image (Runtime / Loops));

   Spawn.Pool.Cleanup;
end Performance;
