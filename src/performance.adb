with Ada.Text_IO;
with Ada.Calendar;

with GNAT.OS_Lib;

with Spawn.Pool;

procedure Performance
is
   use type Ada.Calendar.Time;

   Loops   : constant        := 1000;
   Cmd     : constant String := "true";
   Runtime : Duration        := Duration (0);
   Start   : Ada.Calendar.Time;
   Status  : Boolean;
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

   declare
      Args : GNAT.OS_Lib.Argument_List (1 .. 4);
   begin
      Args (1) := new String'("-o");
      Args (2) := new String'("pipefail");
      Args (3) := new String'("-c");
      Args (4) := new String'(Cmd);

      for I in  1 .. Loops loop
         Start := Ada.Calendar.Clock;
         GNAT.OS_Lib.Spawn
           (Program_Name => "/bin/bash",
            Args         => Args,
            Success      => Status);
         Runtime := Runtime + (Ada.Calendar.Clock - Start);
      end loop;

      for A in Args'Range loop
         GNAT.OS_Lib.Free (X => Args (A));
      end loop;
   end;
   Ada.Text_IO.Put_Line ("* GNAT Spawn:" & Duration'Image (Runtime / Loops));

   Spawn.Pool.Cleanup;
end Performance;
