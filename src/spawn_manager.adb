with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Directories;

with GNAT.OS_Lib;

with ZMQ.Sockets;
with ZMQ.Contexts;
with ZMQ.Messages;

with Spawn.Types;

procedure Spawn_Manager
is
   Shell : constant String := "/bin/bash";
   --  Shell used to execute commands.

   use Ada.Strings.Unbounded;
   use ZMQ;

   Ctx : Contexts.Context;
   S   : Sockets.Socket;
begin
   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put_Line ("Usage: " & Ada.Command_Line.Command_Name
                            & " <address>");
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
      return;
   end if;

   Ctx.Initialize (App_Threads => 1);
   S.Initialize (With_Context => Ctx,
                 Kind         => Sockets.REP);
   S.Bind (Address => Ada.Command_Line.Argument (1));

   loop
      declare
         Request : Messages.Message;
      begin
         Request.Initialize;
         S.recv (Msg   => Request,
                 Flags => 0);

         declare
            Data   : constant Spawn.Types.Data_Type
              := Spawn.Types.Deserialize (Buffer => Request.getData);
            Args   : GNAT.OS_Lib.Argument_List (1 .. 4);
            Dir    : constant String := To_String (Data.Dir);
            Status : Boolean;
         begin
            exit when Data.Do_Quit;

            if Dir /= Ada.Directories.Current_Directory then
               Ada.Directories.Set_Directory (Directory => Dir);
            end if;

            Args (1) := new String'("-o");
            Args (2) := new String'("pipefail");
            Args (3) := new String'("-c");
            Args (4) := new String'(To_String (Data.Command));

            GNAT.OS_Lib.Spawn
              (Program_Name => Shell,
               Args         => Args,
               Success      => Status);

            for A in Args'Range loop
               GNAT.OS_Lib.Free (X => Args (A));
            end loop;

            declare
               Reply  : Messages.Message;
               Result : constant Spawn.Types.Data_Type
                 := (Success => Status,
                     others  => <>);
            begin
               Reply.Initialize
                 (Data => Spawn.Types.Serialize (Data => Result));

               S.Send (Msg => Reply);
               Reply.Finalize;
            end;
            Request.Finalize;
         end;
      end;
   end loop;

   S.Close;
   Ctx.Finalize;
end Spawn_Manager;
