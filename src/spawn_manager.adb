with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Unbounded;

with GNAT.OS_Lib;

with ZMQ.Sockets;
with ZMQ.Contexts;
with ZMQ.Messages;

with Spawn.Types;

procedure Spawn_Manager
is
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
            Args   : GNAT.OS_Lib.Argument_List_Access
              := GNAT.OS_Lib.Argument_String_To_List
                (Arg_String => To_String (Data.Command));
            Status : Boolean;
         begin
            exit when Data.Do_Quit;

            GNAT.OS_Lib.Spawn
              (Program_Name => Args (Args'First).all,
               Args         => Args (Args'First + 1 .. Args'Last),
               Success      => Status);
            GNAT.OS_Lib.Free (Arg => Args);

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
