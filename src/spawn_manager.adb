with Ada.Text_IO;
with Ada.Command_Line;

with GNAT.OS_Lib;

with ZMQ.Sockets;
with ZMQ.Contexts;
with ZMQ.Messages;

with Types;

procedure Spawn_Manager
is
   use ZMQ;

   Ctx    : Contexts.Context;
   S      : Sockets.Socket;
   Result : constant String := "OK";
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
         S.Recv (Msg   => Request,
                 Flags => 0);

         declare
            Command : constant String := Request.getData;
            Args    : GNAT.OS_Lib.Argument_List_Access
              := GNAT.OS_Lib.Argument_String_To_List (Arg_String => Command);
            Status  : Boolean;
         begin
            GNAT.OS_Lib.Spawn
              (Program_Name => Args (Args'First).all,
               Args         => Args (Args'First + 1 .. Args'Last),
               Success      => Status);
            GNAT.OS_Lib.Free (Arg => Args);

            declare
               Reply  : Messages.Message;
               Result : Types.Data_Type := (Success => Status);
            begin
               Reply.Initialize
                 (Data => Types.Serialize (Data => Result));

               S.Send (Msg => Reply);
               Reply.Finalize;
            end;
            Request.Finalize;
         end;
      end;
   end loop;
end Spawn_Manager;
