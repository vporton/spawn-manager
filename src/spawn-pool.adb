with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;

with GNAT.OS_Lib;

with ZMQ.Sockets;
with ZMQ.Contexts;
with ZMQ.Messages;

with Types;

package body Spawn.Pool is

   Ctx : ZMQ.Contexts.Context;

   Mngr_Bin  : constant String := "obj/spawn_manager";
   Addr_Base : constant String := "/tmp/spawn_manager-";


   use Ada.Strings.Unbounded;

   type Socket_Handle is access ZMQ.Sockets.Socket;

   type Socket_Container is record
      Address : Unbounded_String;
      Handle  : Socket_Handle;
      Busy    : Boolean;
   end record;

   package Socket_Map_Package is new Ada.Containers.Ordered_Maps
     (Key_Type     => Unbounded_String,
      Element_Type => Socket_Container);
   package SOMP renames Socket_Map_Package;
   Sockets : SOMP.Map;
   --  Sockets to spawn managers.

   procedure Insert_Socket (S : Socket_Container);
   --  Insert new socket into store.

   function Get_Socket return Socket_Handle;
   --  Return non-busy socket from socket store, mark as busy.

   subtype Chars is Character range 'a' .. 'z';
   package Random_Chars is new Ada.Numerics.Discrete_Random
     (Result_Subtype => Chars);
   Generator : Random_Chars.Generator;

   function Random_String (Len : Positive) return String;
   --  Return a random string of given length.

   -------------------------------------------------------------------------

   procedure Execute (Command : String)
   is
      Query : ZMQ.Messages.Message;
      Sock  : Socket_Handle := Get_Socket;
   begin
      Query.Initialize (Data => Command);
      Sock.Send (Msg => Query);
      Query.Finalize;

      declare
         Resultset : ZMQ.Messages.Message;
         Data      : Types.Data_Type;
      begin
         Resultset.Initialize;
         Sock.recv (Msg => Resultset);

         Data := Types.Deserialize (Buffer => Resultset.getData);
         if Data.Success /= True then
            raise Command_Failed with "Could not execute command";
         end if;

         Resultset.Finalize;
      end;
   end Execute;

   -------------------------------------------------------------------------

   function Get_Socket return Socket_Handle
   is
   begin
      return Sockets.First_Element.Handle;
   end Get_Socket;

   -------------------------------------------------------------------------

   procedure Init
   is
      use type GNAT.OS_Lib.Process_Id;

      Args : GNAT.OS_Lib.Argument_List (1 .. 1);
      Pid  : GNAT.OS_Lib.Process_Id;
      Addr : constant String := "ipc://" & Addr_Base
        & Random_String (Len => 8);
   begin
      Ctx.Initialize (App_Threads => 1);

      Args (1) := new String'(Addr);
      Pid := GNAT.OS_Lib.Non_Blocking_Spawn
        (Program_Name => Mngr_Bin,
         Args         => Args);
      GNAT.OS_Lib.Free (X => Args (1));

      if Pid = GNAT.OS_Lib.Invalid_Pid then
         raise Command_Failed with "Could not initialize spawn pool";
      end if;

      --  TODO: Handle case where no socket exists -> no exception raised

      declare
         Sock : Socket_Handle := new ZMQ.Sockets.Socket;
      begin
         Sock.Initialize (With_Context => Ctx,
                          Kind         => ZMQ.Sockets.REQ);
         Sock.Connect (Address => Addr);
         Insert_Socket (S => (Address => To_Unbounded_String (Addr),
                              Handle  => Sock,
                              Busy    => False));
      end;
   end Init;

   -------------------------------------------------------------------------

   procedure Insert_Socket (S : Socket_Container)
   is
   begin
      Sockets.Insert (Key      => S.Address,
                      New_Item => S);
   end Insert_Socket;

   -------------------------------------------------------------------------

   function Random_String (Len : Positive) return String
   is
      Result : String (1 .. Len);
   begin
      for I in Result'Range loop
         Result (I) := Random_Chars.Random (Gen => Generator);
      end loop;

      return Result;
   end Random_String;

begin
   Random_Chars.Reset (Gen => Generator);
end Spawn.Pool;
