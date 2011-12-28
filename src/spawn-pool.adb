with Ada.Numerics.Discrete_Random;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with GNAT.OS_Lib;
with GNAT.Expect;

with ZMQ.Sockets;
with ZMQ.Contexts;
with ZMQ.Messages;

with Spawn.Types;
with Spawn.Logger;

package body Spawn.Pool is

   Ctx : ZMQ.Contexts.Context;

   Mngr_Bin  : constant String := "obj/spawn_manager";
   Addr_Base : constant String := "/tmp/spawn_manager-";

   use Ada.Strings.Unbounded;

   package L renames Spawn.Logger;

   type Socket_Handle is access ZMQ.Sockets.Socket;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => ZMQ.Sockets.Socket,
      Name   => Socket_Handle);
   --  Free allocated socket memory.

   type Socket_Container is record
      Address   : Unbounded_String;
      Pid       : GNAT.Expect.Process_Descriptor;
      Handle    : Socket_Handle;
      Available : Boolean;
   end record;

   package Socket_Map_Package is new Ada.Containers.Ordered_Maps
     (Key_Type     => Unbounded_String,
      Element_Type => Socket_Container);
   package SOMP renames Socket_Map_Package;

   protected Sockets
   is
      procedure Insert_Socket (S : Socket_Container);
      --  Insert new socket into store.

      procedure Get_Socket (S : out Socket_Container);
      --  Return non-busy socket container from socket store.

      procedure Release_Socket (C : Socket_Container);
      --  Release given socket container.

      procedure Cleanup;
      --  Cleanup socket store.
   private
      Data : SOMP.Map;
   end Sockets;

   subtype Chars is Character range 'a' .. 'z';
   package Random_Chars is new Ada.Numerics.Discrete_Random
     (Result_Subtype => Chars);
   Generator : Random_Chars.Generator;

   function Random_String (Len : Positive) return String;
   --  Return a random string of given length.

   -------------------------------------------------------------------------

   procedure Cleanup
   is
   begin
      Sockets.Cleanup;
   end Cleanup;

   -------------------------------------------------------------------------

   procedure Execute (Command : String)
   is
      Query : ZMQ.Messages.Message;
      Cont  : Socket_Container;
      Req   : Types.Data_Type := (Command => To_Unbounded_String (Command),
                                  others  => <>);
   begin
      Sockets.Get_Socket (S => Cont);
      pragma Debug (L.Log ("Executing command '" & Command & "' using socket "
        & To_String (Cont.Address)));

      Query.Initialize (Data => Types.Serialize (Data => Req));
      Cont.Handle.Send (Msg => Query);
      Query.Finalize;

      declare
         Resultset : ZMQ.Messages.Message;
         Data      : Types.Data_Type;
      begin
         Resultset.Initialize;
         Cont.Handle.recv (Msg => Resultset);

         Data := Types.Deserialize (Buffer => Resultset.getData);
         Sockets.Release_Socket (C => Cont);

         if not Data.Success then
            raise Command_Failed with "Command failed: '" & Command & "'";
         end if;

         Resultset.Finalize;
      end;
   end Execute;

   -------------------------------------------------------------------------

   procedure Init (Manager_Count : Positive := 1)
   is
      use type GNAT.OS_Lib.Process_Id;

      Args : GNAT.OS_Lib.Argument_List_Access;
   begin
      Ctx.Initialize (App_Threads => 1);

      for M in 1 .. Manager_Count loop
         declare
            Pid  : GNAT.Expect.Process_Descriptor;
            Addr : constant String := "ipc://" & Addr_Base
              & Random_String (Len => 8);
         begin
            Args := GNAT.OS_Lib.Argument_String_To_List
              (Arg_String => Mngr_Bin & " " & Addr);

            begin
               GNAT.Expect.Non_Blocking_Spawn
                 (Descriptor  => Pid,
                  Command     => Args (Args'First).all,
                  Args        => Args (Args'First + 1 .. Args'Last),
                  Buffer_Size => 0);

            exception
               when GNAT.Expect.Invalid_Process =>
                  GNAT.OS_Lib.Free (Args);
                  raise Command_Failed with "Unable to fork " & Mngr_Bin;
            end;

            GNAT.OS_Lib.Free (Args);

            --  TODO: Handle case where no socket exists -> no exception raised

            declare
               Sock : constant Socket_Handle := new ZMQ.Sockets.Socket;
            begin
               Sock.Initialize (With_Context => Ctx,
                                Kind         => ZMQ.Sockets.REQ);
               Sock.Connect (Address => Addr);
               Sockets.Insert_Socket
                 (S => (Address   => To_Unbounded_String (Addr),
                        Pid       => Pid,
                        Handle    => Sock,
                        Available => True));
               pragma Debug (L.Log ("Socket " & Addr & " ready"));
            end;
         end;
      end loop;
   end Init;

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

   -------------------------------------------------------------------------

   protected body Sockets
   is
      -------------------------------------------------------------------------

      procedure Cleanup
      is
         E   : Socket_Container;
         Pos : SOMP.Cursor := Data.First;
         Req : ZMQ.Messages.Message;
      begin
         Req.Initialize (Data => Types.Serialize
                         (Data => Types.Shutdown_Token));

         while SOMP.Has_Element (Position => Pos) loop
            E := SOMP.Element (Position => Pos);

            E.Handle.Send (Msg => Req);
            E.Handle.Close;
            Free (X => E.Handle);

            SOMP.Next (Position => Pos);
         end loop;

         Ctx.Finalize;
         Data.Clear;
      end Cleanup;

      ----------------------------------------------------------------------

      procedure Get_Socket (S : out Socket_Container)
      is
         Pos   : SOMP.Cursor := Data.First;
         Found : Boolean     := False;

         procedure Set_Busy
           (Key     :        Unbounded_String;
            Element : in out Socket_Container);
         --  Set state of given socket container to busy.

         procedure Set_Busy
           (Key     :        Unbounded_String;
            Element : in out Socket_Container)
         is
            pragma Unreferenced (Key);
         begin
            Element.Available := False;
         end Set_Busy;
      begin
         while SOMP.Has_Element (Position => Pos) loop
            S := SOMP.Element (Position => Pos);
            if S.Available then
               Data.Update_Element (Position => Pos,
                                    Process  => Set_Busy'Access);
               Found := True;
               exit;
            end if;
            SOMP.Next (Position => Pos);
         end loop;

         if not Found then
            raise Command_Failed with
              "No free spawn manager available, increase the pool size";
         end if;

         pragma Debug (L.Log ("Found available socket "
           & To_String (S.Address)));
      end Get_Socket;

      -------------------------------------------------------------------------

      procedure Insert_Socket (S : Socket_Container)
      is
      begin
         Data.Insert (Key      => S.Address,
                      New_Item => S);
      end Insert_Socket;

      ----------------------------------------------------------------------

      procedure Release_Socket (C : Socket_Container)
      is
         use type ZMQ.Sockets.Socket_Type;

         procedure Set_Available
           (Key     :        Unbounded_String;
            Element : in out Socket_Container);
         --  Set state of given socket container to available.

         procedure Set_Available
           (Key     :        Unbounded_String;
            Element : in out Socket_Container)
         is
            pragma Unreferenced (Key);
         begin
            Element.Available := True;
         end Set_Available;

         Pos : constant SOMP.Cursor := Data.Find (Key => C.Address);
      begin
         Data.Update_Element (Position => Pos,
                              Process  => Set_Available'Access);
         pragma Debug (L.Log ("Socket " & To_String
           (SOMP.Element (Position => Pos).Address) & " released"));
      end Release_Socket;
   end Sockets;

begin
   Random_Chars.Reset (Gen => Generator);
end Spawn.Pool;
