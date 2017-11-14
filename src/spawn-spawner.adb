with Interfaces.C;
with Interfaces.C.Strings;

--  FIXME

package body Spawn.Spawner is

   use Interfaces.C, Interfaces.C.Strings;

   function fork return Process_Id;
   pragma Import (C, fork);

   type C_Cmd_Args is array (Integer range <>) of
     chars_ptr;
   pragma Convention (C, C_Cmd_Args);

   function execv (File : char_array; Args : C_Cmd_Args)
                   return int;
   pragma Import (C, execv);

   procedure Non_Blocking_Spawn
     (Descriptor  : out Process_Descriptor'Class;
      Command     : String)
   is
      Pid : constant Process_Id := fork;
   begin
      case Pid is
         when 0 =>
            --  FIXME: Shall we support shell semantics of use plain execvp()?
            declare
               Cmd : aliased char_array :=
                 To_C (Command);
               R : constant int :=
                 execv (To_C ("/bin/sh"),
                        (1 => To_Chars_Ptr (Cmd'Unchecked_Access)));
               pragma Unreferenced (R);
            begin
               raise Command_Failed with "Cannot exec shell"; --  TODO
            end;
         when -1 =>
            raise Command_Failed with "Cannot fork"; --  TODO
         when others =>
            Descriptor.Pid := Pid;
      end case;
   end Non_Blocking_Spawn;

end Spawn.Spawner;
