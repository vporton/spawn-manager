with Spawn.Pool;

package body Spawn_Pool_Tests is

   use Ahven;

   -------------------------------------------------------------------------

   procedure Execute_Bin_False
   is
   begin
      Spawn.Pool.Execute (Command => "/bin/false");

   exception
      when Spawn.Pool.Command_Failed => null;
   end Execute_Bin_False;

   -------------------------------------------------------------------------

   procedure Execute_Bin_True
   is
   begin
      Spawn.Pool.Execute (Command => "/bin/true");
   end Execute_Bin_True;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Spawn pool tests");
      T.Add_Test_Routine
        (Routine => Execute_Bin_True'Access,
         Name    => "Execute /bin/true");
      T.Add_Test_Routine
        (Routine => Execute_Bin_False'Access,
         Name    => "Execute /bin/false");
   end Initialize;

end Spawn_Pool_Tests;
