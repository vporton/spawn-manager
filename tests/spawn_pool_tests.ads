with Ahven.Framework;

package Spawn_Pool_Tests is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   procedure Initialize (T : in out Testcase);
   --  Initialize testcase.

   procedure Execute_Bin_True;
   --  Execute /bin/true.

   procedure Execute_Bin_False;
   --  Execute /bin/false, should raise an exception.

end Spawn_Pool_Tests;
