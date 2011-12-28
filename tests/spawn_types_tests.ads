with Ahven.Framework;

package Spawn_Types_Tests is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   procedure Initialize (T : in out Testcase);
   --  Initialize testcase.

   procedure Serialize_Data;
   --  Serialize spawn data.

   procedure Deserialize_Data;
   --  Deserialize spawn data.

end Spawn_Types_Tests;
