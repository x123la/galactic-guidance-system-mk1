with Ada.Unchecked_Conversion;
with System;

package body Shared_Memory is
   use type System.Address;

   function GGS_Map_Shared_Memory return System.Address;
   pragma Import (C, GGS_Map_Shared_Memory, "ggs_map_shared_memory");

   function To_Shared_Memory_Access is
     new Ada.Unchecked_Conversion (System.Address, Shared_Memory_Access);

   procedure Initialize is
      Addr : constant System.Address := GGS_Map_Shared_Memory;
   begin
      if Addr = System.Null_Address then
         Global_Memory := null;
      else
         Global_Memory := To_Shared_Memory_Access (Addr);
      end if;
   end Initialize;

   function Is_Initialized return Boolean is
   begin
      return Global_Memory /= null;
   end Is_Initialized;

end Shared_Memory;
