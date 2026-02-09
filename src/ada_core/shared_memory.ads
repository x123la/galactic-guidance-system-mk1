with Interfaces; use Interfaces;
with Interfaces.C;

package Shared_Memory is
   pragma Preelaborate;

   --  Constants matching C header
   Ring_Buffer_Size : constant := 1024;

   --  Raw Types matching C types (stdint.h)
   type Int32 is new Interfaces.Integer_32;
   type UInt32 is new Interfaces.Unsigned_32;
   type UInt64 is new Interfaces.Unsigned_64;
   type UInt8  is new Interfaces.Unsigned_8;

   type Padding_Array is array (1 .. 16) of UInt8;

   --  The Sample Record (Matches Sensor_Sample_t)
   type Sensor_Sample is record
      Timestamp_Ns : UInt64;
      Accel_X      : Int32;
      Accel_Y      : Int32;
      Accel_Z      : Int32;
      Gyro_X       : Int32;
      Gyro_Y       : Int32;
      Gyro_Z       : Int32;
      Mag_X        : Int32;
      Mag_Y        : Int32;
      Mag_Z        : Int32;
      Status_Flags : UInt32;
      Padding      : Padding_Array;
   end record;
   pragma Convention (C, Sensor_Sample); --  Tell Ada to use C memory layout

   --  The Ring Buffer Array
   type Sample_Buffer_Array is array (0 .. Ring_Buffer_Size - 1) of Sensor_Sample;
   pragma Convention (C, Sample_Buffer_Array);

   --  The Shared Memory Area (Matches Shared_Memory_Area_t)
   type Shared_Memory_Area is record
      Write_Index : UInt32; --  C writes here
      Read_Index  : UInt32; --  Ada reads/updates here
      Buffer      : Sample_Buffer_Array;
   end record;
   pragma Volatile (Shared_Memory_Area);
   pragma Convention (C, Shared_Memory_Area);

   --  Import the global variable from C
   --  The C code will define: Shared_Memory_Area_t global_shared_memory;
   Global_Memory : Shared_Memory_Area;
   pragma Import (C, Global_Memory, "global_shared_memory");

end Shared_Memory;
