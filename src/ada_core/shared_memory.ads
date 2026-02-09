with Interfaces; use Interfaces;
with Interfaces.C;
with System;

package Shared_Memory is
   pragma Preelaborate;

   --  Constants matching C header
   Ring_Buffer_Size : constant := 1024;

   --  Raw Types matching C types (stdint.h)
   type Int32 is new Interfaces.Integer_32;
   type UInt32 is new Interfaces.Unsigned_32;
   type UInt64 is new Interfaces.Unsigned_64;
   type UInt8  is new Interfaces.Unsigned_8;

   Status_Source_Mask         : constant UInt32 := 16#0000000F#;
   Status_Source_None         : constant UInt32 := 16#00000000#;
   Status_Source_IIO          : constant UInt32 := 16#00000001#;
   Status_Source_Sim          : constant UInt32 := 16#00000002#;
   Status_Flag_Sensor_Missing : constant UInt32 := 16#00000100#;
   Status_Flag_Sample_Dropped : constant UInt32 := 16#00000200#;
   Status_Flag_Dt_Clamped     : constant UInt32 := 16#00000400#;

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
      Read_Index  : UInt32; --  Legacy field; readers should not update in multi-reader mode.
      Write_Count : UInt64; --  Monotonic sample counter (writer-owned).
      Writer_Pid  : UInt32; --  PID of active writer process (0 if none).
      Reserved0   : UInt32;
      Buffer      : Sample_Buffer_Array;
   end record;
   pragma Volatile (Shared_Memory_Area);
   pragma Convention (C, Shared_Memory_Area);

   type Shared_Memory_Access is access all Shared_Memory_Area;

   procedure Initialize;
   function Is_Initialized return Boolean;

   Global_Memory : Shared_Memory_Access := null;

end Shared_Memory;
