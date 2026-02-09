with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Shared_Memory;
with Physics_Units; use Physics_Units;

procedure Main_INS is
   
   --  Import the C function to start the driver
   procedure Start_C_Driver;
   pragma Import (C, Start_C_Driver, "start_c_driver");

   use Shared_Memory;

   --  Local State
   Current_Read : UInt32 := 0;
   Write_Idx    : UInt32;
   
   --  Conversion Factors (Raw Integer -> Physical Unit)
   --  Example: Accel is in milli-Gs. 1000 = 1G = 9.81 m/s^2
   Accel_Scale : constant Float_64 := 9.80665 / 1000.0;
   
   --  Variables for our "Physics" world
   Raw_Sample   : Sensor_Sample;
   Acc_Vec      : Acceleration_Vector;
   
   --  INS State
   Vel_State    : Velocity_Vector := (0.0, 0.0, 0.0);
   Pos_State    : Position_Vector := (0.0, 0.0, 0.0);
   
   Last_TS      : UInt64 := 0;
   Dt           : Seconds;

   --  Simple loop counter for UI updates
   Cycle_Count : Integer := 0;

begin
   Put_Line ("==================================================");
   Put_Line ("   GEMINI INERTIAL NAVIGATION SYSTEM (FULL)       ");
   Put_Line ("==================================================");
   Put_Line ("Architecture: Zero-Copy Hyper-Plane (Ada + C)");
   Put_Line ("Status: Navigation Core Active");
   
   Start_C_Driver;
   delay 0.1; 
   
   loop
      Write_Idx := Global_Memory.Write_Index;
      
      if Current_Read /= Write_Idx then
         Raw_Sample := Global_Memory.Buffer (Integer(Current_Read));
         
         --  1. Timing (Dt)
         if Last_TS /= 0 then
            Dt := Seconds(Float_64(Raw_Sample.Timestamp_Ns - Last_TS) / 1.0E9);
            
            --  2. Coordinate Transformation & Gravity Compensation
            --  For this demo, we assume the laptop is level.
            --  Subtract Gravity from Z-axis
            Acc_Vec(X_Axis) := Float_64(Raw_Sample.Accel_X) * Accel_Scale;
            Acc_Vec(Y_Axis) := Float_64(Raw_Sample.Accel_Y) * Accel_Scale;
            Acc_Vec(Z_Axis) := (Float_64(Raw_Sample.Accel_Z) * Accel_Scale) - Float_64(Gravity);

            --  3. Integration (Dead Reckoning)
            --  V = V + A*dt
            Vel_State := Vel_State + (Acc_Vec * Dt);
            --  P = P + V*dt
            Pos_State := Pos_State + (Vel_State * Dt);

            --  4. Telemetry
            if Cycle_Count mod 100 = 0 then
               Put ("POS_X: " & Float_64'Image(Pos_State(X_Axis)) & " m | ");
               Put ("VEL_X: " & Float_64'Image(Vel_State(X_Axis)) & " m/s");
               New_Line;
            end if;
         end if;
         
         Last_TS := Raw_Sample.Timestamp_Ns;
         Cycle_Count := Cycle_Count + 1;
         
         Current_Read := (Current_Read + 1) mod Ring_Buffer_Size;
         Global_Memory.Read_Index := Current_Read;
      else
         delay 0.001;
      end if;
   end loop;


end Main_INS;
