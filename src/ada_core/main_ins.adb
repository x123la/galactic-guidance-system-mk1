with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
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
   Write_Count  : UInt64 := 0;
   Last_Write_Count : UInt64 := 0;
   
   --  Conversion Factors (Raw Integer -> Physical Unit)
   --  Driver reports acceleration in mm/s^2.
   Accel_Scale : constant Float_64 := 0.001;
   Dt_Min      : constant Seconds := 0.0001;
   Dt_Max      : constant Seconds := 0.100;
   
   --  Variables for our "Physics" world
   Raw_Sample   : Sensor_Sample;
   Acc_Vec      : Acceleration_Vector;
   
   --  INS State
   Vel_State    : Velocity_Vector := (0.0, 0.0, 0.0);
   Pos_State    : Position_Vector := (0.0, 0.0, 0.0);
   
   Last_TS      : UInt64 := 0;
   First_TS     : UInt64 := 0;
   Dt           : Seconds;

   --  Simple loop counter for UI updates
   Cycle_Count  : Natural := 0;
   Idle_Loops   : Natural := 0;
   Run_Seconds  : Natural := 0;
   Local_Flags  : UInt32;
   Takeover_Attempted : Boolean := False;

   function Has_Flag (Flags, Mask : UInt32) return Boolean is
   begin
      return (Flags and Mask) /= 0;
   end Has_Flag;

   function Source_Name (Flags : UInt32) return String is
      Source : constant UInt32 := Flags and Status_Source_Mask;
   begin
      if Source = Status_Source_IIO then
         return "IIO";
      elsif Source = Status_Source_Sim then
         return "SIM";
      else
         return "NONE";
      end if;
   end Source_Name;

begin
   if Argument_Count >= 1 then
      begin
         Run_Seconds := Natural'Value (Argument (1));
      exception
         when others =>
            Put_Line ("Usage: ./bin/main_ins [run_seconds]");
            Set_Exit_Status (Failure);
            return;
      end;
   end if;

   Put_Line ("==================================================");
   Put_Line ("   GALACTIC INERTIAL NAVIGATION SYSTEM (FULL)     ");
   Put_Line ("==================================================");
   Put_Line ("Architecture: Zero-Copy Hyper-Plane (Ada + C)");
   Put_Line ("Status: Navigation Core Active (real sensor required unless GGS_ALLOW_SIM=1)");
   if Run_Seconds > 0 then
      Put_Line ("Runtime Limit: " & Natural'Image (Run_Seconds) & " s");
   end if;
   
   Start_C_Driver;
   delay 0.1; 
   Initialize;
   if not Is_Initialized then
      Put_Line ("ERROR: Unable to map shared memory region.");
      Set_Exit_Status (Failure);
      return;
   end if;

   --  Attach at the current writer head to avoid replaying stale samples
   Current_Read := Global_Memory.all.Write_Index;
   Last_Write_Count := Global_Memory.all.Write_Count;
   
   loop
      Write_Idx := Global_Memory.all.Write_Index;
      Write_Count := Global_Memory.all.Write_Count;

      if Last_Write_Count /= 0 and then Write_Count > Last_Write_Count + UInt64 (Ring_Buffer_Size) then
         Put_Line ("WARN: Reader lag detected, resyncing to live head.");
         Current_Read := Write_Idx;
         Last_Write_Count := Write_Count;
      end if;
      
      if Current_Read /= Write_Idx then
         Idle_Loops := 0;
         Takeover_Attempted := False;
         Raw_Sample := Global_Memory.all.Buffer (Integer(Current_Read));
         Local_Flags := Raw_Sample.Status_Flags;

         if Has_Flag (Local_Flags, Status_Flag_Sensor_Missing) and then
            (Local_Flags and Status_Source_Mask) = Status_Source_None
         then
            Put_Line ("ERROR: No real accelerometer source found.");
            Put_Line ("Hint: connect a Linux IIO sensor, or set GGS_ALLOW_SIM=1 for intentional fallback.");
            Set_Exit_Status (Failure);
            return;
         end if;
         
         --  1. Timing (Dt)
         if Last_TS /= 0 then
            Dt := Seconds(Float_64(Raw_Sample.Timestamp_Ns - Last_TS) / 1.0E9);
            if Dt < Dt_Min then
               Dt := Dt_Min;
               Local_Flags := Local_Flags or Status_Flag_Dt_Clamped;
            elsif Dt > Dt_Max then
               Dt := Dt_Max;
               Local_Flags := Local_Flags or Status_Flag_Dt_Clamped;
            end if;
            
            --  2. Coordinate Transformation & Gravity Compensation
            --  Current model assumes body frame is approximately level.
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
               Put ("SRC: " & Source_Name (Local_Flags) & " | ");
               Put ("POS_X: " & Float_64'Image(Pos_State(X_Axis)) & " m | ");
               Put ("VEL_X: " & Float_64'Image(Vel_State(X_Axis)) & " m/s");
               if Has_Flag (Local_Flags, Status_Flag_Dt_Clamped) then
                  Put (" | FLAG:DT_CLAMP");
               end if;
               New_Line;
            end if;
         end if;
         
         if First_TS = 0 then
            First_TS := Raw_Sample.Timestamp_Ns;
         end if;
         Last_TS := Raw_Sample.Timestamp_Ns;
         Cycle_Count := Cycle_Count + 1;
         Last_Write_Count := Write_Count;
         
         Current_Read := (Current_Read + 1) mod Ring_Buffer_Size;

         if Run_Seconds > 0 and then First_TS /= 0 and then Last_TS > First_TS then
            if (Last_TS - First_TS) >= UInt64(Run_Seconds) * 1_000_000_000 then
               Put_Line ("Run complete.");
               return;
            end if;
         end if;
      else
         delay 0.001;
         Idle_Loops := Idle_Loops + 1;
         if Idle_Loops > 2000 and then not Takeover_Attempted then
            Put_Line ("WARN: Stream idle, attempting writer takeover.");
            Start_C_Driver;
            delay 0.2;
            Current_Read := Global_Memory.all.Write_Index;
            Last_Write_Count := Global_Memory.all.Write_Count;
            Idle_Loops := 0;
            Takeover_Attempted := True;
         elsif Cycle_Count = 0 and then Idle_Loops > 6000 then
            Put_Line ("ERROR: Driver did not publish samples within 6 seconds.");
            Put_Line ("Check sensor permissions and hardware availability.");
            Set_Exit_Status (Failure);
            return;
         elsif Cycle_Count > 0 and then Idle_Loops > 4000 then
            Put_Line ("ERROR: Sample stream stalled.");
            Set_Exit_Status (Failure);
            return;
         end if;
      end if;
   end loop;


end Main_INS;
