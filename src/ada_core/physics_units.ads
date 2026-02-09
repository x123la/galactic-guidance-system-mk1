package Physics_Units is
   pragma Pure; --  This package has no side effects (SPARK friendly)

   --  Base Types (Float_64 for precision)
   type Float_64 is digits 15;

   --  Dimensional Types: The compiler will prevent mixing these up!
   type Meters is new Float_64;
   type Seconds is new Float_64;
   type Meters_Per_Second is new Float_64;
   type Meters_Per_Second_Squared is new Float_64;
   
   type Radians is new Float_64;
   type Radians_Per_Second is new Float_64;
   
   type Tesla is new Float_64; --  For Magnetometer
   type Gauss is new Float_64;
   
   --  Coordinate System Types (North-East-Down or Body Frame)
   type Axis_Index is (X_Axis, Y_Axis, Z_Axis);
   type Vector_3D is array (Axis_Index) of Float_64;
   
   --  Type-Safe Vector Definitions
   type Acceleration_Vector is new Vector_3D;
   type Velocity_Vector is new Vector_3D;
   type Position_Vector is new Vector_3D;
   
   --  Orientation: Quaternion (W, X, Y, Z)
   type Quaternion is record
      W, X, Y, Z : Float_64;
   end record;

   --  Constants
   Gravity : constant Meters_Per_Second_Squared := 9.80665;
   
   --  Mathematical Operations (State Integration)
   function "+" (Left, Right : Velocity_Vector) return Velocity_Vector;
   function "+" (Left, Right : Position_Vector) return Position_Vector;
   
   --  Integration: Accel * Time = Velocity Change
   function "*" (Left : Acceleration_Vector; Right : Seconds) return Velocity_Vector;
   function "*" (Left : Velocity_Vector; Right : Seconds) return Position_Vector;

   
end Physics_Units;
