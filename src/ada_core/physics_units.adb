package body Physics_Units is

   function "+" (Left, Right : Acceleration_Vector) return Acceleration_Vector is
      Result : Acceleration_Vector;
   begin
      for I in Axis_Index loop
         Result(I) := Left(I) + Right(I);
      end loop;
      return Result;
   end "+";

   function "+" (Left, Right : Velocity_Vector) return Velocity_Vector is
      Result : Velocity_Vector;
   begin
      for I in Axis_Index loop
         Result(I) := Left(I) + Right(I);
      end loop;
      return Result;
   end "+";

   function "+" (Left, Right : Position_Vector) return Position_Vector is
      Result : Position_Vector;
   begin
      for I in Axis_Index loop
         Result(I) := Left(I) + Right(I);
      end loop;
      return Result;
   end "+";

   function "*" (Left : Acceleration_Vector; Right : Seconds) return Velocity_Vector is
      Result : Velocity_Vector;
   begin
      for I in Axis_Index loop
         Result(I) := Left(I) * Float_64(Right);
      end loop;
      return Result;
   end "*";

   function "*" (Left : Velocity_Vector; Right : Seconds) return Position_Vector is
      Result : Position_Vector;
   begin
      for I in Axis_Index loop
         Result(I) := Left(I) * Float_64(Right);
      end loop;
      return Result;
   end "*";


end Physics_Units;
