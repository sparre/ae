package body Analytical_Engine.Types is

   function "+" (Item : in UTF8_String) return Unbounded_UTF8_String is
   begin
      return To_Unbounded_String (String (Item));
   end "+";

   function "+" (Item : in Unbounded_UTF8_String) return UTF8_String is
   begin
      return UTF8_String (To_String (Item));
   end "+";

   function "+" (Item : in YAML.Node_Ref) return Utf8_Strings.Vector is
      Result : Utf8_Strings.Vector;
   begin
      for Index in 1 .. Item.Length loop
         Result.Append (Item.Item (Index).Value);
      end loop;
      return Result;
   end "+";

   function "&" (Left  : in UTF8_String;
                 Right : in Unbounded_UTF8_String) return UTF8_String is
   begin
      return Left & UTF8_String (To_String (Right));
   end "&";

end Analytical_Engine.Types;
