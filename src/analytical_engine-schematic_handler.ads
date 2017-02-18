with YAML;

with Analytical_Engine.Types;

package Analytical_Engine.Schematic_Handler is

   type Instance (<>) is tagged private;
   subtype Class is Instance'Class;

   function Create (Schematic : in YAML.Document_Type)
                   return Class;

   function Name (Item : in Class) return Types.UTF8_String;

   procedure Bootstrap (Item : in out Instance);
   procedure Checkout  (Item : in out Instance);
   procedure Prepare   (Item : in out Instance);
   procedure Build     (Item : in out Instance);
   procedure Install   (Item : in out Instance);

private

   function Checkout_Directory (Item : in Instance)
                               return Types.UTF8_String;

   type Instance is tagged
      record
         Name            : Types.Unbounded_UTF8_String;
         Source_URL      : Types.Unbounded_UTF8_String;
         Source_Branch   : Types.Unbounded_UTF8_String;
         Source_Versions : Types.Unbounded_UTF8_String;
         Prepare_Steps   : Types.UTF8_Strings.Vector;
         Build_Steps     : Types.UTF8_Strings.Vector;
         Install_Steps   : Types.UTF8_Strings.Vector;
      end record;

end Analytical_Engine.Schematic_Handler;
