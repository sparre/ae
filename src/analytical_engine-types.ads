with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded;

with YAML;

package Analytical_Engine.Types is

   subtype UTF8_String is YAML.UTF8_string;

   type Unbounded_UTF8_String is new Ada.Strings.Unbounded.Unbounded_String;

   package UTF8_Strings is
     new Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
                                            Element_Type => UTF8_String,
                                            "="          => YAML."=");

   function "+" (Item : in UTF8_String) return Unbounded_UTF8_String;

   function "+" (Item : in Unbounded_UTF8_String) return UTF8_String;

   function "&" (Left  : in UTF8_String;
                 Right : in Unbounded_UTF8_String) return UTF8_String;

   use all type YAML.Node_Kind;

   function "+" (Item : in YAML.Node_Ref) return UTF8_Strings.Vector
     with Pre => Item.Kind = Sequence_Node;

end Analytical_Engine.Types;
