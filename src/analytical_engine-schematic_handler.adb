with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Shell;

with Analytical_Engine.Environment;

package body Analytical_Engine.Schematic_Handler is

   use Types;
   use all type YAML.Node_Kind;

   function "+" (Item : in String) return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;

   function "&" (Left  : in String;
                 Right : in Unbounded_UTF8_String) return String;

   function Value (Source  : in YAML.Node_Ref;
                   Name    : in YAML.UTF8_String;
                   Default : in YAML.UTF8_String) return YAML.UTF8_String
     with Pre => Source.Kind = Mapping_Node;

   procedure Run_With_Prefixed_Output
     (Prefix            : in     UTF8_String;
      Program           : in     UTF8_String;
      Arguments         : in     Shell.String_Array := Shell.Nil_Strings;
      Working_Directory : in     String := ".");

   procedure Run_With_Prefixed_Output
     (Prefix            : in     UTF8_String;
      Command           : in     UTF8_String;
      Working_Directory : in     String := ".");

   procedure Run_Steps (Item  : in     Class;
                        Steps : in     UTF8_Strings.Vector);

   task type Log_From_Pipe is
      entry Configure (Name : in UTF8_String;
                       Pipe : in Shell.Pipe);
   end Log_From_Pipe;

   task body Log_From_Pipe is
      Step   : Types.Unbounded_UTF8_String;
      Source : Shell.Pipe;
   begin
      accept Configure (Name : in UTF8_String;
                        Pipe : in Shell.Pipe) do
         Step   := +Name;
         Source := Pipe;
      end Configure;

      loop
         Ada.Text_IO.Put (File => Ada.Text_IO.Standard_Output,
                          Item => "[" & Step & "] " & Shell.To_String (Source));
      end loop;
   exception
      when others =>
         Shell.Close (Source);
   end Log_From_Pipe;

   function "&" (Left  : in String;
                 Right : in Unbounded_UTF8_String) return String is
   begin
      return Left & To_String (Right);
   end "&";

   procedure Bootstrap (Item : in out Instance) is
      pragma Unreferenced (Item);
   begin
      Ada.Directories.Create_Directory (Environment.Cache_Directory);
   exception
      when others =>
         null; --  No problem.  We assume it already was there.
   end Bootstrap;

   procedure Build (Item : in out Instance) is
      pragma Unreferenced (Item);
   begin
      Ada.Text_IO.Put_Line (">> build");
   end Build;

   procedure Checkout (Item : in out Instance) is
      use Ada.Directories;
   begin
      Ada.Text_IO.Put_Line (">> checkout");

      if not Exists (Environment.Repos_Directory) then
         Create_Directory (Environment.Repos_Directory);
      end if;

      if Exists (String (Item.Checkout_Directory)) then
         Run_With_Prefixed_Output
           (Prefix            => +Item.Name,
            Program           => "git",
            Arguments         => (1 => +"fetch"),
            Working_Directory => String (Item.Checkout_Directory));
      else
         Run_With_Prefixed_Output
           (Prefix            => +Item.Name,
            Program           => "git",
            Arguments         => (+"clone",
                                  +"--depth",
                                  +"1",
                                  +String (+Item.Source_URL),
                                  +String (Item.Checkout_Directory)));
      end if;

      Run_With_Prefixed_Output
        (Prefix            => +Item.Name,
         Program           => "git",
         Arguments         => (+"checkout",
                               +("origin/" & Item.Source_Branch)),
         Working_Directory => String (Item.Checkout_Directory));
   end Checkout;

   function Checkout_Directory (Item : in Instance)
                               return Types.UTF8_String is
      use Ada.Directories;
   begin
      return
        UTF8_String (Compose (Containing_Directory => Environment.Repos_Directory,
                              Name                 => String (+Item.Name)));
   end Checkout_Directory;

   function Create (Schematic : in YAML.Document_Type)
                   return Class
   is
      Root            : constant YAML.Node_Ref := Schematic.Root_Node;
      Source          : constant YAML.Node_Ref := Root.Item ("source");
      Source_URL      : constant YAML.UTF8_String := Source.Item ("url").Value;
      Source_Branch   : constant YAML.UTF8_String := Value (Source, "ref", "master");
      --  Repos_Directory : constant String := Compose (Environment.Cache_Directory,
      --                                                   "repos");
   begin
      return Instance'(Name            => +Root.Item ("name").Value,
                       Source_URL      => +Source_URL,
                       Source_Branch   => +Source_Branch,
                       Source_Versions => +Source.Item ("versions").Value,
                       Prepare_Steps   => +Source.Item ("prepare"),
                       Build_Steps     => +Source.Item ("build"),
                       Install_Steps   => +Source.Item ("install"));
   end Create;

   procedure Install (Item : in out Instance) is
   begin
      Ada.Text_IO.Put_Line (">> install");
      Item.Run_Steps (Steps => Item.Install_Steps);
   end Install;

   function Name (Item : in Class) return Types.UTF8_String is
   begin
      return +Item.Name;
   end Name;

   procedure Prepare (Item : in out Instance) is
      pragma Unreferenced (Item);
   begin
      Ada.Text_IO.Put_Line (">> prepare");
   end Prepare;

   procedure Run_Steps (Item  : in     Class;
                        Steps : in     UTF8_Strings.Vector) is
   begin
      for Command of Steps loop
         Run_With_Prefixed_Output
           (Prefix            => +Item.Name,
            Command           => Command,
            Working_Directory => String (Item.Checkout_Directory));
      end loop;
   end Run_Steps;

   procedure Run_With_Prefixed_Output
     (Prefix            : in     UTF8_String;
      Program           : in     UTF8_String;
      Arguments         : in     Shell.String_Array := Shell.Nil_Strings;
      Working_Directory : in     String := ".")
   is
      use Shell;
      Prefixed_Output : Log_From_Pipe;
      Output          : Shell.Pipe;
      Process         : Shell.Process :=
                          Start (Program           => String (Program),
                                 Arguments         => Arguments,
                                 Working_Directory => Working_Directory,
                                 Output            => Output) with Unreferenced;
   begin
      Prefixed_Output.Configure (Name => Prefix,
                                 Pipe => Output);
   end Run_With_Prefixed_Output;

   procedure Run_With_Prefixed_Output
     (Prefix            : in     UTF8_String;
      Command           : in     UTF8_String;
      Working_Directory : in     String := ".")
   is
      use Shell;
      Prefixed_Output : Log_From_Pipe;
      Output          : Shell.Pipe;
      Process         : Shell.Process :=
                          Start (Command           => String (Command),
                                 Working_Directory => Working_Directory,
                                 Output            => Output) with Unreferenced;
   begin
      Prefixed_Output.Configure (Name => Prefix,
                                 Pipe => Output);
   end Run_With_Prefixed_Output;

   function Value (Source  : in YAML.Node_Ref;
                   Name    : in YAML.UTF8_String;
                   Default : in YAML.UTF8_String) return YAML.UTF8_String is
   begin
      case Source.Item (Name).Kind is
         when No_Node =>
            return Default;
         when Scalar_Node =>
            return Source.Item (Name).Value;
         when others =>
            return raise Constraint_Error;
      end case;
   end Value;

end Analytical_Engine.Schematic_Handler;
