with Ada.Directories;
with Ada.Environment_Variables;

package body Analytical_Engine.Environment is

   function Cache_Directory return String is
      use Ada.Directories;
   begin
      return Compose (Containing_Directory => Current_Directory,
                      Name                 => ".ae");
   end Cache_Directory;

   procedure Configure is
   begin
      Ada.Environment_Variables.Set (Name  => "SCHEMATIC_INSTALL_PREFIX",
                                     Value => Root_Directory);
      Ada.Environment_Variables.Set (Name  => "SCHEMATIC_PARALLELISM",
                                     Value => "1");
      Ada.Environment_Variables.Set (Name  => "PATH",
                                     Value => Exec_Directory & ":" & Ada.Environment_Variables.Value ("PATH"));
   end Configure;

   function Exec_Directory return String is
      use Ada.Directories;
   begin
      return Compose (Containing_Directory => Root_Directory,
                      Name                 => "bin");
   end Exec_Directory;

   function Repos_Directory return String is
      use Ada.Directories;
   begin
      return Compose (Containing_Directory => Cache_Directory,
                      Name                 => "repos");
   end Repos_Directory;

   function Root_Directory return String is
      use Ada.Directories;
   begin
      return Compose (Containing_Directory => Cache_Directory,
                      Name                 => "root");
   end Root_Directory;

end Analytical_Engine.Environment;
