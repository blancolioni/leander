package body Leander.Core.Kinds is

   type Instance_Class is (Star, KFun);

   type Instance (Class : Instance_Class) is new Abstraction with
      record
         case Class is
            when Star =>
               null;
            when KFun =>
               Left, Right : Reference;
         end case;
      end record;

   overriding function KAp
     (This : Instance)
      return Reference
   is (case This.Class is
          when Star =>
             raise Constraint_Error
               with "cannot extract kind from *",
          when KFun => This.Right);

   overriding function Show
     (This : Instance)
      return String;

   Local_Star_Kind : aliased constant Instance := (Class => Star);

   function Star return Reference
   is (Local_Star_Kind'Access);

   function KFun (Left, Right : Reference) return Reference
   is (new Instance'(KFun, Left, Right));

   ----------
   -- Show --
   ----------

   overriding function Show
     (This : Instance)
      return String
   is
   begin
      case This.Class is
         when Star =>
            return "*";
         when KFun =>
            if Instance (This.Right.all).Class = KFun then
               return This.Left.Show & "->(" & This.Right.Show & ")";
            else
               return This.Left.Show & "->" & This.Right.Show;
            end if;
      end case;
   end Show;

end Leander.Core.Kinds;
