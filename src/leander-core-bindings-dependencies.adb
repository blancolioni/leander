package body Leander.Core.Bindings.Dependencies is

   ---------------------
   -- Component_Count --
   ---------------------

   function Component_Count (Components : Component_Array) return Natural is
      Result : Natural := 0;
   begin
      for C of Components loop
         Result := Natural'Max (Result, C);
      end loop;
      return Result;
   end Component_Count;

   ----------------
   -- Components --
   ----------------

   function Components
     (Bs      : Reference_Array;
      Depends : access function (From, To : Positive) return Boolean := null)
      return Component_Array
   is
      subtype Vertex is Positive range Bs'Range;

      --  Tarjan's algorithm.  It closes a component only once every
      --  component reachable from it is closed, so numbering components in
      --  the order they close puts dependencies first.

      Result   : Component_Array (Bs'Range) := [others => 1];
      Index    : array (Vertex) of Natural := [others => 0];
      Low_Link : array (Vertex) of Positive := [others => 1];
      On_Stack : array (Vertex) of Boolean := [others => False];
      Stack    : array (1 .. Bs'Length) of Vertex;
      Top      : Natural := 0;
      Next     : Positive := 1;
      Closed   : Natural := 0;

      function Edge (From, To : Vertex) return Boolean
      is (if Depends = null
          then Bs (From).Has_Reference (Bs (To).Name)
          else Depends (From, To));

      procedure Visit (V : Vertex);

      -----------
      -- Visit --
      -----------

      procedure Visit (V : Vertex) is
      begin
         Index (V) := Next;
         Low_Link (V) := Next;
         Next := Next + 1;
         Top := Top + 1;
         Stack (Top) := V;
         On_Stack (V) := True;

         for W in Vertex loop
            if W /= V and then Edge (V, W) then
               if Index (W) = 0 then
                  Visit (W);
                  Low_Link (V) := Positive'Min (Low_Link (V), Low_Link (W));
               elsif On_Stack (W) then
                  Low_Link (V) := Positive'Min (Low_Link (V), Index (W));
               end if;
            end if;
         end loop;

         if Low_Link (V) = Index (V) then
            Closed := Closed + 1;
            loop
               declare
                  W : constant Vertex := Stack (Top);
               begin
                  Top := Top - 1;
                  On_Stack (W) := False;
                  Result (W) := Closed;
                  exit when W = V;
               end;
            end loop;
         end if;
      end Visit;

   begin
      for V in Vertex loop
         if Index (V) = 0 then
            Visit (V);
         end if;
      end loop;
      return Result;
   end Components;

end Leander.Core.Bindings.Dependencies;
