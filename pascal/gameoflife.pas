program GameOfLife;

uses
  Crt, SysUtils;

const
  GridSize = 20;

type
  GridType = array[1..GridSize, 1..GridSize] of Integer;

var
  grid, new_grid: GridType;
  generations: Integer;

procedure PrintGrid(var vector: GridType);
var
  i, j: Integer;
begin
  ClrScr;
  for i := 1 to GridSize do
  begin
    for j := 1 to GridSize do
    begin
      if vector[i, j] = 1 then
        Write('O')
      else
        Write('.');
    end;
    Writeln;
  end;
end;

procedure AdvanceGeneration(var grid, new_grid: GridType);
var
  i, j, count, up, down, left, right: Integer;
begin
  for i := 1 to GridSize do
    for j := 1 to GridSize do
      new_grid[i, j] := 0;

  for i := 1 to GridSize do
  begin
    for j := 1 to GridSize do
    begin
      count := 0;

      up := i - 1;
      down := i + 1;
      left := j - 1;
      right := j + 1;

      if up >= 1 then count := count + grid[up, j];
      if down <= GridSize then count := count + grid[down, j];
      if left >= 1 then count := count + grid[i, left];
      if right <= GridSize then count := count + grid[i, right];

      if (up >= 1) and (left >= 1) then count := count + grid[up, left];
      if (up >= 1) and (right <= GridSize) then count := count + grid[up, right];
      if (down <= GridSize) and (left >= 1) then count := count + grid[down, left];
      if (down <= GridSize) and (right <= GridSize) then count := count + grid[down, right];

      if grid[i, j] = 1 then
      begin
        if count < 2 then
          new_grid[i, j] := 0
        else if (count = 2) or (count = 3) then
          new_grid[i, j] := 1
        else if count > 3 then
          new_grid[i, j] := 0;
      end
      else if count = 3 then
        new_grid[i, j] := 1;
    end;
  end;
end;

begin
  FillChar(grid, SizeOf(grid), 0);

  grid[5, 5] := 1;
  grid[6, 5] := 1;
  grid[7, 5] := 1;
  grid[7, 4] := 1;
  grid[6, 3] := 1;

  for generations := 1 to 40 do
  begin
    PrintGrid(grid);
    Writeln('Generation: ', generations);
    Sleep(1000);

    AdvanceGeneration(grid, new_grid);
    grid := new_grid;
  end;
end.