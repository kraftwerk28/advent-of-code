local unpack = unpack or table.unpack

-- Enum
local Side = {Top = 1, Right = 2, Bottom = 3, Left = 4}

local function get_opposite_side(side)
  return ({
    [Side.Top] = Side.Bottom,
    [Side.Bottom] = Side.Top,
    [Side.Left] = Side.Right,
    [Side.Right] = Side.Left,
  })[side]
end

local Piece = {}
Piece.__index = Piece

function Piece:new(grid, label)
  for _, row in ipairs(grid) do
    assert(#row == #grid)
  end
  local p = setmetatable({grid = grid, label = label}, self)
  return p
end

function Piece:strlines()
  local lines = {}
  for _, row in ipairs(self.grid) do
    table.insert(lines, table.concat(row))
  end
  return lines
end

function Piece:__tostring()
  local lines = self:strlines()
  local res = ""
  if self.label ~= nil then
    res = res .. "label: " .. self.label .. '\n'
  end
  return res .. table.concat(lines, '\n') .. '\n'
end

function Piece:fromlines(lines, label)
  local grid = {}
  for _, line in ipairs(lines) do
    local row = {}
    for c in line:gmatch('.') do
      table.insert(row, c)
    end
    table.insert(grid, row)
  end
  return Piece:new(grid, label)
end

function Piece:copy(other)
  local newgrid = {}
  for _, row in ipairs(other.grid) do
    table.insert(newgrid, {unpack(row)})
  end
  return Piece:new(newgrid, other.label)
end

function Piece:size()
  return #self.grid
end

function Piece:getside(side)
  if side == Side.Top then
    return self.grid[1]
  elseif side == Side.Right then
    local ret = {}
    local lastindex = #self.grid[1]
    for i, _ in ipairs(self.grid) do
      ret[i] = self.grid[i][lastindex]
    end
    return ret
  elseif side == Side.Bottom then
    return self.grid[#self.grid]
  elseif side == Side.Left then
    local ret = {}
    for i, _ in ipairs(self.grid) do
      ret[i] = self.grid[i][1]
    end
    return ret
  end
end

function Piece:matches(other_piece, side)
  local s1 = self:getside(side)
  local s2 = other_piece:getside(get_opposite_side(side))
  for i in ipairs(s1) do
    if s1[i] ~= s2[i] then
      return false
    end
  end
  return true
end

function Piece:rotate_clockwise()
  local newgrid = {}
  local s = #self.grid
  for i = 1, s do
    local row = {}
    for j = 1, s do
      row[j] = self.grid[#self.grid - j + 1][i]
    end
    table.insert(newgrid, row)
  end
  return Piece:new(newgrid, self.label)
end

function Piece:rotate_anticlockwise()
  return self:rotate_clockwise():rotate_clockwise():rotate_clockwise()
end

function Piece:flip_horizontal()
  local newgrid, s = {}, #self.grid
  for _, row in ipairs(self.grid) do
    local r = {}
    for j = 1, s do
      r[j] = row[s - j + 1]
    end
    table.insert(newgrid, r)
  end
  return Piece:new(newgrid, self.label)
end

function Piece:flip_vertical()
  local newgrid, s = {}, #self.grid
  for i, row in ipairs(self.grid) do
    local r = {}
    for _, c in ipairs(row) do
      table.insert(r, c)
    end
    newgrid[s - i + 1] = r
  end
  return Piece:new(newgrid, self.label)
end

function Piece:strip_border()
  local p = Piece:copy(self)
  table.remove(p.grid)
  table.remove(p.grid, 1)
  for _, row in ipairs(p.grid) do
    table.remove(row)
    table.remove(row, 1)
  end
  return p
end

function Piece:pattern_matches_at(x, y, pattern)
  local lines = self:strlines()
  local pattern_w = #pattern[1]
  for i, pattern_row in ipairs(pattern) do
    local linepart = lines[y+i-1]:sub(x, x+pattern_w-1)
    if not linepart:find(pattern_row) then return false end
  end
  for i, pattern_row in ipairs(pattern) do
    local linepart = lines[y+i-1]:sub(x, x+pattern_w)
    print(linepart, pattern_row)
  end
  return true
end

Grid = {}
Grid.__index = Grid

function Grid:new()
  local grid = {rows = {}}
  return setmetatable(grid, self)
end

function Grid:copy()
  local rows = {}
  for i, row in pairs(self.rows) do
    local new_row = {}
    for j, piece in pairs(row) do
      new_row[j] = piece:copy()
    end
    rows[i] = new_row
  end
  local grid = Grid:new()
  grid.rows = rows
  return grid
end

function Grid:get_holes()
  local holes = {}
  for y, row in pairs(self.rows) do
    for x, _ in pairs(row) do
      local coords = {{x + 1, y}, {x - 1, y}, {x, y + 1}, {x, y - 1}}
      for _, coord in ipairs(coords) do
        local sx, sy = unpack(coord) -- sibling coord
        if self:at(sx, sy) == nil then
          holes[sy] = holes[sy] or {}
          holes[sy][sx] = true
        end
      end
    end
  end
  local result = {}
  for y, row in pairs(holes) do
    for x, _ in pairs(row) do
      table.insert(result, {x, y})
    end
  end
  local i = 0
  return function()
    i = i + 1
    if result[i] then
      return result[i][1], result[i][2]
    end
  end
end

function Grid:get_edge_coords()
  local res_rows = {}
  for y, row in pairs(self.rows) do
    for x, _ in pairs(row) do
      local offsets = {
        {x + 1, y, Side.Left}, {x - 1, y, Side.Right}, {x, y + 1, Side.Bottom},
        {x, y - 1, Side.Top},
      }
      for _, side in ipairs(offsets) do
        local sx, sy, side_to_check = unpack(side)
        if self:at(sx, sy) == nil then
          res_rows[sy] = res_rows[sy] or {}
          res_rows[sy][sx] = res_rows[sy][sx] or {}
          table.insert(res_rows[sy][sx], side_to_check)
        end
      end
    end
  end
  local res = {}
  for y, row in pairs(res_rows) do
    for x, sides_to_check in pairs(row) do
      table.insert(res, {x, y, sides_to_check})
    end
  end
  return res
end

function Grid:at(x, y)
  local row = self.rows[y]
  if row then
    return row[x]
  end
end

function Grid:set(x, y, piece)
  self.rows[y] = self.rows[y] or {}
  self.rows[y][x] = piece
end

function Grid:piece_fits(x, y, piece)
  local top = self:at(x, y - 1)
  if top and not piece:matches(top, Side.Top) then return false end
  local right = self:at(x + 1, y)
  if right and not piece:matches(right, Side.Right) then return false end
  local bottom = self:at(x, y + 1)
  if bottom and not piece:matches(bottom, Side.Bottom) then return false end
  local left = self:at(x - 1, y)
  if left and not piece:matches(left, Side.Left) then return false end
  return true
end

function Grid:try_match_piece(x, y, piece)
  if self:piece_fits(x, y, piece) then return piece end
  local flip1 = piece:flip_horizontal()
  if self:piece_fits(x, y, flip1) then return flip1 end
  local flip2 = piece:flip_vertical()
  if self:piece_fits(x, y, flip2) then return flip2 end

  local flip3 = flip1:rotate_clockwise()
  if self:piece_fits(x, y, flip3) then return flip3 end
  local flip4 = flip3:rotate_clockwise()
  if self:piece_fits(x, y, flip4) then return flip4 end
  local flip5 = flip4:rotate_clockwise()
  if self:piece_fits(x, y, flip5) then return flip5 end

  local flip6 = flip2:rotate_clockwise()
  if self:piece_fits(x, y, flip6) then return flip6 end
  local flip7 = flip6:rotate_clockwise()
  if self:piece_fits(x, y, flip7) then return flip7 end
  local flip8 = flip7:rotate_clockwise()
  if self:piece_fits(x, y, flip8) then return flip8 end

  piece = piece:rotate_clockwise()
  if self:piece_fits(x, y, piece) then return piece end
  piece = piece:rotate_clockwise()
  if self:piece_fits(x, y, piece) then return piece end
  piece = piece:rotate_clockwise()
  if self:piece_fits(x, y, piece) then return piece end
  return nil
end

function Grid:solve(pieces)
  -- Recursive method
  local holes = self:get_edge_coords()
  -- local init_piece = table.remove(pieces)
  -- self:set(40, 40, init_piece)
  -- for _, p in ipairs(self:get_edge_coords()) do
  --   local x, y, sides = unpack(p)
  --   print(('x = %d, y = %d, sides: %s'):format(x, y, table.concat(sides, ', ')))
  -- end
  -- print(self:get_answer())

  -- while #pieces > 0 do
  -- end

  -- for coord, sides_to_match in pairs(self:get_edge_coords()) do
  --   print(coord, 'sides to match:', table.concat(sides_to_match, '; '))
  -- end
end

local function table_keys(t)
  local keys = {}
  for k, _ in pairs(t) do
    table.insert(keys, k)
  end
  return keys
end

function Grid:get_bounds()
  local row_indexes = table_keys(self.rows)
  local minrow = math.min(unpack(row_indexes))
  local maxrow = math.max(unpack(row_indexes))
  local col_indexes = table_keys(self.rows[minrow])
  local mincol = math.min(unpack(col_indexes))
  local maxcol = math.max(unpack(col_indexes))
  return mincol, minrow, maxcol, maxrow
end

function Grid:get_answer()
  local x1, y1, x2, y2 = self:get_bounds()
  local nums = {
    self:at(x1, y1).label,
    self:at(x1, y2).label,
    self:at(x2, y1).label,
    self:at(x2, y2).label,
  }
  local result = 1
  for i in ipairs(nums) do
    result = result * tonumber(nums[i])
  end
  return result
end

function Grid:__tostring()
  local x1, y1, x2, y2 = self:get_bounds()
  local size
  do
    local _, r = next(self.rows)
    local _, p = next(r)
    size = p:size()
  end
  local alllines = {}
  for y = y1, y2 do
    local rowlines = {}
    for _ = 1, size do table.insert(rowlines, "") end
    for x = x1, x2 do
      local piece = self:at(x, y)
      if piece == nil then
        for i in ipairs(rowlines) do
          rowlines[i] = rowlines[i] .. (" "):rep(size)
        end
      else
        for i, line in ipairs(piece:strlines()) do
          rowlines[i] = rowlines[i] .. line
        end
      end
    end
    for _, line in ipairs(rowlines) do table.insert(alllines, line) end
  end
  return table.concat(alllines, '\n')
end

local function parsepieces()
  local piecelines = {}
  local tilenum = {}
  local pieces = {}
  for line in io.lines(arg[1]) do
    local m = line:match('^Tile%s+(%d+):$')
    if m ~= nil then
      tilenum = tonumber(m)
    elseif #line == 0 then
      table.insert(pieces, Piece:fromlines(piecelines, tilenum))
      piecelines = {}
    else
      table.insert(piecelines, line)
    end
  end
  table.insert(pieces, Piece:fromlines(piecelines, tilenum))
  return pieces
end

local pieces = parsepieces()
local fst_piece = table.remove(pieces)
local grid = Grid:new()
grid:set(1, 1, fst_piece)

-- Straight assemble
while #pieces > 0 do
  local hole_count = 0
  for x, y in grid:get_holes() do
    hole_count = hole_count + 1
    local piece_index = math.random(#pieces)
    local piece = pieces[piece_index]
    local matched = grid:try_match_piece(x, y, piece)
    if matched ~= nil then
      table.remove(pieces, piece_index)
      print(("Matched at %d,%d"):format(x, y))
      grid:set(x, y, matched)
      break
    end
  end
end

-- Part1
print(grid)
print("Part1 answer: " .. grid:get_answer())

-- Part2
local monster_pattern = {
  "..................#.",
  "#....##....##....###",
  ".#..#..#..#..#..#...",
}

local stripped_lines = {}
local x1, y1, x2, y2 = grid:get_bounds()
for y = y1, y2 do
  local rowlines = {}
  for x = x1, x2 do
    local piece = grid:at(x, y)
    local stripped = piece:strip_border()
    for i, piece_row in ipairs(stripped:strlines()) do
      rowlines[i] = (rowlines[i] or "") .. piece_row
    end
  end
  for _, line in ipairs(rowlines) do table.insert(stripped_lines, line) end
end
local assembled = Piece:fromlines(stripped_lines)

assembled = assembled:flip_horizontal()

-- assembled = assembled:flip_horizontal()
-- assembled = assembled:rotate_anticlockwise()

print(assembled)

local monster_count = 0
for y = 1, #assembled.grid - #monster_pattern do
  for x = 1, #assembled.grid[1] - #monster_pattern[1] do
    if assembled:pattern_matches_at(x, y, monster_pattern) then
      print(("Match at (%d, %d)"):format(x, y))
      monster_count = monster_count + 1
    end
  end
end

local hash_count = 0
for _, line in ipairs(assembled:strlines()) do
  local _, cnt = line:gsub("#", "")
  hash_count = hash_count + cnt
end
print("There are " .. monster_count .. " monsters")
print("Part2 answer: " .. hash_count - (15 * monster_count))
