local unpack = unpack or table.unpack

local Side = {Top = 1, Right = 2, Bottom = 3, Left = 4}
local function opposite_side(side)
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

function Piece:__tostring()
  local lines = {}
  for _, row in ipairs(self.grid) do
    table.insert(lines, table.concat(row))
  end
  local res = ''
  if self.label ~= nil then
    res = res .. 'label: ' .. self.label .. '\n'
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
  return Piece:new(newgrid)
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

function Piece:match(other_piece, side)
  local s1 = self:getside(side)
  local s2 = other_piece:getside(opposite_side(side))
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
  return Piece:new(newgrid)
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
  return Piece:new(newgrid)
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
  return Piece:new(newgrid)
end

Grid = {}
Grid.__index = Grid

function Grid:new()
  local res = setmetatable({rows = {}}, self)
  return res
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

function Grid:solve(pieces)
  local init_piece = table.remove(pieces)
  self:set(40, 40, init_piece)
  for _, p in ipairs(self:get_edge_coords()) do
    local x, y, sides = unpack(p)
    print(('x = %d, y = %d, sides: %s'):format(x, y, table.concat(sides, ', ')))
  end
  -- print(self:get_answer())

  -- while #pieces > 0 do
  -- end

  -- for coord, sides_to_match in pairs(self:get_edge_coords()) do
  --   print(coord, 'sides to match:', table.concat(sides_to_match, '; '))
  -- end
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

local function table_keys(t)
  local keys = {}
  for k, _ in pairs(t) do
    table.insert(keys, k)
  end
  return keys
end

function Grid:get_answer()
  local row_indexes = table_keys(self.rows)
  local minrow = math.min(unpack(row_indexes))
  local maxrow = math.max(unpack(row_indexes))
  local col_indexes = table_keys(self.rows[minrow])
  local mincol = math.min(unpack(col_indexes))
  local maxcol = math.max(unpack(col_indexes))
  return self:at(mincol, minrow).label * self:at(mincol, maxrow).label *
           self:at(maxcol, minrow).label * self:at(maxcol, maxrow).label
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
local grid = Grid:new()
grid:solve(pieces)
