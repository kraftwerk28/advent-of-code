local cups = {}
for i = 1, #arg[1] do
  local n = tonumber(arg[1]:sub(i, i))
  table.insert(cups, n)
end
local cur_cup = cups[1]
local unpack = table.unpack or unpack
local min_cup = math.min(unpack(cups))
local max_cup = math.max(unpack(cups))

local function includes(t, value)
  for _, v in ipairs(t) do
    if v == value then
      return true
    end
  end
  return false
end

local function index_of(t, value)
  for i, v in ipairs(t) do
    if v == value then
      return i
    end
  end
  return -1
end

local function rotate(t)
  local last = table.remove(t, 1)
  table.insert(t, last)
end

-- Part 1
local function move()
  local picked = {}
  for _ = 1, 3 do
    table.insert(picked, table.remove(cups, 2))
  end
  local dest_cup = cur_cup
  repeat
    dest_cup = dest_cup - 1
    if dest_cup < min_cup then
      dest_cup = max_cup
    end
  until not includes(picked, dest_cup)
  local dest_index = index_of(cups, dest_cup)
  for i, c in ipairs(picked) do
    table.insert(cups, dest_index + i, c)
  end
  rotate(cups)
  cur_cup = cups[1]
end
for _ = 1, 100 do
  move()
end
while cups[1] ~= 1 do
  rotate(cups)
end
print(table.concat(cups):sub(2))
