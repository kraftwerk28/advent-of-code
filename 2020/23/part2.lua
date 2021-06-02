local cups = {}
local CUPS_COUNT = 1000000
for i = 1, CUPS_COUNT do
  table.insert(cups, i + 1)
end

local input_cups = {}
for i = 1, #arg[1] do
  local n = tonumber(arg[1]:sub(i, i))
  table.insert(input_cups, n)
end
for i, cup in ipairs(input_cups) do
  cups[cup] = input_cups[i + 1] or #input_cups + 1
end
cups[#cups] = input_cups[1]

local current_cup = input_cups[1]
local picked = {}

local function picked_has_cup(cup)
  for _, c in ipairs(picked) do
    if c == cup then
      return true
    end
  end
  return false
end

local function move()
  picked[1] = cups[current_cup]
  picked[2] = cups[picked[1]]
  picked[3] = cups[picked[2]]
  cups[current_cup] = cups[picked[3]]
  local dest_cup = current_cup
  current_cup = cups[current_cup]
  repeat
    dest_cup = dest_cup - 1
    if dest_cup < 1 then
      dest_cup = CUPS_COUNT
    end
  until not picked_has_cup(dest_cup)
  local cup_after_insertion = cups[dest_cup]
  cups[dest_cup] = picked[1]
  cups[picked[3]] = cup_after_insertion
end

for _ = 1, 10000000 do
  move()
end

print(cups[1] * cups[cups[1]])

-- cur_cup = cups[1]
-- for i = #cups + 1, 1000000 do
--   table.insert(cups, i)
-- end

-- for i = 1, 10000000 do
--   if i % 100 == 0 then
--     print(i)
--   end
--   move2()
--   local one_index = index_of(cups, 1)
--   print('index of "1": ' .. one_index)
--   if one_index < 999999 then
--     print(cups[one_index + 1] .. ' ' .. cups[one_index + 2])
--   end
-- end

-- local one_index = index_of(cups, 1)
-- print(cups[one_index + 1] * cups[one_index + 2])
