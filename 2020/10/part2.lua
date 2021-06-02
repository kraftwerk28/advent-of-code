local inp, memo, joltset = {}, {}, {[0] = true}
for line in io.lines(arg[1]) do
  local n = tonumber(line)
  if n ~= nil then
    table.insert(inp, n)
    joltset[n] = true
  end
end
table.sort(inp)
local last = inp[#inp] + 3
table.insert(inp, last)
joltset[last] = true

local function ways_to_jolt(jolt)
  if not joltset[jolt] then
    -- If that jolt doesn't even exist, there're no ways to it
    return 0
  end
  if jolt <= 0 then
    -- There's only one way to get to 1st jolt
    return 1
  end
  if memo[jolt] ~= nil then
    -- Maybe this value is already calculated
    return memo[jolt]
  end
  local r = 0
  for i = 1, 3 do
    -- Sum all ways to that jolt
    -- by picking all (3) jolts, that come before
    r = r + ways_to_jolt(jolt - i)
  end
  memo[jolt] = r
  return r
end

print('Answer: ' .. ways_to_jolt(inp[#inp]))
