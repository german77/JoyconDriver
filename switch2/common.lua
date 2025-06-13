
local function hex(value)
    if value == nil then return "" end
    return string.format('%02x',value)
end

local function getBytes(buffer)
    local length = buffer:len() -1
    local buttons_array = {}
    local buttons_text = " (none)"

    for i=0,length do
        table.insert(buttons_array, hex(buffer:bytes():get_index(i)))
    end

    if #buttons_array ~= 0 then
        buttons_text = " (" .. table.concat(buttons_array, "") .. ")"
    end
    return buttons_text
end

local function getBytes2(buffer, size)
    local buttons_array = {}
    local buttons_text = "00"

    for i=0,size - 1 do
        table.insert(buttons_array, hex(buffer[i]))
    end

    if #buttons_array ~= 0 then
        buttons_text = table.concat(buttons_array, "")
    end
    return buttons_text
end

return {
   hex = hex,
   getBytes = getBytes,
   getBytes2 = getBytes2,
}