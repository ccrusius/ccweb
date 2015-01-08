local sections = {}
local preamble = {}
local function string_at(str,idx)
  if not str or not idx then return nil end
  if string.len(str) < idx then return nil end
  return string.sub(str,idx,idx)
end
local function starts_with(line,expr)
  if not line then return false end
  if type(expr) == "string" then
    return (string.find(line,"^"..expr))
  elseif type(expr) == "table" then
    for k,v in pairs(expr) do
      if starts_with(line,v) then return true end
    end
  else
    assert(false,"starts_with can not handle '"..type(expr).."' expression.")
  end
  return false
end
local function unquoteline(line)
  return (string.gsub(line,"@(.)","%1"))
end
local function print_table(tree,file,prefix)
  file:write("{\n")
  for k,v in pairs(tree) do
    file:write(prefix.."  ["..tostring(k).."] = ")
    if type(v) == "table" then print_table(v,file,prefix.."    ")
    else file:write(tostring(v).."\n") end
  end
  file:write(prefix.."}\n")
end

local function getopt(spec,usage)
  local index = 0
  while true do
    index = index + 1
    local opt = arg[index]
    if not starts_with(opt,"%-") then return index end

    local name, value = opt:match("^%-%-?([^=]+)(=?.*)$")
    local handler = spec[name]

    if not name or not handler then
      io.stderr:write("Unknown option '"..opt.."'\n")
      usage()
      os.exit(1)
    end

    if starts_with(value,"=") then
      value = value:sub(2)
    else
      index = index + 1
      value = arg[index]
    end

    handler(value)
  end
end
local function parse_chunk(line,stop)
  local result, rest = "", line
  while rest ~= "" and not starts_with(rest,stop) do
    local c = string_at(rest,1)
    if c == "@" then
      result = result .. c
      rest = string.sub(rest,2)
      c = string_at(rest,1)
      assert(c,"End of line '"..line.."' found while unquoting.")
    end
    result = result .. c
    rest = string.sub(rest,2)
  end
  return result, rest
end
local function parse_text_line(line,stop)
  local result = { ["line"] = "", ["nodes"] = {} }
  local rest = line
  while rest~="" and not starts_with(rest,stop) do
    local text
    text,rest = parse_chunk(rest,{"|",stop})
    if text ~= "" then table.insert(result["nodes"],{
                         string.len(result["line"])+1,
                         string.len(result["line"])+string.len(text)
                       })
                       result["line"]=result["line"]..text
    elseif not starts_with(rest,stop) then table.insert(result["nodes"],false)
    end
    if starts_with(rest,stop) then return result, rest end
    assert(rest~="","End of line '"..line.."' found while parsing text.")
    rest=string.sub(rest,2)
    text,rest = parse_chunk(rest,"|")
    assert(rest~="","End of line '"..line.."' found while processing verbatim.")
    
    result["line"]=result["line"].."|"
    if text ~= "" then table.insert(result["nodes"],{
                         string.len(result["line"])+1,
                         string.len(result["line"])+string.len(text)
                       })
                       result["line"]=result["line"]..text
    else table.insert(result["nodes"],false) end
    result["line"]=result["line"].."|"
    
    rest=string.sub(rest,2)
  end
  assert(starts_with(rest,stop),"End of line '"..line.."' found while parsing text.")
  return result,rest
end
local function parse_code_line(line)
  local result = {}
  local rest = line
  while rest ~= "" do
    local text

    -- Code
    text, rest = parse_chunk(rest,{"@<","$"})
    if text ~= "" then table.insert(result,text)
    elseif starts_with(rest,"@<") then table.insert(result,false)
    end
    if rest == "" then return result end

    -- Section reference
    rest = string.sub(rest,3)
    text, rest = parse_text_line(rest,"@>")
    assert(rest~="","End of line '"..line.."' found while processing section reference.")
    table.insert(result,text)
    rest = string.sub(rest,3)
  end
  return result
end
local function get_section(line)
  local section = {
                    ["star"]  = false,
                    ["body"]  = {},
                    ["name"]  = nil,
                    ["code"]  = {},
                    ["depth"] = 1
                  }
  local match
  local match2
  match, match2 = string.match(line,"^@%*([0-9]+)%s*([^%s].*)$") if match then
    section["star"] = true
    section["depth"] = tonumber(match)+1
    return section, match2
  end
  match = string.match(line,"^@%*%*?%s*([^%s].*)$") if match then
    section["star"] = true
    if string_at(line,3) == "*" then section["depth"] = 0 end
    return section, match
  end
  match = string.match(line,"^@%s%s*([^%s].*)$") if match then
    return section, match
  end
  if string.find(line,"^@%s*$") then return section, nil end
end
local function get_section_name(line)
  if line and starts_with(line,"@<") then
    local parse, rest = parse_text_line(string.sub(line,3),"@>=")
    if starts_with(rest,"@>=") then return parse["line"] end
  end
end
local function read_section(line,get_next_line)
  local section, rest = get_section(line)
  assert(section,"Line '"..line.."' does not start a section.")
  if not rest then line = get_next_line()
  else line = rest end
  while line and not get_section(line) and not get_section_name(line) do
    table.insert(section["body"],(parse_text_line(line,"$")))
    line = get_next_line()
  end
  section["name"] = get_section_name(line)
  if section["name"] then
    while true do
      line = get_next_line()
      if not line or get_section(line) then break end
      local codeline = parse_code_line(line)
      table.insert(section["code"],codeline)
    end
  end
  table.insert(sections,section)
  return line
end
local function read_file(file_name)
  io.stderr:write("(Reading CCWEB file '"..file_name.."')")
  local file, msg = io.open(file_name)
  assert(file,msg)

  get_next_line = file:lines()
  local line = get_next_line()
  while line and not get_section(line) do
    table.insert(preamble,line)
    line = get_next_line()
  end
  while line do line = read_section(line, get_next_line) end
end
local function tangle_section(name,prefix,file)
  local cur_prefix = "" -- Do not write anything before first line
  for n,section in ipairs(sections) do
    if section["name"] == name then
      local ref_prefix, ref_name, pre
      for n,line in ipairs(section["code"]) do
        file:write(cur_prefix)
        ref_prefix = prefix
        for n,node in ipairs(line) do
          if node then
            if n%2 == 0 then tangle_section(node["line"],ref_prefix,file)
            else ref_prefix = ref_prefix .. string.gsub(node,"."," ")
                 file:write(unquoteline(node))
            end
          end
        end
        cur_prefix = "\n" .. prefix -- Start indenting and writing line breaks.
      end
    end
  end
end
read_file(arg[1])

local processed_main_sections = {}

for n,section in ipairs(sections) do
  local name = section["name"]
  if name and not processed_main_sections[name] and string.find(name,"^%*") then
    processed_main_sections[name] = true
    io.stderr:write("(Writing main section '"..name.."')")
    if name == "*" then tangle_section(name,"",io.output())
    else
      local file_name, status = string.gsub(name,"^%*{(.*)}","%1")
      assert(status == 1,"Chunk '"..name.."' does not specify a file name.")
      local file = io.open(file_name,"w+")
      assert(file,"Could not open '"..file_name.."' for writing.")
      tangle_section(name,"",file)
      file:close()
    end
  end
end