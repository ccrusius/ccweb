local sections = {}
local function unquoteline(line)
  return (string.gsub(line,"@(.)","%1"))
end

local function process_named_reference(line)
  local start, startend = string.find(line,"^@<")
  if not start then
    start, startend = string.find(line,"[^@]@<")
    if not start then return line, nil, nil end
    start = start + 1 -- The true start, past "[^@]"
  end

  local finish = string.find(line,"[^@]@>",start)
  if not finish then return line, nil, nil end

  local pre   = string.sub(line,1,start-1)
  local title = string.sub(line,startend+1,finish)
  local post  = string.sub(line,finish+3)
  return pre, title, post
end

local function secdef_match(line)
  if line then
    local ms, me
    for i,regexp in pairs({"^@[%s%*]%s*", "^@$"}) do
      ms, me = string.find(line,regexp)
      if ms then return ms, me end
    end
  end
end
local function namedef_match(line)
  if line then
    local name, found = string.gsub(line,"^%s*@<%s*(.*)%s*@>=.*$","%1")
    if found == 1 then return name end
  end
end
local function read_section(line,get_next_line)
  local ms, me = secdef_match(line)
  assert(ms,"Line '"..line.."' does not start a section.")
  line = string.sub(line,me+1) -- Remove start section token

  local section = {
                    ["star"] = false,
                    ["body"] = {},
                    ["name"] = nil,
                    ["code"] = {},
                  }
  section["star"] = (line[2] == "*")

  while line and not secdef_match(line) and not namedef_match(line) do
    table.insert(section["body"],line)
    line = get_next_line()
  end
  section["name"] = namedef_match(line)
  if section["name"] then
    while true do
      line = get_next_line()
      if not line or secdef_match(line) then break end
      table.insert(section["code"],line)
    end
  end

  table.insert(sections,section)
  return line
end
local function read_ccweb_file(file_name)
  io.stderr:write("(Reading CCWEB file '"..file_name.."')")
  local file, msg = io.open(file_name)
  assert(file,msg)

  get_next_line = file:lines()
  local line = get_next_line()
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
        repeat
          pre,ref_name,line = process_named_reference(line)
          ref_prefix = ref_prefix .. string.gsub(pre,"."," ")
          file:write(unquoteline(pre))
          if ref_name then tangle_section(ref_name,ref_prefix,file) end
        until not line
        cur_prefix = "\n" .. prefix -- Start indenting and writing line breaks.
      end
    end
  end
end
read_ccweb_file(arg[1])

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
