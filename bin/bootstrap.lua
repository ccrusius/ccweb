local SEC_REGULAR = 1
local SEC_STARRED = 2
local SEC_NAMED   = 3
local SEC_TYPES = {
  [SEC_REGULAR] = "^@ ",
  [SEC_STARRED] = "^@%*",
  [SEC_NAMED]   = "^@<%s*(.*)%s*@>=.*$"
}
local sections = {}
local curr_section_number
local function unquoteline(line)
  return (string.gsub(line,"@\(.\)","%1"))
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



local function get_section_type(line)
  for t,r in pairs(SEC_TYPES) do
    if string.find(line,r) then return t end
  end
  return nil
end
local function read_section(line,get_next_line)
  local section = {
    ["type"] = get_section_type(line),
    ["body"] = {},
  }

  if section["type"] == SEC_NAMED then
    section["name"] = string.gsub(line,SEC_TYPES[SEC_NAMED],"%1")
  elseif section["type"] then
    curr_section_number = curr_section_number + 1
    section["number"] = curr_section_number
  end
  repeat
    line = get_next_line()
    if not line or get_section_type(line) then break end
    table.insert(section["body"],line)
  until false

  table.insert(sections,section)
  return line
end
local function read_ccweb_file(file_name)
  curr_section_number = 0
  local file, msg = io.open(file_name)
  assert(file,msg)

  get_next_line = file:lines()
  local line = get_next_line()
  while line do line = read_section(line, get_next_line) end
end


local function tangle_section(name,prefix,file)
  local cur_prefix = ""
  for n,section in ipairs(sections) do
    if section["name"] == name then
      local ref_prefix, ref_name, pre
      for n,line in ipairs(section["body"]) do
        file:write(cur_prefix)
        ref_prefix = prefix
        repeat
          pre,ref_name,line = process_named_reference(line)
          ref_prefix = ref_prefix .. string.gsub(pre,"."," ")
          file:write(unquoteline(pre))
          if ref_name then tangle_section(ref_name,ref_prefix,file) end
        until not line
        cur_prefix = "\n" .. prefix
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