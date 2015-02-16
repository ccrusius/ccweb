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
tex_quotes = {
  ["&"]  = "{\\AM}", -- Ampersand, from cwebmac
  ["\\"] = "{\\BS}", -- Backslash, from cwebmac
  ["{"]  = "{\\LB}", -- Left brace, from cwebmac
  ["}"]  = "{\\RB}", -- Right brace, from cwebmac
  [" "]  = "~",      -- Non-breakable space
  ["~"]  = "{\\TL}", -- Tilde, from cwebmac
  ["_"]  = "{\\UL}", -- Underline, from cwebmac
  ["^"]  = "{\\CF}", -- Circumflex, from cwebmac
  ["#"]  = "{\\#}",  -- Hash
  ["$"]  = "{\\$}",  -- Dollar
  ["%"]  = "{\\%}",  -- Percent
}
local cross_ref_map = {}
local jobname
local inputname
local function tex_quoted(line)
  local result = ""
  for c in line:gmatch(".") do
    local repl = tex_quotes[c]
    if repl then result = result .. repl
    else result = result .. c
    end
  end
  return "\\hbox{\\tentex "..result.."}"
end
local function build_cross_ref_map()
  for n,section in ipairs(sections) do
    local name = section["name"]
    if name then
      local record = cross_ref_map[name]
      if not record then
        record = {
                   ["secno"]  = -1, -- What is the section number?
                   ["more"]   = {}, -- Other sections adding code to this.
                   ["usedin"] = {}  -- Sections whose code refer to this.
                 }
        record["secno"] = n
      else
        table.insert(record["more"],n)
      end
      cross_ref_map[name] = record
    end
  end
  for n,section in ipairs(sections) do
    for o,codeline in ipairs(section["code"]) do
      for p,node in ipairs(codeline) do if node and p%2 == 0 then
        local name = node["line"]
        assert(cross_ref_map[name],
               "Section @<"..name.."@> does not seem to be defined anywhere.")
        local usedin = cross_ref_map[name]["usedin"]
        if usedin[#usedin] ~= n then
          table.insert(usedin,n)
        end
      end end
    end
  end
end
local function write_cross_reference(prefix,sections,file)
  local n = #sections
  if n == 0 then return end
  file:write(prefix)
  if n > 1  then file:write("s") end
  for k,v in ipairs(sections) do
    if k > 1 then
      if k == n then file:write([[\ET]]) end
      if k == n and n > 2 then file:write("s") end
      if k ~= n then file:write(",") end
    end
    file:write(tostring(v))
  end
  file:write(".")
end
local function write_text_line(text,file)
  local line = text["line"]
  for n, node in ipairs(text["nodes"]) do
    if node then
      local str = unquoteline(string.sub(line,node[1],node[2]))
      if n%2 == 0 then str = tex_quoted(str) end -- even n: verbatim text
      file:write(str)
    end
  end
end
local function write_section_reference(name,file,all_numbers)
  local record = cross_ref_map[name]
  assert(record,"Section @<"..name.."@> does not seem to be defined anywhere.")
  file:write("\\setbox\\cctanglebox\\hbox{")
  if string_at(name,1) == "*" then
    file:write(tex_quoted(unquoteline(name)))
  else
    write_text_line((parse_text_line(name,"$")),file)
  end
  file:write([[}\X]])
  file:write(tostring(record["secno"]))
  if all_numbers then for k,v in ipairs(record["more"]) do
    file:write(","..tostring(v))
  end end
  file:write(":\\box\\cctanglebox\\X")
end
local function write_code_line(code, file)
  for n, node in ipairs(code) do if node then
    if n%2 == 1 then file:write(tex_quoted(unquoteline(node)))
    else write_section_reference(node["line"],file)
    end
  end end
end
local function write_section(secno,section,file)
  if section["star"] then
    file:write("\\N{"..tostring(section["depth"]).."}{"..tostring(secno).."}")
  else
    file:write("\\M{"..tostring(secno).."}")
  end
  for n,line in ipairs(section["body"]) do
    write_text_line(line,file)
    file:write("\n")
  end
  if section["name"] then
    local name = section["name"]
    local record = cross_ref_map[name]
  
    if #(section["body"]) > 0 then file:write("\\Y") end
    file:write("\\B\\4")
    write_section_reference(name,file)
    file:write("${}")
    if record["secno"] ~= secno then file:write("\\mathrel+") end
    file:write("\\equiv{}$\n")
  
    for n,line in ipairs(section["code"]) do
      file:write("\\6")
      write_code_line(line,file)
      file:write("\n")
    end
  
    file:write("\\par")
    if record["secno"] == secno then
      write_cross_reference([[\A]],record["more"],file)
      write_cross_reference([[\U]],record["usedin"],file)
    end
  end
  file:write("\\fi\n")
end
local function usage()
  io.stderr:write("Usage: "..arg[0]..[[ [-job-name <name>] <input>
Weaves <input> file into <input>.tex, or <name>.tex if the -job-name option
is specified.
]])
end
local index = getopt({ ["job-name"] = function(v) jobname = v end }, usage)
inputname = arg[index]
if not inputname then
  io.stderr:write("You must specify the input file name\n")
  usage()
  os.exit(1)
end

if not jobname or jobname == "" then
  jobname = string.gsub(string.gsub(inputname,".*[/\\]",""),"%.[^.]*$","")
end
read_file(inputname)

local file=io.open(jobname .. ".tex","w+")
assert(file,"Could not open '"..jobname..".tex' for writing.")
build_cross_ref_map()
file:write([[\input cwebmac
             \newbox\cctanglebox]])
file:write("\n")
for i,line in ipairs(preamble) do file:write(line.."\n") end
local index_file=io.open(jobname..".idx","w+")
assert(index_file,"Could not open '"..jobname..".idx' for writing.")
index_file:close()
local section_file=io.open(jobname..".scn","w+")
assert(section_file,"Could not open '"..jobname..".scn' for writing.")
local sorted_names = {}
for n,section in ipairs(sections) do table.insert(sorted_names,section["name"]) end
table.sort(sorted_names,function (n1,n2)
    return n1:gsub("[^A-Za-z%*]",""):lower() < n2:gsub("[^A-Za-z%*]",""):lower()
  end)
local last_name
for n,name in ipairs(sorted_names) do
  if name ~= last_name then
    local record = cross_ref_map[name]
    section_file:write([[\I]])
    write_section_reference(name,section_file,true)
    write_cross_reference([[\U]],record["usedin"],section_file)
    section_file:write("\n")
    last_name = name
  end
end
section_file:close()

for n,section in ipairs(sections) do
  write_section(n,section,file)
end

file:write([[\inx
             \fin
             \con]])
file:close()