local chunks = {}
local chunkdef = "^@<%s*(.*)%s*@>=.*$"
local function findchunkref(line)
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
local function unquoteline(line)
  return (string.gsub(line,"@\(.\)","%1"))
end
local STARTS_CHUNK = 1
local STARTS_DOC   = 2
local function startssec(line)
  if string.find(line,"^@[ %*]") then return STARTS_DOC end
  if string.find(line,chunkdef) then return STARTS_CHUNK end
  return false
end
local function readchunk(line,nextline)
  local title = string.gsub(line,chunkdef,"%1")
  if not chunks[title] then chunks[title] = {} end
  line = nextline()
  while line and not startssec(line) do
    table.insert(chunks[title],line)
    line = nextline()
  end
  return line
end
local function printchunk(title,prefix,file)
  local chunk = chunks[title]
  assert(chunk,"Chunk '"..title.."' not found")

  local idx, pre, title, line, refindent
  local chunkindent = ""
  for idx,line in pairs(chunk) do
    file:write(chunkindent)
    refindent = prefix
    repeat
      pre,title,line = findchunkref(line)
      refindent = refindent .. string.gsub(pre,"."," ")
      file:write(unquoteline(pre))
      if title then printchunk(title,refindent,file) end
    until not line
    chunkindent = "\n"..prefix
  end
end
input = arg[1]
inputf, errmsg = io.open(input)
assert(inputf, errmsg)

nextline = inputf:lines()
line = nextline()
while line do
  if startssec(line) == STARTS_CHUNK then line = readchunk(line,nextline)
  else line = nextline() end
end

for title,chunk in pairs(chunks) do
  if string.find(title,"^%*") then
    if title == "*" then printchunk(title,"",io.output())
    else
      local file_name, status = string.gsub(title,"^%*{(.*)}","%1")
      assert(status == 1,"Chunk '"..title.."' does not specify a file name.")
      local file = io.open(file_name,"w+")
      assert(file,"Could not open '"..file_name.."' for writing.")
      printchunk(title,"",file)
      file:close()
    end
  end
end