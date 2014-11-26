input = arg[1]

inputf, errmsg = io.open(input)
assert(inputf, errmsg)

nextline = inputf:lines()

function startswith(str,start)
  return (string.find(str,start,1,true) == 1)
end

chunks = {}

function newchunk(l)
  local title, n = string.gsub(l,"^@<(.*)@>=.*$","%1")
  -- io.stderr:write("Chunk: "..title.."\n")

  if not chunks[title] then chunks[title] = {} end

  l = nextline()
  while l and not string.find(l,"^@[ \*]") and not string.find(l,"^@<.*@>=") do
    -- io.stderr:write("      "..l.."\n")
    table.insert(chunks[title],l)
    l = nextline()
  end

  return l
end

line = nextline()
while line do
  if string.find(line,"^@<.*@>=") then line = newchunk(line,nextline)
  else line = nextline() end
end

function printchunk(title,indent)
  local idx, line
  local chunk = chunks[title]
  assert(chunk,"Chunk '"..title.."' not found")
  local thisindent = ""
  for idx,line in pairs(chunk) do
    -- io.stderr:write(">>> "..line.."\n")
    if idx > 1 then io.write("\n") end
    local start = string.find(line,"^@<",1)
    if not start then start = string.find(line,"[^@]@<",1) end
    local finish = string.find(line,"@>",start,true)
    if start and finish then
      io.write(thisindent..string.gsub(string.sub(line,1,start-1),"@@","@"))
      printchunk(string.sub(line,start+2,finish-1),indent..string.rep(" ",start-1))
      io.write(thisindent..string.gsub(string.sub(line,finish+2),"@@","@"))
    else
      io.write(thisindent..string.gsub(line,"@@","@"))
    end
    thisindent = indent
  end
end

printchunk("*","")
