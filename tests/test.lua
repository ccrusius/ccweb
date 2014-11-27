local luaunit = dofile "./luaunit/luaunit.lua"

local function startswith(str,start)
  return (string.find(str,start,1,true) == 1)
end

local function docctangle(filename)
    print("cctangle "..filename)
    local status = os.execute("lua ../bin/cctangle.lua "..filename..".ccw > "..filename..".out")
    if status == 0 then status = true end -- Windows
    return status
end

local function diff(from,to)
  local fromf,tof,msg

  fromf,msg=io.open(from) assert(fromf,msg)
  tof,  msg=io.open(to)   assert(tof,  msg)

  local nextfrom = fromf:lines()
  local nextto   = tof:lines()

  local hasdiffs = false
  local froml, tol
  repeat
    froml = nextfrom()
    tol   = nextto()
    if froml ~= tol then
      print("----")
      hasdiffs = true
      if not froml then print("+"..tol)
      elseif not tol then print("-"..froml)
      else
        print("-"..froml)
        print("+"..tol)
      end
    end
  until not froml or not tol

  fromf:close()
  tof:close()

  return hasdiffs
end

function test_test001()
  assert(docctangle("test001")==true)
  assert(diff("test001.outref","test001.out")==false)
  os.remove("test001.out")
end

function test_test002()
  assert(docctangle("test002")==true)
  assert(diff("test002.outref","test002.out")==false)
  assert(diff("test002.luaref","test002.lua")==false)
  assert(diff("test002.cref","test002.c")==false)
  os.remove("test002.out")
  os.remove("test002.lua")
  os.remove("test002.c")
end

lu = LuaUnit.new()
os.exit(lu:runSuite())
