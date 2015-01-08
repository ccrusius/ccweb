local luaunit = dofile "./luaunit/luaunit.lua"

local function startswith(str,start)
  return (string.find(str,start,1,true) == 1)
end

local function docmd(cmd)
  local status = os.execute(cmd)
  if status == 0 or status == true then return true end
  return false
end

local function docctangle(filename)
    print("cctangle "..filename)
    return docmd("../bin/cctangle "..filename..".ccw > "..filename..".out")
end

local function doccweave(filename)
    print("ccweave "..filename)
    return docmd("../bin/ccweave "..filename..".ccw")
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

function simple_tangle_test(base)
  assert(docctangle(base)==true)
  assert(diff(base..".outref",base..".out")==false)
  os.remove(base..".out")
end

function simple_weave_test(base)
  assert(doccweave(base)==true)
  assert(diff(base..".texref",base..".tex")==false)
  os.remove(base..".tex")
end

function test_test001() simple_tangle_test("test001") end

function test_test002()
  assert(docctangle("test002")==true)
  assert(diff("test002.outref","test002.out")==false)
  assert(diff("test002.luaref","test002.lua")==false)
  assert(diff("test002.cref","test002.c")==false)
  os.remove("test002.out")
  os.remove("test002.lua")
  os.remove("test002.c")
end

function test_test003() simple_tangle_test("test003") end
function test_test004() simple_weave_test("test004") end

function test_test005()
  local name="test005"
  assert(doccweave(name)==true)
  assert(docmd("pdftex -interaction=nonstopmode "..name)==true)
  os.remove(name..".pdf")
  os.remove(name..".tex")
end

function test_job_name_option()
  print("create jobname.ccw")
  local file=io.open("jobname.ccw","w+")
  assert(file,"Coudn't open jobname.ccw for writing.")
  file:write([[@* Test job name command line option.]])
  file:close()

  local dojobweave = function(jobopt,jobname,inpfile,good)
    local outfile = jobname
    if jobname == "" then outfile = inpfile end
    os.remove(outfile..".tex")
    os.remove(outfile..".idx")
    os.remove(outfile..".scn")

    local cmd = "ccweave.lua "..jobopt..jobname.." "..inpfile
    if inpfile ~= "" then cmd = cmd .. ".ccw" end
    print(">> "..cmd)
    assert(docmd("lua -- ../bin/"..cmd)==good)

    file=io.open(outfile..".tex")
    if good ~= true then
      assert(file == nil,outfile.." should not have been created.")
    else
      assert(file,"Coudn't open "..outfile.." for reading.")
      assert((file:read("*a"):find("Test job name")),"jobname_mod.tex is not what I expected.")
      file:close()
    end

    os.remove(outfile..".tex")
    os.remove(outfile..".idx")
    os.remove(outfile..".scn")

    print("Pass")
  end

  dojobweave("-job-name=","jobname_mod","jobname",true)
  dojobweave("-job-name ","jobname_mod","jobname",true)
  dojobweave("-job-name=","","jobname",true)
  dojobweave("--job-name=","jobname_mod","jobname",true)
  dojobweave("--job-name ","jobname_mod","jobname",true)

  dojobweave("-job-name","","",false)
  dojobweave("-job-name","","jobname",false)
  dojobweave("-jorb-name=","something","jobname",false)

  os.remove("jobname.tex")
  os.remove("jobname.ccw")
end

lu = LuaUnit.new()
os.exit(lu:runSuite())
