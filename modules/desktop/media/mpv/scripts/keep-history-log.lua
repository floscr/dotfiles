local path
local title
local loaded = false

local function pad(str, len, char)
  if char == nil then char = ' ' end
  return str .. string.rep(char, len - #str)
end

local function log(event)
  local f = io.open(os.getenv('HOME') .. '/.cache/mpv_history.log', 'a+')
  f:write(('[%s];;;;%s;;;;%s;;;;%s\n'):format(os.date('%Y-%m-%d %H:%M:%S'), event, path, title))
  f:close()
end

local function log_file_loaded(event)
  path = mp.get_property('path')
  title = mp.get_property_osd('media-title')
  loaded = true
  log('loaded')
end

local function log_end_file(event)
  if not loaded then return end
  loaded = false
  log('unloaded')
end

mp.register_event('file-loaded', log_file_loaded)
mp.register_event('end-file', log_end_file)
