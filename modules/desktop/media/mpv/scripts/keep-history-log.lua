local path
local title
local loaded = false

local function log(event)
  os.execute(("mpv_ctrl add-log %s %s %s"):format(event, path, title))
end

local function log_file_loaded(event)
  path = mp.get_property('path')
  title = mp.get_property_osd('media-title')
  loaded = true
  log('loaded')
end

-- local function log_end_file(event)
--   if not loaded then return end
--   loaded = false
--   log('unloaded')
-- end

mp.register_event('file-loaded', log_file_loaded)
-- mp.register_event('end-file', log_end_file)
