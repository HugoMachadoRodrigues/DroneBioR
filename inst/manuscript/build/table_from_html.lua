--[[
Replace each table-image placeholder with the real table.

The manuscript refers to its tables as images:  ![](figures/table5_performance.png)
Every one of those has an HTML sibling that is the actual source of the table
(figures/table5_performance.html), from which the PNG was rendered. This filter
reads that sibling and splices the parsed table into the document, so the
journal receives a real Word table -- selectable, searchable, restylable -- and
not a picture of one.

The HTML stays the single source of truth: nothing is transcribed into
Markdown, so the two can never drift. Figures are untouched; only images whose
path resolves to an existing .html are replaced, which is exactly the tables.

The HTML <caption> becomes the table's caption, so the caption stops being
pixels baked into a PNG and becomes text.
--]]

--[[ Geometry is not set here.

Column widths and the table's type size are decided in docx_postprocess.py,
which works on the finished XML. They have to be decided together -- when the
columns cannot all be given room for their longest word, the answer is a
smaller type size for that table, not narrower columns -- and only one place
can own that trade.
--]]

--[[ Bold that lives in the stylesheet -----------------------------------------

The tables mark meaning with weight: the stub column of Table 5, the index
names of Table 1, and -- the one that actually matters -- the row holding the
*default* parameter setting in Tables 6 and 7. All of it is CSS
(`tbody td.w{font-weight:700}`, `<tr style="font-weight:700">`), and pandoc's
HTML reader does not turn CSS into Strong, so it all arrived in Word as plain
text and the default row stopped being identifiable.

Row attributes do survive the read, but cell classes do not, so the weight is
put back before the read instead of after: the file's own <style> block says
which classes are bold, and those cells are wrapped in <strong>. Reading the
rule out of the stylesheet rather than hard-coding the class names keeps the
HTML the single source of truth -- add a bold class there and it just works.
--]]

local function bold_classes(html)
  local set = {}
  for css in html:gmatch("<style[^>]*>(.-)</style>") do
    for selector, body in css:gmatch("([^{}]+)%s*{([^}]*)}") do
      local weight = body:match("font%-weight%s*:%s*(%d+)")
      if weight and tonumber(weight) >= 600 then
        for cls in selector:gmatch("%.([%w_-]+)") do set[cls] = true end
      end
    end
  end
  return set
end

local function has_bold_class(attrs, bold)
  -- The generated files are not consistent about quoting: table 8 writes
  -- <td class='w'> and the rest write <td class="w">, so accept either.
  for quote in ('"\''):gmatch(".") do
    for cls in attrs:gmatch("class=" .. quote .. "([^" .. quote .. "]*)" .. quote) do
      for c in cls:gmatch("[%w_-]+") do
        if bold[c] then return true end
      end
    end
  end
  return false
end

local function embolden(html)
  local bold = bold_classes(html)

  -- A whole row set bold, either inline or through a class such as tr.grp.
  html = html:gsub("(<tr[^>]*>)(.-)(</tr>)", function(open, body, close)
    if not (open:match("font%-weight%s*:%s*[6-9]%d%d") or has_bold_class(open, bold)) then
      return open .. body .. close
    end
    body = body:gsub("(<t[dh][^>]*>)(.-)(</t[dh]>)", "%1<strong>%2</strong>%3")
    return open .. body .. close
  end)

  -- Individual cells, and inline spans like the emphasised answers of Table 8.
  local function wrap(open, attrs, body, close)
    if has_bold_class(attrs, bold) then
      return open .. "<strong>" .. body .. "</strong>" .. close
    end
    return open .. body .. close
  end
  html = html:gsub("(<t[dh]%s([^>]*)>)(.-)(</t[dh]>)", wrap)
  html = html:gsub("(<span%s([^>]*)>)(.-)(</span>)", wrap)
  return html
end

local function read_file(path)
  local fh = io.open(path, "r")
  if not fh then return nil end
  local text = fh:read("a")
  fh:close()
  return text
end

-- A table placeholder is a paragraph holding nothing but the image.
local function table_source(blocks)
  if #blocks ~= 1 then return nil end
  local para = blocks[1]
  if para.t ~= "Para" or #para.content ~= 1 then return nil end
  local img = para.content[1]
  if img.t ~= "Image" then return nil end
  local html = img.src:gsub("%.png$", ".html")
  if html == img.src then return nil end
  return html, img.src
end

function Para(para)
  local html, png = table_source({ para })
  if not html then return nil end
  local text = read_file(html)
  if not text then return nil end          -- a figure, not a table: leave it

  local doc = pandoc.read(embolden(text), "html")
  local out = {}
  for _, block in ipairs(doc.blocks) do
    -- The renderer wraps each table in <div class="wrap"> for the PNG shot;
    -- that wrapper has no meaning in the document, so unwrap it.
    if block.t == "Div" then
      for _, inner in ipairs(block.content) do out[#out + 1] = inner end
    else
      out[#out + 1] = block
    end
  end
  if #out == 0 then return nil end
  io.stderr:write(("  + %s -> native table\n"):format(png:match("[^/]+$")))
  return out
end
