function Link(el)
  --Replace markdown extension with html extension
  if string.match(el.target, '.md$') then
    el.target = string.gsub(el.target, "%.md", "")
    --Add html extension
    el.target = el.target .. '.html'
  end
  return el
end
