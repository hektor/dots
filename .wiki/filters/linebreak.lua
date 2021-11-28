function RawInline (el)
  if el.format:match '^html' and el.text:match '%<br ?/?%>' then
    return pandoc.LineBreak()
  end
end
