start
  = meaning

meaning
  = tokens:token*

token
  = t:tex { return { kind: "tex", data: t }; }
  / i:image { return { kind: "image", data: i }; }
  / w:word { return { kind: "word", data: w }; }
  / s:spacing { return { kind: "spacing", data: s }; }

tex
  = "$$" chars:texChar* "$$" { return chars.join(""); }

texChar
  = !"$$" char:. { return char; }

image
  = "![" alt:($ [^\]]*) "](" src:($ [^)]*) ")" { return { alt: alt, src: src }; }

word
  = chars:[A-Za-z0-9_'$]+ { return chars.join(""); }

spacing
  = spaces:space+ { return spaces.join(""); }

space
  = !image char:[^A-Za-z0-9_'$] { return char; }
