start
  = meaning

meaning
  = tokens:token*

token
  = t:tex { return ["tex", t] }
  / w:word { return ["word", w] }
  / s:spacing { return ["spacing", s] }

tex
  = "$$" chars:texChar* "$$" { return chars.join(""); }

texChar
  = !"$$" char:. { return char; }

word
  = chars:[A-Za-z0-9_'$]+ { return chars.join(""); }

spacing
  = spaces:[^A-Za-z0-9_'$]+ { return spaces.join(""); }
