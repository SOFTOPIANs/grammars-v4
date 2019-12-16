 import Foundation

print("Plain string")
NSLog("Interpolated string \(v), \(v.field)")

 /*let longStr = """
 Qwe
 Rty
 """*/

print("response = \(String(outer: value))")

print("BinaryExpr inside interpolation block '\(left ?? "")'")

let simpleMultiline = 
"""
fjkgfnldgfn
\(name ?? "")
fndjgnsjlgb
"""

let normalMultiline = 
 """
 <!DOCTYPE "; html PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
 <html>
 <head>
 <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
 <meta name="viewport" content="width=device-width,initial-scale=1">
 <link rel="stylesheet" href="stylesheet.css" type="text/css" media="all" />
 </head>
 <body>
 <h1>\(name ?? "")</h1>
 <h2>Getting Started</h2>
 <p>\(desc ?? "")</p>
 </body>
 </html>
 """