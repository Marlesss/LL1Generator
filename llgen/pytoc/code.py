b :: Boolean
b = true
a :: Int
a = 12
for i in 1...3 {
  if b {
    printStr("abab\n")
  } else {
    printStr("abacaba\n")
  }
}

if 1 < 4 {
 if (4 != 6) and (5 == (1 + 4)) {
  printStr("ABC\n")
}
}

x :: Int
readInt(x)

printInt(x + 5 * 2 - 4)

while (x > 0) and (4 + 1 < 10) {
  printInt(x)
  x = x - 1
}