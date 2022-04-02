import pyDOE2

# print(pyDOE2.fracfact("a b c abc"))

# pyDOE2.pbdesign()

# pyDOE2.fullfact()

def ff2n(n):
    n = int(n)
    # print(n)
    return pyDOE2.ff2n(n)
    
    
def fracFact(doeString):
  doeString = str(doeString)
  # print(n)
  return pyDOE2.fracfact(doeString)


def ccd(n, CPFB, CPSB, alpha, face):
  n = int(n)
  # print(CPFB)
  # print(CPSB)
  # print(alpha)
#   print(face)
  if (alpha == "Orthogonal"):
      alpha = 'o'
  else :
      alpha = 'r'

  if (face == "Circumscribed (CCC)") :
      face = 'ccc'
  elif (face == "Inscribed (CCI)") :
      face = 'cci'
  else :
      face = 'ccf'
  
  center=[]
  center.append(CPFB)
  center.append(CPSB)

  # print(n)
  print(pyDOE2.ccdesign(n, center=center, alpha=alpha, face=face))
  return pyDOE2.ccdesign(n, center=center, alpha=alpha, face=face)

# ccd(5, 0,0,  'r','Inscribed (CCI)')

pyDOE2.fullfact([2,3,4])

# print(pyDOE2.ccdesign(5, [0,0], 'o', face='cci'))


