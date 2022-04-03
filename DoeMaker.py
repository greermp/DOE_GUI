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
  return pyDOE2.ccdesign(n, center=center, alpha=alpha, face=face)


def pbd(n):
    n = int(n)
    # print(n)
    return pyDOE2.pbdesign(n)

z=pyDOE2.fracfact_by_res(5, 4)
z=pyDOE2.fracfact("a b c abc")


def bbd(n, cp):
    n = int(n)
    cp = int(cp) 
    return pyDOE2.bbdesign(n, cp)

def lhc(n, samples, criterion):
    n = int(n)
    samples = int(samples)
    if criterion == "random":
        return pyDOE2.lhs(n, samples=samples)
    return pyDOE2.lhs(n, samples=samples, criterion=criterion)

# print(pyDOE2.fracfact_opt(n_factors=5, n_erased=2, max_attempts=10))

# print(z)
# print(pyDOE2.fracfact_aliasing(design=z))


print(pyDOE2.fullfact([2,4,5]))

pyDOE2.fullfact([2,3,4])


pyDOE2.pbdesign
# print(pyDOE2.ccdesign(5, [0,0], 'o', face='cci'))


