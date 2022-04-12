import pyDOE2
import numpy as np

# 2 level Full Factorial
def ff2n(n):
    n = int(n)
    return pyDOE2.ff2n(n)
    
# 2 Level fractional Factorial, based on input STR
# Param: doeString: 'A B C', 'A B AB', etc
def fracFact(doeString):
  doeString = str(doeString)
  return pyDOE2.fracfact(doeString)

# 2 Central comosite deisgn.  3 or 5 levels.
# Params:
#     n: number of factors
#     CPFB: Center points for factorial block
#     CPSB: Center points for Star block
#     Face: How to gereate alpha or 'Star' Points
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

# z=pyDOE2.fracfact_by_res(5, 4)
# z=pyDOE2.fracfact("a b c abc")


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
    
def mlff(aList):
  # print(aList)
  aList = [int(x) for x in aList] 
  return pyDOE2.fullfact(aList)
  
# mlff([2.0, 2.0, 2.0])

# print(pyDOE2.fracfact_opt(n_factors=5, n_erased=2, max_attempts=10))

# print(z)
# print(pyDOE2.fracfact_aliasing(design=pyDOE2.fullfact([2,3,4])))

# 
# print(type(pyDOE2.fullfact([2,4,5])))
# 
# pyDOE2.fullfact([2,3,4])
# 
# 
# pyDOE2.pbdesign
# z= np.array(pyDOE2.fracfact_by_res(n=6,res=4))
# print(type(z))
# print(pyDOE2.fracfact_by_res(n=11, res=5))
