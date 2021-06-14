from py_server_beta import u, prim, pi, freevar, instantiate

# this is Pi|T: U| T -> T
UnitType = pi(u(), pi(freevar(0), freevar(0), 1), 0)

print(UnitType)
# now we want to apply this to T = String
StrType = prim("StrType")
print(StrType)

StrTypeToStrType = instantiate(UnitType["right"], StrType, 0)
print(StrTypeToStrType)
