def distinct_powers_py(amin=2, amax=100, bmin=2, bmax=100):
    return len({pow(a,b) for a in range(amin, amax+1) for b in range(bmin, bmax+1)})

print(distinct_powers_py())  # 9183
