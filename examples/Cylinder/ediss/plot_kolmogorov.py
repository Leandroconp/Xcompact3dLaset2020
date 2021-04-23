import pandas as pd
import matplotlib.pyplot as plt
import numpy as np


with open("ek_homogeneous/maxloc_alldomain/kolmogorov.txt", 'r') as f2:
    M2 = pd.read_table(f2, sep='\s+')
with open("ek_homogeneous/maxloc_halfdomain/kolmogorov.txt", 'r') as f1:
    M1 = pd.read_table(f1, sep='\s+')

plt.plot(M2['time'],M2['dxnk'],label=r"$\Delta x_1 / \eta_k$ - all domain", color='blue',  linewidth=2.0)
plt.plot(M1['time'],M1['dxnk'],'--',label=r"$\Delta x_1 / \eta_k$ - Vortex wake", color='blue',  linewidth=2.0)
plt.plot(M2['time'],M2['dynk'],label=r"$\Delta x_2 / \eta_k$ all domain", color='green',  linewidth=2.0)
plt.plot(M1['time'],M1['dynk'],'--',label=r"$\Delta x_2 / \eta_k$ Vortex wake", color='green',  linewidth=2.0)
plt.plot(M2['time'],M2['dznk'],label=r"$\Delta x_3 / \eta_k$ all domain", color='red',  linewidth=2.0)
plt.plot(M1['time'],M1['dznk'],'--',label=r"$\Delta x_3 / \eta_k$ Vortex wake", color='red',  linewidth=2.0)

# Save figure
#plt.ylabel(r"$C_D,C_L$")
plt.xlabel(r"$t$")
plt.xlim(300, 470)
plt.ylim(4, 12)
plt.legend(loc='lower right')
plt.grid(True)
plt.savefig('kolmogorov.pdf')
plt.show()