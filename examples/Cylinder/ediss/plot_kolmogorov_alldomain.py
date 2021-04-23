import pandas as pd
import matplotlib.pyplot as plt
import numpy as np


with open("ek_3D/maxloc_alldomain/kolmogorov.txt", 'r') as f1:
    M1 = pd.read_table(f1, sep='\s+')
with open("ek_homogeneous/maxloc_alldomain/kolmogorov.txt", 'r') as f2:
    M2 = pd.read_table(f2, sep='\s+')

plt.plot(M1['time'],M1['dxnk'],label=r"$\Delta x_1 / \eta_k 3D$", color='blue',  linewidth=2.0)
plt.plot(M2['time'],M2['dxnk'],'--',label=r"$\Delta x_1 / \eta_k$ ht", color='blue',  linewidth=2.0)

plt.plot(M1['time'],M1['dynk'],label=r"$\Delta x_2 / \eta_k$ 3D", color='green',  linewidth=2.0)
plt.plot(M2['time'],M2['dynk'],'--',label=r"$\Delta x_2 / \eta_k$ ht", color='green',  linewidth=2.0)

plt.plot(M1['time'],M1['dznk'],label=r"$\Delta x_3 / \eta_k$ 3D", color='red',  linewidth=2.0)
plt.plot(M2['time'],M2['dznk'],'--',label=r"$\Delta x_3 / \eta_k$ ht", color='red',  linewidth=2.0)

# Save figure
#plt.ylabel(r"$C_D,C_L$")
plt.xlabel(r"$t$")
plt.xlim(300, 450)
plt.ylim(5, 13)
plt.legend(loc='lower right')
plt.grid(True)
plt.savefig('kolmogorov_alldomain.pdf')
plt.show()